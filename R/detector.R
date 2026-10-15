# Statistical detector for LoginLens. Uses ONLY: user_id, timestamp, status.
# Ground-truth labels are never read here (see DETECTOR_INPUT_COLS).
suppressPackageStartupMessages({ library(dplyr); library(lubridate) })

DETECTOR_INPUT_COLS <- c("event_id", "user_id", "timestamp", "status")

# Documented constants (see report / Insights tab for rationale)
BASELINE_PARAMS <- list(
  alpha      = 0.5,  # additive (Laplace-type) smoothing pseudo-count per hour bin
  bandwidth  = 1,    # circular Gaussian kernel sd, in hours: neighbouring hours share evidence
  min_events = 20,   # fewer baseline events -> fall back to pooled (global) hour profile
  sd_floor   = 1,    # at least 1 login/day of natural variation (avoids sd = 0 divisions)
  z_cap      = 10    # keeps scores finite
)
BURST_WINDOW_MIN <- 10  # trailing window for counting failed logins

# Threshold profiles. "relaxed" flags least, "strict" flags most (lower cut-offs).
#  hour_ratio: flag if P(hour | user) < hour_ratio / 24  (i.e. rarer than that fraction of uniform)
#  z:          flag if running daily-login z-score >= z
#  burst:      flag if >= burst failed logins within BURST_WINDOW_MIN minutes
threshold_profiles <- function() {
  list(relaxed  = list(hour_ratio = 0.10, z = 3.5, burst = 6),
       balanced = list(hour_ratio = 0.20, z = 3.0, burst = 4),
       strict   = list(hour_ratio = 0.30, z = 2.5, burst = 3))
}

# 24x24 circular Gaussian smoothing matrix; rows (and columns) sum to 1.
circular_kernel <- function(bandwidth = 1) {
  d <- outer(0:23, 0:23, function(a, b) { x <- abs(a - b); pmin(x, 24 - x) })
  W <- exp(-d^2 / (2 * bandwidth^2))
  W / rowSums(W)
}

# Smoothed hour probabilities from a vector of hours: P(h) = (K n)_h + alpha) / (N + 24 alpha)
hour_probs <- function(hours, alpha, bandwidth) {
  hours <- hours[!is.na(hours)]
  n <- tabulate(hours + 1L, nbins = 24)
  (as.vector(circular_kernel(bandwidth) %*% n) + alpha) / (length(hours) + 24 * alpha)
}

circ_mean_hour <- function(hours) {
  hours <- hours[!is.na(hours)]
  if (!length(hours)) return(NA_real_)
  a <- hours * 2 * pi / 24
  (atan2(sum(sin(a)), sum(cos(a))) * 24 / (2 * pi)) %% 24
}

# Fit per-user profiles from BASELINE events only.
fit_baseline <- function(baseline, params = BASELINE_PARAMS) {
  baseline <- baseline[!is.na(baseline$user_id) & !is.na(baseline$timestamp), , drop = FALSE]
  gh <- hour(baseline$timestamp)
  n_days <- if (nrow(baseline)) as.integer(diff(range(as.Date(baseline$timestamp)))) + 1L else 0L
  users <- sort(unique(baseline$user_id))
  per_user <- lapply(users, function(u) {
    b <- baseline[baseline$user_id == u, ]
    h <- hour(b$timestamp)
    daily <- as.vector(table(factor(as.Date(b$timestamp),
                                    levels = as.character(seq(min(as.Date(baseline$timestamp)), by = "day",
                                                              length.out = n_days)))))
    list(n = nrow(b), probs = hour_probs(h, params$alpha, params$bandwidth),
         circ_mean = circ_mean_hour(h), mean = mean(daily),
         sd = if (n_days >= 2) sd(daily) else NA_real_)
  })
  names(per_user) <- users
  list(params = params, n_days = n_days,
       global = hour_probs(gh, params$alpha, params$bandwidth), users = per_user)
}

# Threshold-independent features for every event (past-only: no look-ahead).
compute_features <- function(events, baseline_fit) {
  p <- baseline_fit$params
  miss <- setdiff(DETECTOR_INPUT_COLS, names(events))
  if (length(miss)) stop("Missing required columns: ", paste(miss, collapse = ", "))
  ev <- as.data.frame(events[, DETECTOR_INPUT_COLS, drop = FALSE])  # drops label columns by design
  n <- nrow(ev)
  out <- data.frame(ev, login_hour = integer(n), circ_dist_hours = numeric(n), hour_prob = numeric(n),
                    hour_surprisal_bits = numeric(n), day_count = integer(n), user_mean_daily = numeric(n),
                    user_sd_daily = numeric(n), freq_z = numeric(n), fail_burst_count = integer(n),
                    valid = logical(n), stringsAsFactors = FALSE)
  if (n == 0) return(out)
  out$valid <- !is.na(ev$user_id) & !is.na(ev$timestamp)
  out$login_hour <- hour(ev$timestamp)
  fail <- !is.na(ev$status) & ev$status == "failure"
  ord <- order(ev$user_id, ev$timestamp)
  grp <- paste(ev$user_id, as.Date(ev$timestamp))
  for (i in seq_len(n)) {
    u <- baseline_fit$users[[ev$user_id[i]]]
    ok <- out$valid[i] && !is.null(u) && u$n >= p$min_events
    pr <- if (ok) u$probs else baseline_fit$global
    h <- out$login_hour[i]
    out$hour_prob[i] <- if (out$valid[i]) pr[h + 1] else NA
    out$circ_dist_hours[i] <- if (ok) circ_dist(h, u$circ_mean) else NA
    out$user_mean_daily[i] <- if (ok) u$mean else NA
    out$user_sd_daily[i] <- if (ok) max(u$sd, p$sd_floor, na.rm = TRUE) else NA
  }
  out$hour_surprisal_bits <- -log2(out$hour_prob)
  # running count of today's logins (event is the k-th of its user-day), then z-score
  o <- order(grp, ev$timestamp)
  k <- integer(n); k[o] <- ave(seq_len(n), grp[o], FUN = seq_along)
  out$day_count <- ifelse(out$valid, k, NA)
  out$freq_z <- pmin((out$day_count - out$user_mean_daily) / out$user_sd_daily, p$z_cap)
  # failed logins by the same user in the trailing window (inclusive of this event)
  tsec <- as.numeric(ev$timestamp); W <- BURST_WINDOW_MIN * 60
  for (u in unique(ev$user_id[fail & out$valid])) {
    idx <- which(fail & out$valid & ev$user_id == u); idx <- idx[order(tsec[idx])]
    t <- tsec[idx]
    out$fail_burst_count[idx] <- seq_along(t) - findInterval(t - W, t)
  }
  out
}

# Apply a threshold profile -> anomaly_score (>=1 means flagged), flag, reason.
apply_thresholds <- function(feat, profile) {
  n <- nrow(feat)
  feat$anomaly_score <- numeric(n); feat$flag <- logical(n); feat$reason <- character(n)
  if (n == 0) return(feat)
  hour_thr_bits <- -log2(profile$hour_ratio / 24)
  r_hour  <- ifelse(is.na(feat$hour_surprisal_bits), 0, feat$hour_surprisal_bits / hour_thr_bits)
  r_freq  <- ifelse(is.na(feat$freq_z), 0, pmax(feat$freq_z, 0) / profile$z)
  r_burst <- feat$fail_burst_count / profile$burst
  feat$anomaly_score <- ifelse(feat$valid, pmax(r_hour, r_freq, r_burst), NA)
  feat$flag <- ifelse(feat$valid, feat$anomaly_score >= 1, FALSE)
  rs <- function(cond, txt) ifelse(cond, txt, "")
  parts <- cbind(
    rs(feat$valid & r_hour >= 1, sprintf("Rare login hour (p=%.4f, %.0fh from the user's usual time)",
                                         feat$hour_prob, feat$circ_dist_hours)),
    rs(feat$valid & r_freq >= 1, sprintf("High daily logins (#%s today, z=%.1f vs usual %.1f/day)",
                                         feat$day_count, feat$freq_z, feat$user_mean_daily)),
    rs(feat$valid & r_burst >= 1, sprintf("Failed-login burst (%s failures in %d min)",
                                          feat$fail_burst_count, BURST_WINDOW_MIN)))
  txt <- apply(parts, 1, function(r) paste(r[r != ""], collapse = "; "))
  feat$reason <- ifelse(!feat$valid, "Not scored: missing user or timestamp",
                        ifelse(feat$flag, paste0(txt, " - unusual, not a confirmed attack"), ""))
  feat
}

score_events <- function(events, baseline_fit, profile) {
  apply_thresholds(compute_features(events, baseline_fit), profile)
}
