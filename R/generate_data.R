# Synthetic login-event generator for LoginLens.
# ALL DATA IS FICTIONAL. No real login records are used or required.
# Regenerate: Rscript R/generate_data.R   (writes data/login_events.csv)

suppressPackageStartupMessages(library(lubridate))

LABEL_COLS <- c("is_injected_anomaly", "anomaly_type")  # evaluation-only columns
DEVICES    <- c("Laptop", "Phone", "Tablet", "Desktop")
LOCATIONS  <- c("Northvale", "Eastbrook", "Southport", "Westfield", "Lakemont", "Ironridge")

# Circular distance between hours on a 24h clock (max 12).
circ_dist <- function(a, b, period = 24) {
  d <- abs(a - b) %% period
  pmin(d, period - d)
}

generate_login_data <- function(seed = 42, n_users = 25, baseline_days = 30, eval_days = 30,
                                start_date = as.Date("2025-03-03"),
                                n_unusual_time = 30, n_freq_days = 6, n_bursts = 6,
                                benign_odd_hour_p = 0.10,  # normal logins at a random hour
                                benign_streak_p = 0.01) {  # normal mistyped-password streaks
  set.seed(seed)
  users <- data.frame(
    user_id    = sprintf("U%02d", seq_len(n_users)),
    mean_hour  = ifelse(runif(n_users) < 0.8, runif(n_users, 8, 15), runif(n_users, 19, 23.5)),
    sd_hour    = runif(n_users, 1.2, 3),
    rate       = runif(n_users, 0.8, 3.5),   # mean logins per weekday
    fail_p     = runif(n_users, 0.02, 0.08),
    device1    = sample(DEVICES, n_users, TRUE),
    device2    = sample(DEVICES, n_users, TRUE),
    home       = sample(LOCATIONS, n_users, TRUE),
    stringsAsFactors = FALSE
  )
  total_days <- baseline_days + eval_days
  dates <- start_date + seq_len(total_days) - 1

  draw_hour <- function(u, n) {
    odd <- runif(n) < benign_odd_hour_p
    h <- round(rnorm(n, u$mean_hour, u$sd_hour)) %% 24
    h[odd] <- sample(0:23, sum(odd), TRUE)
    h
  }
  mk <- function(u, day, hour, status, ts = NULL) {
    n <- length(hour)
    if (is.null(ts)) ts <- as.POSIXct(day, tz = "UTC") + hour * 3600 + sample(0:3599, n, TRUE)
    data.frame(user_id = u$user_id, timestamp = ts, status = status,
               device = ifelse(runif(n) < 0.8, u$device1, u$device2),
               location = ifelse(runif(n) < 0.95, u$home, sample(LOCATIONS, n, TRUE)),
               stringsAsFactors = FALSE)
  }

  rows <- list()
  for (i in seq_len(n_users)) {
    u <- users[i, ]
    for (d in seq_along(dates)) {
      mu <- u$rate * ifelse(wday(dates[d], week_start = 1) >= 6, 0.6, 1)
      n <- rnbinom(1, size = 4, mu = mu)  # over-dispersed counts
      if (n == 0) next
      ev <- mk(u, dates[d], draw_hour(u, n), ifelse(runif(n) < u$fail_p, "failure", "success"))
      # benign streaks: 2-3 quick failures then a success (normal but looks suspicious)
      s <- which(runif(n) < benign_streak_p)
      for (k in s) {
        m <- sample(2:3, 1)
        ts <- ev$timestamp[k] - cumsum(runif(m, 30, 120))
        st <- mk(u, dates[d], rep(0, m), "failure", ts)
        ev$status[k] <- "success"
        ev <- rbind(ev, st)
      }
      rows[[length(rows) + 1]] <- ev
    }
  }
  ev <- do.call(rbind, rows)
  ev$is_injected_anomaly <- FALSE
  ev$anomaly_type <- NA_character_

  # ---- inject anomalies into the EVALUATION period only ----
  eval_dates <- dates[(baseline_days + 1):total_days]
  inject <- function(df, type) {
    df$is_injected_anomaly <- TRUE; df$anomaly_type <- type; df
  }
  pick <- function() list(u = users[sample(n_users, 1), ], day = sample(eval_dates, 1))
  inj <- list()
  for (j in seq_len(n_unusual_time)) {          # unusual login hour (4-12h from usual)
    p <- pick()
    h <- round(p$u$mean_hour + sample(c(-1, 1), 1) * runif(1, 4, 12)) %% 24
    inj[[length(inj) + 1]] <- inject(mk(p$u, p$day, h, "success"), "unusual_time")
  }
  for (j in seq_len(n_freq_days)) {             # high daily frequency (3-8 extra logins)
    p <- pick(); k <- sample(3:8, 1)
    inj[[length(inj) + 1]] <- inject(mk(p$u, p$day, draw_hour(p$u, k), "success"), "high_frequency")
  }
  for (j in seq_len(n_bursts)) {                # failed-login burst (3-9 failures in 8 min)
    p <- pick(); k <- sample(3:9, 1)
    t0 <- as.POSIXct(p$day, tz = "UTC") + (round(p$u$mean_hour) %% 24) * 3600
    inj[[length(inj) + 1]] <- inject(mk(p$u, p$day, rep(0, k), "failure",
                                        t0 + sort(runif(k, 0, 480))), "failed_burst")
  }
  ev <- rbind(ev, do.call(rbind, inj))

  ev$timestamp <- as.POSIXct(floor(as.numeric(ev$timestamp)), origin = "1970-01-01", tz = "UTC")  # whole seconds
  ev <- ev[order(ev$timestamp, ev$user_id), ]
  ev$event_id   <- sprintf("E%05d", seq_len(nrow(ev)))
  ev$login_hour <- hour(ev$timestamp)
  ev$day_of_week <- as.character(wday(ev$timestamp, label = TRUE, abbr = TRUE, week_start = 1,
                                      locale = "C"))
  ev$period <- ifelse(as.Date(ev$timestamp) < start_date + baseline_days, "baseline", "evaluation")
  ev$data_note <- "SYNTHETIC"
  rownames(ev) <- NULL
  ev[, c("event_id", "user_id", "timestamp", "login_hour", "day_of_week", "status", "device",
         "location", "period", "data_note", LABEL_COLS)]
}

write_login_data <- function(path = "data/login_events.csv", ...) {
  ev <- generate_login_data(...)
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  out <- ev; out$timestamp <- format(out$timestamp, "%Y-%m-%d %H:%M:%S", tz = "UTC")
  write.csv(out, path, row.names = FALSE)
  invisible(ev)
}

read_login_data <- function(path = "data/login_events.csv") {
  ev <- read.csv(path, stringsAsFactors = FALSE)
  ev$timestamp <- ymd_hms(ev$timestamp, tz = "UTC")
  ev
}

if (sys.nframe() == 0 && !interactive()) {
  ev <- write_login_data()
  cat("Wrote data/login_events.csv:", nrow(ev), "synthetic events\n")
}
