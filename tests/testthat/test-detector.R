ev <- generate_login_data(seed = 42)
base <- ev[ev$period == "baseline", ]
fit <- fit_baseline(base)
prof <- threshold_profiles()
mk <- function(user, ts, status = "success") data.frame(event_id = paste0("X", seq_along(ts)), user_id = user,
  timestamp = as.POSIXct(ts, tz = "UTC"), status = status, stringsAsFactors = FALSE)

test_that("circular distance wraps around midnight", {
  expect_equal(circ_dist(23, 1), 2); expect_equal(circ_dist(0, 12), 12); expect_equal(circ_dist(5, 5), 0)
  expect_equal(circ_dist(c(1, 22), 23), c(2, 1))
})
test_that("hour probabilities are valid, smoothed and circular", {
  p <- hour_probs(c(rep(23, 10)), 0.5, 1)
  expect_equal(sum(p), 1); expect_true(all(p > 0))
  expect_gt(p[1], p[13])            # hour 0 borrows evidence from neighbour 23
  expect_equal(hour_probs(integer(0), 0.5, 1), rep(1/24, 24))   # empty history -> uniform
  expect_equal(sum(hour_probs(c(1, NA, 2), 0.5, 1)), 1)
})
test_that("circular mean handles wrap", {
  expect_equal(circ_dist(circ_mean_hour(c(23, 1)), 0), 0, tolerance = 1e-9); expect_true(is.na(circ_mean_hour(integer(0))))
})
test_that("baseline uses baseline rows only", {
  ev2 <- ev; ev2$timestamp[ev2$period == "evaluation"] <- ev2$timestamp[ev2$period == "evaluation"] + 7200
  expect_identical(fit_baseline(ev2[ev2$period == "baseline", ]), fit)
})
test_that("labels never influence detection", {
  feat <- compute_features(ev, fit)
  shuffled <- ev; shuffled$is_injected_anomaly <- sample(shuffled$is_injected_anomaly)
  shuffled$anomaly_type <- NULL
  expect_identical(compute_features(shuffled, fit), feat)
  expect_identical(compute_features(ev[, DETECTOR_INPUT_COLS], fit), feat)
  expect_false(any(c("is_injected_anomaly", "anomaly_type") %in% names(feat)))
  expect_false(any(grepl("anomaly|truth|label", deparse(body(compute_features)), ignore.case = TRUE) &
                     grepl("is_injected", deparse(body(compute_features)))))
})
test_that("scores are finite, flags consistent with score, reasons present", {
  s <- score_events(ev, fit, prof$balanced)
  expect_true(all(is.finite(s$anomaly_score))); expect_true(all(s$flag == (s$anomaly_score >= 1)))
  expect_true(all(nzchar(s$reason[s$flag]))); expect_true(all(!nzchar(s$reason[!s$flag])))
  expect_true(all(grepl("not a confirmed attack", s$reason[s$flag])))
})
test_that("stricter profiles flag at least as many events (monotone thresholds)", {
  n <- sapply(prof, function(p) sum(score_events(ev, fit, p)$flag))
  expect_true(n["relaxed"] <= n["balanced"] && n["balanced"] <= n["strict"])
})
test_that("burst detection counts failures in trailing window only", {
  u <- base$user_id[1]
  e <- mk(u, c("2025-05-01 03:00:00","2025-05-01 03:01:00","2025-05-01 03:02:00","2025-05-01 03:03:00","2025-05-01 05:00:00"), "failure")
  f <- compute_features(e, fit)
  expect_equal(f$fail_burst_count, c(1, 2, 3, 4, 1))
  expect_true(score_events(e, fit, prof$balanced)$flag[4])
  expect_match(score_events(e, fit, prof$balanced)$reason[4], "burst")
  expect_equal(compute_features(mk(u, "2025-05-01 03:00:00", "success"), fit)$fail_burst_count, 0)
})
test_that("unusual hour flagged, usual hour not, midnight wrap OK", {
  u <- names(fit$users)[1]; usual <- round(fit$users[[u]]$circ_mean); far <- (usual + 12) %% 24
  f <- compute_features(mk(u, sprintf("2025-05-01 %02d:30:00", c(usual, far))), fit)
  expect_gt(f$hour_prob[1], f$hour_prob[2]); expect_equal(f$circ_dist_hours[2], 12, tolerance = 1.5)
  s <- apply_thresholds(f, prof$strict); expect_false(s$flag[1]); expect_true(s$flag[2])
})
test_that("frequency z-score: running count, zero variance, insufficient history", {
  u <- names(fit$users)[1]
  e <- mk(u, sprintf("2025-05-01 12:%02d:00", 0:14)); f <- compute_features(e, fit)
  expect_equal(f$day_count, 1:15); expect_true(all(diff(f$freq_z) > 0)); expect_true(f$freq_z[15] <= 10)
  # zero variance baseline: every day exactly 2 logins -> sd 0, floor keeps z finite
  zb <- mk("Z", rep(sprintf("2025-04-%02d 10:00:00", 1:20), each = 2))
  zfit <- fit_baseline(zb); expect_equal(zfit$users$Z$sd, 0)
  zf <- compute_features(mk("Z", sprintf("2025-05-01 10:%02d:00", 0:5)), zfit)
  expect_true(all(is.finite(zf$freq_z))); expect_equal(zf$user_sd_daily[1], 1)
  # unknown user -> pooled hour profile, no frequency evidence, no NaN/Inf
  nf <- compute_features(mk("NEWUSER", "2025-05-01 03:00:00"), fit)
  expect_equal(nf$hour_prob, fit$global[4]); expect_true(is.na(nf$freq_z))
  expect_equal(apply_thresholds(nf, prof$strict)$anomaly_score >= 0, TRUE)
})
test_that("missing and empty input handled", {
  expect_equal(nrow(score_events(ev[0, ], fit, prof$balanced)), 0)
  e <- mk("U01", c("2025-05-01 10:00:00", NA)); e$user_id[1] <- NA
  s <- score_events(e, fit, prof$balanced)
  expect_false(any(s$flag)); expect_true(all(is.na(s$anomaly_score))); expect_match(s$reason[1], "Not scored")
  e2 <- mk("U01", "2025-05-01 10:00:00", NA); expect_false(is.na(score_events(e2, fit, prof$balanced)$anomaly_score))
  expect_error(compute_features(data.frame(a = 1), fit), "Missing required")
  empty_fit <- fit_baseline(base[0, ]); expect_equal(empty_fit$global, rep(1/24, 24))
  expect_true(all(is.finite(score_events(ev[1:20, ], empty_fit, prof$balanced)$anomaly_score)))
})
