test_that("confusion matrix and metrics match hand calculation", {
  # flags: 1 1 1 0 0 0 0 0 ; truth: 1 1 0 1 0 0 0 0 -> TP2 FP1 FN1 TN4
  m <- confusion_metrics(c(1,1,1,0,0,0,0,0) == 1, c(1,1,0,1,0,0,0,0) == 1)
  expect_equal(unlist(m[c("TP","FP","TN","FN")]), c(TP=2, FP=1, TN=4, FN=1))
  expect_equal(m$precision, 2/3); expect_equal(m$recall, 2/3); expect_equal(m$fpr, 1/5); expect_equal(m$accuracy, 6/8)
  expect_equal(m$flagged, 3)
})
test_that("undefined metrics are NA, never NaN/Inf", {
  m <- confusion_metrics(rep(FALSE, 5), rep(FALSE, 5)); expect_true(is.na(m$precision)); expect_true(is.na(m$recall))
  expect_equal(m$fpr, 0); expect_equal(m$accuracy, 1)
  m2 <- confusion_metrics(rep(TRUE, 3), rep(TRUE, 3)); expect_true(is.na(m2$fpr)); expect_equal(m2$precision, 1)
  m3 <- confusion_metrics(logical(0), logical(0)); expect_true(all(is.na(m3[c("precision","recall","fpr","accuracy")])))
  expect_equal(confusion_metrics(c(TRUE, NA), c(TRUE, TRUE))$TP, 1)   # NA pairs dropped
})
ev <- generate_login_data(seed = 42)
fit <- fit_baseline(ev[ev$period == "baseline", ])
test_that("threshold comparison uses one dataset and is computed, not assumed", {
  e <- ev$period == "evaluation"; feat <- compute_features(ev, fit)[e, ]
  r <- compare_profiles(feat, ev$is_injected_anomaly[e], threshold_profiles())
  expect_equal(r$profile, c("relaxed", "balanced", "strict")); expect_true(all(r$TP + r$FP + r$TN + r$FN == sum(e)))
  expect_true(all(diff(r$flagged) >= 0))
  r2 <- compare_profiles(feat, ev$is_injected_anomaly[e], list(a = list(hour_ratio = .2, z = 3, burst = 4)))
  expect_equal(r2$flagged, r$flagged[2])   # changing settings changes results consistently
})
test_that("filter_events and CSV export", {
  s <- score_events(ev, fit, threshold_profiles()$balanced); s$device <- ev$device
  f <- filter_events(s, users = "U01", status = "failure", flag = "all")
  expect_true(all(f$user_id == "U01" & f$status == "failure"))
  d <- as.Date(c("2025-04-10", "2025-04-12")); fd <- filter_events(s, date_range = d)
  expect_true(all(as.Date(fd$timestamp) >= d[1] & as.Date(fd$timestamp) <= d[2]))
  expect_true(all(filter_events(s, flag = "flagged")$flag)); expect_false(any(filter_events(s, flag = "normal")$flag))
  expect_equal(nrow(filter_events(s, search = "zzzz-none")), 0); expect_equal(nrow(filter_events(s[0, ])), 0)
  out <- tempfile(fileext = ".csv"); export_csv(f[, c("timestamp", "user_id", "flag", "reason")], out)
  r <- read.csv(out); expect_equal(nrow(r), nrow(f)); expect_false("is_injected_anomaly" %in% names(r))
})
test_that("app server logic works (testServer)", {
  skip_if_not_installed("shiny")
  old <- setwd(ROOT); on.exit(setwd(old))
  app <- source("app.R")$value
  shiny::testServer(app, {
    session$setInputs(profile = "balanced", f_user = NULL, f_status = NULL, f_flag = "all", f_search = "",
                      f_date = range(as.Date(EVENTS$timestamp[EVAL_IDX])),
                      c_hour = 0.2, c_z = 3, c_burst = 4)
    expect_equal(stats()$n_events, sum(EVAL_IDX))
    expect_equal(nrow(lab()), 4)
    session$setInputs(profile = "strict"); n_strict <- stats()$n_flagged
    session$setInputs(profile = "relaxed"); expect_lte(stats()$n_flagged, n_strict)
    session$setInputs(f_flag = "flagged"); expect_true(all(filtered()$flag))
    session$setInputs(c_burst = 2); expect_true(lab()$flagged[4] >= lab()$flagged[2])
  })
})
test_that("app starts and serves HTTP", {
  skip_if(Sys.which("curl") == "", "curl missing")
  port <- 4399; log <- tempfile()
  system(sprintf("Rscript -e \"shiny::runApp('%s', port=%d, launch.browser=FALSE)\" > %s 2>&1 & echo $! > %s.pid",
                 ROOT, port, log, log))
  on.exit(system(sprintf("kill $(cat %s.pid)", log), ignore.stdout = TRUE, ignore.stderr = TRUE))
  code <- "000"
  for (i in 1:30) { Sys.sleep(1)
    code <- suppressWarnings(system2("curl", c("-s", "-o", "/dev/null", "-w", "%{http_code}", sprintf("http://127.0.0.1:%d", port)), stdout = TRUE))
    if (identical(code, "200")) break }
  expect_identical(code, "200")
})
