test_that("confusion matrix and metrics match hand calculation", {
  # flags: 1 1 1 0 0 0 0 0 ; truth: 1 1 0 1 0 0 0 0 -> TP2 FP1 FN1 TN4
  m <- confusion_metrics(c(1,1,1,0,0,0,0,0) == 1, c(1,1,0,1,0,0,0,0) == 1)
  expect_equal(unlist(m[c("TP","FP","TN","FN")]), c(TP=2, FP=1, TN=4, FN=1))
  expect_equal(m$precision, 2/3); expect_equal(m$recall, 2/3); expect_equal(m$fpr, 1/5); expect_equal(m$accuracy, 6/8)
  expect_equal(m$flagged, 3)
  expect_equal(m$specificity, 4/5); expect_equal(m$f1, 2/3)       # F1 = 2PR/(P+R) = 2(2/3)(2/3)/(4/3)
  expect_equal(m$specificity, 1 - m$fpr)
  m4 <- confusion_metrics(c(TRUE, TRUE, FALSE, FALSE), c(TRUE, FALSE, FALSE, FALSE))  # TP1 FP1 TN2 FN0
  expect_equal(c(m4$precision, m4$recall, m4$f1), c(0.5, 1, 2/3))
})
test_that("F1 and specificity are NA when undefined", {
  m <- confusion_metrics(rep(FALSE, 4), rep(FALSE, 4)); expect_true(is.na(m$f1)); expect_equal(m$specificity, 1)
  m2 <- confusion_metrics(rep(TRUE, 3), rep(TRUE, 3)); expect_true(is.na(m2$specificity)); expect_equal(m2$f1, 1)
  m3 <- confusion_metrics(c(FALSE, TRUE), c(TRUE, FALSE)); expect_equal(m3$f1, NA_real_)   # P = R = 0 -> 0/0
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
    session$setInputs(f_search = "zzz-no-such-record"); expect_equal(nrow(filtered()), 0)
    f <- tempfile(); export_csv(filtered()[, show_cols], f); expect_equal(nrow(read.csv(f)), 0)
    session$setInputs(f_search = "")
    session$setInputs(c_burst = 2); expect_true(lab()$flagged[4] >= lab()$flagged[2])
  })
})
test_that("app starts and serves HTTP", {
  skip_on_os("windows")  # uses a Unix shell (&, $!, kill); the in-process testServer test above covers Windows
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
