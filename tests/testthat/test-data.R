ev <- generate_login_data(seed = 42)
test_that("schema and valid values", {
  expect_true(all(c("event_id","user_id","timestamp","login_hour","day_of_week","status","device",
                    "location","period","is_injected_anomaly","anomaly_type") %in% names(ev)))
  expect_gte(nrow(ev), 2500); expect_lte(nrow(ev), 5000)
  expect_true(length(unique(ev$user_id)) %in% 20:30)
  expect_true(all(ev$status %in% c("success","failure")))
  expect_true(all(ev$login_hour %in% 0:23))
  expect_true(all(ev$day_of_week %in% c("Mon","Tue","Wed","Thu","Fri","Sat","Sun")))
  expect_true(all(ev$device %in% DEVICES)); expect_true(all(ev$location %in% LOCATIONS))
  expect_false(anyDuplicated(ev$event_id) > 0); expect_false(is.unsorted(ev$timestamp))
  expect_true(all(ev$data_note == "SYNTHETIC")); expect_false(anyNA(ev[c("user_id","timestamp","status")]))
})
test_that("anomalies injected only in evaluation period, at a modest rate", {
  expect_true(all(ev$period[ev$is_injected_anomaly] == "evaluation"))
  expect_setequal(unique(ev$anomaly_type[ev$is_injected_anomaly]), c("unusual_time","high_frequency","failed_burst"))
  rate <- mean(ev$is_injected_anomaly[ev$period == "evaluation"]); expect_true(rate > 0.01 && rate < 0.15)
})
test_that("reproducible with fixed seed, different with another", {
  expect_identical(ev, generate_login_data(seed = 42))
  expect_false(identical(ev$timestamp, generate_login_data(seed = 7)$timestamp))
})
test_that("CSV round trip", {
  f <- tempfile(fileext = ".csv"); w <- write_login_data(f, seed = 1); r <- read_login_data(f)
  expect_equal(nrow(r), nrow(w)); expect_equal(r$timestamp, w$timestamp); expect_equal(r$event_id, w$event_id)
})
