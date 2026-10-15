# Run all tests: Rscript tests/run_tests.R
setwd(if (file.exists("R/detector.R")) "." else ".."); 
res <- testthat::test_dir("tests/testthat", reporter = "summary", stop_on_failure = FALSE)
df <- as.data.frame(res)
cat(sprintf("\nTOTAL: %d expectations passed, %d failed, %d skipped, %d errors\n",
            sum(df$passed), sum(df$failed), sum(df$skipped), sum(df$error)))
quit(status = as.integer(sum(df$failed) + sum(df$error) > 0))
