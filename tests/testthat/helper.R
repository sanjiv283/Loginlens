ROOT <- normalizePath(file.path(testthat::test_path(), "..", ".."))
for (f in c("generate_data", "detector", "evaluate")) source(file.path(ROOT, "R", paste0(f, ".R")))
