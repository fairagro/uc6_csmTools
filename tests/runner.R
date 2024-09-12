devtools::install_local(".", force=TRUE)
test_files <- list.files("tests/testthat", pattern = "^test.*\\.R$", full.names = TRUE)

# Run only the included test files
for (test_file in test_files) {
  testthat::test_file(test_file)
}