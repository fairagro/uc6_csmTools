file <- system.file("extdata/lte_seehausen/2_sim/PlantGro.OUT", package = "csmTools")

# we do expect the pipeline to run before the tests
# reason: the pipeline runs ~20min and writes multiple files
# devtools::check() or R CMD check . would trigger the pipeline each build...

expect_equal_by_last_digit <- function(actual, expected) {
    if (!is.numeric(actual) || !is.numeric(expected)) {
        stop("Both arguments must be numeric")
    }

    # Function to determine the number of decimal places
    get_decimal_places <- function(num) {
        if (num %% 1 == 0) {
            return(0)
        } else {
            return(nchar(strsplit(as.character(num), "\\.")[[1]][2]))
        }
    }

    # Determine the maximum number of decimal places between actual and expected
    decimal_places_actual <- get_decimal_places(actual)
    decimal_places_expected <- get_decimal_places(expected)
    max_decimal_places <- max(decimal_places_actual, decimal_places_expected)

    # Calculate the tolerance
    tolerance <- 10^(-max_decimal_places) * (1 + 1e-8) # floating point precision, multiply tolerance by 1.00000008
    difference <- abs(actual - expected)
    expect_true(difference <= tolerance, info = sprintf("Expected %s to differ from %s only in the last digit (%s) - difference is %s", actual, expected, tolerance, difference))
}

is_numeric <- function(inpt) {
    # suppress warning: NAs introduced by coercion 
    return (suppressWarnings(!is.na(as.numeric(inpt))))
}

test_that("pipeline_has_run", {
    expect_true(file.exists(file))
})

test_that("pipeline_file_is_valid", {
    output <- DSSAT::read_output(file_name = file)
    expect_no_error(output)
})

# test expect_equal_by_last_digit helper function
test_that("Values differ only in the last digit", {
    expect_equal_by_last_digit(7446, 7445)
    expect_equal_by_last_digit(530.9, 531.0)
    expect_equal_by_last_digit(1.934, 1.933)
    expect_equal_by_last_digit(1.0001, 1.0002)
    expect_equal_by_last_digit(1.000000001, 1.000000002)
    expect_equal_by_last_digit(0.018, 0.019)
})

test_that("PlantGro.OUT is plausible", {
    valid <- system.file("extdata/test_fixtures/PlantGro.OUT", package = "csmTools")

    expected <- DSSAT::read_output(file_name = valid)
    actual <- DSSAT::read_output(file_name = file)

    for (i in 1:nrow(expected)) {
        for (j in 1:ncol(expected)) {
            if (is_numeric(expected[i, j]) && is_numeric(actual[i, j])) {
                expect_equal_by_last_digit(as.numeric(actual[i, j]), as.numeric(expected[i, j]))
            } else {
                expect_equal(expected[i, j], actual[i, j])
            }
        }
    }
})
