test_that(
  "test is_false with a logical input returns true when false",
  {
    x <- c(TRUE, FALSE, NA)
    expected <- c(FALSE, TRUE, FALSE)
    actual <- assertive.base::is_false(x)
    expect_equal(strip_attributes(actual), expected)
    expect_equal(names(actual), as.character(x))
    expect_equal(cause(actual), noquote(c("true", "", "missing")))
  }
)

test_that(
  "test is_na with a logical input returns true when NA", 
  {
    x <- c(TRUE, FALSE, NA)
    expected <- c(FALSE, FALSE, TRUE)
    actual <- is_na(x)
    expect_equal(strip_attributes(actual), expected)
    expect_equal(names(actual), as.character(x))
    expect_equal(cause(actual), noquote(c("true", "false", "")))
  }
) 

test_that(
  "test is_not_false with a logical input returns true when not false", 
  {
    x <- c(TRUE, FALSE, NA)
    expected <- c(TRUE, FALSE, TRUE)
    actual <- is_not_false(x)
    expect_equal(strip_attributes(actual), expected)
    expect_equal(names(actual), as.character(x))
    expect_equal(cause(actual), noquote(c("", "false", "")))
  }
)

test_that(
  "test is_not_na with a logical input returns true when not NA", 
  {
    x <- c(TRUE, FALSE, NA)
    expected <- c(TRUE, TRUE, FALSE)
    actual <- is_not_na(x)
    expect_equal(strip_attributes(actual), expected)
    expect_equal(names(actual), as.character(x))
    expect_equal(cause(actual), noquote(c("", "", "missing")))
  }
) 

test_that(
  "test is_not_true with a logical input returns true when not true", 
  {
    x <- c(TRUE, FALSE, NA)
    expected <- c(FALSE, TRUE, TRUE)
    actual <- is_not_true(x)
    expect_equal(strip_attributes(actual), expected)
    expect_equal(names(actual), as.character(x))
    expect_equal(cause(actual), noquote(c("true", "", "")))
  }
) 

test_that(
  "test is_true with a logical input returns true when true", 
  {
    x <- c(TRUE, FALSE, NA)
    expected <- c(TRUE, FALSE, FALSE)
    actual <- assertive.base::is_true(x)
    expect_equal(strip_attributes(actual), expected)
    expect_equal(names(actual), as.character(x))
    expect_equal(cause(actual), noquote(c("", "false", "missing")))
  }
) 
