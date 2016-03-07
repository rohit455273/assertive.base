test_that("test.coerce_to.numeric_vector_to_data_frame.returns_data_frame", 
  {
    x <- 1:5
    expected <- data.frame(x = x)
    expect_equal(suppressWarnings(coerce_to(x, "data.frame")), expected)
    expect_warning(coerce_to(x, "data.frame"))
  })

test_that(
  "dont_stop with multiple errors and warnings successfully runs",
  {
    expected <- list(
      'stop("If you don\'t stop;")' = simpleError("If you don't stop;"),
      'warning("Someone\'s gonna find yo\' ass dead (this is a warning)")' = simpleWarning("Someone's gonna find yo' ass dead (this is a warning)"),
      'warning("Someone\'s gonna poison your food (this is a warning)")' = simpleWarning("Someone's gonna poison your food (this is a warning)"),
      'stop("Don\'t stop, no no, you\'ll be sorry")' = simpleError("Don\'t stop, no no, you\'ll be sorry"),
      'stop("Don\'t stop, thinking about tomorrow")' = simpleError("Don't stop, thinking about tomorrow"),
      'stop("Don\'t stop, it\'ll soon be here")' = simpleError("Don't stop, it'll soon be here")
    )
    actual <- dont_stop(
      {
        # With apologies to Lil' Kim
        stop("If you don't stop;")
        warning("Someone's gonna find yo' ass dead (this is a warning)")
        warning("Someone's gonna poison your food (this is a warning)")
        stop("Don't stop, no no, you'll be sorry")
        
        # Bonus errors for David, Jenny and other Fleetwood Mac fans
        stop("Don't stop, thinking about tomorrow")
        stop("Don't stop, it'll soon be here")
      }
    )
    expect_identical(actual, expected)
  }
)

test_that(
  "dont_stop works with objects that don't deparse to a single string",
  {
    expected <- list("function() {}" = function() {})
    actual <- dont_stop(
      # deparse returns a character vector 
      function() {} 
    )
    # don't test for identicality due to function environment
    expect_equal(actual, expected)
  }
)


test_that(
  "test.parenthesise.character_input.returns_parenthesised_input",  
  {
    x <- "foo"
    types <- eval(formals(parenthesise)$type)
    actual <- vapply(
      types,
      function(type) parenthesise(x, type),
      character(1),
      USE.NAMES = FALSE
    )
    expected <- c(
      "(foo)", "[foo]", "{foo}", "<foo>", "\u3008foo\u3009", 
      "- foo -", "\u2013 foo \u2013", "\u2014foo\u2014", ", foo, "
    )
    expect_identical(actual, expected)
  }
)

test_that("test.use_first.a_list_double_indexing.returns_contents_of_first_element", 
  {
    x <- as.list(letters)
    expected <- "a"
    expect_identical(expected, suppressWarnings(use_first(x)))
    expect_warning(use_first(x))
  })

test_that("test.use_first.a_list_single_indexing.returns_first_element", {
  x <- as.list(letters)
  expected <- list("a")
  expect_identical(expected, suppressWarnings(use_first(x, "[")))
  expect_warning(use_first(x, "["))
})

test_that("test.use_first.a_scalar.returns_x", {
  x <- "a"
  expected <- x
  expect_identical(expected, use_first(x))
})

test_that("test.use_first.a_vector_double_indexing.returns_first_element", 
  {
    x <- letters
    expected <- "a"
    expect_identical(expected, suppressWarnings(use_first(x)))
    expect_warning(use_first(x))
  })

test_that("test.use_first.a_vector_single_indexing.returns_first_element", 
  {
    x <- letters
    expected <- "a"
    expect_identical(expected, suppressWarnings(use_first(x, "[")))
    expect_warning(use_first(x, "["))
  })

test_that("test.use_first.empty.throws_error", {
  x <- NULL
  expect_error(use_first(x))
}) 
