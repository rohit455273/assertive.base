test_that(
  "assertionError returns a simpleError with extra classes", {
    expected <- structure(
      list(message = "Uh-oh!", call = NULL, predicate_name = NULL),
      class = c(
        "assertionError", "assertionCondition", 
        "simpleError", "error", "condition"
      )
    )
    actual <- assertionError("Uh-oh!")
    expect_identical(actual, expected)
  }
)

test_that(
  "assertionWarning returns a simpleWarning with extra classes", {
    expected <- structure(
      list(message = "Uh-oh!", call = NULL, predicate_name = NULL),
      class = c(
        "assertionWarning", "assertionCondition", 
        "simpleWarning", "warning", "condition"
      )
    )
    actual <- assertionWarning("Uh-oh!")
    expect_identical(actual, expected)
  }
)

test_that(
  "assertionMessage returns a simpleMessage with extra classes", {
    expected <- structure(
      list(message = "Uh-oh!", call = NULL, predicate_name = NULL),
      class = c(
        "assertionMessage", "assertionCondition", 
        "simpleMessage", "message", "condition"
      )
    )
    actual <- assertionMessage("Uh-oh!")
    expect_identical(actual, expected)
  }
)