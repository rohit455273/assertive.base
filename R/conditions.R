#' Condition classes
#' 
#' Error, warning, and message classes derived from their simple equivalents.
#' @param message A string describing the problem.
#' @param call A call describing the source of the condition.
#' @return An object of class \code{assertionError}, \code{assertionWarning}, or
#' \code{assertionMessage}.
#' @note These objects behave the same as the standard-issue \code{simpleError},
#' \code{simpleWarning}, and \code{simpleMessage} objects from base-R.  The
#' extra class allows you to provide custom handling for assertions inside 
#' \code{tryCatch}.
#' @examples 
#' tryCatch(
#'   assert_all_are_true(FALSE), 
#'   error = function(e) 
#'   {
#'     if(inherits(e, "assertionCondition"))
#'     {
#'       # Handle assertions
#'       message("This is an assertion condition.")
#'     } else
#'     {
#'       # Handle other error types
#'     }
#'   }
#' )
#' @export
assertionError <- function(message, call = NULL)
{
  class <- c("assertionError", "assertionCondtion", "simpleError", "error", "condition")
  structure(
    list(
      message = as.character(message), 
      call = call
    ), 
    class = class
  )
}

#' @rdname assertionError
#' @export
assertionWarning <- function(message, call = NULL)
{
  class <- c("assertionWarning", "assertionCondtion", "simpleWarning", "error", "condition")
  structure(
    list(
      message = as.character(message), 
      call = call
    ), 
    class = class
  )
}

#' @rdname assertionError
#' @export
assertionMessage <- function(message, call = NULL)
{
  class <- c("assertionMessage", "assertionCondtion", "simpleMessage", "message", "condition")
  structure(
    list(
      message = as.character(message), 
      call = call
    ), 
    class = class
  )
}

