#' Wrapper to vapply that returns booleans.
#' 
#' Wrapper to \code{\link{vapply}} for functions that return a boolean (logical 
#' scalar) value.
#' 
#' @param x A vector (atomic or list).
#' @param predicate A predicate (function that returns a bool) to apply.
#' elementwise to \code{x}.
#' @param ... Passed to \code{vapply}.
#' @return A logical vector.
#' @note \code{USE.NAMES} is set to \code{TRUE}
#' @seealso \code{\link{vapply}}.
bapply <- function(x, predicate, ...)
{
  vapply(x, predicate, logical(1L), ..., USE.NAMES = TRUE)
}

#' Call a function, and give the result names.
#'
#' Calls a function, and names the result with the first argument.
#'
#' @param fn A function to call.  See note below.
#' @param x The first input to \code{fn}.
#' @param ... Optional additional inputs to \code{fn}.
#' @return The result of \code{fn(x, ...)}, with names given by the
#' argument \code{x}.
#' @note The function, \code{fn}, should return an object with the 
#' same length as the input \code{x}.
#' @examples
#' \dontrun{
#' call_and_name(is.finite, c(1, Inf, NA))
#' }
#' @seealso \code{\link{cause}} and \code{\link{na}}.
call_and_name <- function(fn, x, ...)
{
  y <- fn(x, ...)
  if(!is_identical_to_true(length(y) == length(x)))
  {
    warning(
      "Vector of names is different length to results.  Trying to resize."
    )
    length(x) <- length(y)
  }
  dim(y) <- dim(x)
  names(y) <- x
  y
}

#' Print a variable and capture the output
#' 
#' Prints a variable and captures the output, collapsing the value to a single 
#' string.
#' @param x A variable.
#' @return A string.
#' @seealso \code{\link[base]{print}}, \code{\link[utils]{capture.output}}
#' @examples
#' \dontrun{
#' # This is useful for including data frames in warnings or errors
#' message("This is the CO2 dataset:\n", print_and_capture(CO2))
#' }
print_and_capture <- function(x)
{
  paste(capture.output(print(x)), collapse = "\n")
}

#' Truncate a string
#' 
#' Truncates a character vector to have a maximum length.
#' @param x A character vector, or something coercible to one.
#' @param width A positive integer.
#' @return A character vector
#' @examples
#' \dontrun{
#' truncate(c("abcd", "efghi", "jklmno", "pqrstuv"), 5)
#' }
truncate <- function(x, width = getOption("width"))
{
  x <- as.character(x)
  ifelse(
    nchar(x) > width,
    # paste0(substring(x, 1, width - 1), "\u2026") would be better, but some
    # setups don't display unicode properly.
    paste0(substring(x, 1, width - 3), "..."),
    x
  )
} 

