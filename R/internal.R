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
#' @importFrom utils capture.output
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

