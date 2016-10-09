merge.NULL <- function(x, y, ...)
{
  return(y)
}

names_never_null <- function(x)
{
  nms <- names(x)
  if(is.null(nms))
  {
    nms <- character(length(x))
  }  
  nms
}

#' Truncate a string
#' 
#' Truncates a character vector to have a maximum length.
#' @param x A character vector, or something coercible to one.
#' @param width A positive integer.
#' @return A character vector
#' @seealso \code{\link[base]{abbreviate}}
#' @examples
#' assertive.base:::truncate(c("abcd", "efghi", "jklmno", "pqrstuv"), 5)
#' @noRd
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

