#' @rdname Truth
#' @export
is_false <- function(x)
{
  x <- coerce_to(x, "logical")
  call_and_name(
    function(x) 
    {
      ok <- !x & !is.na(x)
      set_cause(ok, ifelse(is.na(x), "missing", "true"))
    }, 
    x
  )
}

#' @rdname Truth
#' @export
is_na <- function(x)
{
  # Do not coerce_to(x, "logical"); this breaks, e.g., character vectors
  call_and_name(
    function(x)
    {
      ok <- is.na(x)
      set_cause(ok, ifelse(x, "true", "false"))
    }, 
    x
  )
}

#' @rdname Truth
#' @export
is_not_na <- function(x)
{
  # Do not coerce_to(x, "logical"); this breaks, e.g., character vectors
  call_and_name(
    function(x)
    {
      ok <- !is.na(x)
      set_cause(ok, "missing")
    }, 
    x
  )
}

#' @rdname Truth
#' @export
is_not_false <- function(x)
{
  x <- coerce_to(x, "logical")
  call_and_name(
    function(x)
    {
      ok <- x | is.na(x)
      set_cause(ok, "false")
    }, 
    x
  )
}

#' @rdname Truth
#' @export
is_not_true <- function(x)
{
  x <- coerce_to(x, "logical")
  call_and_name(
    function(x)
    {
      ok <- !x | is.na(x)
      set_cause(ok, "true")
    }, 
    x
  )
}

#' @rdname Truth
#' @export
is_true <- function(x)
{
  x <- coerce_to(x, "logical")
  call_and_name(
    function(x) 
    {
      ok <- x & !is.na(x)
      set_cause(ok, ifelse(is.na(x), "missing", "false"))   
    }, 
    x
  )
}
