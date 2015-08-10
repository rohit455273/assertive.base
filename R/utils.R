#' Wrapper to vapply that returns booleans
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
#' @export
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
#' same length as the input \code{x}.  For speed and simplicity, this
#' isn't checked; it is up to the developer of the assertion to make
#' sure that this condition holds.
#' @examples
#' \dontrun{
#' call_and_name(is.finite, c(1, Inf, NA))
#' }
#' @seealso \code{\link{cause}} and \code{\link{na}}.
#' @export
call_and_name <- function(fn, x, ...)
{
  y <- fn(x, ...)
  dim(y) <- dim(x)
  names(y) <- x
  y
}

#' Get the dimensions of an object
#' 
#' Get the dimensions of an object, retuning the length if that object has no
#' \code{dim} attribute.
#' @param x Any object.
#' @return A integer vector of non-negative values.
#' @seealso \code{\link[base]{NROW}}, \code{\link[base]{dim}}
#' @examples
#' # For data frames and matrices, DIM is the same as dim.
#' DIM(sleep) 
#' # For vectors (and other objects without a dim attribute), DIM is the 
#' # same as length.
#' DIM(1:10)
#' DIM(list(x = 1:10))
#' @export
DIM <- function(x)
{
  dim_x <- dim(x)
  if(is.null(dim_x)) length(x) else dim_x
}

#' Run code without stopping
#' 
#' Runs code without stopping, warnings and errors are only printed.
#' @param ... Passed to \code{tryCatch}.
#' @return The expression that was passed in is run.
#' @note This function is dangerous, since it overrides warnings and errors.
#' Its intended use is for documenting examples of errors.
#' @examples
#' dont_stop(warning("!!!"))
#' dont_stop(stop("!!!"))
#' f <- function() g()
#' g <- function() stop("!!!")
#' dont_stop(f())
#' @export
dont_stop <- function(...)
{
  # The expression, without dont_stop().
  cl <- sys.call()[[2]]
  p <- function(e) 
  {
    # If the error call claims to be to doTryCatch, then nothing interesting
    # was captured, so use the parent call that we captured earlier.
    if(identical(e$call[[1]], as.name("doTryCatch")))
    {
      e$call <- cl
    }
    print(e)
  }
  tryCatch(..., warning = p, error = p)
}

#' Get the name of a variable in the parent frame
#'
#' Gets the name of the input in the parent frame.
#'
#' @param x Variable to get the name of.
#' @return A string giving the name of the input in the parent frame.
#' @export
get_name_in_parent <- function(x)
{  
  paste0(
    deparse(
      do.call(
        substitute, 
        list(substitute(x), parent.frame())
      )
    ),
    collapse = ""
  )
}

#' Merge two lists
#' 
#' Merges two lists, taking duplicated elements from the first list.
#' @param x A list.
#' @param y A list.
#' @param ... Ignored.
#' @return A list, combining elements from \code{x} and \code{y}.
#' @seealso \code{\link{merge_dots_with_list}}, \code{\link[base]{merge}}
#' @examples
#' merge(
#'   list(foo = 1, bar = 2, baz = 3), 
#'   list(foo = 4, baz = 5, quux = 6)
#' )
#' @method merge list
#' @export
merge.list <- function(x, y, ...)
{
  if(is.null(y)) return(x)
  y <- coerce_to(y, "list")
  all_names <- c(names(x), names(y))
  all_values <- c(x, y)
  if(anyDuplicated(all_names) > 0)
  {
    warning(
      sprintf(
        "Duplicated arguments: %s", 
        toString(all_names[duplicated(all_names)])
      )
    )
    all_values <- all_values[!duplicated(all_names)]
  }
  all_values
}

merge.NULL <- function(x, y, ...)
{
  return(y)
}

#' Merge ellipsis args with a list.
#'
#' Merges variable length ellipsis arguments to a function with a list argument.
#'
#' @param ... Some inputs.
#' @param l A list.
#' @note If any arguments are present in both the \code{...} and \code{l} 
#' arguments, the \code{...} version takes preference, and a warning is thrown.
#' @return A list containing the merged inputs.
#' @seealso \code{\link{merge.list}}, \code{\link[base]{merge}}
#' @examples
#' merge_dots_with_list(
#'   foo = 1, 
#'   bar = 2, 
#'   baz = 3, 
#'   l = list(foo = 4, baz = 5, quux = 6)
#' )
#' @export
merge_dots_with_list <- function(..., l = list())
{
  dots <- list(...)
  l <- coerce_to(l, "list")
  merge(dots, l)
}

#' Get the number of elements
#' 
#' Gets the number of elements in an object.
#' @param x Any object.
#' @return A non-negative integer of the number of elements.
#' @note For atomic objects, the number of elements is the product of the
#' dimensions, as calculated by \code{\link{DIM}}.  For recursive objects,
#' the number of elements is the sum of the number of elements of each of
#' their atomic components.
#' @seealso \code{\link{DIM}}
#' @examples
#' n_elements(1:10)
#' n_elements(NULL)
#' n_elements(data.frame(x = 1:5, y = rnorm(5)))
#' n_elements(list(1:5, list(1:3, list(1:7))))
#' n_elements(var) # depends upon the length of the body
#' @export
n_elements <- function(x)
{
  if(is.recursive(x))
  {
    sum(vapply(x, n_elements, integer(1)))
  } else
  {
    as.integer(prod(DIM(x)))
  }  
}

#' Wrap a string in brackets
#'
#' Parenthesise a character vector by wrapping elements in brackets, 
#' dashes or commas.
#' @param x Character vector to wrap in parenthenses.
#' @param type String naming the type of parenthesis.
#' @return A character vector of the input wrapped in parentheses.
#' @note English grammar terminology is awfully confusing.  The verb 'to 
#' parenthesise' means to wrap a phrase in brackets or dashes or commas,
#' thus denoting it as supplementary material that could be left out.
#' A 'parenthesis' as a noun is often used as a synonym for a round bracket.
#' @seealso \code{\link[base]{sQuote}}
#' @examples
#' paste("There were three", parenthesise(3), "mice in the experiment.")
#' paste(
#'   "I love parmos", 
#'   parenthesise("Teesside's finest culinary invention", "en_dashes"), 
#'   "but they are sure to give me heart disease."
#' )
#' parenthesise(letters[1:5], "curly")
#' paste0(
#'   "The R language", 
#'   parenthesise("an offshoot of S and Scheme", "commas"), 
#'   "is quite good for data analysis."
#' )
#' @export
parenthesize <- function(x, 
  type = c("round_brackets", "square_brackets", "curly_brackets", "angle_brackets", "chevrons", "hyphens", "en_dashes", "em_dashes", "commas")) 
{
  type <- match.arg(type)
  x <- coerce_to(x, "character")
  before <- switch(
    type,
    round_brackets  = "(",
    square_brackets = "[",
    curly_brackets  = "{",
    angle_brackets  = "<",
    chevrons        = "\u3008",
    hyphens         = "- ",
    en_dashes       = "\u2013 ",
    em_dashes       = "\u2014",
    commas          = ", "
  )
  after <- switch(
    type,
    round_brackets  = ")",
    square_brackets = "]",
    curly_brackets  = "}",
    angle_brackets  = ">",
    chevrons        = "\u3009",
    hyphens         = " -",
    en_dashes       = " \u2013",
    em_dashes       = "\u2014",
    commas          = ", "
  )
  paste0(before, x, after)
}

#' @rdname parenthesize
#' @export
parenthesise <- parenthesize

#' Strip all attributes from a variable
#'
#' Strips all the attributes from a variable.
#'
#' @param x Input to strip.
#' @return \code{x}, without attributes.
#' @examples
#' x <- structure(c(foo = 1, bar = 2), some_attr = 3)
#' x2 <- strip_attributes(x)
#' attributes(x)
#' attributes(x2)
#' @export
strip_attributes <- function(x)
{
  attributes(x) <- NULL
  x
}
 
#' Only use the first element of a vector
#'
#' If the input is not scalar, then only the first element is returned, 
#' with a warning.
#'
#' @param x Input that should be scalar.
#' @param indexer Either double indexing, \code{"[["} (the default) or
#' single indexing \code{"["}.
#' @return If \code{x} is scalar, it is returned unchanged, otherwise
#' only the first element is returned, with a warning.
#' @export
use_first <- function(x, indexer = c("[[", "["))
{
  # Can't use assert_is_non_empty, is_scalar in next lines because those 
  # functions calls this one.
  if(length(x) == 0L)
  {
    stop(sprintf("%s has length 0.", get_name_in_parent(x)))
  }
  if(length(x) == 1L)
  {
    return(x)
  }
  indexer <- match.fun(match.arg(indexer))
  warning(
    sprintf(
      "Only the first value of %s will be used.",
      sQuote(get_name_in_parent(x))
    ),
    call. = FALSE
  )
  indexer(x, 1L)
}
