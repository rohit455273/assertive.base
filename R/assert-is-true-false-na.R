#' @rdname is_true
#' @export
assert_all_are_false <- function(x)
{                                                     
  msg <- gettextf("%s are not all FALSE.", get_name_in_parent(x))
  assert_engine(is_false, x, msg = msg)        
}

#' @rdname is_true
#' @export
assert_any_are_false <- function(x)
{                                                     
  msg <- gettextf("%s are all not FALSE.", get_name_in_parent(x))
  assert_engine(is_false, x, msg = msg, what = "any")        
}
#' @rdname is_true
#' @export
assert_all_are_na <- function(x)
{                                                     
  msg <- gettextf("%s are not all NA.", get_name_in_parent(x))
  assert_engine(is_na, x, msg = msg)        
}

#' @rdname is_true
#' @export
assert_any_are_na <- function(x)
{                                                     
  msg <- gettextf("%s are all not NA.", get_name_in_parent(x))
  assert_engine(is_na, x, msg = msg, what = "any")        
}

#' @rdname is_true
#' @export
assert_all_are_not_false <- function(x)
{                                                      
  msg <- gettextf("%s contains FALSEs.", get_name_in_parent(x))
  assert_engine(is_not_false, x, msg = msg)
}

#' @rdname is_true
#' @export
assert_any_are_not_false <- function(x)
{                                                      
  msg <- gettextf("%s are all FALSE.", get_name_in_parent(x))
  assert_engine(is_not_false, x, msg = msg, what = "any")
}

#' @rdname is_true
#' @export
assert_all_are_not_na <- function(x)
{                                                      
  msg <- gettextf("%s contains NAs.", get_name_in_parent(x))
  assert_engine(is_not_na, x, msg = msg)
}

#' @rdname is_true
#' @export
assert_any_are_not_na <- function(x)
{                                                      
  msg <- gettextf("%s are all NA.", get_name_in_parent(x))
  assert_engine(is_not_na, x, msg = msg, what = "any")
}

#' @rdname is_true
#' @export
assert_all_are_not_true <- function(x)
{                                                      
  msg <- gettextf("%s contains TRUEs.", get_name_in_parent(x))
  assert_engine(is_not_true, x, msg = msg)
}

#' @rdname is_true
#' @export
assert_any_are_not_true <- function(x)
{                                                      
  msg <- gettextf("%s are all TRUE.", get_name_in_parent(x))
  assert_engine(is_not_true, x, msg = msg, what = "any")
}

#' @rdname is_true
#' @export
assert_all_are_true <- function(x)
{                                                     
  msg <- gettextf("%s are not all TRUE.", get_name_in_parent(x))
  assert_engine(is_true, x, msg = msg)        
}

#' @rdname is_true
#' @export
assert_any_are_true <- function(x)
{                                                     
  msg <- gettextf("%s are all not TRUE.", get_name_in_parent(x))
  assert_engine(is_true, x, msg = msg, what = "any")        
}
