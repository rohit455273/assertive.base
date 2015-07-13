#' @rdname is_true
#' @export
assert_is_identical_to_false <- function(x, allow_attributes = FALSE)
{                                                  
  assert_engine(
    is_identical_to_false,
    x, 
    allow_attributes = allow_attributes, 
    .xname = get_name_in_parent(x)
  )      
}

#' @rdname is_true
#' @export
assert_is_identical_to_na <- function(x, allow_attributes = FALSE)
{                                                  
  assert_engine(
    is_identical_to_na,
    x,
    allow_attributes = allow_attributes, 
    .xname = get_name_in_parent(x)
  )    
}

#' @rdname is_true
#' @export
assert_is_identical_to_true <- function(x, allow_attributes = FALSE)
{                                                  
  assert_engine(
    is_identical_to_true,
    x,
    allow_attributes = allow_attributes, 
    .xname = get_name_in_parent(x)
  )    
}
