[![Project Status: Active - The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/0.1.0/active.svg)](https://www.repostatus.org/#active)
[![Is the package on CRAN?](https://www.r-pkg.org/badges/version/assertive.base)](https://www.r-pkg.org/pkg/assertive.base)
[![SemaphoreCI Build Status](https://semaphoreci.com/api/v1/projects/01fd8743-d3d2-42ad-b63d-e94c5844e951/635080/badge.svg)](https://semaphoreci.com/richierocks/assertive-base)
[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/ubs74w5tm2mxgfne?svg=true)](https://ci.appveyor.com/project/richierocks/assertive-base)

# assertive.base

A minimal set of predicates and assertions used by *[assertive](https://bitbucket.org/richierocks/assertive)*, 
for package developers who want to include run-time testing features in their own packages.  Most of the documentation is on the assertive page.  End-users will usually want to use *assertive* directly.


### Installation

To install the stable version, type:

```{r}
install.packages("assertive.base")
```

To install the development version, you first need the *devtools* package.

```{r}
install.packages("devtools")
```

Then you can install the *assertive.base* package using

```{r}
library(devtools)
install_bitbucket("richierocks/assertive.base")
```

### Predicates

There are six functions that accept (expressions resolving to) logical vectors,
and return logical vectors:

`is_true` returns a logical vector that is `TRUE` when the input is `TRUE`
(`x & !is.na(x)`).

`is_false` returns a logical vector that is `TRUE` when the input is `FALSE`
(`!x & !is.na(x)`).

`is_na` returns a logical vector that is `TRUE` when the input is `NA`
(a wrapper to `is.na(x)`).

...and their negations:

`is_not_true` returns a logical vector that is `TRUE` when the input is `FALSE` 
or `NA` (`x | is.na(x)`).

`is_not_false` returns a logical vector that is `TRUE` when the input is `TRUE` 
or `NA` (`!x | is.na(x)`).

`is_not_na` returns a logical vector that is `TRUE` when the input is `TRUE` 
or `FALSE` (`!is.na(x)`).

There are four functions that return single logical values:

`is_identical_to_true` returns `TRUE` is effectively `identical(x, TRUE)` (like 
`isTRUE`), but it lets you choose whether or not attributes are allowed on `x`.

`is_identical_to_false` and `is_identical_to_na` works similarly with `FALSE` 
and `NA`.

`are_identical` wraps `base::identical`, checking if two expressions return the 
same thing.

### Assertions

Predicates that return a vector have two corresponding assertions.  For example,
`is_true` has `assert_all_are_true` and `assert_any_are_true`.

Predicates returning a single logical value have one corresponding assertion.
For example, `is_identical_to_true` has `assert_is_identical_to_true`.

### Utilities

`use_first` takes the first value of a vector, warning you if it one longer than 
length one.

`coerce_to` is a wrapper to `as`, changing an object's type with a warning.

`get_name_in_parent` gets the name of a variable in the parent environment 
(stopping you have to remember `deparse(substitute())` arcana).

`strip_attributes` strips the attributes from an object.

`merge_dots_with_list` merges the contents of `...` with a list argument, to 
allow users to pass arguments to your function in either form.

`dont_stop` runs code without stopping at errors, which is useful for 
demonstrating errors in examples.

`parenthesise` wraps a string in parentheses.

`bapply` is a wraps `vapply`, always returning a logical vector.