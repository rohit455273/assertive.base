library(testthat)
library(devtools)
library(assertive.base)

with_envvar(
  c(LANG = "en_US"),
  test_check("assertive.base")
)
