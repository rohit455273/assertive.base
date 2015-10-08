## Release Summary

This release fixes the behaviour of is_na and is_not_na that broke downstream
packages.

## Test Environments

* Windows 7, R-devel r69463 and R-3.2.2patched r69455
* Red Hat 4.4.7-4, R-3.1.2

## R CMD check results

There were no ERRORs or WARNINGs.

## Downstream dependencies

This fixes issues with the downstream matrixpls package.
