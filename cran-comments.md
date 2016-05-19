## Release Summary

This release contains improvements to how error messages are reported throughout the assertive.* packages.

## Test Environments

* Local Windows 7 & 10, R-devel 
* Semaphore CI + Ubuntu 14.04, R-devel and R-release
* AppVeyor + Windows Server 2012, R-devel

## R CMD check results

There were no ERRORs or WARNINGs.

## Downstream dependencies

This breaks a few tests in assertive.datetimes, assertive.files, and assertive.numbers.  These, along with some other assertive.* packages, will be updated once this version appears on CRAN.
