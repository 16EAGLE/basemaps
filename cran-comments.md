## Test environments

* Ubuntu 20.04.2, R 4.0.5
* Ubuntu 20.04.2, R-devel
* Ubuntu 16.04.6, R 3.6.3
* Ubuntu 16.04.6, R 4.0.2
* Ubuntu 16.04.6, R-devel
* Max OS X 10.15.7, R 4.0.5
* Windows Server 2019, R 4.0.5
* Windows Server 2012 R2, R 4.0.5

## R CMD check results

0 errors | 0 warnings | 0 notes

* Reply to comment #1 of initial submission: forwarded URLs have been replaced accordingly.
* Reply to comment #2 of intiial submission: checked examples, tests and writing functions to not write anywhere else than tempdir() by default. Tests always use paste0(tempdir(), "/basemaps/") for writing.

