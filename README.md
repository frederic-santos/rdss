rdss
====

`rdss` is an R-shiny application for making sex estimation somewhat easier. The global approach follows the philosophy of "diagnose sexuelle secondaire", described by [Murail et. al (1999)](https://doi.org/10.1002/(SICI)1099-1212(199901/02)9:1%3C39::AID-OA458%3E3.0.CO;2-V).

Feature requests or bug reports are welcome.

# Video tutorial

A video tutorial, illustrating the main features of `rdss`, is [available on Vimeo](https://vimeo.com/465373176).

# Installation of the R package `rdss` from GitLab

This R package is still at an early stage of development, and is not (yet) hosted on CRAN.

## Install prerequisites

1. Make sure that [Git](https://git-scm.com/) and a [recent version of R](https://cran.r-project.org/) (newer than 4.0.0) are installed.

2. Install the R package `remotes` by typing the following command line into the R console:

```r
install.packages("remotes")
```

3. Install build environment:
    * **Windows:** Install latest version of *[Rtools](https://cran.r-project.org/bin/windows/Rtools/)*. During the installation process, make sure to select *"Edit the system path"*.
    * **OSX:** Install *[XCODE](https://developer.apple.com/xcode/)*

## Install `rdss`

Run the following command in R:

```r
remotes::install_git('https://gitlab.com/f-santos/rdss.git')
```
	
# Run `rdss`

To start the graphical interface, run the following commands into the R console:

```r
library(rdss)
start_dss()
```
