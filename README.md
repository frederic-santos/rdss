rdss
====

`rdss` is an R-shiny application for making sex estimation somewhat easier. The global approach follows the philosophy of "diagnose sexuelle secondaire", described by [Murail et. al (1999)](https://doi.org/10.1002/(SICI)1099-1212(199901/02)9:1%3C39::AID-OA458%3E3.0.CO;2-V).

Feature requests or bug reports are welcome.

![rdss_UI](./inst/rdss_UI.png "An overview of `rdss` user interface")

## Documentation

### Publication

This R package has been extensively described and documented [in an article](https://doi.org/10.1002/oa.2957) published in the *International Journal of Osteoarchaeology*. This article can be seen as the official (and the main) documentation of `rdss`.

### Video tutorial

A video tutorial, illustrating the main features of `rdss`, is [available on Vimeo](https://vimeo.com/465373176). This video presents an older version of `rdss` (v0.9.7); some slight changes in the user interface have been made in the newest version.

### Package vignette

A package vignette is available for those users who would also like to use `rdss` internal functions in R scripts, for performing sex estimation through the command line. The package vignette can be consulted by running the following command into the R console:

```r
vignette(package = "rdss", topic = "intro_rdss")
```

## Installation of the R package `rdss` from GitLab

This R package is still at an early stage of development, and is not (yet) hosted on CRAN.

### Install prerequisites

1. Make sure that [Git](https://git-scm.com/) and a [recent version of R](https://cran.r-project.org/) (newer than 4.0.0) are installed.

2. Install the R package `remotes` by typing the following command line into the R console:

   ```r
   install.packages("remotes")
   ```

3. Install build environment:
    * **Linux**: no additional operation required.
    * **OSX**: install *[XCODE](https://developer.apple.com/xcode/)*.
    * **Windows**: install the latest version of *[Rtools](https://cran.r-project.org/bin/windows/Rtools/)*. In particular, make sure to follow the steps of the section "Putting Rtools on the PATH" from the help page.

### Install `rdss`

Run the following command in R:

```r
remotes::install_git('https://gitlab.com/f-santos/rdss.git', build_vignette = TRUE)
```

If you go through an error or want a lighter install because you will not use `rdss` through the command line interface, you can alternatively run:

```r
remotes::install_git('https://gitlab.com/f-santos/rdss.git', build_vignette = FALSE)
```
	
## Run `rdss`

To start the graphical interface, run the following commands into the R console:

```r
library(rdss)
start_dss()
```

## Citing `rdss` in a scientific article

Citation info can be found by executing the following instruction into the R console:

```r
citation("rdss")
```

## License

`rdss` is available under a [CeCILL 2.1 free software license](http://cecill.info/licences/Licence_CeCILL_V2.1-en.html).
