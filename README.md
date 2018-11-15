
<!-- README.md is generated from README.Rmd. Please edit that file -->
PMxploit
========

<img id="logo" src="pmxploit.svg" alt="pmxploit" />

The goal of pmxploit is to facilitate NONMEM post-processing analysis. It provides helper functions for visualizing and summarizing population analyses results, as well as numerical quality criteria computation and between run comparison.

It is compatible with run executed with NONMEM 7.3 and higher.

Requirements
------------

To take full advantage of `pmxploit` capacities, users should mind applying some "conventions" in writing NONMEM control stream files (see [article](articles/nonmem_requirements.html)).

Interactive post-processing
---------------------------

<a href="#"><img id="thumbnail" src="pmxploit_screen.png" data-toggle="modal" data-target="#myModal"></a>

<img id="animation" src="pmxploit_preview.gif" alt="pmxploit" />

It embeds an interactive web application based on `shiny` to facilite and speed up run analysis and comparison. To start this application, call the following `R` function:

``` r
library(pmxploit)

nonmem_toolbox()
# or, to open an archive at startup
nonmem_toolbox(archive_path = "path/to/run_archive")
```

You can also start the application from a linux command:

``` bash
pmxploit
# or, to open an archive at startup
pmxploit /path/to/run_archive
```
