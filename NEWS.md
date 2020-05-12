# pmxploit 3.0.7

* Functions `nm_bootstrap`, `nm_chain` and `nm_jacknife` were removed
* Compatibility changes for `tibble` package version 3.0.0

# pmxploit 2.1

* Control stream parsing enhanced:
    * Initial values
    * BLOCK
    * $PRIOR
* Control stream generation for use of prior information
* Increased text size for `theme_pmx` ggplot2 theme.
* Possibility to download a ggplot2 object (as an R serialized file *.rds, to be loaded then with `readRDS()` function) of the plots generated in the shiny application for custom plot editiong.
* Unit tests enhanced

# pmxploit 2.0.1

* Bug fixes:
    * Loading of runs with MAXEVAL=0 in $EST
* Enhanced Windows compatibility

# pmxploit 2.0

* Second release (October 2017)
* NONMEM 7.4.1 support
* Parsing improved (control stream, xml and iterations files)
* Shiny application:
    * Source archives can be updated with plots, tables and dynamic reports
    * Various UI enhancements
* Documentation update and pkgdown website

# pmxploit 1.0.1

* Minor updates and bug fixes

# pmxploit 1.0

* First release (January 2017)
