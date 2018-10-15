kayadata
========

**GitHub:** [![Build
Status](https://travis-ci.org/jonathan-g/kayadata.svg?branch=master)](https://github.com/jonathan-g/kayadata/commits/master)

**GitLab:** [![Build
Status](https://gitlab.jgilligan.org/gilligan_teaching/ees_3310/ees_3310_software/kayadata/badges/master/build.svg)](https://gitlab.jgilligan.org/gilligan_teaching/ees_3310/ees_3310_software/kayadata/commits/master)

This package loads Kaya-identity data, synthesized from several sources.

To install and load the package, first install either the `pacman` or
`devtools` package from CRAN:

``` r
install.packages("devtools")
devtools::install_github("jonathan-g/kayadata")
library(kayadata)
```

or

``` r
install.packages("pacman")
library(pacman)
p_load_gh("jonathan-g/kayadata")
```

Once you’ve installed it, then you just need to use the command
`library(kayadata)` to load the package.

Some of the functions the package provides are:

-   `kaya_region_list()`: Get a list of available countries and regions.
-   `get_kaya_data()`: Get data for a specific country. Example:

``` r
finland_data = get_kaya_data("Finland")
head(finland_data)
```

    ## # A tibble: 6 x 12
    ##   country  year       P      G      E     F     g     e     f    ef G_ppp
    ##   <ord>   <int>   <dbl>  <dbl>  <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
    ## 1 Finland  1960 0.00443 0.0540 NA      NA    12.2 NA     NA     NA     NA
    ## 2 Finland  1961 0.00446 0.0581 NA      NA    13.0 NA     NA     NA     NA
    ## 3 Finland  1962 0.00449 0.0599 NA      NA    13.3 NA     NA     NA     NA
    ## 4 Finland  1963 0.00452 0.0618 NA      NA    13.7 NA     NA     NA     NA
    ## 5 Finland  1964 0.00455 0.0651 NA      NA    14.3 NA     NA     NA     NA
    ## 6 Finland  1965 0.00456 0.0685  0.383  24.8  15.0  5.60  64.6  362.    NA
    ## # ... with 1 more variable: G_mer <dbl>

-   `project_top_down()`: Project future population, GDP, energy use,
    and emissions. Example:

``` r
finland_2050 = project_top_down("Finland", 2050)
finland_2050
```

    ## # A tibble: 1 x 10
    ##   country  year       P     G     g     E     F     e     f    ef
    ##   <chr>   <dbl>   <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
    ## 1 Finland  2050 0.00581 0.393  67.7  1.25  30.5  3.19  24.3  77.5

-   `get_fuel_mix`: Get the fuel mix (coal, gas, oil, nuclear, and
    renewables) for a region or country. Example:

``` r
finland_mix = get_fuel_mix("Finland")
finland_mix
```

    ## # A tibble: 6 x 7
    ##   country country_code geography  year fuel         quads   pct
    ##   <chr>   <chr>        <chr>     <int> <ord>        <dbl> <dbl>
    ## 1 Finland FIN          nation     2017 Oil         0.384  35.1 
    ## 2 Finland FIN          nation     2017 Natural Gas 0.0626  5.73
    ## 3 Finland FIN          nation     2017 Coal        0.163  14.9 
    ## 4 Finland FIN          nation     2017 Nuclear     0.204  18.6 
    ## 5 Finland FIN          nation     2017 Hydro       0.133  12.1 
    ## 6 Finland FIN          nation     2017 Renewables  0.147  13.4

-   `plot_kaya`: Plot trends in kaya variables for a given region or
    country. Example:
    ![](README_files/figure-markdown_github/plot-kaya-1.png)
    ![](README_files/figure-markdown_github/plot-kaya-world-1.png)

After you install the package, you can get more help inside RStudio by
typing `help(package="kayadata")` in the R console window.
