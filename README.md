kayadata
========

**GitHub:** [![Build
Status](https://travis-ci.org/gilligan-ees-3310/kayadata.svg?branch=master)](https://github.com/gilligan-ees-3310/kayadata/commits/master)

**GitLab:** [![Build
Status](https://gitlab.jgilligan.org/gilligan_teaching/ees_3310/ees_3310_software/kayadata/badges/master/build.svg)](https://gitlab.jgilligan.org/gilligan_teaching/ees_3310/ees_3310_software/kayadata/commits/master)

This package loads Kaya-identity data, synthesized from several sources.

To install and load the package, first install either the `pacman` or
`devtools` package from CRAN:

    install.packages("devtools")
    devtools::install_github("gilligan-ees-3310/kayadata")
    library(kayadata)

or

    install.packages("pacman")
    p_load_gh("gilligan-ees-3310/kayadata")

Once you’ve installed it, then you just need to use the command
`library(kayadata)` to load the package.

Some of the functions the package provides are:

-   `country_list()`: Get a list of available countries.
-   `get_kaya_data()`: Get data for a specific country. Example:
    `finland_data = get_kaya_data("Finland")`
-   `project_top_down()`: Project future population, GDP, energy use,
    and emissions. Example:
    `finland_2050 = project_top_down("Finland", 2050)`
-   `get_fuel_mix`: Get the fuel mix (coal, gas, oil, nuclear, and
    renewables) for a country. Example:
    `finland_mix = get_fuel_mix("Finland")`

From RStudio, you can get more help by typing `help(package="kayadata")`
in the R console window.
