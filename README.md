kayadata
================
Jonathan Gilligan
2023-07-13

# kayadata

<!-- badges: start -->

[![CRAN Status
Badge](https://www.r-pkg.org/badges/version-last-release/kayadata)](https://cran.r-project.org/package=kayadata)
[![DOI](https://zenodo.org/badge/108232691.svg)](https://zenodo.org/badge/latestdoi/108232691)
[![License:
MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![R-CMD-check](https://github.com/jonathan-g/kayadata/workflows/R-CMD-check/badge.svg)](https://github.com/jonathan-g/kayadata/actions)
<!-- badges: end -->

This package loads Kaya-identity data, synthesized from several sources.

To install the package from CRAN:

``` r
install.packages("kayadata")
```

Once you’ve installed it, then you just need to use the command
`library(kayadata)` to load the package.

Some of the functions the package provides are:

- `kaya_region_list()`: Get a list of available countries and regions.
- `get_kaya_data()`: Get data for a specific country. Example:

``` r
mexico_data = get_kaya_data("Mexico") 
mexico_data %>% filter(year >= 1965) %>% 
  select(region:ef) %>%
  head()
#> # A tibble: 6 × 10
#>   region  year     P     G     E     F     g     e     f    ef
#>   <ord>  <int> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1 Mexico  2022 0.128  1.24  8.27  506.  9.76  6.65  61.1  407.
#> 2 Mexico  2021 0.127  1.21  7.57  450.  9.53  6.27  59.4  373.
#> 3 Mexico  2020 0.126  1.15  7.04  418.  9.15  6.11  59.3  363.
#> 4 Mexico  2019 0.125  1.25  7.64  480. 10.0   6.10  62.8  383.
#> 5 Mexico  2018 0.124  1.26  7.73  489  10.1   6.16  63.2  390.
#> 6 Mexico  2017 0.123  1.23  7.83  503. 10.0   6.37  64.3  410.
```

- `project_top_down()`: Project future population, GDP, energy use, and
  emissions. Example:

``` r
mexico_2050 = project_top_down("Mexico", 2050)
mexico_2050
#> # A tibble: 1 × 10
#>   region  year     P     G     g     E     F     e     f    ef
#>   <chr>  <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1 Mexico  2050 0.157  2.45  15.6  10.5  577.  4.29  54.9  236.
```

- `plot_kaya`: Plot trends in Kaya variables for a given region or
  country. Example:

``` r
us_kaya = get_kaya_data("United States")
plot_kaya(us_kaya, "ef", y_lab = "Carbon intensity of economy",
          start_year = 2000, stop_year = 2010, log_scale = TRUE,
          trend_line = TRUE)
```

<img src="man/figures/README-plot-kaya-1.png" width="100%" />

``` r
world_kaya = get_kaya_data("World")
plot_kaya(world_kaya, "P", start_year = 2000, stop_year = 2010, log_scale = FALSE,
          trend_line = FALSE)
```

<img src="man/figures/README-plot-kaya-world-1.png" width="100%" /> \*
`get_fuel_mix`: Get the fuel mix (coal, gas, oil, nuclear, and
renewables) for a region or country. Example:

``` r
mexico_mix = get_fuel_mix("Mexico")
mexico_mix
#> # A tibble: 5 × 5
#> # Groups:   region, year [1]
#>   region  year fuel         quads   frac
#>   <chr>  <int> <ord>        <dbl>  <dbl>
#> 1 Mexico  2022 Coal        0.237  0.0286
#> 2 Mexico  2022 Oil         3.91   0.472 
#> 3 Mexico  2022 Natural Gas 3.30   0.399 
#> 4 Mexico  2022 Nuclear     0.0948 0.0115
#> 5 Mexico  2022 Renewables  0.749  0.0905
```

- `plot_fuel_mix`: Plot the fuel mix in a donut chart

``` r
plot_fuel_mix(mexico_mix)
```

<img src="man/figures/README-plot-fuel-mix-1.png" width="100%" />

After you install the package, you can get more help inside RStudio by
typing `help(package="kayadata")` in the R console window.

## Code of conduct

Please note that this project is released with a [Contributor Code of
Conduct](https://jonathan-g.github.io/kayadata/CODE_OF_CONDUCT.html). By
participating in this project you agree to abide by its terms.
