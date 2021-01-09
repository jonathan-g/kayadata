kayadata
================
Jonathan Gilligan
2019-12-21

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

-   `kaya_region_list()`: Get a list of available countries and regions.
-   `get_kaya_data()`: Get data for a specific country. Example:

``` r
mexico_data = get_kaya_data("Mexico") 
mexico_data %>% filter(year >= 1965) %>% 
  select(region:ef) %>%
  head()
#> # A tibble: 6 x 10
#>   region  year     P     G     E     F     g     e     f    ef
#>   <ord>  <int> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1 Mexico  2019 0.128  1.31  7.32  455. 10.3   5.58  62.2  347.
#> 2 Mexico  2018 0.126  1.31  7.42  467. 10.4   5.66  62.8  355.
#> 3 Mexico  2017 0.125  1.29  7.48  477. 10.3   5.82  63.7  371.
#> 4 Mexico  2016 0.123  1.26  7.38  469. 10.2   5.86  63.5  372.
#> 5 Mexico  2015 0.122  1.22  7.29  463. 10.0   5.96  63.6  379.
#> 6 Mexico  2014 0.120  1.18  7.30  460.  9.84  6.16  63.0  388.
```

-   `project_top_down()`: Project future population, GDP, energy use,
    and emissions. Example:

``` r
mexico_2050 = project_top_down("Mexico", 2050)
mexico_2050
#> # A tibble: 1 x 10
#>   region  year     P     G     g     E     F     e     f    ef
#>   <chr>  <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1 Mexico  2050 0.157  2.57  16.3  10.1  589.  3.93  58.2  229.
```

-   `plot_kaya`: Plot trends in Kaya variables for a given region or
    country. Example:

``` r
us_kaya = get_kaya_data("United States")
plot_kaya(us_kaya, "ef", y_lab = "Carbon intensity of economy",
          start_year = 2000, stop_year = 2010, log_scale = TRUE,
          trend_line = TRUE)
#> `geom_smooth()` using formula 'y ~ x'
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
#> # A tibble: 5 x 5
#> # Groups:   region, year [1]
#>   region  year fuel         quads   frac
#>   <chr>  <int> <ord>        <dbl>  <dbl>
#> 1 Mexico  2019 Coal        0.481  0.0657
#> 2 Mexico  2019 Oil         3.12   0.426 
#> 3 Mexico  2019 Natural Gas 3.09   0.423 
#> 4 Mexico  2019 Nuclear     0.0951 0.0130
#> 5 Mexico  2019 Renewables  0.528  0.0722
```

-   `plot_fuel_mix`: Plot the fuel mix in a donut chart

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
