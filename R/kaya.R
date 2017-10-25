#' Conversion factor: quads per MTOE
#' @keywords internal
mtoe = 1 / 25.2 # quads

globalVariables(c("fuel_mix", "kaya_data", "country", "country_code", "geography", "year"))

#' Get a list of countries in the Kaya data
#'
#' @return a vector of country names
#' @export
kaya_country_list <- function() {
  levels(kaya_data$country) %>%
    as.character()
}

#' Get Kaya data for a country
#'
#' @param country_name The name of a country to look up
#'
#' @return a tibble of Kaya identity data for the country
#' @export
get_kaya_data <- function(country_name) {
  kaya_data %>%
    dplyr::filter(country == country_name) %>%
    dplyr::select(-country_code, -geography) %>%
    invisible()
}

#' Get fuel mix for a country
#'
#' @param country_name The name of a country to look up
#'
#' @return a tibble of fuel mix for the country.
#'   That is, the number of quads of each fuel and the
#'   fraction of total primary energy coming from that fuel.
#' @export
get_fuel_mix <- function(country_name) {
  fuel_mix %>%
    dplyr::filter(country == country_name) %>%
    top_n(1, year) %>%
    invisible()
}
