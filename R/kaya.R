#' Conversion factor: quads per MTOE
#' @keywords internal
mtoe = 1 / 25.2 # quads

globalVariables(c("fuel_mix", "kaya_data", "country", "country_code",
                  "geography", "year", "P", "G", "E", "F", "g", "e", "f", "ef",
                  "td_values", "td_trends"))

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
#' @return a tibble of Kaya identity data for the country:
#' \describe{
#'   \item{P}{Population, in billions}
#'   \item{G}{Gross demestic product, in trillions of constant 2010 U.S. dollars.}
#'   \item{E}{Total primary energy consumption, in quads}
#'   \item{F}{CO2 emissions from fossil fuel consumption, in millions of metric
#'            tons of carbon}
#'   \item{g}{Per-capita GDP, in thousands of constant 2010 U.S. dollars per person.}
#'   \item{e}{Energy intensity of the economy, in quads per trillion dollars.}
#'   \item{f}{Emissions intensity of the energy supply, in million metric tons
#'            of carbon per quad.}
#'   \item{ef}{Emissions intensity of the economy, in metric tons of carbon per
#'             million dollars of GDP.}
#' }
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

#' Get top-down trends for Kaya variables for a country
#'
#' @param country_name The name of a country to look up
#'
#' @return a tibble of trends for P, G, E, F, g, e, f, and ef for the country,
#' in percent per year.
#' @export
top_down_trend <- function(country_name) {
  td_trends %>%
    dplyr::filter(country == country_name) %>%
    mutate(g = G - P, e = E - G, f = F - E, ef = F - G) %>%
    invisible()
}

#' Get top-down projections of Kaya variables for a country
#'
#' @param country_name The name of a country to look up
#'
#' @return a tibble of values for P, G, E, F, g, e, f, and ef for the country:
#' \describe{
#'   \item{P}{Population, in billions}
#'   \item{G}{Gross demestic product, in trillions of constant 2010 U.S. dollars.}
#'   \item{E}{Total primary energy consumption, in quads}
#'   \item{F}{CO2 emissions from fossil fuel consumption, in millions of metric
#'            tons of carbon}
#'   \item{g}{Per-capita GDP, in thousands of constant 2010 U.S. dollars per person.}
#'   \item{e}{Energy intensity of the economy, in quads per trillion dollars.}
#'   \item{f}{Emissions intensity of the energy supply, in million metric tons
#'            of carbon per quad.}
#'   \item{ef}{Emissions intensity of the economy, in metric tons of carbon per
#'             million dollars of GDP.}
#' }
#' @export
top_down_values <- function(country_name) {
  td_values %>%
    dplyr::filter(country == country_name) %>%
    mutate(g = G/P, e = E/G, f = F/E, ef = F/G) %>%
    invisible()
}
