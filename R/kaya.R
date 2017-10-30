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
#'   \item{country}{The name of the country}
#'   \item{year}{The year}
#'   \item{P}{Population, in billions}
#'   \item{G}{Gross demestic product, in trillions of constant 2010 U.S. dollars.}
#'   \item{E}{Total primary energy consumption, in quads}
#'   \item{F}{CO2 emissions from fossil fuel consumption, in millions of metric
#'            tons }
#'   \item{g}{Per-capita GDP, in thousands of constant 2010 U.S. dollars per person.}
#'   \item{e}{Energy intensity of the economy, in quads per trillion dollars.}
#'   \item{f}{Emissions intensity of the energy supply, in million metric tons
#'            per quad.}
#'   \item{ef}{Emissions intensity of the economy, in metric tons per
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
    select(country, P, G, g, E, F, e, f, ef) %>%
  invisible()
}

#' Get top-down projections of Kaya variables for a country
#'
#' @param country_name The name of a country to look up
#'
#' @return a tibble of values for P, G, E, F, g, e, f, and ef for the country:
#' \describe{
#'   \item{country}{The name of the country}
#'   \item{P}{Population, in billions}
#'   \item{G}{Gross demestic product, in trillions of constant 2010 U.S. dollars.}
#'   \item{E}{Total primary energy consumption, in quads}
#'   \item{F}{CO2 emissions from fossil fuel consumption, in millions of metric
#'            tons }
#'   \item{g}{Per-capita GDP, in thousands of constant 2010 U.S. dollars per person.}
#'   \item{e}{Energy intensity of the economy, in quads per trillion dollars.}
#'   \item{f}{Emissions intensity of the energy supply, in million metric tons
#'            per quad.}
#'   \item{ef}{Emissions intensity of the economy, in metric tons per
#'             million dollars of GDP.}
#' }
#' @export
top_down_values <- function(country_name) {
  td_values %>%
    dplyr::filter(country == country_name) %>%
    mutate(g = G/P, e = E/G, f = F/E, ef = F/G) %>%
    select(country, year, P, G, g, E, F, e, f, ef) %>%
    invisible()
}

#' Get top-down projections of Kaya variables for a country for a given year
#'
#' @param country_name The name of a country to look up
#'
#' @param year The year to project to
#'
#' @return a tibble of values for P, G, E, F, g, e, f, and ef for the country:
#' \describe{
#'   \item{country}{The name of the country}
#'   \item{year}{The year}
#'   \item{P}{Population, in billions}
#'   \item{G}{Gross demestic product, in trillions of constant 2010 U.S. dollars.}
#'   \item{E}{Total primary energy consumption, in quads}
#'   \item{F}{CO2 emissions from fossil fuel consumption, in millions of metric
#'            tons }
#'   \item{g}{Per-capita GDP, in thousands of constant 2010 U.S. dollars per person.}
#'   \item{e}{Energy intensity of the economy, in quads per trillion dollars.}
#'   \item{f}{Emissions intensity of the energy supply, in million metric tons
#'            per quad.}
#'   \item{ef}{Emissions intensity of the economy, in metric tons per
#'             million dollars of GDP.}
#' }
#' @export
project_top_down <- function(country_name, year) {
  y = year
  td_values %>%
    dplyr::filter(country == country_name) %>%
    mutate(g = G/P, e = E/G, f = F/E, ef = F/G) %>%
    summarize_at(vars(-country, -year), funs(approx(x = year, y = ., xout = y)$y)) %>%
    mutate(country = country_name, year = y) %>%
    select(country, year, P, G, g, E, F, e, f, ef) %>%
    invisible()
}


#' Get emission factors for different energy sources
#'
#' @return a tibble of values for emissions factors, in million metric
#'         tons of carbon dioxide per quad of energy.
#' @export
emissions_factors <- function() {
  tibble(
    fuel = c("Coal", "Oil", "Gas", "Nuclear", "Renewable"),
    emission_factor = c(94.4, 70.0, 53.1, 0.0, 0.0)
  )
}

#' Get power output from generation sources
#'
#' Nameplate capacity and capacity factors for different electrical generation
#' technologies. The average power supplied over a year is the nameplate
#' capacity times the capacity factor.
#'
#' @return a tibble of values for generation sources
#' \describe{
#'     \item{fuel}{Energy source: Coal, Nuclear, Gas, Solar Thermal, or Wind}
#'     \item{description}{Text description of the power source}
#'     \item{nameplate_capacity}{Maximum sustained power output, in megawatts}
#'     \item{capacity_factor}{Capacity factor: the fraction of the nameplate
#'         capacity that the plant can provide, averaged over a typical year}
#' }
#' @export
generation_capacity <- function() {
  tibble(
    fuel = c("Coal", "Nuclear", "Gas", "Solar Thermal", "Wind"),
    description = c("Large coal-fired power plant",
                    "Large nuclear power plant",
                    "Gas-fired power plant",
                    "Concentrated solar-thermal power plant",
                    "Wind turbine"),
    nameplate_capacity = c(1000, 1000, 500,  200,  2.5),
    capacity_factor    = c(0.53, 0.75, 0.56, 0.30, 0.30)
    )
}

#' The number of megawatts it takes to replace a quad.
#'
#' The number of megawatts of average power output over a year to
#' produce one quad of energy
#'
#' @return The number of megawatts equivalent to one quad per year.
megawatts_per_quad <- function() {
  1.1E4
}
