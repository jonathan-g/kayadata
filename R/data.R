#' Kaya identity data for many countries
#'
#' A dataset containing Kaya identity parameters
#' P, G, E, F, g, e, f, and ef for many countries
#'
#' @format A tibble containing 3536 rows and 12 variables:
#' \describe{
#'   \item{country}{Country name}
#'   \item{country_code}{Three-letter country code}
#'   \item{year}{The year}
#'   \item{P}{Population, in billions}
#'   \item{G}{Gross demestic product, in trillions of constant 2010 U.S. dollars.}
#'   \item{E}{Total primary energy consumption, in quads}
#'   \item{F}{CO2 emissions from fossil fuel consumption, in millions of tons of carbon}
#'   \item{g}{Per-capita GDP, in thousands of constant 2010 U.S. dollars per person.}
#'   \item{e}{Energy intensity of the economy, in quads per trillion dollars.}
#'   \item{f}{Emissions intensity of the energy supply, in million metric tons
#'            of carbon per quad.}
#'   \item{ef}{Emissions intensity of the economy, in metric tons of carbon per
#'             million dollars of GDP.}
#' }
#' @source \url{https://data.worldbank.org/indicator/SP.POP.TOTL},
#' \url{https://data.worldbank.org/indicator/NY.GDP.MKTP.KD}, and
#' \url{https://www.bp.com/en/global/corporate/energy-economics/statistical-review-of-world-energy/downloads.html}
"kaya_data"


#' Mix of fuels contributing to primary energy supply for many countries
#'
#' A dataset containing the fuel mix of how many quads and what fraction of
#' total primary energy supply comes from coal, gas, oil, nuclear, and
#' renewable sources.
#'
#' @format A tibble containing 840 rows and 5 variables
#' \describe{
#'   \item{country}{Country name}
#'   \item{year}{The year}
#'   \item{fuel}{The fuel: Coal, Gas, Oil, Nuclear, and Renewables}
#'   \item{quads}{The number of quads of that fuel consumed in the given country
#'                and year}
#'   \item{pct}{The percentage of that country's total primary energy consumption
#'              from the fuel}
#' }
#' @source \url{https://www.bp.com/en/global/corporate/energy-economics/statistical-review-of-world-energy/downloads.html}
"fuel_mix"

#' Top-down projections of future Kaya variables for many countries
#'
#' A dataset containing top-down projections of P, G, and E, from the
#' EIA's International Energy Outlook 2017.
#'
#' @format A tibble containing 336 rows and 5 variables
#' \describe{
#'    \item{country}{Country name}
#'    \item{year}{The year}
#'   \item{P}{Population, in billions}
#'   \item{G}{Gross demestic product, in trillions of constant 2010 U.S. dollars}
#'   \item{E}{Total primary energy consumption, in quads}
#'   \item{F}{Total CO2 emissions, in millions of metric tons of carbon}
#' }
#' @source \url{https://www.eia.gov/outlooks/ieo/ieo_tables.php}
"td_values"

#' Top-down projections of trends in Kaya variables for many countries
#'
#' A dataset containing top-down projections of trends in P, G, and E,
#' from the EIA's International Energy Outlook 2017.
#'
#' @format A tibble containing 42 rows and 5 variables
#' \describe{
#'    \item{country}{Country name}
#'    \item{year}{The year}
#'   \item{P}{Trend in population, in percent per year}
#'   \item{G}{Trend in gross demestic product, in percent per year}
#'   \item{E}{Trend in total primary energy consumption, in percent per year}
#'   \item{F}{Trend in CO2 emissions, in percent per year}
#' }
#' @source \url{https://www.eia.gov/outlooks/ieo/ieo_tables.php}
"td_trends"
