#' Conversion factor: quads per MTOE
#' @keywords internal
mtoe = 1 / 25.2 # quads

globalVariables(c("fuel_mix", "kaya_data", "country", "country_code",
                  "geography", "year", "P", "G", "E", "F", "g", "e", "f", "ef",
                  "td_values", "td_trends"))

#' Look up country name from country code
#'
#' @param country_code The three-letter country code
#' @param data Data frame in which to look up `country_code`
#'
#' @return The corresponding country name, or NULL if there is no such country
lookup_country_code <- function(country_code, data = kayadata::kaya_data) {
  country_name <- data %>%
    dplyr::select(country, country_code) %>% dplyr::distinct() %>%
    dplyr::filter(country_code == (!!country_code)) %$% country %>%
    as.character()
  if (is.null(country_name) || length(country_name) == 0) {
    country_name = NULL
    warning("There is no country with code ", country_code, ".")
  }
  country_name
}

#' Get a list of countries in the Kaya data
#'
#' @return a vector of country names
#' @export
kaya_country_list <- function() {
  levels(kayadata::kaya_data$country) %>%
    as.character()
}

#' Get Kaya data for a country
#'
#' @param country_name The name of a country to look up
#' @param country_code Optional three-letter country code to look up instead
#'                     of the `country_name`
#'
#' @return a tibble of Kaya identity data for the country:
#' \describe{
#'   \item{country}{The name of the country}
#'   \item{year}{The year}
#'   \item{P}{Population, in billions}
#'   \item{G}{Gross demestic product, in trillions of constant 2010 U.S.
#'            dollars.}
#'   \item{E}{Total primary energy consumption, in quads}
#'   \item{F}{CO2 emissions from fossil fuel consumption, in millions of metric
#'            tons }
#'   \item{g}{Per-capita GDP, in thousands of constant 2010 U.S. dollars per
#'            person.}
#'   \item{e}{Energy intensity of the economy, in quads per trillion dollars.}
#'   \item{f}{Emissions intensity of the energy supply, in million metric tons
#'            per quad.}
#'   \item{ef}{Emissions intensity of the economy, in metric tons per
#'             million dollars of GDP.}
#' }
#' @export
get_kaya_data <- function(country_name, country_code = NULL) {
  if (! is.null(country_code)) {
    country_name <- lookup_country_code(country_code)
    if (is.null(country_name)) {
      country_name = ""
    }
  }

  data <- kayadata::kaya_data %>%
    dplyr::select(-country_code, -geography) %>%
    dplyr::filter(country == country_name)
  if (nrow(data) == 0 && is.null(country_code)) {
    warning("There is no data for country ", country_name)
  }
  data
}

#' Get fuel mix for a country
#'
#' @param country_name The name of a country to look up
#' @param country_code Optional three-letter country code to look up instead
#'                     of the `country_name`
#'
#' @return a tibble of fuel mix for the country.
#'   That is, the number of quads of each fuel and the
#'   fraction of total primary energy coming from that fuel.
#' @export
get_fuel_mix <- function(country_name, country_code = NULL) {
  if (! is.null(country_code)) {
    country_name <- lookup_country_code(country_code,
                                                   kayadata::fuel_mix)
    if (is.null(country_name) || length(country_name) == 0) {
      country_name = ""
    }
  }
  data <- kayadata::fuel_mix %>%
    dplyr::filter(country == country_name) %>%
    dplyr::top_n(1, year)
  if (nrow(data) == 0 && is.null(country_code)) {
    warning("There is no data for country ", country_name)
  }
  data
}

#' Get top-down trends for Kaya variables for a country
#'
#' @param country_name The name of a country to look up
#' @param country_code Optional three-letter country code to look up instead
#'                     of the `country_name`
#'
#' @return a tibble of trends for P, G, E, F, g, e, f, and ef for the country,
#' in percent per year.
#' @export
top_down_trend <- function(country_name, country_code = NULL) {
  if (! is.null(country_code)) {
    country_name <- lookup_country_code(country_code,
                                                   kayadata::td_trends)
    if (is.null(country_name) || length(country_name) == 0) {
      country_name = ""
    }
  }
  data <- kayadata::td_trends %>%
    dplyr::filter(country == country_name) %>%
    dplyr::mutate(g = G - P, e = E - G, f = F - E, ef = F - G) %>%
    dplyr::select(country, P, G, g, E, F, e, f, ef)
  if (nrow(data) == 0 && is.null(country_code)) {
    warning("There is no data for country ", country_name)
  }
  data
}

#' Get top-down projections of Kaya variables for a country
#'
#' @param country_name The name of a country to look up
#' @param country_code Optional three-letter country code to look up instead
#'                     of the `country_name`
#'
#' @return a tibble of values for P, G, E, F, g, e, f, and ef for the country:
#' \describe{
#'   \item{country}{The name of the country}
#'   \item{P}{Population, in billions}
#'   \item{G}{Gross demestic product, in trillions of constant 2010 U.S.
#'            dollars.}
#'   \item{E}{Total primary energy consumption, in quads}
#'   \item{F}{CO2 emissions from fossil fuel consumption, in millions of metric
#'            tons }
#'   \item{g}{Per-capita GDP, in thousands of constant 2010 U.S. dollars per
#'            person.}
#'   \item{e}{Energy intensity of the economy, in quads per trillion dollars.}
#'   \item{f}{Emissions intensity of the energy supply, in million metric tons
#'            per quad.}
#'   \item{ef}{Emissions intensity of the economy, in metric tons per
#'             million dollars of GDP.}
#' }
#' @export
top_down_values <- function(country_name, country_code) {
  if (! is.null(country_code)) {
    country_name <- lookup_country_code(country_code,
                                                   kayadata::td_values)
    if (is.null(country_name) || length(country_name) == 0) {
      country_name = ""
    }
  }
  data <- kayadata::td_values %>%
    dplyr::filter(country == country_name) %>%
    dplyr::mutate(g = G/P, e = E/G, f = F/E, ef = F/G) %>%
    dplyr::select(country, year, P, G, g, E, F, e, f, ef)
  if (nrow(data) == 0 && is.null(country_code)) {
    warning("There is no data for country ", country_name)
  }
  data
}

#' Get top-down projections of Kaya variables for a country for a given year
#'
#' @param country_name The name of a country to look up
#' @param country_code Optional three-letter country code to look up instead
#'                     of the `country_name`
#'
#' @param year The year to project to
#'
#' @return a tibble of values for P, G, E, F, g, e, f, and ef for the country:
#' \describe{
#'   \item{country}{The name of the country}
#'   \item{year}{The year}
#'   \item{P}{Population, in billions}
#'   \item{G}{Gross demestic product, in trillions of constant 2010 U.S.
#'            dollars.}
#'   \item{E}{Total primary energy consumption, in quads}
#'   \item{F}{CO2 emissions from fossil fuel consumption, in millions of metric
#'            tons }
#'   \item{g}{Per-capita GDP, in thousands of constant 2010 U.S. dollars per
#'            person.}
#'   \item{e}{Energy intensity of the economy, in quads per trillion dollars.}
#'   \item{f}{Emissions intensity of the energy supply, in million metric tons
#'            per quad.}
#'   \item{ef}{Emissions intensity of the economy, in metric tons per
#'             million dollars of GDP.}
#' }
#' @export
project_top_down <- function(country_name, year, country_code = NULL) {
  if (! is.null(country_code)) {
    country_name <- lookup_country_code(country_code,
                                                   kayadata::td_values)
    if (is.null(country_name) || length(country_name) == 0) {
      country_name = ""
    }
  }
  if (year < min(kayadata::td_values$year, na.rm=T) ||
              year > max(kayadata::td_values$year, na.rm=T)) {
    stop("Projecting top-down values only works for year betweeen ",
         min(kayadata::td_values$year, na.rm = T), " and ",
         max(kayadata::td_values$year, na.rm = T), ".")
  }

  data <- kayadata::td_values %>%
    dplyr::filter(country == country_name) %>%
    dplyr::summarize_at(vars(-country, -country_code, -geography, -year),
                 dplyr::funs(approx(x = year, y = ., xout = (!!year))$y)) %>%
    dplyr::mutate(country = country_name, year = (!!year)) %>%
    dplyr::select(country, year, P, G, g, E, F, e, f, ef)
  if (nrow(data) == 0 && is.null(country_code)) {
    warning("There is no data for country ", country_name)
  }
  data
}


#' Get emission factors for different energy sources
#'
#' @return a tibble of values for emissions factors, in million metric
#'         tons of carbon dioxide per quad of energy.
#' @export
emissions_factors <- function() {
  tibble(
    fuel = c("Coal", "Oil", "Natural Gas", "Nuclear", "Renewables"),
    emission_factor = c(94.4, 70.0, 53.1, 0.0, 0.0)
  ) %>%
    mutate(fuel = ordered(fuel, levels = c("Coal", "Natural Gas", "Oil",
                                           "Nuclear", "Renewables")))
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
    fuel = c("Coal", "Nuclear", "Natural Gas", "Solar Thermal", "Wind"),
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
#' @export
megawatts_per_quad <- function() {
  1.1E4
}
