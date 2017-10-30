library(tidyverse)
library(stringr)
library(readxl)
library(janitor)

#' Conversion factor: quads per MTOE
mtoe = 1 / 25.2 # quads

#' List of data files, to look up when loading data
kaya_data_files <- list(
  # Data from World Bank
  # https://data.worldbank.org/
  # https://data.worldbank.org/indicator/SP.POP.TOTL
  pop = file.path("raw_data", "API_SP.POP.TOTL_DS2_en_csv_v2.csv"),
  # Data from World Bank
  # https://data.worldbank.org/indicator/NY.GDP.MKTP.KD
  # GDP constant 2010 US dollars
  # Because PPP adjusted GDP is not available before 1990
  gdp = file.path("raw_data", "API_NY.GDP.MKTP.KD_DS2_en_csv_v2.csv"),
  # Data from BP Statistical Review of World Energy
  # https://www.bp.com/en/global/corporate/energy-economics/statistical-review-of-world-energy/downloads.html
  energy = file.path("raw_data", "bp-statistical-review-of-world-energy-2017-underpinning-data.xlsx")
)

#' Translations to reconcile different ways of writing nation names in different
#' databases.
nation_translations <- list(
  world_bank = c(", +(Islamic|Arab) +Rep\\. *$" = "",
                 "^Korea, +Rep\\. *$" = "South Korea",
                 "^Russian Federation" = "Russia",
                 "^Slovak Republic$" = "Slovakia",
                 " +SAR, +China *$" = "",
                 " +members *$" = "", ", +RB *$" = ""),
  bp = c("^Total +" = "", "&" = "and", "of which: +" = "",
         "^US$" = "United States",
         "^Russian Federation" = "Russia",
         "China Hong Kong SAR" = "Hong Kong")
)

#' Load Kaya-Identity data
#'
prepare_kaya <- function() {

  population = suppressWarnings(suppressMessages(read_csv(kaya_data_files$pop, skip = 4))) %>%
    clean_names() %>% select(-starts_with('indicator'), -x62) %>%
    gather(key = year, value = population, -(country_name:country_code)) %>%
    mutate(year = str_replace_all(year, '^x', '') %>% as.integer(),
           population = population * 1E-9)

  gdp = suppressWarnings(suppressMessages(read_csv(kaya_data_files$gdp, skip = 4))) %>%
    clean_names() %>% select(-starts_with('indicator'), -x62) %>%
    gather(key = year, value = gdp, -(country_name:country_code)) %>%
    mutate(year = str_replace_all(year, '^x', '') %>% as.integer(),
           gdp = gdp * 1E-12)


  kaya_data = full_join(population, gdp, by = c("country_name", "country_code", "year")) %>%
    rename(P = population, G = gdp, country = country_name) %>%
    mutate(country = str_replace_all(country, nation_translations$world_bank),
           country_code = factor(country_code),
           g = G / P)

  energy = read_excel(kaya_data_files$energy, "Primary Energy Consumption",
                      range = "A3:BA94", na = c('', 'NA', "na", "N/A/", "n/a")) %>%
    clean_names() %>% rename(country = million_tonnes_oil_equivalent) %>%
    filter(! is.na(country)) %>%
    gather(key = year, value = primary_energy_mtoe, -country) %>%
    mutate(year = str_replace(year, "^x", "") %>% as.integer(),
           country = str_replace_all(country, nation_translations$bp),
           primary_energy_quads = primary_energy_mtoe * mtoe)

  carbon <- read_excel(kaya_data_files$energy, "Carbon Dioxide Emissions",
                       range = "A3:BA94", na = c('', 'NA', "na", "N/A/", "n/a")) %>%
    clean_names() %>% rename(country = million_tonnes_carbon_dioxide) %>%
    filter(! is.na(country)) %>%
    gather(key = year, value = mmt_co2, -country) %>%
    mutate(year = str_replace(year, "^x", "") %>% as.integer(),
           country = str_replace_all(country, nation_translations$bp),
           mmtc = mmt_co2 * 12 / (12 + 32))

  energy = inner_join(energy, carbon, by = c('year', 'country')) %>%
    select(country, year, E = primary_energy_quads, F = mmt_co2)

  #
  # P = population in billions
  # G = world GDP in trillion dollars
  # E = energy consumption in quads
  # F = CO2 emissions in million tons of carbon
  #
  # g = thousand dollars per person
  # e = quads per trillion dollars
  # f = million tons of carbon per quad
  #
  kaya_data = kaya_data %>% inner_join(energy, by = c("country", "year")) %>%
    mutate(country = factor(country)) %>%
    mutate(e = E / G, f = F / E, ef = e * f)

  exclude_countries <- c('Other Middle East', 'North America')

  world <- c('World')
  regions <- c('North America')

  kaya_data = kaya_data %>%
    filter(! country %in% exclude_countries) %>%
    mutate(geography = ifelse(country %in% world, 'world',
                              ifelse(country %in% regions, 'region',
                                     'country')) %>%
             ordered(levels = c('country', 'region', 'world'))) %>%
    arrange(desc(geography), country, year)

  invisible(kaya_data)
}

#' Load fuel_mix data
prepare_fuel_mix <- function() {
  fuel_levels <- c('coal', 'gas', 'oil', 'nuclear', 'renewables', 'total')
  fuel_labels <- c('Coal', 'Natural Gas', 'Oil', 'Nuclear', 'Renewables', 'Total')

  sheet = "Primary Energy - Cons by fuel"

  countries = read_excel(kaya_data_files$energy,  sheet, range = 'A3:A94',
                         na = c('-', '', 'NA', 'N/A', 'na', 'n/a')) %>%
    clean_names() %>%
    rename(country = million_tonnes_oil_equivalent)

  year1 = read_excel(kaya_data_files$energy, sheet, range = "H2", col_names = FALSE) %>%
    simplify() %>% unname()

  ebf1 = read_excel(kaya_data_files$energy, sheet, range = 'B3:H94',
                    na = c('-', '', 'NA', 'N/A', 'na', 'n/a')) %>%
    clean_names() %>%
    mutate(year = year1)

  year2 = read_excel(kaya_data_files$energy, sheet, range = "O2", col_names = FALSE) %>%
    simplify() %>% unname()

  ebf2 = read_excel(kaya_data_files$energy, sheet, range = 'I3:O94',
                    na = c('-', '', 'NA', 'N/A', 'na', 'n/a')) %>%
    clean_names() %>%
    mutate(year = year2)

  ebf = bind_rows(ebf1, ebf2) %>%
    rename(gas = natural_gas, nuclear = nuclear_energy, hydro = hydro_electric,
           renewables = renew_ables) %>%
    mutate(renewables = renewables + hydro) %>%
    select(year, oil, gas, coal, nuclear, renewables)

  fuel_mix = bind_rows(countries, countries) %>% bind_cols(ebf) %>%
    filter(!is.na(country)) %>%
    mutate(country = str_replace_all(country, nation_translations$bp)) %>%
    gather(key = fuel, value = quads, -country, -year) %>%
    mutate(fuel = ordered(fuel, levels = fuel_levels, labels = fuel_labels),
           quads = ifelse(is.na(quads), 0.0, quads)) %>%
    group_by(country, year) %>%
    mutate(quads = quads * mtoe, pct = 100 * quads / sum(quads, na.rm = T)) %>%
    ungroup()

  invisible(fuel_mix)
}

prepare_data_files = function() {
  kaya_data = prepare_kaya()
  fuel_mix = prepare_fuel_mix()
  devtools::use_data(kaya_data, fuel_mix)
}

prepare_data_files()
