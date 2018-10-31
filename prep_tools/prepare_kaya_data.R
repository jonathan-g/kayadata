#' Load necessary libraries
library(tidyverse)
library(readxl)
library(forcats)
library(janitor)
library(wbstats)
library(rprojroot)

#' Conversion factors:
quad_per_mtoe <- 0.0396832072
quad_per_MJ <- 9.47817120E-13
quad_per_EJ <- 0.947817120

#' Paths for data files
base_dir <- find_root("DESCRIPTION")
processed_data_path <- file.path(base_dir, "data")
raw_data_path <- file.path(base_dir, "raw_data")

#' URLs, filenames, and World Bank indicator IDs for different variables
data_params <- within(list(), {

  # List of data files, to look up when loading data
  bp_spreadsheet_url  <- str_c("https://www.bp.com/content/dam/bp/en/corporate/excel/",
                               "energy-economics/statistical-review/",
                               "bp-stats-review-2018-all-data.xlsx")
  bp_spreadsheet_name <- "bp-stats-review-2018-all-data.xlsx"
  bp_spreadsheet_path <- file.path(raw_data_path, bp_spreadsheet_name)

  bp_energy_sheet     <- "Primary Energy Consumption"
  bp_co2_sheet        <- "Carbon Dioxide Emissions"
  bp_fuel_mix_sheet   <- "Primary Energy - Cons by fuel"

  bp_energy_file      <- "bp_primary_energy.Rds"
  bp_co2_file         <- "bp_emissions.Rds"
  bp_fuel_mix_file    <- "bp_fuel_mix.Rds"

  bp_energy_path      <- file.path(raw_data_path, bp_energy_file)
  bp_co2_path         <- file.path(raw_data_path, bp_co2_file)
  bp_fuel_mix_path    <- file.path(raw_data_path, bp_fuel_mix_file)

  wb_pop_id           <- "SP.POP.TOTL"       # Total population
  wb_gdp_ppp_id       <- "NY.GDP.MKTP.PP.KD" # GDP in PPP (const. 2011 int. $)
  wb_gdp_mer_id       <- "NY.GDP.MKTP.KD"    # GDP in market prices (const. 2010 US$)
  wb_deflator_2005_id <- "PA.NUS.PPP.05"
  wb_deflator_2011_id <- "PA.NUS.PPP"

  wb_pop_file         <- "wb_pop.Rds"
  wb_gdp_ppp_file     <- "wb_gdp_ppp.Rds"
  wb_gdp_mer_file     <- "wb_gdp_mer.Rds"
  wb_deflator_file    <- "wb_deflator.Rds"

  wb_pop_path      <- file.path(raw_data_path, wb_pop_file)
  wb_gdp_ppp_path  <- file.path(raw_data_path, wb_gdp_ppp_file)
  wb_gdp_mer_path  <- file.path(raw_data_path, wb_gdp_mer_file)
  wb_deflator_path <- file.path(raw_data_path, wb_deflator_file)

})

#' Information for organizing world regions and converting
#' geographic names to be consistent across the BP and World Bank
#' databases.
bp_regions <- c("Total North America", "Total S. & Cent. America",
             "Total Europe", "Total CIS", "Total Middle East",
             "Total Asia Pacific", "Total Africa", "Total World",
             "European Union \\#", "Non-OECD", "of which: OECD", "CIS"
)

bp_other_nations <- c("Other Caribbean", "Other South America",
                   "Other Europe", "Other CIS", "Other Middle East",
                   "Other Northern Africa", "Other Southern Africa",
                   "Other Asia Pacific",
                   "Central America",
                   "Middle Africa", "Eastern Africa", "Western Africa"
)

bp_omit_patterns <- str_c("^([^A-Za-z]", "Notes:", "^Note:", "Growth rates",
                          "w Less|n/a)",
                       "^This does not allow", "^Our data", sep = "|",
                       "^USSR includes Georgia")

nation_translations <- list(
  world_bank = c(", *(Islamic|Arab) +Rep(\\.|ublic) *$" = "",
                 "Syrian Arab Republic" = "Syria",
                 "^Congo, +Dem\\. +Rep\\. *$" = "Congo-Kinshasa",
                 "^Congo, +Rep\\. *$" = "Congo-Brazzaville",
                 "^Korea, +Rep\\. *$" = "South Korea",
                 "^Korea, +Dem\\. +People.s +Rep\\. *$" = "North Korea",
                 "^Lao PDR" = "Laos",
                 "^Micronesia, +Fed\\. +Sts\\. *$" = "Micronesia",
                 "^Macedonia, +FYR *$" = "Macedonia",
                 "^Russian Federation" = "Russia",
                 "^Slovak Republic$" = "Slovakia",
                 "^Taiwan, China" = "Taiwan",
                 "^Yemen, +Rep\\. *$" = "Yemen",
                 " +SAR, +China *$" = "", ", +The" = "",
                 " +members *$" = "", ", +RB *$" = "",
                 "\\&" = "and", " +" = " "),
  bp = c("^Total +" = "", "\\&" = "and", "of which: +" = "",
         "^US$" = "United States",
         "European Union \\#" = "European Union",
         "S\\. +and +Cent\\. +America" = "Latin America and Caribbean",
         "^Russian Federation" = "Russia",
         "China Hong Kong SAR" = "Hong Kong",
         " +" = " ")
)

wb_countries <- wbcountries() %>%
  mutate_all(str_trim) %>%
  mutate(country = str_replace_all(country, nation_translations$world_bank),
         region = str_replace_all(region, nation_translations$world_bank)) %>%
  select(iso3c, iso2c, country, regionID, region) %>% distinct() %>%
  mutate(geography = ifelse(country == "world", "world",
                              ifelse(region == "Aggregates", "region",
                                     "nation")) %>%
           ordered(levels = c("nation", "region", "world"))) %>%
  arrange(geography, country)

all_countries <- wb_countries %>% bind_rows(
  tibble(iso3c = NA, iso2c = NA,
         country = c("Asia Pacific", "Middle East", "Non-OECD",
                     "USSR"),
         geography = ordered("region", levels = levels(wb_countries$geography))
  )
) %>% arrange(geography, country) %>% distinct()

fct_wb_country <- ordered(NULL, levels = all_countries$country)

wb_regions <- wb_countries %>% select(country, iso2c, iso3c, region) %>%
  distinct() %>%
  filter(region == "Aggregates") %>% select(-region) %>%
  bind_rows(tibble(country = c("Asia Pacific", "Middle East", "Non-OECD"),
                   iso2c = NA, iso3c = NA)) %>%
  arrange(country)

#' Fix up BP data from the form in the spreadsheet
fix_bp_regions <- function(df) {
  # World Bank region is Europe and Central Asia, which groups CIS in with
  # all European countries.
  not_europe <- df %>%
    filter(geography != "region" | ! place %in% c("CIS", "Europe"))
  europe <- df %>%
    filter(geography == "region", place %in% c("CIS", "Europe"))
  vars <- names(europe) %>%
    discard(~.x %in% c("value", "geography", "place")) %>% syms()
  europe <- europe %>%
    group_by(!!!vars) %>% summarize(value = sum(value)) %>% ungroup() %>%
    mutate(place = "Europe and Central Asia", geography = "region")
  df <- bind_rows(not_europe, europe)

  # World Bank North America is US, Canada, and Bermuda
  # BP North America is US, Canada, and Mexico
  # Harmonize by choosing just US and Canada
  not_north_america <- df %>%
    filter(geography != "region" | place != "North America" )
  north_america <- df %>% filter(place %in% c("United States", "Canada"))
  vars <- names(europe) %>%
    discard(~.x %in% c("value", "geography", "place")) %>% syms()
  north_america <- north_america %>%
    group_by(!!!vars) %>% summarize(value = sum(value)) %>% ungroup() %>%
    mutate(place = "North America", geography = "region")
  df <- bind_rows(not_north_america, north_america)

  # World Bank is Latin America and Caribbean, which does not include Bermuda
  # BP is South and Central America, which includes Bermuda and not Mexico
  # Harmonize by including Bermuda and Mexico
  not_latin_america <- df %>%
    filter(geography != "region" | place != "Latin America and Caribbean")
  latin_america <- df %>%
    filter(place %in% c("Mexico", "Latin America and Caribbean"))
  vars <- names(europe) %>%
    discard(~.x %in% c("value", "geography", "place")) %>% syms()
  latin_america <- latin_america %>%
    group_by(!!!vars) %>% summarize(value = sum(value)) %>% ungroup() %>%
    mutate(place = "Latin America and Caribbean", geography = "region")
  df <- bind_rows(not_latin_america, latin_america)

  df <- df %>% left_join(
    wb_countries %>%
      filter(region != "Aggregates") %>% select(place = country, iso2c, iso3c),
    by = "place")

  invisible(df)
}

fix_bp <- function(df, kaya_var, bp_scenario = "BPStat2017") {
  var_unit <- names(df)[[1]]
  df <- df %>% clean_names() %>% select(-matches("^x[0-9]{4}_")) %>%
    rename(place = 1) %>%
    filter(!is.na(place), !str_detect(place, bp_omit_patterns)) %>%
    gather(-place, key = year, value = value) %>%
    mutate(year = as.integer(str_replace(year, "^x", "")),
           place = str_trim(place)) %>%
    mutate(geography = ifelse(place == "Total World", "world",
                                ifelse(place %in% bp_regions, "region",
                                       ifelse(place %in% bp_other_nations,
                                              "others", "nation")))) %>%
    mutate(place = str_replace_all(place, nation_translations$bp)) %>%
    filter(geography != "others") %>%
    select(year, value, place, geography) %>%
    fix_bp_regions() %>%
    mutate(indicator = kaya_var, unit = var_unit, model = "History",
           scenario = bp_scenario) %>%
    arrange(geography, place, year)
  invisible(df)
}

drop_wb_regions <- function(df) {
  region_isos <- wb_regions$iso3c

  df <- df %>% filter(! iso3c %in% region_isos)
  invisible(df)
}

fix_wb_regions <- function(df) {
  region_isos <- wb_regions$iso3c
  wb_rc <- set_names(wb_regions$country, wb_regions$iso3c)

  bp_regions <- list(
    asia_pacific = wb_rc[c("EAS", "SAS")], # East Asia & Pacific; South Asia
    africa = wb_rc[c("MEA", "SSF")],       # North Africa & Middle East; Sub-Saharan Africa
    north_america = wb_rc["NAC"],          # North America
    lac = wb_rc["LCN"]                     # Latin America & Caribbean
  )

  asia_pacific <- df %>% filter(country %in% bp_regions$asia_pacific)
  middle_east <- df %>%
    filter(country %in% c("Bahrain", "Djibouti", "Iran", "Iraq", "Israel",
                          "Jordan", "Kuwait", "Lebanon", "Oman", "Qatar",
                          "Saudi Arabia", "Syria", "United Arab Emirates",
                          "West Bank and Gaza", "Yemen"))
  africa <- df %>% filter(country %in% bp_regions$africa)
  bermuda <- df %>% filter(country %in% c("Bermuda"))
  north_america <- df %>% filter(country %in% c("United States", "Canada"))
  lac <- df %>% filter(country %in% c(bp_regions$lac, "Bermuda"))
  oecd <- df %>% filter(country %in% c("OECD"))
  world <- df %>% filter(country %in% c("World"))

  # Don't drop OECD, World, Europe & Central Asia
  clean_df <- df %>%
    filter(! iso3c %in% discard(region_isos, ~.x %in% c("OED", "WLD", "ECS")))

  vars <- names(df) %>%
    discard(~.x %in% c("value", "place", "country", "iso3c", "iso2c")) %>%
    syms()

  # BP puts all of Asia and Pacific together, so do the same here.
  asia_pacific <- asia_pacific %>%
    group_by(!!!vars) %>% summarize(value = sum(value)) %>% ungroup() %>%
    mutate(country = "Asia Pacific")

  # BP lumps all Africa together; WB groups North Africa and Middle East
  # separately from Sub-Saharan Africa. Subtract out Middle East from
  # North Africa and Middle East and add the result to Sub-Saharan Africa
  # to get all of Africa
  middle_east <- middle_east %>%
    group_by(!!!vars) %>% summarize(value = sum(value)) %>% ungroup() %>%
    mutate(country = "Middle East")
  africa <- africa %>% bind_rows(mutate(middle_east, value = -value)) %>%
    group_by(!!!vars) %>% summarize(value = sum(value)) %>% ungroup() %>%
    mutate(country = "Africa")

  north_america <- north_america %>%
    group_by(!!!vars) %>% summarize(value = sum(value)) %>% ungroup() %>%
    mutate(country = "North America")

  lac <- lac %>%
    group_by(!!!vars) %>% summarize(value = sum(value)) %>% ungroup() %>%
    mutate(country = "Latin America and Caribbean")

  non_oecd <- oecd %>% mutate(value = -value) %>% bind_rows(world) %>%
    group_by(!!!vars) %>% summarize(value = sum(value)) %>% ungroup() %>%
    mutate(country = "Non-OECD")

  # message("OECD = (", str_c(names(oecd), collapse = ", "), ")")

  df <- bind_rows(clean_df, asia_pacific, middle_east, africa, north_america,
                  lac, non_oecd)
  invisible(df)
}

#' Load data from the BP spreadsheet
load_bp <- function(fname, sheet, kaya_var, unit_id, unit, scale = 1.0,
                    scenario = "BPStat2017") {
  df <- read_excel(fname, sheet = sheet, col_names = TRUE,
                   skip = 2, na = c("", "NA", "na", "n/a", "N/A"))
  df <- df %>% fix_bp(kaya_var, scenario) %>%
    # Use quosure for unit to reference "unit" in parent environment,
    # not column of tibble.
    mutate(value = value / scale, unit_id = unit_id, unit = (!!unit),
           geography = ordered(geography,
                               levels = c("nation", "region", "world")),
           place = ordered(place, levels = levels(fct_wb_country))) %>%
    select(model, scenario, place, year, indicator, value, unit_id, unit,
           geography, iso3c, iso2c)
    invisible(df)
}

load_wb <- function(indicator_id, kaya_var, unit_id, unit, scale = 1.0,
                    scenario = "World Bank",
                    regions = c("fix", "drop", "wb", "original")) {
  region_flag <- match.arg(regions)
  df <- wb(indicator = indicator_id) %>% as_tibble() %>%
    mutate(indicator = kaya_var,
           country = country %>% str_trim() %>%
             str_replace_all(nation_translations$world_bank))
  if (region_flag == "fix") {
    df <- df %>% fix_wb_regions()
  } else if (region_flag == "drop") {
    df <- df %>% drop_wb_regions()
  } else {
    # no action
  }
  df <- df %>% rename(place = country) %>%
    mutate(year = as.integer(date),
           unit_id = unit_id, unit = unit, value = value / scale,
           model = "History", scenario = scenario,
           geography = ifelse(place == "World", "world",
                                ifelse(place %in% wb_regions$country,
                                       "region", "nation")) %>%
             ordered(levels = c("nation", "region", "world")),
           iso2c = ifelse(geography == "nation", iso2c, NA),
           iso3c = ifelse(geography == "nation", iso3c, NA),
           place = ordered(place, levels = levels(fct_wb_country))) %>%
    select(model, scenario, place, year, indicator, value, unit_id, unit,
           geography, iso3c, iso2c) %>%
    arrange(geography, place)
  invisible(df)
}

load_primary_energy <- function(fname) {
  df <- load_bp(fname, data_params$bp_energy_sheet, "E", "quad", "Quad",
                1.0 / quad_per_mtoe)
  invisible(df)
}

load_co2 <- function(fname) {
  df <- load_bp(fname, data_params$bp_co2_sheet, "F", "mmtco2",
                "Million Metric Tons CO2")
  invisible(df)
}

load_fuel_mix <- function(fname) {
  col_names <- read_excel(fname, data_params$bp_fuel_mix_sheet, skip = 1,
                          col_names = FALSE, col_types = "text", n_max = 2) %>%
    mutate(X__1 = c("year", "variable")) %>%
    gather(-X__1, key = column, value = name) %>%
    spread(key = X__1, value = name) %>%
    mutate(column = str_replace_all(column, "^X_+", "") %>% as.integer()) %>%
    arrange(desc(column)) %>%
    mutate(year = cummin(ifelse(is.na(year), 9999, year))) %>%
    mutate(variable = str_replace_all(variable, "- *", "") %>%
             str_c(year, sep = ".")) %>%
    select(column, variable) %>%
    bind_rows(tibble(column = 1, variable = "place")) %>%
    arrange(column)
  fuel_mix <- read_excel(fname, data_params$bp_fuel_mix_sheet, skip = 3,
                         col_names = col_names$variable,
                         na = c("", "na", "NA", "n/a", "N/A")) %>%
    clean_names()
  fuel_mix <- fuel_mix %>% gather(-place, key = variable, value = value) %>%
    mutate(year = str_replace(variable, "[a-z_]+_([0-9]+)$", "\\1") %>%
             as.integer(),
           fuel = str_replace_all(variable, "_[0-9]+$", "")) %>%
    select(-variable)
  fuel_mix <- fuel_mix %>%
    mutate(place = str_trim(place)) %>%
    filter(!is.na(place), !str_detect(place, bp_omit_patterns)) %>%
    mutate(geography = ifelse(place == "Total World", "world",
                                ifelse(place %in% bp_regions, "region",
                                       ifelse(place %in% bp_other_nations,
                                              "others", "nation")))) %>%
    mutate(place = str_replace_all(place, nation_translations$bp)) %>%
    filter(geography != "others") %>%
    select(place, year, fuel, value, geography) %>%
    fix_bp_regions() %>%
    mutate(value = value * quad_per_mtoe, unit_id = "quad", unit = "Quad",
           model = "History", scenario = "BPStat2017") %>%
    select(model, scenario, place, year, fuel, value, unit_id, unit,
           geography, iso3c, iso2c) %>%
    arrange(geography, year, place)
  fuel_mix <- fuel_mix %>% filter(fuel != "total") %>%
    left_join(fuel_mix %>% filter(fuel == "total") %>%
                select(place, year, total = value),
              by = c("place", "year")) %>%
    mutate(frac = value / total,
           fuel = str_replace_all(fuel, c("_" = " ", " +energy" = "",
                                          " +electric" = "")) %>%
                                    str_to_title() %>% str_trim()) %>%
    select(model, scenario, place, year, fuel, value, total, frac, unit_id,
           unit, geography, iso3c, iso2c)

  invisible(fuel_mix)
}

#' Load Kaya-Identity data
#'
prepare_kaya <- function(force_wb = FALSE, force_bp = FALSE,
                         force_download = FALSE) {
  if ((force_bp && force_download) ||
      ! file.exists(data_params$bp_spreadsheet_path)) {
    download.file(url = data_params$bp_spreadsheet_url,
                  destfile = data_params$bp_spreadsheet_path,
                  mode = "wb")
  }

  if (force_bp) {
    energy   <- load_primary_energy(data_params$bp_spreadsheet_path)
    co2      <- load_co2(data_params$bp_spreadsheet_path)
    fuel_mix <- load_fuel_mix(data_params$bp_spreadsheet_path)
    write_rds(energy,   path = data_params$bp_energy_path)
    write_rds(co2,      path = data_params$bp_co2_path)
    write_rds(fuel_mix, path = data_params$bp_fuel_mix_path)
  } else {
    energy   <- read_rds(data_params$bp_energy_path)
    co2      <- read_rds(data_params$bp_co2_path)
    fuel_mix <- read_rds(data_params$bp_fuel_mix_path)
  }

  if (force_wb || ! file.exists(data_params$wb_pop_path)) {
    population <- load_wb(data_params$wb_pop_id, kaya_var = "P",
                          unit_id = "billion", unit = "billion", scale = 1E9)
    write_rds(population, path = data_params$wb_pop_path)
  } else {
    population <- read_rds(data_params$wb_pop_path)
  }

  if (force_wb || ! file.exists(data_params$wb_gdp_ppp_path)) {
    gdp_ppp <- load_wb(data_params$wb_gdp_ppp_id, kaya_var = "G_ppp",
                       unit_id = "trillion", unit = "trillion", scale = 1E12)
    write_rds(gdp_ppp, path = data_params$wb_gdp_ppp_path)
  } else {
    gdp_ppp <- read_rds(data_params$wb_gdp_ppp_path)
  }

  if (force_wb || ! file.exists(data_params$wb_gdp_mer_path)) {
    gdp_mer <- load_wb(data_params$wb_gdp_mer_id, kaya_var = "G_mer",
                       unit_id = "trillion", unit = "trillion", scale = 1E12)
    write_rds(gdp_mer, path = data_params$wb_gdp_mer_path)
  } else {
    gdp_mer <- read_rds(data_params$wb_gdp_mer_path)
  }

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
  kaya_data <- bind_rows(population, gdp_ppp, gdp_mer, energy, co2) %>%
    select(place, geography, iso2c, iso3c, year, indicator, value) %>%
    spread(key = indicator, value = value) %>%
    mutate(G = G_mer, g = G / P, e = E / G, f = F / E, ef = F / G,
           iso3c = case_when(
             place == "World" ~ "WLD",
             place == "Latin America and Caribbean" ~ "LCN",
             place == "North America" ~ "NAC",
             place == "OECD" ~ "OED",
             place == "Africa" ~ "AFR",
             place == "Europe and Central Asia" ~ "ECS",
             TRUE ~ iso3c)) %>%
    arrange(geography, place) %>%
    select(region = place, region_code = iso3c, geography,
           year,
           P, G, E, F, g, e, f, ef, G_ppp, G_mer)

  invisible(kaya_data)
}

prepare_fuel_mix <- function(force_bp = FALSE) {
  if (force_bp || ! file.exists(data_params$bp_fuel_mix_path)) {
    fuel_mix <- load_fuel_mix(data_params$bp_spreadsheet_path)
    write_rds(fuel_mix, path = data_params$bp_fuel_mix_path)
  } else {
    fuel_mix <- read_rds(data_params$bp_fuel_mix_path)
  }

  fuel_mix <- fuel_mix %>%
    mutate(
      iso3c = case_when(
        place == "World" ~ "WLD",
        place == "Latin America and Caribbean" ~ "LCN",
        place == "North America" ~ "NAC",
        place == "OECD" ~ "OED",
        place == "Africa" ~ "AFR",
        place == "Europe and Central Asia" ~ "ECS",
        TRUE ~ iso3c),
      fuel = ordered(fuel, levels = c("Coal", "Oil", "Natural Gas", "Nuclear",
                                      "Hydro", "Renewables"))
    ) %>%
    select(region = place, region_code = iso3c, geography,
           year, fuel, quads = value, frac)

    invisible(fuel_mix)
}

prepare_data_files = function(overwrite = FALSE, force_recalc = FALSE,
                              force_download = FALSE) {
  kaya_data = prepare_kaya(force_wb = force_recalc, force_bp = force_recalc,
                           force_download = force_download)
  fuel_mix = prepare_fuel_mix(force_bp = force_recalc)
  tryCatch(usethis::use_data(kaya_data, fuel_mix,
                              internal = FALSE, overwrite = overwrite,
                              compress = "xz"),
           error = warning)
}

# prepare_data_files()
