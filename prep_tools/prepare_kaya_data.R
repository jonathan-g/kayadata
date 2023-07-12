#' Load necessary libraries
library(tidyverse)
library(readxl)
library(forcats)
library(janitor)
library(wbstats)
library(rprojroot)

#' Conversion factors:
quad_per_mtoe <- 0.0396832072
quad_per_EJ <- 0.947817120
quad_per_MJ <- quad_per_EJ * 1.0E-12

#' Paths for data files
base_dir <- find_root("DESCRIPTION")
processed_data_path <- file.path(base_dir, "data")
raw_data_path <- file.path(base_dir, "raw_data")

#' URLs, filenames, and World Bank indicator IDs for different variables
data_params <- within(list(), {

  # List of data files, to look up when loading data
  es_year <- 2023
  es_spreadsheet_name <- str_c("EI-stats-review-", es_year, "-all-data.xlsx")
  es_spreadsheet_url <- str_c(
    "https://www.energyinst.org/__data/assets/excel_doc/0007/1055545/",
    "EI-stats-review-all-data.xlsx"
  )
  es_spreadsheet_path <- file.path(raw_data_path, es_spreadsheet_name)
  es_scenario <- str_c("EIStat", es_year)

  es_energy_sheet     <- "Primary Energy Consumption"
  es_co2_sheet        <- "CO2 Emissions from Energy"
  es_fuel_mix_sheet   <- "Primary Energy - Cons by fuel"

  es_energy_file      <- "es_primary_energy.Rds"
  es_co2_file         <- "es_emissions.Rds"
  es_fuel_mix_file    <- "es_fuel_mix.Rds"

  es_energy_path      <- file.path(raw_data_path, es_energy_file)
  es_co2_path         <- file.path(raw_data_path, es_co2_file)
  es_fuel_mix_path    <- file.path(raw_data_path, es_fuel_mix_file)

  wb_pop_id           <- "SP.POP.TOTL"       # Total population
  wb_gdp_ppp_id       <- "NY.GDP.MKTP.PP.KD" # GDP in PPP (const. 2017 int. $)
  wb_gdp_mer_id       <- "NY.GDP.MKTP.KD"    # GDP in market prices (const. 2015 US$)
  wb_deflator_id <- "PA.NUS.PPP"

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
#' geographic names to be consistent across the Energy Institute and World Bank
#' databases.
es_regions <- c("Total North America", "Total S. & Cent. America",
                "Total Europe", "Total CIS", "Total Middle East",
                "Total Asia Pacific", "Total Africa", "Total World",
                "European Union \\#", "Non-OECD", "of which: OECD", "CIS"
)

es_other_nations <- c("Other Caribbean", "Other South America",
                      "Other Europe", "Other CIS", "Other Middle East",
                      "Other Northern Africa", "Other Southern Africa",
                      "Other Asia Pacific",
                      "Central America",
                      "Middle Africa", "Eastern Africa", "Western Africa"
)

es_omit_patterns <- str_c("^([^A-Za-z]", "Notes:", "^Note:", "Growth rates",
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
  ei = c("^Total +" = "", "\\&" = "and", "of which: +" = "",
         "^US$" = "United States",
         "European Union \\#" = "European Union",
         "S\\. +and +Cent\\. +America" = "Latin America and Caribbean",
         "^Russian Federation" = "Russia",
         "China Hong Kong SAR" = "Hong Kong",
         " +" = " ")
)

wb_country_df <- wb_countries() %>%
  mutate_all(str_trim) %>%
  mutate(country = str_replace_all(country, nation_translations$world_bank),
         region = str_replace_all(region, nation_translations$world_bank)) %>%
  select(iso3c, country, regionID = region_iso3c, region) %>%
  distinct() %>%
  mutate(geography = ifelse(country == "world", "world",
                            ifelse(region == "Aggregates", "region",
                                   "nation")) %>%
           ordered(levels = c("nation", "region", "world"))) %>%
  arrange(geography, country)

all_countries <- wb_country_df %>% bind_rows(
  tibble(iso3c = NA,
         country = c("Asia Pacific", "Middle East", "Non-OECD",
                     "USSR"),
         geography = ordered("region", levels = levels(wb_country_df$geography))
  )
) %>% arrange(geography, country) %>% distinct()

fct_wb_country <- ordered(NULL, levels = all_countries$country)

wb_region_df <- wb_country_df %>% select(country, iso3c, region) %>%
  distinct() %>%
  filter(region == "Aggregates") %>%
  select(region = country, regionID = iso3c) %>%
  distinct() %>%
  bind_rows(tibble(region = c("Asia Pacific", "Middle East", "Non-OECD"),
                   regionID = NA)) %>%
  arrange(region)

#' Fix up Energy Institute data from the form in the spreadsheet
fix_es_regions <- function(df) {
  # World Bank region is Europe and Central Asia, which groups CIS in with
  # all European countries.
  not_europe <- df %>%
    filter(geography != "region" | ! place %in% c("CIS", "Europe"))
  europe <- df %>%
    filter(geography == "region", place %in% c("CIS", "Europe"))
  vars <- names(europe) %>%
    discard(~.x %in% c("value", "geography", "place")) %>% syms()
  europe <- europe %>%
    group_by(!!!vars) %>% summarize(value = sum(value), .groups = "drop") %>%
    mutate(place = "Europe and Central Asia", geography = "region")
  df <- bind_rows(not_europe, europe)

  # World Bank North America is US, Canada, and Bermuda
  # Energy Institute North America is US, Canada, and Mexico
  # Harmonize by choosing just US and Canada
  not_north_america <- df %>%
    filter(geography != "region" | place != "North America" )
  north_america <- df %>% filter(place %in% c("United States", "Canada"))
  vars <- names(europe) %>%
    discard(~.x %in% c("value", "geography", "place")) %>% syms()
  north_america <- north_america %>%
    group_by(!!!vars) %>% summarize(value = sum(value), .groups = "drop") %>%
    mutate(place = "North America", geography = "region")
  df <- bind_rows(not_north_america, north_america)

  # World Bank is Latin America and Caribbean, which does not include Bermuda
  # Energy Institute is South and Central America, which includes Bermuda and
  # not Mexico
  # Harmonize by including Bermuda and Mexico
  not_latin_america <- df %>%
    filter(geography != "region" | place != "Latin America and Caribbean")
  latin_america <- df %>%
    filter(place %in% c("Mexico", "Latin America and Caribbean"))
  vars <- names(europe) %>%
    discard(~.x %in% c("value", "geography", "place")) %>% syms()
  latin_america <- latin_america %>%
    group_by(!!!vars) %>% summarize(value = sum(value), .groups = "drop") %>%
    mutate(place = "Latin America and Caribbean", geography = "region")
  df <- bind_rows(not_latin_america, latin_america)

  df <- df %>% left_join(
    wb_country_df %>%
      filter(region != "Aggregates") %>% select(place = country, iso3c),
    by = "place")

  invisible(df)
}

fix_es <- function(df, kaya_var, es_scenario = get("es_scenario",
                                                   envir = globalenv())) {
  var_unit <- names(df)[[1]]
  df <- df %>% clean_names() %>% select(-matches("^x[0-9]{4}_")) %>%
    rename(place = 1) %>%
    filter(!is.na(place), !str_detect(place, es_omit_patterns)) %>%
    gather(-place, key = year, value = value) %>%
    filter(! is.na(place)) %>%
    mutate(year = as.integer(str_replace(year, "^x", "")),
           place = str_trim(place),
           value = as.numeric(value)) %>%
    mutate(geography = ifelse(place == "Total World", "world",
                              ifelse(place %in% es_regions, "region",
                                     ifelse(place %in% es_other_nations,
                                            "others", "nation")))) %>%
    mutate(place = str_replace_all(place, nation_translations$ei)) %>%
    filter(geography != "others") %>%
    select(year, value, place, geography) %>%
    fix_es_regions() %>%
    mutate(indicator = kaya_var, unit = var_unit, model = "History",
           scenario = es_scenario) %>%
    arrange(geography, place, year)
  invisible(df)
}

drop_wb_regions <- function(df) {
  region_isos <- wb_region_df$iso3c

  df <- df %>% filter(! iso3c %in% region_isos)
  invisible(df)
}

make_regions <- function() {
  region_ids <- wb_region_df$regionID
  wb_rc <- set_names(wb_region_df$region, wb_region_df$regionID)

  regions <- list(
    asia_pacific = wb_rc[c("EAS", "SAS")], # East Asia & Pacific; South Asia
    africa = wb_rc[c("MEA", "SSF")],       # North Africa & Middle East; Sub-Saharan Africa
    north_america = wb_rc["NAC"],          # North America
    lac = wb_rc["LCN"]                     # Latin America & Caribbean
  ) %>% as_tibble() %>%
    pivot_longer(everything(), names_to = "var", values_to = "region") %>%
    distinct() %>%
    arrange(var) %>%
    left_join(wb_country_df, by = "region") %>%
    mutate(
      region = case_when(
        var == "asia_pacific" ~ "Asia Pacific",
        var == "africa" ~ "Africa",
        var == "lac" ~ "Latin America and Caribbean",
        TRUE ~ region
      ),
      regionID = case_when(
        var == "asia_pacific" ~ NA_character_,
        var == "africa" ~ "AFR",
        TRUE ~ regionID
      )
    )

  # Energy Institute lumps all Africa together; WB groups North Africa and
  # Middle Eastseparately from Sub-Saharan Africa. Subtract out Middle East
  # from North Africa and Middle East and add the result to Sub-Saharan Africa
  # to get all of Africa
  middle_east <- tibble(var = "middle_east", region = "Middle East",
                        regionID = NA,
                        country = c("Bahrain", "Djibouti", "Iran", "Iraq",
                                    "Israel", "Jordan", "Kuwait", "Lebanon",
                                    "Oman", "Qatar", "Saudi Arabia", "Syria",
                                    "United Arab Emirates",
                                    "West Bank and Gaza", "Yemen")) %>%
    left_join(select(wb_country_df, -region, -regionID), by = "country")

  africa <- regions %>%
    filter(var == "africa") %>%
    filter(! country %in% middle_east$country) %>%
    mutate(region = "Africa")

  regions <- regions %>%
    filter(var != "africa") %>%
    bind_rows(middle_east, africa)

  # Energy Institute lumps all Caribbean nations, including US territories,
  # into LAC/LCN; North America is just USA and Canada.
  # WB puts Bermuda into North America.
  # Here we move Bermuda to LAC so both groups are defined the same way.
  #
  bermuda <- wb_country_df %>%
    filter(country == "Bermuda") %>%
    select(-region, -regionID)

  lac <- regions %>% filter(var == "lac")

  bermuda_lac <- head(lac, 1) %>%
    select(var, region, regionID) %>%
    bind_cols(bermuda)

  north_america <- regions %>% filter(iso3c %in% c("USA", "CAN"))

  regions <- regions %>% filter(! var %in% c("lac", "north_america")) %>%
    bind_rows(north_america, bermuda_lac, lac)

  oecd_countries <- c("Austria", "Belgium", "Czech Republic", "Denmark",
                      "Estonia", "Finland", "France", "Germany", "Greece",
                      "Hungary", "Iceland", "Ireland", "Italy", "Latvia",
                      "Lithuania", "Luxembourg", "Netherlands", "Norway",
                      "Poland", "Portugal", "Slovakia", "Slovenia", "Spain",
                      "Sweden", "Switzerland", "Turkey", "United Kingdom",
                      "Australia", "Canada", "Chile", "Israel", "Japan",
                      "Mexico", "New Zealand", "South Korea", "United States")

  non_oecd_countries <- wb_country_df %>%
    filter(geography == "nation") %>%
    pull(country) %>%
    discard(~.x %in% oecd_countries)

  oecd <- tibble(var = "oecd", region = "OECD", regionID = "OED",
                 country = oecd_countries) %>%
    left_join(select(wb_country_df, -region, -regionID), by = "country")

  non_oecd <- tibble(var = "non_oecd", region = "Non-OECD", regionID = NA,
                     country = non_oecd_countries) %>%
    left_join(select(wb_country_df, -region, -regionID), by = "country")

  regions <- regions %>% bind_rows(oecd, non_oecd)
  invisible(regions)
}

region_df <- make_regions()

gen_region <- function(regions, df, var, name, df_vars = NULL,
                       ignore_na = FALSE) {
  if (is.null(df_vars)) {
    df_vars <- names(df) %>%
      discard(~.x %in% c("value", "place", "country", "iso3c",
                         "footnote", "last_updated", "obs_status")) %>%
      syms()
  }
  rgn <- regions %>% filter(var == !!var)
  region_names <- rgn %>% select(country = region, iso3c = regionID) %>%
    distinct()
  if (nrow(region_names) != 1) {
    stop("Can't build region names for var = ", var)
  }
  rgn <- rgn %>%
    left_join(df, by = "country", suffix = c("", ".ctry")) %>%
    filter(! is.na(indicator))
  tst <- rgn %>%
    mutate(wrong = iso3c != iso3c.ctry) %>%
    filter(wrong)
  if (nrow(tst) > 0) {
    wrong_ctry <- tst$country
    warning("Mismatch ISO codes in ", name, " for countries ",
            str_c(wrong_ctry, collapse = ", "))
  }
  rgn <- rgn %>% select(-iso3c.ctry) %>%
    group_by(!!!df_vars)

  if (ignore_na) {
    rgn <- rgn %>%
      summarize(value = ifelse(all(is.na(value)), NA,
                               sum(value, na.rm = TRUE)),
                .groups = "drop")

  } else {
    rgn <- rgn %>%
      summarize(value = sum(value, na.rm = FALSE), .groups = "drop")

  }

  rgn <- rgn %>% left_join(region_names, by = character())

  invisible(rgn)
}

fix_wb_regions <- function(df, ignore_na = FALSE) {
  region_ids <- wb_region_df$regionID
  wb_rc <- set_names(wb_region_df$region, wb_region_df$regionID)

  world <- df %>% filter(country %in% c("World"))

  # Don't drop OECD, World, Europe & Central Asia
  clean_df <- df %>%
    filter(! iso3c %in% discard(region_ids, ~.x %in% c("OED", "WLD", "ECS")))

  vars <- names(df) %>%
    discard(~.x %in% c("value", "place", "country", "iso3c",
                       "footnote", "last_updated", "obs_status")) %>%
    syms()

  # Energy Institute puts all of Asia and Pacific together, so do the same here.
  asia_pacific  <- gen_region(region_df, df, "asia_pacific", "Asia Pacific",
                              ignore_na = ignore_na)
  middle_east   <- gen_region(region_df, df, "middle_east", "Middle East",
                              ignore_na = ignore_na)
  africa        <- gen_region(region_df, df, "africa", "Africa",
                              ignore_na = ignore_na)
  north_america <- gen_region(region_df, df, "north_america", "North America",
                              ignore_na = ignore_na)
  lac           <- gen_region(region_df, df, "lac",
                              "Latin America and Caribbean",
                              ignore_na = ignore_na)
  non_oecd      <- gen_region(region_df, df, "non_oecd", "Non-OECD",
                              ignore_na = ignore_na)

  df <- bind_rows(clean_df, asia_pacific, middle_east, africa, north_america,
                  lac, non_oecd)
  invisible(df)
}

#' Load data from the Energy Institute spreadsheet
load_es <- function(fname, sheet, kaya_var, unit_id, unit, scale = 1.0,
                    scenario = NULL) {
  if (is.null(scenario))
    scenario <- data_params$es_scenario
  df <- read_excel(fname, sheet = sheet, col_names = TRUE,
                   skip = 2, na = c("", "NA", "na", "n/a", "N/A")) %>%
    clean_names()
  dups <- names(df) %>% keep(~str_detect(.x, "[0-9]{4}_[0-9]")) %>%
    { set_names(str_replace_all(., "_[0-9]+$", ""), .) } %>%
    { dd <- duplicated(.); .[!dd] }
  fixit <- sort(dups, decreasing = TRUE, na.last = TRUE)[1]
  df <- df %>% rename(!!fixit := names(fixit)) %>%
    select(-matches("^x[0-9]+_[0-9]+$"))
  df <- df %>% fix_es(kaya_var, scenario) %>%
    # Use quosure for unit to reference "unit" in parent environment,
    # not column of tibble.
    mutate(value = value / scale, unit_id = unit_id, unit = (!!unit),
           geography = ordered(geography,
                               levels = c("nation", "region", "world")),
           place = ordered(place, levels = levels(fct_wb_country))) %>%
    select(model, scenario, place, year, indicator, value, unit_id, unit,
           geography, iso3c) %>%
    filter(!is.na(place))

  invisible(df)
}

load_wb <- function(indicator_id, kaya_var, unit_id, unit, scale = 1.0,
                    scenario = "World Bank",
                    regions = c("fix", "drop", "wb", "original"),
                    ignore_na = FALSE) {
  region_flag <- match.arg(regions)
  df <- wb_data(indicator = indicator_id, country = "all",
                return_wide = FALSE) %>%
    select(-iso2c) %>%
    mutate(indicator = kaya_var,
           country = country %>% str_trim() %>%
             str_replace_all(nation_translations$world_bank))
  if (region_flag == "fix") {
    df <- df %>% fix_wb_regions(ignore_na = ignore_na)
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
                              ifelse(place %in% wb_region_df$region,
                                     "region", "nation")) %>%
             ordered(levels = c("nation", "region", "world")),
           iso3c = ifelse(geography == "nation", iso3c, NA),
           place = ordered(place, levels = levels(fct_wb_country))) %>%
    select(model, scenario, place, year, indicator, value, unit_id, unit,
           geography, iso3c) %>%
    arrange(geography, place)
  invisible(df)
}

load_primary_energy <- function(fname) {
  df <- load_es(fname, data_params$es_energy_sheet, "E", "quad", "Quad",
                1.0 / quad_per_EJ)
  invisible(df)
}

load_co2 <- function(fname) {
  df <- load_es(fname, data_params$es_co2_sheet, "F", "mmtco2",
                "Million Metric Tons CO2")
  invisible(df)
}

load_fuel_mix <- function(fname) {
  col_names <- read_excel(fname, data_params$es_fuel_mix_sheet, skip = 1,
                          col_names = FALSE, col_types = "text", n_max = 2) %>%
    clean_names() %>%
    mutate(x1 = c("year", "variable")) %>%
    gather(-x1, key = column, value = name) %>%
    spread(key = x1, value = name) %>%
    mutate(column = str_replace_all(column, "^x+", "") %>% as.integer()) %>%
    arrange(desc(column)) %>%
    mutate(year = cummin(ifelse(is.na(year), 9999, year))) %>%
    mutate(variable = str_replace_all(variable, "- *", "") %>%
             str_c(year, sep = ".")) %>%
    select(column, variable) %>%
    bind_rows(tibble(column = 1, variable = "place")) %>%
    arrange(column)
  fuel_mix <- read_excel(fname, data_params$es_fuel_mix_sheet,
                         range = cell_limits(ul = c(5,1),
                                             lr = c(NA, nrow(col_names))),
                         col_names = col_names$variable,
                         na = c("", "na", "NA", "n/a", "N/A")) %>%
    clean_names() %>%
    filter(!is.na(place))
  fuel_mix <- fuel_mix %>% gather(-place, key = variable, value = value) %>%
    mutate(year = str_replace(variable, "[a-z_]+_([0-9]+)$", "\\1") %>%
             as.integer(),
           value = as.numeric(value),
           fuel = str_replace_all(variable, "_[0-9]+$", "")) %>%
    select(-variable)
  fuel_mix <- fuel_mix %>%
    mutate(place = str_trim(place)) %>%
    filter(!is.na(place), !str_detect(place, es_omit_patterns)) %>%
    mutate(geography = ifelse(place == "Total World", "world",
                              ifelse(place %in% es_regions, "region",
                                     ifelse(place %in% es_other_nations,
                                            "others", "nation")))) %>%
    mutate(place = str_replace_all(place, nation_translations$ei)) %>%
    filter(geography != "others") %>%
    select(place, year, fuel, value, geography) %>%
    fix_es_regions() %>%
    mutate(value = value * quad_per_EJ, unit_id = "quad", unit = "Quad",
           model = "History", scenario = "EIStat2023") %>%
    select(model, scenario, place, year, fuel, value, unit_id, unit,
           geography, iso3c) %>%
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
           unit, geography, iso3c)

  invisible(fuel_mix)
}

#' Load Kaya-Identity data
#'
prepare_kaya <- function(force_wb = FALSE, force_es = FALSE,
                         force_download = FALSE, ignore_na = TRUE) {
  if ((force_es && force_download) ||
      ! file.exists(data_params$es_spreadsheet_path)) {
    download.file(url = data_params$es_spreadsheet_url,
                  destfile = data_params$es_spreadsheet_path,
                  mode = "wb")
  }

  if (force_es) {
    energy   <- load_primary_energy(data_params$es_spreadsheet_path)
    co2      <- load_co2(data_params$es_spreadsheet_path)
    fuel_mix <- load_fuel_mix(data_params$es_spreadsheet_path)
    write_rds(energy,   file = data_params$es_energy_path)
    write_rds(co2,      file = data_params$es_co2_path)
    write_rds(fuel_mix, file = data_params$es_fuel_mix_path)
  } else {
    energy   <- read_rds(data_params$es_energy_path)
    co2      <- read_rds(data_params$es_co2_path)
    fuel_mix <- read_rds(data_params$es_fuel_mix_path)
  }

  if (force_wb || ! file.exists(data_params$wb_pop_path)) {
    population <- load_wb(data_params$wb_pop_id, kaya_var = "P",
                          unit_id = "billion", unit = "billion", scale = 1E9,
                          ignore_na = ignore_na)
    write_rds(population, file = data_params$wb_pop_path)
  } else {
    population <- read_rds(data_params$wb_pop_path)
  }

  if (force_wb || ! file.exists(data_params$wb_gdp_ppp_path)) {
    gdp_ppp <- load_wb(data_params$wb_gdp_ppp_id, kaya_var = "G_ppp",
                       unit_id = "trillion", unit = "trillion", scale = 1E12,
                       ignore_na = ignore_na)
    write_rds(gdp_ppp, file = data_params$wb_gdp_ppp_path)
  } else {
    gdp_ppp <- read_rds(data_params$wb_gdp_ppp_path)
  }

  if (force_wb || ! file.exists(data_params$wb_gdp_mer_path)) {
    gdp_mer <- load_wb(data_params$wb_gdp_mer_id, kaya_var = "G_mer",
                       unit_id = "trillion", unit = "trillion", scale = 1E12,
                       ignore_na = ignore_na)
    write_rds(gdp_mer, file = data_params$wb_gdp_mer_path)
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
    select(place, geography, iso3c, year, indicator, value) %>%
    pivot_wider(names_from = "indicator", values_from = "value") %>%
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
           P, G, E, F, g, e, f, ef, G_ppp, G_mer) %>%
    group_by(region) %>%
    mutate_at(vars(P:G_mer), list(na = ~all(is.na(.)))) %>%
    ungroup() %>% filter(! P_na, !(G_ppp_na | G_mer_na), !E_na, !F_na) %>%
    select(-ends_with("_na"))

  invisible(kaya_data)
}

prepare_fuel_mix <- function(force_es = FALSE) {
  if (force_es || ! file.exists(data_params$es_fuel_mix_path)) {
    fuel_mix <- load_fuel_mix(data_params$es_spreadsheet_path)
    write_rds(fuel_mix, file = data_params$es_fuel_mix_path)
  } else {
    fuel_mix <- read_rds(data_params$es_fuel_mix_path)
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
                              force_download = FALSE,
                              ignore_na = TRUE) {
  kaya_data <- prepare_kaya(force_wb = force_recalc, force_es = force_recalc,
                            force_download = force_download,
                            ignore_na = ignore_na)
  fuel_mix <- prepare_fuel_mix(force_es = force_recalc)
  tryCatch(usethis::use_data(kaya_data, fuel_mix,
                             internal = FALSE, overwrite = overwrite,
                             compress = "xz"),
           error = warning)
  invisible(list(kaya_data = kaya_data, fuel_mix = fuel_mix))
}

# prepare_data_files()
