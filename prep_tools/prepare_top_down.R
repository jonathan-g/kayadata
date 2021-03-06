library(magrittr)
library(tidyverse)
library(readxl)
library(stringr)
library(forcats)
library(janitor)
library(wbstats)

prepare_regions <- function() {
  nation_translations <- list(
    world_bank = c(", *(Islamic|Arab) +Rep(\\.|ublic) *$" = "",
                   "Syrian Arab Republic" = "Syria",
                   "^Congo, +Dem\\. +Rep\\. *$" = "Congo-Kinshasa",
                   "^Congo, +Rep\\. *$" = "Congo-Brazzaville",
                   "^Korea, +Rep\\. *$" = "South Korea",
                   "^Korea, +Dem\\. +People.s +Rep\\. *$" = "North Korea",
                   "^Micronesia, +Fed\\. +Sts\\. *$" = "Micronesia",
                   "^Macedonia, +FYR *$" = "Macedonia",
                   "^Russian Federation" = "Russia",
                   "^Slovak Republic$" = "Slovakia",
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

  td_countries = c("United States" = "United States", "Canada" = "Canada",
                   "Mexico" = "Mexico and Chile", "Chile" = "Mexico and Chile",
                   "Japan" = "Japan", "South Korea" = "South Korea",
                   "Australia" = "Australia and New Zealand",
                   "New Zealand" = "Australia and New Zealand",
                   "Russia" = "Russia", "China" = "China", "India" = "India",
                   "Brazil" = "Brazil")

  td_regions = c(oecd_americas = "OECD Americas",
                 oecd_europe = "OECD Europe",
                 oecd_asia = "OECD Asia",
                 non_oecd_eurasia = "Non-OECD Europe and Eurasia Other",
                 non_oecd_asia = "Non-OECD Asia Other",
                 middle_east = "Middle East",
                 africa = "Africa",
                 non_oecd_americas = "Non-OECD Americas Other")

  countries <- wb_countries() %>%
    mutate_all(str_trim) %>%
    mutate(country = str_replace_all(country, nation_translations$world_bank),
           region = str_replace_all(region, nation_translations$world_bank)) %>%
    select(iso3c, iso2c, country, regionID = region_iso3c, region) %>%
    distinct() %>%
    mutate(geography = ifelse(country == "world", "world",
                              ifelse(region == "Aggregates", "region",
                                     "nation")) %>%
             ordered(levels = c("nation", "region", "world"))) %>%
    arrange(geography, country)


  oecd <- c("Australia", "Austria", "Belgium", "Canada", "Chile",
            "Czech Republic", "Denmark", "Estonia", "Finland", "France",
            "Germany", "Greece", "Hungary", "Iceland", "Ireland", "Israel",
            "Italy", "Japan", "South Korea", "Latvia", "Luxembourg", "Mexico",
            "Netherlands", "New Zealand", "Norway", "Poland", "Portugal",
            "Slovakia", "Slovenia", "Spain", "Sweden", "Switzerland", "Turkey",
            "United Kingdom", "United States")

  middle_east <- c("Bahrain", "Djibouti", "Iran", "Iraq", "Israel", "Jordan",
                   "Kuwait", "Lebanon", "Oman", "Qatar", "Saudi Arabia",
                   "Syria", "United Arab Emirates", "West Bank and Gaza",
                   "Yemen")

  eurasia <- countries %>%
    filter(geography == "nation", regionID == "ECS") %$% country
  asia <- countries %>%
    filter(geography == "nation", regionID %in% c("EAS", "SAS")) %$% country
  africa <- countries %>%
    filter(geography == "nation", regionID %in% c("MEA", "SSF"),
           ! country %in% middle_east) %$% country
  americas <- countries %>%
    filter(geography == "nation", regionID %in% c("NAC", "LCN")) %$% country

  oecd_americas <- intersect(americas, oecd)
  non_oecd_americas <- setdiff(americas, oecd)

  oecd_europe <- intersect(eurasia, oecd)
  non_oecd_eurasia <- setdiff(eurasia, oecd)

  oecd_asia <- intersect(asia, oecd)
  non_oecd_asia <- setdiff(asia, oecd)

  region_lst = list(
    oecd_americas = oecd_americas,
    non_oecd_americas = non_oecd_americas,
    oecd_asia = oecd_asia,
    non_oecd_asia = non_oecd_asia,
    oecd_europe = oecd_europe,
    non_oecd_eurasia = non_oecd_eurasia,
    middle_east = middle_east,
    africa = africa
  )

  regions = region_lst %>% map(~discard(.x, .x %in% names(td_countries)))

  region_tbl <- regions %>% map2_df(names(.), ~tibble(rgn = .y, country = .x))
  region_tbl <- td_regions %>%
    map2_df(names(.), ~tibble(rgn = .y, region = .x)) %>%
    full_join(region_tbl, by = "rgn")

  country_tbl <- td_countries %>%
    map2(names(.), ~tibble(country = .y, region = .x,
                           rgn = str_to_lower(.x) %>%
                             str_replace_all("[^a-z0-9]+", "_"))) %>%
    bind_rows()

  region_tbl <- bind_rows(region_tbl, country_tbl)

  invisible(list(td_countries = td_countries, td_regions = td_regions,
                 regions = regions, region_tbl = region_tbl))
}

read_top_down_var <- function(var, file_name, kaya_data) {
  var_name = enquo(var)

  x = prepare_regions()

  td_countries = x$td_countries
  td_regions = x$td_regions
  regions = x$regions

  td_translations = c(
    " +/a" = "",
    "^Total *" = ""
  )

  last_line = c(ieotab_1.xlsx = 30,
                ieotab_3.xlsx = 30,
                ieotab_10.xlsx = 30,
                ieotab_14.xlsx = 31)

  range = str_c("A3:J", last_line[file_name])

  td_df = read_excel(file.path("raw_data", file_name), range = range) %>%
    clean_names() %>%
    rename(trend = average_annual_percent_change_2015_50) %>%
    mutate(trend = trend / 100)

  td_regions <- td_df %>%
    filter(region %in% c("Total OECD", "Total Non-OECD", "Total World")) %>%
    mutate(region = str_replace_all(region, "Total *", ""))

  td_df <- td_df %>%
    filter(! is.na(x2015),
           ! region %in% c("Total OECD", "Total Non-OECD", "Total World")) %>%
    mutate(region = str_replace_all(region, td_translations))

  td_others = td_df %>% filter(region == 'Other') %>%
    mutate(region = str_c(c("Non-OECD Europe and Eurasia", "Non-OECD Asia",
                            "Non-OECD Americas"), region, sep = " "))

  td_df = td_df %>% filter(region != "Other") %>% bind_rows(td_others)

  td_df <- td_df %>% left_join(x$region_tbl, by = "region") %>% select(-rgn) %>%
    filter(!is.na(region)) %>%
    bind_rows(td_regions) %>%
    mutate(region = ifelse(is.na(country), region, country)) %>%
    select(-country)

  td_trend = td_df %>% select(region, trend) %>%
    mutate(variable = var)

  kd <- kaya_data %>% filter(year == 2015) %>%
    select(region, region_code, geography, ref = !!var_name) %>%
    mutate(region = as.character(region))

  td_df <- td_df %>% select(-trend) %>% left_join(kd, by = "region") %>%
    mutate(x = x2015) %>%
    mutate_at(vars(matches("^x2[0-9]+$")),
              list(~. * ref / x)) %>%
    select(-x, -ref) %>%
    gather(key = year, value = value, matches("^x2[0-9]+$")) %>%
             mutate(year = str_replace(year, "^x", "") %>% as.integer(),
                    variable = var) %>%
    filter(! is.na(value))

  td_trend = td_trend %>% left_join(kd, by = "region") %>%
    select(-ref)

  invisible(list(trend = td_trend, values = td_df))
}

prepare_top_down <- function(overwrite = FALSE) {
  files = c(E = "ieotab_1.xlsx",
            P = "ieotab_14.xlsx",
            G = "ieotab_3.xlsx",
            F = "ieotab_10.xlsx")

  load(file.path('data', 'kaya_data.rda'))

  td_values = tibble()
  td_trends = tibble()

  for (var in names(files)) {
    td = read_top_down_var(var, files[var], kaya_data)
    td_values = bind_rows(td_values, td$values)
    td_trends = bind_rows(td_trends, td$trend)
  }

  td_values = spread(td_values, key = variable, value = value) %>%
    mutate(g = G / P, e = E / G, f = F / E, ef = F / G) %>%
    select(region, region_code, geography, year,
           P, G, E, F, g, e, f, ef)
  td_trends = td_trends %>% spread(key = variable, value = trend) %>%
    mutate(g = G - P, e = E - G, f = F - E, ef = F - G) %>%
    select(region, region_code, geography,
           P, G, E, F, g, e, f, ef)


  tryCatch(usethis::use_data(td_values, td_trends, overwrite = overwrite,
                              internal = FALSE, compress = "xz"),
           error = warning)
}
