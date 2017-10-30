library(tidyverse)
library(readxl)
library(stringr)
library(forcats)
library(janitor)

prepare_regions <- function() {

  oecd = c("Australia", "Austria", "Belgium", "Canada", "Chile",
           "Czech Republic", "Denmark", "Estonia", "Finland", "France",
           "Germany", "Greece", "Hungary", "Iceland", "Ireland",
           "Israel", "Italy", "Japan", "Luxembourg", "Mexico",
           "Netherlands", "New Zealand", "Norway", "Poland", "Portugal",
           "Slovakia", "Slovenia", "South Korea", "Spain",
           "Sweden", "Switzerland", "Turkey", "United Kingdom",
           "United States")

  africa = c("Algeria", "South Africa")

  americas = c("Argentina", "Brazil", "Canada", "Chile", "Colombia", "Ecuador",
               "Mexico", "Peru", "Venezuela", "Trinidad and Tobago",
               "United States")

  asia = c("Afghanistan", "Australia", "Bangladesh", "China",
           "Hong Kong", "India", "Indonesia", "Japan", "Malaysia", "New Zealand",
           "Pakistan", "Philippines", "Singapore", "South Korea", "Thailand",
           "Vietnam")

  eurasia = c("Azerbaijan", "Kazakhstan", "Turkmenistan", "Uzbekistan",
              "Russia")

  europe = c("Austria", "Belarus", "Bulgaria", "Belgium", "Czech Republic",
             "Denmark", "Finland", "France", "Germany", "Greece", "Hungary",
             "Ireland", "Israel", "Italy", "Lithuania", "Netherlands", "Norway",
             "Poland", "Portugal", "Romania", "Slovakia", "Spain", "Sweden",
             "Switzerland", "Turkey", "Ukraine", "United Kingdom")

  middle_east = c("Egypt", "Iran", "Kuwait", "Qatar", "Saudi Arabia",
                  "United Arab Emirates")

  oecd_americas = americas %>% keep(~.x %in% oecd)
  non_oecd_americas = americas %>% discard(~.x %in% oecd)

  oecd_asia = asia %>% keep(~.x %in% oecd)
  non_oecd_asia = asia %>% discard(~.x %in% oecd)

  oecd_europe = europe %>% keep(~.x %in% oecd)
  non_oecd_eurasia = c(europe, eurasia) %>% discard(~.x %in% oecd)

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

  regions = lapply(region_lst, function(x) discard(x, ~.x %in% names(td_countries))) %>%
    set_names(names(region_lst))

  invisible(list(td_countries = td_countries, td_regions = td_regions,
                 regions = regions))
}

read_top_down_var <- function(var, file_name) {

  x = prepare_regions()

  td_countries = x$td_countries
  td_regions = x$td_regions
  regions = x$regions

  td_translations = c(
    " +/a" = ""
  )

  td_df = read_excel(file.path("raw_data", file_name), range = "A3:J29") %>%
    clean_names() %>% filter(! is.na(x2015)) %>%
    mutate(region = str_replace_all(region, td_translations)) %>%
    rename(trend = average_annual_percent_change_2015_50)

  td_others = td_df %>% filter(region == 'Other') %>%
    mutate(region = str_c(c("Non-OECD Europe and Eurasia", "Non-OECD Asia",
                            "Non-OECD Americas"), region))
  td_df = td_df %>% filter(region != "Other") %>% bind_rows(td_others)

  td = tibble()

  for (ctry in names(td_countries)) {
    cx = td_countries[ctry]
    td = bind_rows(td, filter(td_df, region == cx) %>% mutate(country = ctry))
  }

  for (rgn in names(td_regions)) {
    rx = td_regions[rgn]
    tx = filter(td_df, region == rx)
    for (ctry in regions[[rgn]]) {
      td = bind_rows(td, tx %>% mutate(country = ctry))
    }
  }

  tdx = td %>% select(-region, -trend) %>%
    gather_(key = "year", value = "value", select_vars(names(.), -country)) %>%
    mutate(year = str_replace(year, "^x", "") %>% as.integer(), variable = var)

  td_trend = td %>% select(country, trend) %>% mutate(variable = var)

  invisible(list(trend = td_trend, values = tdx))
}

prepare_top_down <- function() {
  files = c(E = "ieotab_1.xls",
            P = "ieotab_14.xlsx",
            G = "ieotab_3.xls",
            F = "ieotab_10.xlsx")

  td_values = tibble()
  td_trends = tibble()

  for (var in names(files)) {
    td = read_top_down_var(var, files[var])
    td_values = bind_rows(td_values, td$values)
    td_trends = bind_rows(td_trends, td$trend)
  }

  td_values = spread(td_values, key = variable, value = value) %>%
    mutate(G = G / 1000, P = P / 1000)
  td_trends = spread(td_trends, key = variable, value = trend)

  save(td_values, file = file.path("data", "td_values.rda"))
  save(td_trends, file = file.path("data", "td_trends.rda"))
}
