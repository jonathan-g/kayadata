globalVariables(c("in_range", "fuel", "quads", "pct", "label",
                  "qmin", "qmax", "."))


#' Plot kaya-identity variable
#'
#' @param kaya_data A tibble with kaya-identity data
#'
#' @param variable The name of the variable to plot (character)
#'
#' @param start_year The year to start highlighting the data (should correspond
#' to the beginning of the trend calculation). Set to \code{NULL} to turn off
#' highlighting.
#'
#' @param stop_year The year to stop highlighting the data (should correspond
#' to the beginning of the trend calculation). Set to \code{NULL} to turn off
#' highlighting.
#'
#' @param y_lab Optional label for the y-axis
#'
#' @param log_scale Use log scale for y axis
#'
#' @param trend_line Include a trend line
#'
#' @param points Plot points in addition to the line.
#'
#' @return A plot oblect.
#'
#' @export
#' @importFrom magrittr %>%
plot_kaya <- function(kaya_data, variable,
                      start_year = NA, stop_year = NA,
                      y_lab = NULL,
                      log_scale = FALSE, trend_line = FALSE,
                      points = TRUE) {
  labels <- c(P =  'Population (billions)',
              G =  'Gross Domestic Product ($ trillion)',
              E =  'Energy consumption (quads)',
              F =  'Fossil-fuel carbon emissions (million metric tons)',
              g =  'Per-capita GDP ($ thousand)',
              e =  'Energy intensity of economy (quads per $trillion)',
              f =  'Carbon intensity of energy supply (MMT per quad)',
              ef = 'Carbon intensity of economy (tons CO2 per $million)'
  )

  if (is.null(y_lab)) y_lab <- labels[variable]
  if (is.null(start_year) || is.null(stop_year)) {
    start_year = NULL
    stop_year = NULL
  } else {
    if (is.na(start_year)) start_year = 1980
    if (is.na(stop_year)) stop_year = max(kaya_data$year)
  }

  color_scale = scale_color_manual(values = c("TRUE" = "dark blue",
                                              "PRE" = "cornflowerblue",
                                              "POST" = "cornflowerblue"
  ))
  legend = guides(color = FALSE, shape = FALSE)

  if (!any(is.null(start_year), is.null(stop_year))) {
    df <- bind_rows(
      kaya_data %>% filter(year <= start_year) %>% mutate(in_range = "PRE"),
      kaya_data %>% filter(year >= stop_year) %>% mutate(in_range = "POST"),
      kaya_data %>% filter(between(year, start_year, stop_year)) %>%
        mutate(in_range = "TRUE")
    )
  } else {
    df <- kaya_data %>% mutate(in_range = TRUE)
  }

  p <- ggplot(df, aes_string(x = "year", y = variable, color = "in_range"))
  p <- p +
    geom_line(size = 1, na.rm = TRUE)

  if (points) {
    p <- p + geom_point(size = 3, na.rm = TRUE)
  }
  p <- p + color_scale + legend

  if (log_scale) {
    p <- p + scale_y_log10()
  }

  if (trend_line) {
    p <- p + geom_smooth(method = "lm", data = filter(df, in_range == "TRUE"),
                         na.rm = TRUE)
  }

  p <- p +
    labs(x = "Year", y = y_lab) +
    theme_bw(base_size = 20) +
    theme(axis.title.y = element_text(vjust=1.2),
          axis.title.x = element_text(vjust=-0.1),
          legend.key = element_rect(color = NA))
  p
}


#' Plot fuel mix
#'
#' @param fuel_mix A tibble with the mixture of fuels:
#' \describe{
#'   \item{fuel}{The name of the fuel}
#'   \item{quads}{The number of quads per year the country or region consumes}
#'   \item{pct}{The percentage of the country's energy that comes from that fuel}
#' }
#' @param collapse_renewables Combine Hydro and other Renewables into a single
#'        category.
#' @return A plot oblect.
#'
#' @export
plot_fuel_mix <- function(fuel_mix, collapse_renewables = TRUE) {
  if (collapse_renewables) {
    fuel_mix <- fuel_mix %>%
      mutate(fuel = fct_recode(fuel, Renewables = "Hydro")) %>%
      group_by(fuel) %>% summarize(quads = sum(quads), pct = sum(pct))
    color_scale <- c("Coal" = "#e31a1c", "Natural Gas" = "#fdbf6f",
                     "Oil" = "#ff7f00", "Nuclear" = "#33a04c",
                     "Renewables" = "#b2dfca", "Total" = "#a6cee3")
  } else {
    color_scale <- c("Coal" = "#e31a1c", "Natural Gas" = "#fdbf6f",
                     "Oil" = "#ff7f00", "Nuclear" = "#33a04c",
                     "Hydro" = "#69d9a4", "Renewables" = "#b2dfca",
                     "Total" = "#a6cee3")

  }
  fd <- fuel_mix %>%
    arrange(fuel) %>%
    mutate(qmin = cumsum(lag(quads, default=0)), qmax = cumsum(quads))
  labels <- fd %>% mutate(label = paste0(fuel, ": ", round(quads,2),
                                         " quads (", round(pct,1), "%)")) %>%
    arrange(fuel) %>% select(fuel, label) %>%
    tidyr::spread(key = fuel, value = label) %>% unlist()
  if (FALSE) {
    message(paste0(levels(fd$fuel), collapse=", "))
  }
  ggplot(fd, aes(ymin = qmin, ymax = qmax, fill = fuel)) +
    geom_rect(xmin = 2, xmax = 4, na.rm = TRUE) +
    coord_polar(theta = "y") +
    xlim(c(0,4)) +
    scale_fill_manual(values = color_scale,
                      breaks = names(labels), labels = labels, name = "Fuel") +
    theme_bw(base_size = 20) +
    theme(panel.grid=element_blank(),
          axis.text=element_blank(),
          axis.ticks=element_blank())
}
