#' Plot kaya-identity variable
#'
#' @param kaya_data A tibble with kaya-identity data
#'
#' @param variable The name of the variable to plot (character)
#'
#' @param start_year The year to start highlighting the data (should correspond
#' to the beginning of the trend calculation)
#'
#' @param y_lab Optional label for the y-axis
#'
#' @return A plot oblect.
#'
#' @export
kaya_plot <- function(kaya_data, variable, start_year = NULL, y_lab = NULL) {
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
  if (is.null(start_year)) start_year = 1980

  color_scale = scale_color_manual(values = c("TRUE" = "dark blue",
                                              "FALSE" = "cornflowerblue"))
  legend = guides(color = FALSE, shape = FALSE)

  df <- kaya_data %>% filter(year <= start_year) %>% mutate(in_range = "FALSE") %>%
    bind_rows(
      kaya_data %>% filter(year >= start_year) %>% mutate(in_range = "TRUE")
    )

  p <- ggplot(df, aes_string(x = "year", y = variable, color = "in_range"))
  p + geom_line(size = 1) + geom_point(size = 3) +
    color_scale +
    legend +
    labs(x = "Year", y = y_lab) +
    theme_bw(base_size = 20) +
    theme(axis.title.y = element_text(vjust=1.2),
          axis.title.x = element_text(vjust=-0.1),
          legend.key = element_rect(color = NA))
}
