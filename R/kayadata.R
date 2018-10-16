# kayadata is open-source software; you can redistribute it and/or
# modify it under the terms of the MIT License as published by the Open Source
# Initiative.
#
# kayadata is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
# FITNESS FOR A PARTICULAR PURPOSE.  See the MIT License for more details.
#
# You should have received a copy of the MIT License along with this program; if
# not, see <https://opensource.org/licenses/MIT>.

#' kayadata package
#'
#' @docType package
#' @name kayadata-package
#'
#' @description
#'
#' kayadata is a package for loading Kaya identity data for many countries
#' into R. Data covers 1960-2017 for population and GDP, and 1965-2017 for
#' energy and fossil-fuel CO2 emissions.
#'
#' The package uses data on population and GDP from the World Bank,
#' using market exchange rates (MER) for GDP because those data go back to
#' 1960. From 1990 onward, Purchasing-Power-Parity (PPP) GDP figures are
#' available as \code{G_ppp} but using these would require re-calculating
#' \code{G}, \code{g}, \code{e}, and \code{ef} in the \code{kaya_data}
#' data frame.
#'
#' The package uses data on energy consumption and fossil-fuel CO2 emissions
#' from the
#' \href{https://www.bp.com/en/global/corporate/energy-economics/statistical-review-of-world-energy.html}{2018 BP Statistical Review of World Energy}
#'
#' @section License:
#'
#'  The \pkg{kayadata} package is open source licensed under the
#'  MIT License.
#'
#' @section Bug reports:
#' \itemize{
#'  \item kayadata issue tracker (\url{https://github.com/jonathan-g/kayadata/issues})
#' }
#'
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr distinct filter mutate select summarize summarize_at top_n
#'             funs
#' @importFrom forcats fct_recode
#' @import dplyr
#' @import ggplot2
#' @importFrom stats approx
#' @importFrom tidyr gather spread
#'
NULL
