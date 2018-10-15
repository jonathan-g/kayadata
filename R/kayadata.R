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
#' into R.
#'
#' @section License:
#'
#'  The \pkg{kayatool} package is open source licensed under the
#'  MIT License.
#'
#' @section Bug reports:
#' \itemize{
#'  \item kayatool issue tracker (\url{https://github.com/gilligan-ees-3310/kayadata/issues})
#' }
#'
#'
#' @importFrom magrittr "%>%"
#' @importFrom dplyr distinct filter mutate select summarize summarize_at top_n
#'             funs
#' @importFrom forcats fct_recode
#' @import dplyr
#' @import ggplot2
#' @importFrom stats approx
#' @importFrom tidyr gather spread
#'
NULL
