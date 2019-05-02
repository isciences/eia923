#' Return the download URL for a given year's EIA-923 data
#'
#' @param year year of data
#' @return url to zipfile
#' @export
url_for_year <- function(year) {
  stopifnot(year >= 2001)

  sprintf('https://www.eia.gov/electricity/data/eia923/%sxls/f%d_%d.zip',
          ifelse(year < 2018, 'archive/', ''),
          ifelse(year < 2008, 906920, 923),
          year)
}
