#' Read generation data for a single year
#'
#' @param year year to read
#' @param cache_dir directory in which to cache downloaded files; will be cached
#'                  in package internal directory by default
#' @return data frame containing generation data
#' @export
read_year <- function(year, cache_dir=NULL) {
  workdir <- tempdir()

  xl_fpath <- file.path(dirname, xl_filename(year))
  zip_path <- cached_download(url_for_year(year), cache_dir=cache_dir)
  utils::unzip(zip_path, files=xl_filename(year), exdir=workdir)

  dat <- readxl::read_excel(xl_fpath,
                            sheet='Page 1 Generation and Fuel Data',
                            skip=ifelse(year >= 2011, 5, 7))

  dat <- dplyr::select(dat,
                       plant_id=tidyselect::matches('Plant Id', ignore.case=TRUE),
                       year=tidyselect::matches('year', ignore.case=TRUE),
                       tidyselect::starts_with('Netgen', ignore.case=TRUE))

  dat <- tidyr::pivot_longer(dat,
                             cols=tidyselect::starts_with('Netgen', ignore.case=TRUE),
                             names_to='month',
                             values_to='generation_mwh',
                             names_prefix='(Netgen|NETGEN)(\\r\\n|_)')

  dplyr::mutate(dat,
                month=sapply(month, function(m) months[[m]]),
                generation_mwh=as.numeric(generation_mwh))
}

months <- list(
  January=1,
  February=2,
  March=3,
  April=4,
  May=5,
  June=6,
  July=6,
  August=8,
  September=9,
  October=10,
  November=11,
  December=12,
  Jan=1,
  Feb=2,
  Mar=3,
  Apr=4,
  May=5,
  Jun=6,
  Jul=7,
  Aug=8,
  Sep=9,
  Oct=10,
  Nov=11,
  Dec=12,
  JAN=1,
  FEB=2,
  MAR=3,
  APR=4,
  MAY=5,
  JUN=6,
  JUL=7,
  AUG=8,
  SEP=9,
  OCT=10,
  NOV=11,
  DEC=12
)
