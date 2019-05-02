#' Download a file and cache it locally
#'
#' @param url       url to download
#' @param cache_dir directory to which file should be saved. If not
#'                  provided, it will be saved in the package's directory.
#' @return path to the downloaded file
cached_download <- function(url, cache_dir=NULL) {
  if (is.null(cache_dir)) {
    cache_dir <- file.path(find.package('eia923'), 'cache')
  }

  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive=TRUE)
  }

  fpath <- file.path(cache_dir, basename(url))

  if (!file.exists(fpath)) {
    utils::download.file(url, fpath)
  }

  fpath
}
