#' Read plant data from form EIA-860
#'
#' For each plant, compute the dominant fuel type.
#'
#' @inheritParams read_year
#' @return data frame containing plant data
#' @importFrom magrittr %>%
#' @export
read_plants <- function(cache_dir=NULL) {
  form_860_url <- 'https://www.eia.gov/electricity/data/eia860/xls/eia8602017.zip'
  workdir <- tempdir()

  zip_path <- cached_download(form_860_url, cache_dir=cache_dir)
  xl_fname <- '2___Plant_Y2017.xlsx'
  utils::unzip(zip_path, files=xl_fname, exdir=workdir)
  xl_fpath <- file.path(workdir, xl_fname)

  # Parse out location information for each plant
  plant_dat <- readxl::read_excel(xl_fpath,
                                  sheet='Plant',
                                  skip=1) %>%
    dplyr::select(plant_id='Plant Code',
                  plant_name='Plant Name',
                  address='Street Address',
                  city=City,
                  state=State,
                  zip=Zip,
                  lat=Latitude,
                  lon=Longitude)

  xl_fname <- '3_1_Generator_Y2017.xlsx'
  utils::unzip(zip_path, files=xl_fname, exdir=workdir)
  xl_fpath <- file.path(workdir, xl_fname)

  # Parse information on each generating unit (a plant can have multiple generating units)
  # Ignore all secondary fuels.
  generator_dat <- suppressWarnings(readxl::read_excel(xl_fpath,
                                                       sheet='Operable',
                                                       skip=1)) %>%
    dplyr::select(plant_id='Plant Code',
                  capacity_mw='Nameplate Capacity (MW)',
                  primary_fuel='Energy Source 1')

  # Figure out the dominant fuel type for each plant, based on the dominant fuel
  # of each generating unit.
  dominant_fuel <- generator_dat %>%
    dplyr::group_by(plant_id, primary_fuel) %>%
    dplyr::summarize(capacity_mw=sum(capacity_mw)) %>%
    dplyr::top_n(1, capacity_mw) %>%
    dplyr::select(plant_id, primary_fuel)

  total_capacity <- generator_dat %>%
    dplyr::group_by(plant_id) %>%
    dplyr::summarize(capacity_mw=sum(capacity_mw))

  dplyr::inner_join(plant_dat,
    dominant_fuel, by='plant_id') %>%
    dplyr::inner_join(total_capacity, by='plant_id')
}
