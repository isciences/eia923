#' Returns the Excel file name used for a given year's data
#'
#' @param year year
#' @return filename
#' @export
xl_filename <- function(year) {
  if (year >= 2014 || year == 2012) {
    xl_fname <- sprintf('EIA923_Schedules_2_3_4_5_M_12_%04d_Final_Revision.xlsx', year)
  } else if (year >= 2011) {
    xl_fname <- sprintf('EIA923_Schedules_2_3_4_5_%04d_Final_Revision.xlsx', year)
  } else if (year == 2010) {
    xl_fname <- sprintf('EIA923 SCHEDULES 2_3_4_5 Final %04d.xls', year)
  } else if (year == 2009) {
    xl_fname <- 'EIA923 SCHEDULES 2_3_4_5 M Final 2009 REVISED 05252011.XLS'
  } else if (year == 2008) {
    xl_fname <- sprintf('eia923December%04d.xls', year)
  } else if (year >= 2003) {
    xl_fname <- sprintf('f906920_%04d.xls', year)
  } else {
    xl_fname <- sprintf('f906920y%04d.xls', year)
  }

  xl_fname
}
