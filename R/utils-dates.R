#' Parse an excel date (text or integer) into date
#'
#' Excel stores date as days since 1900-01-01
#'
#' @param e_date the text or integer vector
#' @return a correct date
#'
#' @export
alg_parse_excel_date <- function(e_date) {
  excel_epoch <- lubridate::ymd("1900-01-01")
  excel_epoch + as.integer(e_date)
}
