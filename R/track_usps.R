
#' track_usps
#'
#' @param number tracking number as numeric or string
#'
#' @return string containing status of package
#' @export
#'
#' @import httr xml2 stringr rvest magrittr
#'
#' @examples track_usps(3505504435669285650242)
track_usps <- function(number) {

  url_full <- paste0("https://tools.usps.com/go/TrackConfirmAction.action?tLabels=", number)

  url_read <- xml2::read_html(url_full)

  status_raw <- rvest::html_nodes(url_read, css = ".delivery_status")

  status_clean <- rvest::html_text(status_raw) %>%
    stringr::str_replace_all("\\n", "") %>%
    stringr::str_replace_all("\\t", "") %>%
    stringr::str_replace_all("\\r", "") %>%
    stringr::str_trim() %>%
    stringr::str_squish()

  return(status_clean)

}
