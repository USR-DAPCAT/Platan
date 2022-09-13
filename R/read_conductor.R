#' @title read_conductor
#' @description Llegir el fitxer conductor de diferents formats txt o rds o xls xlsx o data_frame tibble
#' @param fitxer fitxer que el convertirem a tibble.
#' @param ... altres funcions
#' @return una taula tibble
#' @export
#' @importFrom dplyr "%>%"

read_conductor<-function(fitxer,...) {
  # fitxer<-here::here(fitxer_cataleg)
  # Si el fitxer es un data_frame saltar
  if (any(class(fitxer) %in% c("tbl_df","tbl","data.frame")))

    dt <- tibble::as_tibble(fitxer)

  else {

    if (stringr::str_detect(fitxer,"\\.txt$")){

      dt<-data.table::fread(fitxer) %>% tibble::as_tibble()

    } else if (stringr::str_detect(fitxer,"\\.rds$")) {

      dt<-readRDS(fitxer,...) %>%tibble::as_tibble()

    } else if (stringr::str_detect(fitxer,"\\.xls$")) {

      dt<-readxl::read_excel(fitxer,...) %>% tibble::as_tibble()

    } else if (stringr::str_detect(fitxer,"\\.xlsx$")) {

      dt<-readxl::read_excel(fitxer,...) %>% tibble::as_tibble()

    } else if (stringr::str_detect(fitxer,"\\.xlS$")) {

      dt<-readxl::read_excel(fitxer,...) %>% tibble::as_tibble()

    } else if (stringr::str_detect(fitxer,"\\.sav$")) {

      dt<-foreign::read.spss(fitxer,use.value.labels = T,to.data.frame = T,...) %>% tibble::as_tibble()
    }
    else {stop("format de dades no reconegut ")}
  }

}

