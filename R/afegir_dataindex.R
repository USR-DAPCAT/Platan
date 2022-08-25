#' @title Afegeix data-index.
#' @description Afegeix data-index Dinamica o / Constant si no existeix
#' @param dt_historic       xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
#' @param bd.dindex xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
#' @return La data índex
#' @export afegir_dataindex
#' @examples
#' u=rnorm(1000,100,12)
#'
afegir_dataindex<-function(dt_historic,bd.dindex="20161231") {

  # dt_historic=dt
  # bd.dindex=bd.dindex

  # Si es una constant generar una columna constant
  if (is.numeric(bd.dindex) | is.character(bd.dindex)){
    rrr<-dt_historic %>%
      dplyr::mutate(dtindex=bd.dindex) %>%
      data.table

  }

  # Si es una bd amb data index fusionar data index
  if (!(is.numeric(bd.dindex) | is.character(bd.dindex))) {

    # Fusionar a l'historic la data index movil
    rrr<-dt_historic %>%
      dplyr::inner_join(bd.dindex, by="idp") %>%
      rename(dtindex=tidyselect::last_col()) %>% ## Renomenar dtindex (última columna de bd.index)
      data.table
  }

  rrr

}



