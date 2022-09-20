#' @title Afegeix data-index.
#' @description Afegeix data-index Dinamica o / Constant si no existeix
#' @param dt_historic Base de dades medica estructura long amb idp:identificació pacient
#' @param bd.dindex Data Index , que afagirem a la nostra base de dades
#' @return La data índex
#' @export
#' @importFrom dplyr "%>%"
#' @examples
#'
#' variables_dtindex<-afegir_dataindex(dt_variables,bd.dindex=20081231)
#' variables_dtindex

afegir_dataindex<-function(dt_historic,bd.dindex="20161231") {

   #dt_historic=dt
   #bd.dindex=bd.dindex

  #dt_historic=dt_variables
  #bd.dindex="20161231"


  # Si es una constant generar una columna constant
  if (is.numeric(bd.dindex) | is.character(bd.dindex)){
    rrr<-dt_historic%>%
      dplyr::mutate(dtindex=bd.dindex)%>%data.frame

  }

  # Si es una bd amb data index fusionar data index
  if (!(is.numeric(bd.dindex) | is.character(bd.dindex))) {

    # Fusionar a l'historic la data index movil
    rrr<-dt_historic %>%
      dplyr::inner_join(bd.dindex, by="idp")%>%
       dplyr::rename(dtindex=tidyselect::last_col())%>% ## Renomenar dtindex (última columna de bd.index)
        data.frame
  }

  rrr

}



