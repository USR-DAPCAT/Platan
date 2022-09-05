#' @title Afegeix data-index.
#' @description Afegeix data-index Dinamica o / Constant si no existeix
#' @param dt_historic Base de dades medica estructura long amb idp:identificació pacient
#' @param bd.dindex Data Index , que afagirem a la nostra base de dades
#' @return La data índex
#' @export afegir_dataindex
#' @examples
#' idp=rep(1:5,each=5)
#' dat=rep(c(20080101,20070101,20060101,20050101,20040101),times=5)
#' val=round(rnorm(50,5,1.9),digits=2)
#' cod="GLICADA"
#' dt_variables<-data.frame(idp=idp,dat=dat,val=val,cod=cod)
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
      dplyr::mutate(dtindex=bd.dindex)%>%data.table

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



