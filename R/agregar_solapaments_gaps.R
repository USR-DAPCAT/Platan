#' @title Agrega solapamentsamb gaps
#' @description Historic de farmacs.Elimina solapaments i discontinuitats petites i retorna dades sense discontinuitats ni solapaments.
#' @param dt xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
#' @param id xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
#' @param datainici xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
#' @param datafinal xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
#' @param gap xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
#' @param sel xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
#' @return Retorna dades amb : id, datainici i datafi amb menys registres, havent eliminat solapaments i gaps (discontinuitat petita)
#' @export agregar_solapaments_gaps
#' @examples
#' u=rnorm(1000,100,12)
#'
agregar_solapaments_gaps<-function(dt=dades,id="idp",datainici="data",datafinal="datafi",gap=5,sel=F){

  # dt=FX.FACTURATS_PRESCRITS_GRUPS
  # gap=60
  # datainici="dat"
  # datafinal="datafi"
  # id="idp"

  # Conversió a Sym per evaluació
  datainici_sym<-rlang::sym(datainici)
  datafinal_sym<-rlang::sym(datafinal)
  idp_sym=rlang::sym(id)

  # Seleccionar dades necessaries amb noms sense sym::
  dt<-dt %>% dplyr::select(idp=!!idp_sym, data=!!datainici_sym,datafi=!!datafinal_sym)%>%
    mutate(data=lubridate::ymd(data),datafi=lubridate::ymd(datafi))

  #filtrem els errors!!!!
  origen<-dt
  dt<-dt%>%mutate(error=case_when(datafi<data~1 ,
                                  is.na(data) ~ 1,
                                  is.na(datafi) ~ 1,
                                  TRUE ~0))
  # Printa errors
  if(sel){
    errors<-dt %>% dplyr::filter(error == 1)
    warning("ull! aquests són possibles d'errors de dates!,que s'han ELIMINAT!")
  }
  # Filtra
  if (sel) { dt<-dt %>% dplyr::filter(error == 0) }
  if (sel==F) { dt<-dt }

  # 1. Eliminar solapaments [!!!]
  dt2<-dt %>%
    group_by(idp) %>% arrange(data) %>%
    mutate(indx = c(0, cumsum(as.numeric(lead(data)) >cummax(as.numeric(datafi)+gap))[-n()]))%>%
    group_by(idp, indx) %>%
    summarise(data = min(data), datafi = max(datafi),.groups = 'drop') %>%
    dplyr::select(-indx) %>% ungroup()

  # list(dades0=origen,dades1=dt,dades2=dt2)

  # Renombro noms dels camps originals
  colnames(dt2)<-c(idp_sym,datainici_sym,datafinal_sym)

  dt2

}
