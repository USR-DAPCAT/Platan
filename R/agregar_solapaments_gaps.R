#' @title                    agregar_solapaments_gaps
#' @description              agregar_solapaments_gaps
#' @param dt                 dt
#' @param id                 id
#' @param datainici          datainici
#' @param datafinal          datafinal
#' @param gap                gap
#' @param sel                sel
#' @export                   agregar_solapaments_gaps
agregar_solapaments_gaps<-function(dt="dades",
                                   id="idp",
                                   datainici="data",
                                   datafinal="datafi",
                                   gap=5,
                                   sel=F){

  # NUMERO_24)

  #

  # Historic de farmacs: idp, datinici,datafi, gap
  # Elimina solapaments i discontinuitats petites i retorna dades sense discontinuitats ni solapaments amb igual o menys registres
  # Retorna dades amb : id, datainici i datafi amb menys registres, havent eliminat solapaments i gaps (discontinuitat petita)



  # dt=FX.FACTURATS_PRESCRITS_GRUPS
  # gap=60
  # datainici="dat"
  # datafinal="datafi"
  # id="idp"

  # Conversio a Sym per evaluacio
  datainici_sym<-rlang::sym(datainici)
  datafinal_sym<-rlang::sym(datafinal)
  # idp_sym=rlang::sym(id)
  idp_sym=id

  # Seleccionar dades necessaries amb noms sense sym::
  # dt<-dt %>% dplyr::select(idp=!!idp_sym, data=!!datainici_sym,datafi=!!datafinal_sym)%>%
  #   mutate(data=lubridate::ymd(data),datafi=lubridate::ymd(datafi))

  dt<-dt %>% dplyr::select(idp_sym, data=!!datainici_sym,datafi=!!datafinal_sym)%>%
    dplyr::mutate(data=lubridate::ymd(data),datafi=lubridate::ymd(datafi))

  #filtrem els errors!!!!
  origen<-dt
  dt<-dt%>%dplyr::mutate(error=dplyr::case_when(datafi<data~1 ,
                                                is.na(data) ~ 1,
                                                is.na(datafi) ~ 1,
                                                TRUE ~0))
  # Printa errors
  if(sel){
    errors<-dt %>% dplyr::filter(error == 1)
    warning("ull! aquests son possibles d'errors de dates!,que s'han ELIMINAT!")
  }
  # Filtra
  if (sel) { dt<-dt %>% dplyr::filter(error == 0) }
  if (sel==F) { dt<-dt }

  # 1. Eliminar solapaments [!!!]
  dt2<-dt %>%
    dplyr::group_by_at(idp_sym) %>% dplyr::arrange(data) %>%
    dplyr::mutate(indx = c(0, cumsum(as.numeric(dplyr::lead(data)) >cummax(as.numeric(datafi)+gap))[-dplyr::n()]))%>%
    dplyr::group_by_at(c(c(idp_sym), "indx")) %>%
    dplyr::summarise(data = min(data), datafi = max(datafi),.groups = 'drop') %>%
    dplyr::select(-indx) %>% dplyr::ungroup()

  # list(dades0=origen,dades1=dt,dades2=dt2)

  # Renombro noms dels camps originals
  colnames(dt2)<-c(idp_sym,datainici_sym,datafinal_sym)

  dt2

}
