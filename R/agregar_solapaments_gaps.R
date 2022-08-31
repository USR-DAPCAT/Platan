#' @title Agrega solapaments amb gaps
#' @description Historic de farmacs.Elimina solapaments i discontinuitats petites i retorna dades sense discontinuitats ni solapaments.
#' @param dt Base de dades de fàrmacs
#' @param id Identificador
#' @param datainici Data Inici Fàrmac
#' @param datafinal Data Final Fàrmac
#' @param gap GAPS
#' @param sel Selecccionem
#' @return Retorna dades amb : id, datainici i datafi amb menys registres, havent eliminat solapaments i gaps (discontinuitat petita)
#' @export agregar_solapaments_gaps
#' @examples
#' idp=rep(1:5,each=5)
#' dat=rep(c(20080115,20080115,20080115,20080115,20080215),times=5)
#' dbaixa=rep(c(20080215,20080215,20080215,20080215,20080315),times=5)
#' cod=rep(c("A10BB01","A10BD01","A10BD04","A10BA02","J01DD07"),times=5)
#' dt_prescripcions<-data.frame(idp=idp,cod=cod,dat=dat,dbaixa=dbaixa)
#'
#' gaps<-agregar_solapaments_gaps(dt=dt_prescripcions,id="idp",datainici="dat",datafinal="dbaixa",gap=2,sel=F)
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
