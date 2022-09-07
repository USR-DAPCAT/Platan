#' @title Agregar facturacio.
#' @description Retorna tibble (data.table) amb la suma d'envasos o data primera dispensació dins d'una finestra de temps per idp-dataindex
#' @param dt Base de dades de Fármac Facturats (idp,cod-A10BB01-,dat-200801,env).
#' @param finestra.dies Finestra de dies a partir de la data.index.
#' @param dt.agregadors Catàleg, a on tenim els agregadors a prtir del codi.
#' @param bd.dindex Data on comencem a contar els dies
#' @param prefix  Prefix dels agregadors, normalment FF.
#' @param camp_agregador Camp agregador.
#' @param agregar_data Ens diu la data del fàrmac dins la finestra.
#' @param acumular Null.
#' @param cataleg_mana Catàleg
#' @return Taula agregada facturacio
#' @export
#' @importFrom dplyr "%>%"
#' @examples
#' idp=rep(1:5,each=5)
#' dat=rep(c(200801,200801,200801,200801,200802),times=5)
#' cod=rep(c("A10BB01","A10BD01","A10BD04","A10BA02","J01DD07"),times=5)
#' env=rep(1:5,each=5)
#' dt_facturacio<-data.frame(idp=idp,cod=cod,dat=dat,env=env)
#'
#' domini="farmacs_facturats"
#' cod=c("A10BB01","A10BD01","A10BD04","A10BA02","J01DD07")
#' agr_Farmac=c("Sulfonilureas","Biguanidas","Tiazolidinadiones","Biguanidas","Antibioticos")
#' dt_cataleg<-data.frame(domini=domini,cod=cod,agr_Farmac=agr_Farmac)
#'
#' dt.agregadors=dplyr::select(dt_cataleg,cod,agr=agr_Farmac)
#'
#' dtagr_facturacio<-agregar_facturacio(
#' dt=dt_facturacio,
#' bd.dindex=20080120,
#' finestra.dies=c(-90,0),
#' dt.agregadors=dt.agregadors,
#' prefix="FF.",
#' camp_agregador="agr",
#' agregar_data=TRUE,
#' cataleg_mana=TRUE,
#' acumular=NULL)
#' dtagr_facturacio

agregar_facturacio<-function(dt=dt_facturacio,
                             finestra.dies=c(-90,0),
                             dt.agregadors=dt.agregadors,
                             bd.dindex="20161231",
                             prefix="FD.",
                             camp_agregador="agr",
                             agregar_data=F,
                             acumular=NULL,
                             cataleg_mana=F){



  # dt=dt_facturats_dosis
  # finestra.dies = c(0,90)
  # camp_agregador="agr"
  # dt.agregadors = conductor_idpp4
  # bd.dindex = dt_dindex
  # prefix="FDD1."
  # agregar_data=F
  # acumular="DD_env"
  # cataleg_mana=T
  # rm(list=ls())

  agregador_sym<-rlang::sym(camp_agregador)
  ## Filtrar CATALEG per agrupador per camp_agregador
  dt.agregadors<-dt.agregadors %>% dplyr::select(cod,agr=!!agregador_sym)
  dt.agregadors<-dt.agregadors %>% filter(!is.na(agr))

  # filtrar dt farmacs només per agregadors d'interes (camp_agregador)
  print("Filtrant per farmac agregador")
  dt<-dt %>%dplyr::semi_join(dt.agregadors, by="cod")

  #### Afegir data index en l'historic de farmacs
  print("Afegint data index en historic de farmacs")
  dt<-afegir_dataindex(dt,bd.dindex)

  # Si no existeix agr el creo
  if (!("agr" %in% colnames(dt))) { dt<-dt %>%dplyr::mutate(agr=NA) }

  #### Filtrar dt  per finestra temporal i genero data i datafi
  print("Filtrant historic per finestra temporal i generant data i datafi")
  # Recode els numeros infinits
  finestra.dies=ifelse(finestra.dies==+Inf,99999,finestra.dies)
  finestra.dies=ifelse(finestra.dies==-Inf,-99999,finestra.dies)
  ##

  # Filtro missings en dtindex (Si no peta)
  dt<-dt %>% dplyr::filter(!is.na(dtindex))

  pepito<-dt %>% dplyr::mutate (
    data=lubridate::ymd(paste0(as.character(dat),"15")),    # Data arrodonida al dia 15
    datafi=data+(env*30),                  # Genero data fi en funció dels envasos
    dtindex=lubridate::ymd(dtindex)) %>%
    tibble::as_tibble()

  # Estimo el nombre d'envasos de solapament per codi i agrego per codi diferent
  print ("Estimo el nombre d'envasos de solapament per codi i agrego per codi diferent")

  pepito<-pepito %>%
    dplyr::mutate(interval2=dtindex+finestra.dies[2],
                  interval1=dtindex+finestra.dies[1],
                  overlap = pmax(pmin(interval2, datafi) - pmax(interval1,data) + 1,0),
                  overlap=as.numeric(overlap),
                  env2=overlap/30) %>%
    dplyr::select(-agr,-dat,-interval2,-interval1,env,-env,env=env2) %>%      # Netejo variables
    dplyr::filter(env>0.05)    # Selecciono files amb solapament d'envasos dins finestra (Elimino env>0.05)


  # Capturo Agregador de CATALEG i elimino duplicats

  print("Capturo Agregador de CATALEG i elimino duplicats")
  pepito<- pepito %>%
    dplyr::inner_join(dplyr::select(dt.agregadors,c(cod,agr)), by="cod") %>%      # Capturo agregador del cataleg
    dplyr::distinct(idp,dtindex,cod,agr,data,datafi,.keep_all = TRUE) %>%         # Elimino duplicats per idp-dtindex-cod-agr
    tibble::as_tibble()

  # Agregació de nombre d'envasos per defecte
  print("Agregant facturacio")

  if (!(agregar_data)) {
    dt_agregada <- pepito %>%                   # Agrego --> Suma de numero d'envasos per idp-dtindex-agr
      dplyr::select(c(idp,dtindex,agr,env)) %>%
      tibble::as_tibble() %>%
      dplyr::group_by(idp,dtindex,agr) %>%
      dplyr::summarise(FX=sum(env,na.rm=T)) %>%
      dplyr::ungroup()
  }

  # Acumulat de dosis (per exemple)
  if (!is.null(acumular)) {
    acumular<-rlang::sym(acumular)
    dt_agregada <- pepito %>%                   # Agrego --> Suma d'indicador acumulat per idp-dtindex-agr
      dplyr::select(c(idp,dtindex,agr,!!acumular)) %>%
      tibble::as_tibble() %>%
      dplyr::group_by(idp,dtindex,agr) %>%
      dplyr::summarise(FX=sum(!!acumular,na.rm=T)) %>%
      dplyr::ungroup()

  }

  #  Si s'ha d'agregar data primera Facturació
  if (agregar_data){
    dt_agregada <- pepito %>%                    # Agrego --> data mínima
      dplyr::mutate(
        int1=dtindex+finestra.dies[1],  # Si solapament inclou tota la finestra afago limit inferior de la finestra
        data0=ifelse(data>=int1,data,int1)) %>%
      tibble::as_tibble() %>%
      dplyr::select(c(idp,dtindex,agr,data=data0)) %>%
      dplyr::group_by(idp,dtindex,agr) %>%
      dplyr::slice(which.min(data)) %>%
      dplyr::ungroup() %>%
      dplyr::rename(FX=data) %>%
      dplyr::mutate(FX=as.Date(FX,origin = "1970-01-01"))

  }

  # Previ aplanamenta Si vull agregar agregadors missings de cataleg he d'afegir-los abans d'aplanar

  if (cataleg_mana) {
    # Selecciono agregadors en cataleg sense codi en dt
    # tots els codis que tenen algun agregador en dt i els que no

    dt_temp2<-dplyr::select(pepito,cod) %>% distinct(cod) %>%dplyr::left_join(dplyr::select(dt.agregadors,c(cod,agr)),by="cod")
    pp<-dplyr::select(dt.agregadors,agr) %>% distinct() %>%dplyr::anti_join(dt_temp2 %>% distinct(agr),by="agr")
    porca<-dt_agregada %>% distinct(idp,dtindex) %>% base::merge(pp) %>%tibble:: as_tibble()
    # Afegeixo en dt.temp els nous agregadors buits i fusiono amb dt.temp
    dt_agregada<-dt_agregada %>% bind_rows(porca)

  }
  ##################

  # Aplanamenta
  print("Aplanamenta")
  dt_agregada<-dt_agregada %>%
    tidyr::spread(agr,FX,sep=".") %>%
    dplyr::mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .)))
  # mutate_if(is.numeric, list(ifelse(is.na(.), 0, .)))

  names(dt_agregada) <- sub("agr.", prefix, names(dt_agregada))   # Afegir prefix a noms de variables

  dt_agregada

}
