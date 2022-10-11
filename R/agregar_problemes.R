#' @title                Agregar problemes de salut.
#' @description          Afegeix data-index Dinamica o / Constant si no existeix
#' @param dt             Base de dades dels Diàgnostics (idp,cod-E11.5-,dat-20150309).
#' @param bd.dindex      Data on comencem a contar els dies.
#' @param dt.agregadors  Catàleg, a on tenim els agregadors a prtir del codi.
#' @param finestra.dies  Finestra de dies a partir de la data.index.
#' @param prefix         Prefix dels agregadors, normalment DG.
#' @param camp_agregador Camp agregador.
#' @param keep.code      Guarda els codis.
#' @param cataleg_mana   Catàleg.
#' @return               Taula agregada de problemes
#' @export
#' @importFrom           dplyr "%>%"
#' @examples
#'
#' dtagr_diagnostics<-agregar_problemes(dplyr::select(dt_diagnostics,idp,cod,dat),
#' bd.dindex = 20100101,
#' dt.agregadors=dt_cataleg,
#' finestra.dies=c(-Inf,0),prefix = "DG.",
#' cataleg_mana=TRUE)
#'
#'dtagr_diagnostics
#'
agregar_problemes<-function(dt="PROBLEMES",bd.dindex="20161231",dt.agregadors="CATALEG",finestra.dies=c(-Inf,0),prefix="DG.",camp_agregador="agr",keep.code=F,cataleg_mana=F) {


  # dt=dt_problemes
  # bd.dindex ="20191231"
  # dt.agregadors=dt_cataleg
  # finestra.dies=c(-Inf,0)
  # prefix = "DG."
  # camp_agregador = "agr"
  # keep.code=F
  # cataleg_mana=T

  ## afegir en dataindex de BDINDEX si bd.dindex<>""
  #### Afegir + data index (+dtindex) en l'historic de problemes

  dt<-afegir_dataindex(dt,bd.dindex)

  ## filtrar per intervals de dates

  # Convertir dates a numeric
  if (class(dt$dat)=="Date") dt$dat_num=as.numeric(dt$dat)
  if (class(dt$dtindex)=="Date") dt$dtindex_num=as.numeric(dt$dtindex)

  if (class(dt$dat)!="Date") dt$dat_num=as.Date(as.character(dt$dat),format="%Y%m%d") %>% as.numeric()
  if (class(dt$dtindex)!="Date") dt$dtindex_num=as.Date(as.character(dt$dtindex),format="%Y%m%d") %>% as.numeric()

  dt<-dt %>% tibble::as_tibble()

  ##### filtrar per intervals de dates
  dt<-dt %>% dplyr::filter(dat_num>= dtindex_num +finestra.dies[1] &
                             dat_num<= dtindex_num +finestra.dies[2])

  # dt<-dt[data.table::between(
  #   lubridate::ymd(dat),
  #   lubridate::ymd(dtindex)+finestra.dies[1],
  #   lubridate::ymd(dtindex)+finestra.dies[2])]

  ## Filtrar CATALEG PER CAMP AGREGADOR
  camp_agregador_sym<-rlang::sym(camp_agregador)

  dt.agregadors<-dt.agregadors %>%
    dplyr::select(cod,agr=!!camp_agregador_sym) %>%
    dplyr::filter(!is.na(agr))

  ## Captura d'agregadors     ######
  dt<-dt %>%
    # camps mínims que necessito per agregar
    dplyr::select(c(idp,dtindex,cod,dat)) %>%                                             # Selecciono camps mínims
    # Capturo Agregador de CATALEG
    dplyr::inner_join(dplyr::select(dt.agregadors,c(cod,agr)), by="cod")                  # Capturo agregador del cataleg

  ## Formatejo
  dt.temp<-dt %>%
    # Eliminar duplicats agafant el primer registre (dat mes antiga --> minima)
    # Agrupar= unic reg per idp-agr (mes antic segons data)
    dplyr::group_by(idp,dtindex,agr) %>%                                          # Agrupo per idp agr
    dplyr::slice(which.min(dat)) %>%                                              # Selecciono més antic
    dplyr::ungroup() # desagrupo

  # Si s'han d'incloure els agregadors sense codi en base de dades s'ha d'ampliar dt (dt.temp) i afegir cod null sese dat

  if (cataleg_mana) {
    # Selecciono agregadors en cataleg sense codi en dt
    # tots els codis que tenen algun agregador en dt i els que no
    dt_temp2<-dplyr::select(dt,cod) %>% dplyr::distinct(cod) %>% dplyr::left_join(dplyr::select(dt.agregadors,c(cod,agr)),by="cod")
    pp<-dplyr::select(dt.agregadors,agr) %>% dplyr::distinct() %>% dplyr::anti_join(dt_temp2 %>% dplyr::distinct(agr),by="agr")
    porca<-dt.temp %>% dplyr::distinct(idp,dtindex) %>% merge(pp) %>% tibble::as_tibble()
    # Afegeixo en dt.temp els nous agregadors buits i fusiono amb dt.temp
    dt.temp<-dt.temp %>% dplyr::bind_rows(porca)
  }

  # RESHAPE una data per agregador
  # seleccionar camps i Reshape
  dt.agregat<-dt.temp %>%
    dplyr::select(idp,agr,dat,dtindex) %>%  # Selecciono agregador i data
    # RESHAPE per agregador i em quedo la data
    tidyr::spread(agr,dat,sep=".")    # Reshape

  names(dt.agregat) <- sub("agr.", prefix, names(dt.agregat))   # Afegir prefix en noms de variables

  # Si MANTING codi (cod)
  if (keep.code) {
    dt.agregat_cod<-dt.temp %>%
      dplyr::select(idp,agr,cod,dtindex) %>%  # Selecciono agregador i data
      # RESHAPE per agregador i em quedo la data
      tidyr::spread(agr,cod,sep="_")                                                        # Reshape
    names(dt.agregat_cod) <- sub("agr_", "cod_", names(dt.agregat_cod))
    dt.agregat<-dt.agregat %>% dplyr::left_join(dt.agregat_cod,by=c("idp","dtindex"))
  }

  dt.agregat

}


