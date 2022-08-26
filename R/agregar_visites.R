#' @title Agregar visites.
#' @description Envio la historic de visites i retorno numero de visites en la finestra de temps
#' @param dt xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
#' @param finestra.dies xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
#' @param bd.dindex xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
#' @param finestra.dies xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
#' @param N xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
#' @param data xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
#' @return Numero de visites en la finestra de temps
#' @export agregar_visites
#' @examples
#' u=rnorm(1000,100,12)
#'
agregar_visites<-function(dt=VISITES,bd.dindex=20161231,finestra.dies=c(-365,0),N="NA",data="NA"){

  # dt=visites_dt
  # bd.dindex = "20161231"
  # finestra.dies=c(-Inf,0)
  # N="NA"
  # data="NA"

  N_sym=rlang::sym(N)
  data_sym=rlang::sym(data)

  ## Afegir en dataindex (+dtindex) en historic de Visites
  dt<-afegir_dataindex(dt,bd.dindex)

  ##### filtrar per intervals de dates
  # Convertir dates a numeric
  if (class(dt$dat)=="Date") dt$dat_num=as.numeric(dt$dat)
  if (class(dt$dtindex)=="Date") dt$dtindex_num=as.numeric(dt$dtindex)

  if (class(dt$dat)!="Date") dt$dat_num=as.Date(as.character(dt$dat),format="%Y%m%d") %>% as.numeric()
  if (class(dt$dtindex)!="Date") dt$dtindex_num=as.Date(as.character(dt$dtindex),format="%Y%m%d") %>% as.numeric()

  ##### filtrar per intervals de dates
  dt<-dt %>% dplyr::filter(dat_num>= dtindex_num +finestra.dies[1] &
                             dat_num<= dtindex_num +finestra.dies[2])

  ##### Agregar (Suma de visites en interval independentment del tipus)

  if (N=="NA" & data=="NA") {paco<-dt %>%
    dplyr::group_by(idp,dtindex,cod) %>%                    # Agrupo per id
    dplyr::count() %>%           # Conto el numero visites per codi
    dplyr::ungroup()
  }

  if(N!="NA" & data=="NA") {
    paco<-dt %>%
      dplyr::group_by(idp,dtindex,cod) %>%                    # Agrupo per id
      dplyr::summarize(n=sum(!!N_sym)) %>%           # Conto el numero visites per codi
      dplyr::ungroup()
  }

  # Si s'ha d'agregar data agafo la data minima
  if(data!="NA") {
    paco<-dt %>%
      dplyr::group_by(idp,dtindex,cod) %>%  # Agrupo per id
      dplyr::summarize(n=min(!!data_sym,na.rm=T)) %>%       # data minima
      dplyr::ungroup()
  }

  # RESHAPE per idp
  visites <- paco[,c("idp","dtindex","cod","n")] %>%
    dplyr::select(idp,dtindex,visites=cod,n) %>%
    tidyr::spread(visites,n,sep = "_")

  paco <- paco %>%
    dplyr::select(idp,dtindex,visites=cod,n) %>%
    tidyr::spread(visites,n,sep = "_")

  # NA = 0
  visites[is.na(paco)]<-0

  ###  Computo visites globals
  paco<-paco %>% dplyr::select(idp,dtindex)  # Separo id de visites

  visites<-visites %>%        #  Sumo totes les visites
    dplyr::select(starts_with("visites")) %>%
    mutate(visites_TOT=rowSums(.) )

  paco<-paco %>% cbind(visites) %>% as_tibble()

  paco

}
