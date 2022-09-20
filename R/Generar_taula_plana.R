#' @title Generar la taula plana, a partir dels parametres dels agregadors
#' @description Taula plana
#' @param dt  data.índex
#' @param cataleg Catàleg
#' @param parametres Paràmetres
#' @param ... altres funcions
#' @return Retorna una taula plana a partir dels parametres dels agregadors
#' @export
#' @importFrom dplyr "%>%"
#' @examples
#
#' #bd data Index.
#' idp=c(1,2,3,4,5)
#' dtindex=c(20220101,20220101,20220101,20220101,20220101)
#' dt_index<-data.frame(idp=idp,dtindex=dtindex)
#' dt_index<-tibble::as_tibble(dt_index)
#' dt_index$dtindex<-as.character(dt_index$dtindex)
#'
#' #Cataleg:
#' domini="farmacs_facturats"
#' cod=c("A10BB01","A10BD01","A10BD04","A10BA02","J01DD07")
#' agr_Farmac=c("Sulfonilureas","Biguanidas","Tiazolidinadiones","Biguanidas","Antibioticos")
#' dt_cataleg1<-data.frame(domini=domini,cod=cod,agr="",agr_Farmac=agr_Farmac)
#'
#' domini="farmacs_prescrits"
#' cod=c("A10BB01","A10BD01","A10BD04","A10BA02","J01DD07")
#' agr_Farmac=c("Sulfonilureas","Biguanidas","Tiazolidinadiones","Biguanidas","Antibioticos")
#' dt_cataleg2<-data.frame(domini=domini,cod=cod,agr="",agr_Farmac=agr_Farmac)
#'
#' domini="diagnostics"
#' cod=c("E11","I25","I50.9","I10")
#' agr=c("DM2","ISQ.CRONICA","INS.CARD","HTA")
#' dt_cataleg3<-data.frame(domini=domini,cod=cod,agr=agr,agr_Farmac="")
#' dt_cataleg<-rbind(dt_cataleg1,dt_cataleg2,dt_cataleg3)
#'
#' dt_cataleg
#'
#'
#' #Parametres:
#' fitxer=c("dt_diagnostics","dt_farmacs_facturats","dt_farmacs_prescrits","dt_variables")
#' domini=c("diagnostics","farmacs_facturats","farmacs_prescrits","variables")
#' Finestra1=c(-Inf,-Inf,-Inf,-Inf)
#' Finestra2=c(0,0,0,0)
#' camp=c("agr","agr_Farmac","agr_Farmac","cod")
#' funcio=c("first","first","first","last")
#' prefix =c("DG.","FF.","FP.","Valor.")
#' dt_parametres<-data.frame(cbind(fitxer,domini,Finestra1,Finestra2,camp,prefix,funcio))
#' dt_parametres
#'
#'
#' Taula_plana<-Generar_taula_plana(
#' dt=dt_index,
#' cataleg=dt_cataleg,
#' parametres=dt_parametres)
#'
#' Taula_plana

Generar_taula_plana<-function(dt="dt_index",
                              cataleg="dt_cataleg",
                              parametres="dt_parametres",...)
{



  #dt=dt_index
  #cataleg=dt_cataleg
  #parametres=dt_parametres


  #dt<-dt%>%transmute(idp,dtindex=as.character(dtindex))

  # cataleg=fitxer_cataleg
  # parametres=fitxer_cataleg
  # dt=dt_dindex
  # cataleg = fitxer_cataleg
  # parametres = here::here("cataleg_precav_emilio.xls")
  # sheet="parametres_2010"
  # dt_dindex %>% dplyr::select(idp,dtindex),
  # cataleg = here::here("Cataleg_DAPRET.xls")
  # parametres=here::here("Cataleg_DAPRET.xls")
  # parametres=fitxer_cataleg
  # sheet="parametres"

  # dt=dt_temp
  # cataleg = CATALEG
  # parametres = here::here("cataleg_codis.xls"),sheet="parametres_2010"


  cataleg<-read_conductor(cataleg,col_types="text")


  parametres<-read_conductor(parametres,...)%>%dplyr::filter(!(is.na(fitxer)| is.na(domini)))
  parametres<-read_conductor(parametres) %>%dplyr::filter(!(is.na(fitxer)| is.na(domini)))

  # parametres<-read_conductor(parametres,sheet="parametres_2010")
  # parametres<-read_conductor(fitxer_cataleg,sheet="parametres")

  # Llegeixo parametres
  # Agrego problemes de salut
  # Capturar: Dades, finestra, camp_agregador i prefix, filtrar cataleg

  # Depurar parametres ---------------------------------
  # en funciÃ³ de la existencia dels fitxers si no existeixen -----
  exist_file<-parametres$fitxer %>% rlang::set_names(parametres$fitxer) %>%purrr::map(~exists(.x)) %>% purrr::map(~as.integer(.x)) %>%
    unlist() %>%tibble:: as_tibble()
  parametres<-parametres %>%dplyr:: bind_cols(exist_file) %>%dplyr:: rename("exist"=value)

  arxius_inexistens<-parametres %>%dplyr::filter(exist==0) %>%dplyr::pull(fitxer) %>% unique()
  # Warning
  if (nrow(parametres %>% dplyr::filter(exist==0))>0) { warning(paste0("No existeixen alguns fitxers:", paste0(arxius_inexistens,collapse = ", ")))}
  # filtro parametres logics
  parametres<-parametres %>% dplyr::filter(exist==1)

  # Reomplir cataleg_mana si no existeix (Per defecte tots T)
  if (!"cataleg_mana"%in%colnames(parametres)) parametres$cataleg_mana<-T
  parametres<-parametres %>% dplyr::mutate(cataleg_mana=dplyr::if_else(cataleg_mana%in%c("F","FALSE"),F,T,missing=T))

  # Data de tall (data o / dtindex o nom de fitxer  -------
  # Generar camp tall si no existeix
  # Triar arxiu bd.dindex en funciÃ³ de punt de tall?

  # Si no existeix crear-lo i posar-hi dataindex
  if(!"tall" %in% colnames(parametres)) parametres<-parametres %>% dplyr::mutate(tall=deparse(substitute(dt)))
  # Si existeix evaluar cada fila i afegir el nom del dt
  if("tall" %in% colnames(parametres)) {
    parametres<-parametres %>% dplyr::mutate(
      tall=as.character(tall),
      tall=dplyr::if_else(is.na(tall) | tall%in%c("dtindex") ,deparse(substitute(dt)),tall)
    )
  }

  # PROBLEMES  ---------------------------------
  # Seleccionar parametres i cataleg
  par_problemes<-
    parametres %>% dplyr::filter(domini=="diagnostics")

  if (nrow(par_problemes)>0) {


    cat_problemes <-cataleg %>% dplyr::filter(domini%in% c("diagnostics","cmbdh","cmbdh_diag",
                                                    "cmbdh_procediments","cmbdh_diagnostics",
                                                    "cmbdh_procediments_cim10scp","DIAG","derivacions") )

    # Generar dades historic en funciÃ³ del nom fitxer
    nom_fitxer<-  par_problemes %>% dplyr::distinct(fitxer) %>% dplyr::pull()
    nom_fitxer<-rlang::set_names(nom_fitxer,nom_fitxer)
    dt_historic<-nom_fitxer %>% purrr::map_df(~eval(rlang::sym(.x)),.id="nom_fitxer") %>%dplyr:: semi_join(dt,by="idp") %>%dplyr:: select(nom_fitxer,idp,cod,dat)

    # Generar agregacions problemes segons llista de parametres
    DTAGR_PROBLEMES<-
      purrr::pmap(dplyr::transmute(par_problemes,fitxer,as.numeric(Finestra1),as.numeric(Finestra2),prefix,camp,tall,cataleg_mana),

           ~agregar_problemes(
             dt=dt_historic %>% dplyr::filter(nom_fitxer==..1),
             bd.dindex=eval(parse(text=..6)),
             dt.agregadors=cat_problemes,
             finestra.dies=c(..2,..3),
             prefix=..4,
             camp_agregador=..5,
             cataleg_mana=..7)
      ) %>%
      purrr::reduce(dplyr::full_join,by=c("idp","dtindex")) %>%   # Juntar-ho tot
      dplyr::mutate(dtindex=lubridate::as_date(dtindex) %>% data.to.string)

  } else DTAGR_PROBLEMES<-dt


  # FARMACS FACTURATS -------------------------------
  # Seleccionar parametres i cataleg
  par_farmacs<-
    parametres %>%
    dplyr::filter(domini%in% c("farmacs","farmacs_facturats","facturats"))
  # NomÃ©s passar si en parametres existeix
  if (nrow(par_farmacs)>0) {

    # Cataleg
    cat_farmacs <-cataleg %>%dplyr:: filter(domini%in% c("farmacs_facturats","farmacs","farmacs_prescrits"))

    # Generar dades historic en funciO del nom fitxer
    nom_fitxer<-  par_farmacs %>% dplyr::distinct(fitxer) %>% dplyr::pull()
    nom_fitxer<-rlang::set_names(nom_fitxer,nom_fitxer)


    dt_historic<-nom_fitxer %>%purrr::map_df(~eval(rlang::sym(.x)),.id="nom_fitxer") %>%dplyr:: semi_join(dt,by="idp")


    DTAGR_FARMACS<-
      purrr::pmap(dplyr::transmute(par_farmacs,fitxer,as.numeric(Finestra1),as.numeric(Finestra2),prefix,camp,tall,cataleg_mana),

           ~agregar_facturacio(
             dt_historic %>%dplyr::select(idp,cod,dat,env) %>%dplyr::filter(nom_fitxer==..1),
             bd.dindex=eval(parse(text=..6)),
             finestra.dies=c(..2,..3),
             dt.agregadors=cat_farmacs,
             prefix=..4,
             camp_agregador=..5,
             agregar_data=F,
             cataleg_mana = ..7)
      ) %>%
      purrr::reduce(dplyr::full_join,by=c("idp","dtindex")) %>%  # Juntar-ho tot
      dplyr::mutate(dtindex=data.to.string(dtindex))

  } else DTAGR_FARMACS<-dt

  # FARMACS PRESCRITS  -------------------------------
  # Seleccionar parametres i cataleg
  par_farmacs<- parametres %>% dplyr::filter(domini %in% c("farmacs_prescrits","prescrits"))

  # NomÃ©s passar si en parametres existeix
  if (nrow(par_farmacs)>0) {

    # Cataleg
    cat_farmacs <-cataleg %>% dplyr::filter(domini%in% c("farmacs_facturats","farmacs","farmacs_prescrits"))

    # Generar dades historic en funciÃ³ del nom fitxer
    nom_fitxer<-  par_farmacs %>%dplyr:: distinct(fitxer) %>% dplyr::pull()
    nom_fitxer<-rlang::set_names(nom_fitxer,nom_fitxer)

    dt_historic<-nom_fitxer %>%purrr:: map_df(~eval(rlang::sym(.x)),.id="nom_fitxer") %>%dplyr:: semi_join(dt,by="idp")


    DTAGR_FARMACS_PR<-
      purrr::pmap(dplyr::transmute(par_farmacs,fitxer,as.numeric(Finestra1),as.numeric(Finestra2),prefix,camp,tall,cataleg_mana),

           ~agregar_prescripcions(
             dt_historic %>% dplyr::select(idp,cod,dat,dbaixa) %>% dplyr::filter(nom_fitxer==..1),
             bd.dindex=eval(parse(text=..6)),
             finestra.dies=c(..2,..3),
             dt.agregadors=cat_farmacs,
             prefix=..4,
             camp_agregador=..5,
             agregar_data=F,
             cataleg_mana = ..7)
      ) %>%
      purrr::reduce(dplyr::full_join,by=c("idp","dtindex")) %>%  # Juntar-ho tot
      dplyr::mutate(dtindex=data.to.string(dtindex))

  } else DTAGR_FARMACS_PR<-dt


  # ANALITIQUES  quantis-------
  par_analit<-
    parametres %>%
    dplyr::filter(domini %in% c("analitiques","variables","cliniques")) %>%
    dplyr::mutate(val_tipus=NA)

  nom_fitxer<-  par_analit %>% dplyr::distinct(fitxer) %>% dplyr::pull()
  nom_fitxer<-rlang::set_names(nom_fitxer,nom_fitxer)

  if (nrow(par_analit)>0) {
    val_tipus<-nom_fitxer %>%
      purrr::map(~eval(rlang::sym(.x))) %>%
      purrr::map_df(~class(.x$val)) %>%
      data.table::transpose() %>%dplyr::pull(V1)
    par_analit<-par_analit %>%dplyr:: select(-val_tipus) %>%
      dplyr::left_join(tibble::tibble(fitxer=nom_fitxer,val_tipus),by="fitxer")
  }





  par_analit_quanti<-par_analit %>%dplyr:: filter(val_tipus!="character")

  if (nrow(par_analit_quanti)>0) {

    # Generar dades historic en funciÃ³ del nom fitxer
    nom_fitxer<-  par_analit_quanti %>% dplyr::distinct(fitxer) %>% dplyr::pull()

    nom_fitxer<-rlang::set_names(nom_fitxer,nom_fitxer)
    dt_historic<-nom_fitxer %>%purrr:: map_df(~eval(rlang::sym(.x)),.id="nom_fitxer") %>%dplyr:: semi_join(dt,by="idp")

    DTAGR_ANALITIQUES<-
      purrr::pmap(dplyr::transmute(par_analit_quanti,
                     fitxer,as.numeric(Finestra1),as.numeric(Finestra2),prefix,funcio,camp,tall),

           ~ agregar_analitiques(dt_historic %>% dplyr::filter(nom_fitxer==..1),
                                 bd.dindex = eval(parse(text=..7)),
                                 finestra.dies = c(..2,..3),
                                 sufix = c(..4,".dies"),
                                 fun=..5,
                                 camp=..6)

      ) %>%

      purrr::reduce(dplyr::full_join,by=c("idp","dtindex")) %>%  # Juntar-ho tot
      dplyr::mutate(dtindex=lubridate::as_date(dtindex) %>% data.to.string)

  } else DTAGR_ANALITIQUES<-dt


  #### ANALITIQUES character

  par_analit_quali<-par_analit %>%dplyr:: filter(val_tipus=="character")

  if (nrow(par_analit_quali)>0) {

    # Generar dades historic en funciÃ³ del nom fitxer
    nom_fitxer<-  par_analit_quali %>% dplyr::distinct(fitxer) %>% dplyr::pull()

    nom_fitxer<-rlang::set_names(nom_fitxer,nom_fitxer)
    dt_historic<-nom_fitxer %>% purrr:: map_df(~eval(rlang::sym(.x)),.id="nom_fitxer") %>%dplyr:: semi_join(dt,by="idp")




    DTAGR_ANALITIQUES_char<-
      purrr::pmap(

        dplyr::transmute(par_analit_quali,
                  fitxer,as.numeric(Finestra1),as.numeric(Finestra2),prefix,funcio,camp,tall),

        ~ agregar_analitiques(dt_historic %>% dplyr::filter(nom_fitxer==..1),
                              bd.dindex = eval(parse(text=..7)),
                              finestra.dies = c(..2,..3),
                              sufix = c(..4,".dies"),
                              fun=..5,
                              camp=..6)

      ) %>%

      purrr::reduce(dplyr::full_join,by=c("idp","dtindex")) %>%  # Juntar-ho tot
      dplyr::mutate(dtindex=lubridate::as_date(dtindex) %>% data.to.string)

  } else DTAGR_ANALITIQUES_char<-dt


  # Finalment: juntar-ho tot -------------

  # Generar data index
  # Agafar talls diferents i generar fitxer amb diferents dates index
  talls<-parametres %>%
    dplyr::distinct(tall)%>%
     dplyr::filter(!is.na(tall))%>%
      dplyr::filter(is.numeric(eval(parse(text=tall))) | is.character(eval(parse(text=tall))))%>%
       dplyr::mutate(temp=1,dtindex=tall)

  dt_temp<-dt %>%
    dplyr::select(idp)%>%
     dplyr::mutate(temp=1)%>%
      dplyr::inner_join(talls,by="temp")%>%
        dplyr::transmute(idp,dtindex=as.character(dtindex))

  #No existeixen alguns fitxers:dt_farmacs_facturats, dt_farmacs_prescrits

  dt<-dt %>% dplyr::bind_rows(dt_temp) %>%dplyr:: distinct() %>% dplyr::arrange(idp,dtindex) %>% stats::na.omit()

  dt%>%
   dplyr::full_join(DTAGR_PROBLEMES) %>%
     dplyr::full_join(DTAGR_FARMACS) %>%
      dplyr::full_join(DTAGR_FARMACS_PR) %>%
       dplyr::full_join(DTAGR_ANALITIQUES) %>%
        dplyr::full_join(DTAGR_ANALITIQUES_char)

}



