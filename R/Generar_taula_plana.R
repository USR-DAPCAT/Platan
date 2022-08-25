#' @title Generar la taula plana, a partir dels parametres dels agregadors
#' @description Taula plana
#' @param dt xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
#' @param cataleg xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
#' @param parametres xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
#' @return Retorna una taula plana a partir dels parametres dels agregadors
#' @export Generar_taula_plana
#' @examples
#' u=rnorm(1000,100,12)
#'
Generar_taula_plana<-function(dt=dt_index,
                              cataleg=dt_cataleg,
                              parametres=dt_parametres,...)
{

  # cataleg=fitxer_cataleg
  # parametres=fitxer_cataleg
  # dt=dt_dindex
  # cataleg = fitxer_cataleg
  # parametres = here::here("cataleg_precav_emilio.xls")
  # sheet="parametres_2010"
  # dt_dindex %>% select(idp,dtindex),
  # cataleg = here::here("Cataleg_DAPRET.xls")
  # parametres=here::here("Cataleg_DAPRET.xls")
  # parametres=fitxer_cataleg
  # sheet="parametres"



  cataleg<-read_conductor(cataleg,col_types="text")
  parametres<-read_conductor(parametres,...) %>% filter(!(is.na(fitxer)| is.na(domini)))
  # parametres<-read_conductor(parametres,sheet="parametres")
  # parametres<-read_conductor(fitxer_cataleg,sheet="parametres")

  # Llegeixo parametres
  # Agrego problemes de salut
  # Capturar: Dades, finestra, camp_agregador i prefix, filtrar cataleg

  # Depurar parametres ---------------------------------
  # en funció de la existencia dels fitxers si no existeixen -----
  exist_file<-parametres$fitxer %>% set_names(parametres$fitxer) %>%
    map(~exists(.x)) %>% map(~as.integer(.x)) %>%
    unlist() %>% as_tibble()
  parametres<-parametres %>% bind_cols(exist_file) %>% rename("exist"=value)

  arxius_inexistens<-parametres %>% filter(exist==0) %>% pull(fitxer) %>% unique()
  # Warning
  if (nrow(parametres %>% filter(exist==0))>0) { warning(paste0("No existeixen alguns fitxers:", paste0(arxius_inexistens,collapse = ", ")))}
  # filtro parametres logics
  parametres<-parametres %>% filter(exist==1)

  # Reomplir cataleg_mana si no existeix (Per defecte tots T)
  if (!"cataleg_mana"%in%colnames(parametres)) parametres$cataleg_mana<-T
  parametres<-parametres %>% mutate(cataleg_mana=if_else(cataleg_mana%in%c("F","FALSE"),F,T,missing=T))

  # Data de tall (data o / dtindex o nom de fitxer  -------
  # Generar camp tall si no existeix
  # Triar arxiu bd.dindex en funció de punt de tall?

  # Si no existeix crear-lo i posar-hi dataindex
  if(!"tall" %in% colnames(parametres)) parametres<-parametres %>% mutate(tall=deparse(substitute(dt)))
  # Si existeix evaluar cada fila i afegir el nom del dt
  if("tall" %in% colnames(parametres)) {
    parametres<-parametres %>% mutate(
      tall=as.character(tall),
      tall=if_else(is.na(tall) | tall%in%c("dtindex") ,deparse(substitute(dt)),tall)
    )
  }

  # PROBLEMES  ---------------------------------
  # Seleccionar parametres i cataleg
  par_problemes<-
    parametres %>% filter(domini=="diagnostics")

  if (nrow(par_problemes)>0) {


    cat_problemes <-cataleg %>% filter(domini%in% c("diagnostics","cmbdh","cmbdh_diag",
                                                    "cmbdh_procediments","cmbdh_diagnostics",
                                                    "cmbdh_procediments_cim10scp","DIAG","derivacions") )

    # Generar dades historic en funció del nom fitxer
    nom_fitxer<-  par_problemes %>% distinct(fitxer) %>% pull()
    nom_fitxer<-set_names(nom_fitxer,nom_fitxer)
    dt_historic<-nom_fitxer %>% map_df(~eval(sym(.x)),.id="nom_fitxer") %>% semi_join(dt,by="idp") %>% select(nom_fitxer,idp,cod,dat)

    # Generar agregacions problemes segons llista de parametres
    DTAGR_PROBLEMES<-
      pmap(transmute(par_problemes,fitxer,as.numeric(Finestra1),as.numeric(Finestra2),prefix,camp,tall,cataleg_mana),

           ~agregar_problemes(
             dt=dt_historic %>% filter(nom_fitxer==..1),
             bd.dindex=eval(parse(text=..6)),
             dt.agregadors=cat_problemes,
             finestra.dies=c(..2,..3),
             prefix=..4,
             camp_agregador=..5,
             cataleg_mana=..7)
      ) %>%
      reduce(full_join,by=c("idp","dtindex"))   # Juntar-ho tot

  } else DTAGR_PROBLEMES<-dt


  # FARMACS FACTURATS -------------------------------
  # Seleccionar parametres i cataleg
  par_farmacs<-
    parametres %>%
    filter(domini%in% c("farmacs","farmacs_facturats","facturats"))
  # Només passar si en parametres existeix
  if (nrow(par_farmacs)>0) {

    # Cataleg
    cat_farmacs <-cataleg %>% filter(domini%in% c("farmacs_facturats","farmacs","farmacs_prescrits"))

    # Generar dades historic en funció del nom fitxer
    nom_fitxer<-  par_farmacs %>% distinct(fitxer) %>% pull()
    nom_fitxer<-set_names(nom_fitxer,nom_fitxer)


    dt_historic<-nom_fitxer %>% map_df(~eval(sym(.x)),.id="nom_fitxer") %>% semi_join(dt,by="idp")

    #
    DTAGR_FARMACS<-
      pmap(transmute(par_farmacs,fitxer,as.numeric(Finestra1),as.numeric(Finestra2),prefix,camp,tall,cataleg_mana),

           ~agregar_facturacio(
             dt_historic %>% select(idp,cod,dat,env) %>% filter(nom_fitxer==..1),
             bd.dindex=eval(parse(text=..6)),
             finestra.dies=c(..2,..3),
             dt.agregadors=cat_farmacs,
             prefix=..4,
             camp_agregador=..5,
             agregar_data=F,
             cataleg_mana = ..7)
      ) %>%
      reduce(full_join,by=c("idp","dtindex")) %>%  # Juntar-ho tot
      mutate(dtindex=data.to.string(dtindex))

  } else DTAGR_FARMACS<-dt

  # FARMACS PRESCRITS  -------------------------------
  # Seleccionar parametres i cataleg
  par_farmacs<- parametres %>% filter(domini %in% c("farmacs_prescrits","prescrits"))

  # Només passar si en parametres existeix
  if (nrow(par_farmacs)>0) {

    # Cataleg
    cat_farmacs <-cataleg %>% filter(domini%in% c("farmacs_facturats","farmacs","farmacs_prescrits"))

    # Generar dades historic en funció del nom fitxer
    nom_fitxer<-  par_farmacs %>% distinct(fitxer) %>% pull()
    nom_fitxer<-set_names(nom_fitxer,nom_fitxer)

    dt_historic<-nom_fitxer %>% map_df(~eval(sym(.x)),.id="nom_fitxer") %>% semi_join(dt,by="idp")

    #
    DTAGR_FARMACS_PR<-
      pmap(transmute(par_farmacs,fitxer,as.numeric(Finestra1),as.numeric(Finestra2),prefix,camp,tall,cataleg_mana),

           ~agregar_prescripcions(
             dt_historic %>% select(idp,cod,dat,dbaixa) %>% filter(nom_fitxer==..1),
             bd.dindex=eval(parse(text=..6)),
             finestra.dies=c(..2,..3),
             dt.agregadors=cat_farmacs,
             prefix=..4,
             camp_agregador=..5,
             agregar_data=F,
             cataleg_mana = ..7)
      ) %>%
      reduce(full_join,by=c("idp","dtindex")) %>%  # Juntar-ho tot
      mutate(dtindex=data.to.string(dtindex))

  } else DTAGR_FARMACS_PR<-dt


  # ANALITIQUES  quantis-------
  par_analit<-
    parametres %>%
    filter(domini %in% c("analitiques","variables","cliniques")) %>%
    mutate(val_tipus=NA)

  nom_fitxer<-  par_analit %>% distinct(fitxer) %>% pull()
  nom_fitxer<-set_names(nom_fitxer,nom_fitxer)

  if (nrow(par_analit)>0) {
    val_tipus<-nom_fitxer %>%
      map (~eval(sym(.x))) %>%
      map_df(~class(.x$val)) %>%
      data.table::transpose() %>% pull(V1)
    par_analit<-par_analit %>% select(-val_tipus) %>%
      left_join(tibble(fitxer=nom_fitxer,val_tipus),by="fitxer")
  }

  par_analit_quanti<-par_analit %>% filter(val_tipus!="character")

  if (nrow(par_analit_quanti)>0) {

    # Generar dades historic en funció del nom fitxer
    nom_fitxer<-  par_analit_quanti %>% distinct(fitxer) %>% pull()

    nom_fitxer<-set_names(nom_fitxer,nom_fitxer)
    dt_historic<-nom_fitxer %>% map_df(~eval(sym(.x)),.id="nom_fitxer") %>% semi_join(dt,by="idp")

    DTAGR_ANALITIQUES<-
      pmap(transmute(par_analit_quanti,
                     fitxer,as.numeric(Finestra1),as.numeric(Finestra2),prefix,funcio,camp,tall),

           ~ agregar_analitiques(dt_historic %>% filter(nom_fitxer==..1),
                                 bd.dindex = eval(parse(text=..7)),
                                 finestra.dies = c(..2,..3),
                                 sufix = c(..4,".dies"),
                                 fun=..5,
                                 camp=..6)

      ) %>%

      reduce(full_join,by=c("idp","dtindex")) %>%  # Juntar-ho tot
      mutate(dtindex=lubridate::as_date(dtindex) %>% data.to.string)

  } else DTAGR_ANALITIQUES<-dt


  #### ANALITIQUES character

  par_analit_quali<-par_analit %>% filter(val_tipus=="character")

  if (nrow(par_analit_quali)>0) {

    # Generar dades historic en funció del nom fitxer
    nom_fitxer<-  par_analit_quali %>% distinct(fitxer) %>% pull()

    nom_fitxer<-set_names(nom_fitxer,nom_fitxer)
    dt_historic<-nom_fitxer %>% map_df(~eval(sym(.x)),.id="nom_fitxer") %>% semi_join(dt,by="idp")

    DTAGR_ANALITIQUES_char<-
      pmap(

        transmute(par_analit_quali,
                  fitxer,as.numeric(Finestra1),as.numeric(Finestra2),prefix,funcio,camp,tall),

        ~ agregar_analitiques(dt_historic %>% filter(nom_fitxer==..1),
                              bd.dindex = eval(parse(text=..7)),
                              finestra.dies = c(..2,..3),
                              sufix = c(..4,".dies"),
                              fun=..5,
                              camp=..6)

      ) %>%

      reduce(full_join,by=c("idp","dtindex")) %>%  # Juntar-ho tot
      mutate(dtindex=lubridate::as_date(dtindex) %>% data.to.string)

  } else DTAGR_ANALITIQUES_char<-dt


  # Finalment: juntar-ho tot -------------

  # Generar data index
  # Agafar talls diferents i generar fitxer amb diferents dates index
  talls<-parametres %>%
    distinct(tall) %>% filter(!is.na(tall)) %>%
    filter(is.numeric(eval(parse(text=tall))) | is.character(eval(parse(text=tall)))
    ) %>% mutate(temp=1,dtindex=tall)

  dt_temp<-dt %>% select(idp) %>% mutate(temp=1) %>% inner_join(talls,by="temp") %>%
    transmute(idp,dtindex=as.character(dtindex))

  dt<-dt %>% bind_rows(dt_temp) %>% distinct() %>% arrange(idp,dtindex) %>% na.omit()

  dt %>%
    full_join(DTAGR_PROBLEMES) %>%
    full_join(DTAGR_FARMACS) %>%
    full_join(DTAGR_FARMACS_PR) %>%
    full_join(DTAGR_ANALITIQUES) %>%
    full_join(DTAGR_ANALITIQUES_char)

}
