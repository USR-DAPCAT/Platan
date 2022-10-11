#' @title                  Genera mostres.
#' @description            Llegir tots els fitxers RDS dins d'un directori generar una mostra aleatoria i salvar-lo en un directori mostra
#' @param directori        Directori a on tenim les DADES ORIGINALS
#' @param fitxer_poblacio  Nom del fitxer de Poblacio
#' @param mida_mostra      Mida de la mostra
#' @param prefix           Prefix de la mostra
#' @param directori_test   Directori Test per la nostra MOSTRA
#' @param llavor           llavor aleatoria
#' @param idp              idp
#' @return                 La mostra
#' @export
generar_mostra_fitxers<-function(directori="data",
                                 fitxer_poblacio="METPLUS_entregable_poblacio_20181126_190346.rds",
                                 mida_mostra=10000,
                                 prefix="test",
                                 directori_test="mostra",
                                 idp="idp",
                                 llavor="1234") {

  #directori="../../DADES/HTG/mostra"
  #fitxer_poblacio="HTCPANCR_entregable_poblacio_20210701_235119.rds"
  #mida_mostra=30
  #prefix="test"
  #directori_test="mostra2"
  #idp="idp"
  #llavor="1234"

  set.seed(llavor)

  # Funció interna per llegir fitxer txt o rds
  LLEGIR.fitxer_poblacio<-function(n,directori,fitxer) {

    if (stringr::str_detect(fitxer,"\\.txt$")){
      dt<-data.table::fread(directori %>% here::here(fitxer)) %>% dplyr::as_tibble() %>% dplyr::sample_n(size=n)}

    if (stringr::str_detect(fitxer,"\\.rds$")){
      dt<-readRDS(directori %>% here::here(fitxer)) %>% dplyr::as_tibble() %>% dplyr::sample_n(size=n)}
    dt}


  # Funció interna per llegir fitxer txt o rds
  LLEGIR.fitxer<-function(n,directori,fitxer) {

    if (stringr::str_detect(fitxer,"\\.txt$")){
      dt<-data.table::fread(directori %>% here::here(fitxer)) %>% dplyr::as_tibble() %>% head(n)}

    if (stringr::str_detect(fitxer,"\\.rds$")){
      dt<-readRDS(directori %>% here::here(fitxer)) %>%dplyr:: as_tibble() %>% head(n)}
    dt}


  # Llista de fitxers .rds | .txt
  llista_de_fitxers<-list.files(directori) [list.files(directori) %>% stringr::str_detect("\\.rds$") |
                                              list.files(directori) %>% stringr::str_detect("\\.txt$")]

  # Genero el directori mostra
  directori_mostra<-paste0(directori,"/",directori_test)
  if (!file.exists(directori_mostra)) {
    # Crear directori si no existeix
    dir.create(file.path(directori,directori_test), showWarnings = FALSE)
  }

  # Si NO existeix algun fitxer GENERAR LOS / Si EXISTEIX algun  saltar
  if (!file.exists(paste0(directori_mostra,"/",llista_de_fitxers)) %>% any()) {

    # Llegir ids mostra de fitxer poblacio
    dt_ids<-LLEGIR.fitxer_poblacio(mida_mostra,directori,fitxer_poblacio) %>% dplyr::select(!!dplyr::sym(idp))

    # Posar noms per que els guardi
    llista_de_fitxers<-stats::setNames(llista_de_fitxers,llista_de_fitxers)
    # Llegir fitxers complerts


    llista_rds<-llista_de_fitxers %>% purrr::map(~LLEGIR.fitxer(n=Inf,directori = directori,fitxer=.x))

    # Filtrar via semijoint de tota la llista
    llista_rds_redux<-llista_rds %>% purrr::map(~semi_join(.x,dt_ids))

    # Ara salvar-los en un surbdirectori amb el nom triat

    # Genero noms de fitxers dins directori test
    llista_de_fitxers<-stringr::str_replace_all(llista_de_fitxers, "\\.txt$", ".rds")
    llista_de_noms_fitxers_nous<-paste0(directori_mostra,"/",prefix,llista_de_fitxers)


    # Salvo en format rds tots els fitxers en directori
    # saveRDS(llista_rds_redux[[1]],file=llista_de_fitxers_nous[1])
    purrr::map2(llista_rds_redux,llista_de_noms_fitxers_nous,~saveRDS(.x,file=.y))

  }

  if (file.exists(paste0(directori_mostra,"/",llista_de_fitxers)) %>% any()) {
    print ("Algun d'aquests fitxers ja existeix")
  }


}

