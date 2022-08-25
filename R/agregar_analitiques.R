#' @title Agregar prescripcions.
#' @description Retorna tibble (data.table) amb els valors analitiques en temps previ en dies.
#' @param dt xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
#' @param bd.dindex xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
#' @param finestra.dies xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
#' @param sufix xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
#' @param fun xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
#' @return Taula agregada analitiques
#' @export agregar_analitiques
#' @examples
#' u=rnorm(1000,100,12)
#'
agregar_analitiques<-function(dt=ANALITIQUES,bd.dindex="20161231",finestra.dies=c(-Inf,Inf),sufix = c(".valor", ".dies"),fun="last",camp="cod"){

  # dt =dt_temp
  # bd.dindex =dt_index
  # finestra.dies=c(+1,+Inf)
  # sufix = c(".valor", ".dies")
  # fun="last"
  # fun="first"
  # camp="cod"
  #### Afegir + data index (+dtindex) en l'historic de variables

  print("Afegint dt.index")

  dt<-dt %>% select(idp,dat,cod:=!!sym(camp),val)

  dt<-afegir_dataindex(dt,bd.dindex)

  # Convertir dates a numeric
  print ("Convertir dates a numeric")

  if (class(dt$dat)!="Date") dt$dat=as.Date(as.character(dt$dat),format="%Y%m%d") %>% as.numeric()
  if (class(dt$dat)=="Date") dt$dat=as.numeric(dt$dat)

  if (class(dt$dtindex)!="Date") dt$dtindex=as.Date(as.character(dt$dtindex),format="%Y%m%d") %>% as.numeric()
  if (class(dt$dtindex)=="Date") dt$dtindex=as.numeric(dt$dtindex)
  ##### filtrar per intervals de dates

  print("Filtrant dates")

  dt<-dt %>% dplyr::filter(dat>= dtindex +finestra.dies[1] &
                             dat<= dtindex +finestra.dies[2])

  print ("Seleccionant unic registre per variable-id")

  ##  Filtro valors sense missings i calculo dies entre ddates
  paco<- dt %>% filter(val!=-9) %>% dplyr::filter(!is.na(val)) %>%      # elimino missings
    dplyr::mutate(dies=dtindex -dat)                                    # Calculo els dies fins data index

  ### Generar funcions agregacio valor i data (dies que han passat)
  if (fun=="last") funcioresum<<-function(x=val,y=dies) dplyr::nth(x,which.min(y))
  if (fun=="last") funcioresum_dies<<-function(x=val,y=dies) min(y,na.rm = T)

  if (fun=="first") funcioresum<<-function(x=val,y=dies) dplyr::nth(x,which.max(y))
  if (fun=="first") funcioresum_dies<<-function(x=val,y=dies) max(y,na.rm = T)

  if (fun=="close") funcioresum<<-function(x=val,y=dies) dplyr::nth(x,which.min(abs(y)))
  if (fun=="close") funcioresum_dies<<-function(x=val,y=dies) min(abs(y))

  if (fun=="mean") funcioresum<<-function(x=val,y=dies) mean(x,na.rm = T)
  if (fun=="mean") funcioresum_dies<<-function(x=val,y=dies) mean(y,na.rm = T)

  if (fun=="median") funcioresum<<-function(x=val,y=dies) median(x,na.rm = T)
  if (fun=="median") funcioresum_dies<<-function(x=val,y=dies) median(y,na.rm = T)

  if (fun=="sd") funcioresum<<-function(x=val,y=dies) sd(x,na.rm = T)
  if (fun=="sd") funcioresum_dies<<-function(x=val,y=dies) mean(y,na.rm = T)

  if (fun=="min") funcioresum<<-function(x=val,y=dies) valor=min(x,na.rm = T)
  if (fun=="min") funcioresum_dies<<-function(x=val,y=dies) dplyr::nth(y,which.min(x))

  if (fun=="max") funcioresum<<-function(x=val,y=dies) max(x,na.rm = T)
  if (fun=="max") funcioresum_dies<<-function(x=val,y=dies) dplyr::nth(y,which.max(x))

  if (fun=="sum") funcioresum<<-function(x=val,y=dies) sum(x,na.rm = T)
  if (fun=="sum") funcioresum_dies<<-function(x=val,y=dies) mean(y,na.rm = T)

  ### Agregacio per idp
  paco1<-paco %>%
    dplyr::group_by(idp,dtindex,cod) %>%                                    # Agrupo
    mutate(val=funcioresum(val,dies)) %>%                   # Resum de valor
    dplyr::slice(1L) %>%                                    # Unica fila per idp+cod
    dplyr::ungroup()  %>%
    select(idp,cod,dtindex,val)

  ### Agregacio de dies per idp
  paco2<-paco %>%
    dplyr::group_by(idp,dtindex,cod) %>%                    # Agrupo
    mutate(dies=funcioresum_dies(val,dies)) %>%             # Resum de dies
    dplyr::slice(1L) %>%                                    # Unica fila per idp+cod
    dplyr::ungroup() %>%
    select(idp,cod,dtindex,dies)

  paco<-paco1 %>% dplyr::left_join(paco2,by=c("idp","cod","dtindex"))


  print ("Reshaping")

  # RESHAPE valors d'Analitiques
  analitiques.valor <- paco[,c("idp","dtindex","cod","val")] %>%
    tidyr::spread(cod,val)

  # RESHAPE Dies
  analitiques.dies <- paco[,c("idp","dtindex","cod","dies")] %>%
    tidyr::spread(cod,dies)

  print ("Join: valor+dies")

  # JOINT Valors i dies
  analitiques.idp<-full_join(analitiques.valor, analitiques.dies, by=c("idp","dtindex"),suffix = sufix)

  analitiques.idp

}
