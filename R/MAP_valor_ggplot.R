#' @title Grafic Analitiques valors
#' @description Grafic Analitiques valors
#' @param dt xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
#' @param id xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
#' @param datainicial xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
#' @param val xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
#' @param grup_color xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
#' @param Nmostra xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
#' @param llavor xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
#' @param finestraX xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
#' @param title xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
#' @return Dibuixa punts-valors grafic analitiques
#' @export MAP_valor_ggplot
#' @examples
#' u=rnorm(1000,100,12)
#'
MAP_valor_ggplot<-function(
  dt=mostra50,
  id="idp",
  datainicial="dat",
  val="val",
  grup_color="agr",
  Nmostra=1,
  finestraX=c(-Inf,Inf),
  llavor=123,
  title="Evolució de valors"
)
{

  # dt=VARIABLES %>% filter(cod %in% c("HBA1C"))
  # datainicial ="dat"
  # id="idp"
  # val="val"
  # grup_color = "cod"
  # Nmostra = 4
  # finestraX=c(-Inf,Inf)
  # llavor=126


  if (finestraX[1]==-Inf) {porca1<-min(dt %>% pull(datainicial)) %>% ymd()}
  if (finestraX[2]==+Inf) {porca2<-max(dt %>% pull(datainicial)) %>% ymd()}
  if (finestraX[1]!=-Inf) {porca1<-finestraX[1] %>% ymd()}
  if (finestraX[2]!=+Inf) {porca2<-finestraX[2] %>% ymd()}


  # Interpretacio com a parametre
  grup_color<-rlang::sym(grup_color)
  datainicial<-rlang::sym(datainicial)
  id<-rlang::sym(id)
  val<-rlang::sym(val)

  # Formatejo a data
  dt<-dt %>% mutate(dat=lubridate::ymd(!!datainicial))

  # Llistat de codis d'analitiques
  analitiques_list<-dt%>%distinct(!!grup_color)%>%dplyr::pull()

  set.seed(llavor) # S'ha d'actualitzar

  # Seleccionar sample
  id_sample<-dt%>% distinct(!!id) %>%sample_n(size=Nmostra)
  dt<-id_sample %>% left_join(dt,by=quo_name(id)) #
  #

  # Construcció del identificador id-grup
  dt<-dt%>%mutate(id_plot=paste0(stringr::str_sub(!!id,1,6),!!grup_color))

  # Grafica plot de la variable
  ggplot(dt,aes(x =!!datainicial,y =id_plot,color=id_plot))+

    geom_line(aes(!!datainicial, !!val))+

    geom_point(aes(!!datainicial, !!val),color="black")+

    labs(title = title)+ theme(plot.title = element_text(size=25,hjust = 0.5))+

    theme(axis.text = element_text(colour = "black",size = 10))+
    theme(panel.grid.major = element_line(colour = "grey80",size=0.001))+
    theme(axis.line = element_line(colour = "black",size = 0.9))+

    scale_colour_brewer(palette = "Set1")+
    xlim(porca1,porca2)+

    geom_text(vjust = -0.5, hjust=0, size = 3,aes(x =!!datainicial, y =!!val,label = paste(round(!!val, 2),""))) +
    theme(legend.position="top",legend.background = element_rect(fill="gray80",size=1, linetype="solid", colour ="black"))
}
