#' @title Grafic Analitiques
#' @description Grafic Analitiques
#' @param dt xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
#' @param id xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
#' @param datainicial xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
#' @param val xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
#' @param grup_color xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
#' @param Nmostra xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
#' @param llavor xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
#' @param finestraX xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
#' @param id_AGG xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
#' @return Dibuixa punts grafic analitiques
#' @export MAP_punts_ggplot
#' @examples
#' u=rnorm(1000,100,12)
#'
MAP_punts_ggplot<-function(
  dt=mostra50,
  id="idp",
  datainicial="dat",
  val="val",
  grup_color="agr",
  Nmostra=Inf,
  llavor=123,
  finestraX=c(-Inf,+Inf),
  id_AGG=F
)
{

  # dt=VARIABLES
  # id="idp"
  # datainicial ="dat"
  # val="val"
  # grup_color = "cod"
  # Nmostra = 2
  # finestraX=c(-Inf,+Inf)
  # llavor=126
  # id_AGG=T


  if (finestraX[1]==-Inf) porca1<-min(dt %>% pull(datainicial))  %>% ymd()
  if (finestraX[2]==+Inf) porca2<-max(dt %>% pull(datainicial))  %>% ymd()

  if (finestraX[1]!=-Inf) porca1<-finestraX[1] %>% ymd()
  if (finestraX[2]!=+Inf) porca2<-finestraX[2] %>% ymd()


  # Interpretacio com a parametre
  grup_color<-rlang::sym(grup_color)
  datainicial<-rlang::sym(datainicial)
  id<-rlang::sym(id)
  val<-rlang::sym(val)

  # Converteix data a data inicial
  dt<-dt %>% mutate(dat=ymd(!!datainicial))

  # Cal estandarditzar valor

  # Llista de nombre d'analitiques
  analitiques_list<-dt%>%distinct(!!grup_color)%>%dplyr::pull()

  set.seed(llavor) # S'ha d'actualitzar
  #
  id_sample<-dt %>% distinct(!!id) %>% sample_n(size=Nmostra)
  dt<-id_sample %>% left_join(dt,by=quo_name(id)) #


  # Construccio del identificador id-grup

  if (id_AGG){
    dt<-dt%>%mutate(id_plot=paste0(stringr::str_sub(!!id,1,6),!!grup_color),id_num=as.numeric(factor(!!id)))
  }
  if (id_AGG ==F) {
    dt<-dt%>%mutate(id_plot=paste0(stringr::str_sub(!!id,1,6)),id_num=as.numeric(factor(!!id))) }

  ggplot(dt,aes(x =!!datainicial,y =id_plot,color=!!grup_color))+
    geom_point(aes(!!datainicial, id_plot)) +
    geom_point(aes(size = !!val))+
    labs(title = "Històric de determinacions")+theme(plot.title = element_text(size=30,hjust = 0.5))+

    theme(axis.text = element_text(colour = "black",size = 10))+
    theme(panel.grid.major = element_line(colour = "grey80",size=0.001))+
    theme(axis.line = element_line(colour = "black",size = 0.9))+

    scale_colour_brewer(palette = "Set1")+
    xlim(porca1,porca2)+
    geom_text(vjust = -0.5, hjust=0, size = 3,aes(x =!!datainicial, y =id_plot,label = paste(round(!!val, 2),""))) +
    theme(legend.position="top",legend.background = element_rect(fill="gray80",size=1, linetype="solid", colour ="black"))+
    scale_y_discrete(breaks= dt %>% pull(id_plot),labels=dt %>% pull(id_num))
  #

}
