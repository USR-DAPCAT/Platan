#' @title Dibuixa mapa temporal
#' @description Dibuixa mapa temporal univariant per verificar solapaments
#' @param dades xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
#' @param datainicial xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
#' @param datafinal xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
#' @param id xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
#' @param Nmostra xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
#' @param add_point xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
#' @param add_final xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
#' @param set_seed xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
#' @return Dibuixa mapa temporal univariant per verificar solapaments
#' @export MAP_ggplot_univariant
#' @examples
#' u=rnorm(1000,100,12)
#'
MAP_ggplot_univariant<-function(dades=dt,
                                datainicial="data",
                                datafinal="datafi",
                                id="idp_temp",
                                Nmostra=10,
                                add_point=NA,
                                add_final=NA,
                                set_seed=123) {

  # dades=dades %>% filter(situacio=="T" | situacio=="D")
  # datainicial="dtindex"
  # datafinal="datafi_seguiment"
  # id="idp"
  # Nmostra=10
  # add_point=NA
  # add_final="situacio"

  # Conversió a Sym per evaluació
  datainicial<-rlang::sym(datainicial)
  datafinal<-rlang::sym(datafinal)
  id_sym<-rlang::sym(id)

  # mostrejo
  dades<-mostreig_ids(dt=dades,id=id,n_mostra = Nmostra,set_seed=set_seed)

  # if (Nmostra!=Inf) id_sample<-dades %>% distinct(!!id) %>% sample_n(size=Nmostra)
  # dt<-id_sample %>% left_join(dt,by=quo_name(id)) #

  # Calculo dies de duració
  dades<-dades %>%  mutate(dia0=!!datainicial,diaf=!!datafinal,days_duration=diaf-dia0)

  # Gráfico el tema
  figura<- ggplot2::ggplot(dades,ggplot2::aes(x =dia0,y =!!id_sym))+
    ggplot2::geom_segment(ggplot2::aes(x =dia0, xend=diaf, y =!!id_sym, yend = !!id_sym),arrow =  ggplot2::arrow(length = ggplot2::unit(0.01, "npc"))) +
    ggplot2::geom_point(ggplot2::aes(dia0, !!id_sym)) +
    ggplot2::geom_text(vjust = -0.5, hjust=0, size = 3, ggplot2::aes(x =dia0, y = !!id_sym,label = paste(round(days_duration, 2), "days")))+
    ggplot2::scale_colour_brewer(palette = "Set1")+
    ggplot2::theme(legend.position="top",legend.background =  ggplot2::element_rect(fill="gray80",size=1, linetype="solid", colour ="black"))

  if (!is.na(add_point)) {
    figura<-figura+
      geom_point(aes(!!rlang::sym(add_point),!!id_sym),size=3,shape=8,colour="red") +
      geom_text(vjust = -0.5, hjust=0, size = 2,aes(x =!!rlang::sym(add_point), y = !!id_sym,label = add_point))
  }

  if (!is.na(add_final)) {
    figura<- figura + ggplot2::geom_point(ggplot2::aes(diaf, !!id_sym,colour=!!rlang::sym(add_final) %>% as.factor()))
  }

  figura

}

