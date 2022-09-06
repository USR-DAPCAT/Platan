#' @title Dibuixa l'histograma d'una variable superposant-ne la densitat normal ajustada
#' @description Funció que dibuixa l'histograma d'una variable x superposant la densitat normal
#' ajustada. Si l'usuari ho desitja, pot superposar també un estimador de nucli de la densitat
#' @param x vector de dades l'histograma del qual es calcularà
#' @param dens valor lògic: TRUE=Superposar estimador de nucli de la densitat
#' @param ... altres funcions
#' @return histograma amb la densitat normal superposada
#' @export normalHist
#' @examples
#' u=rnorm(1000,100,12)
#' normalHist(u);
#' normalHist(u,dens=TRUE)
#' normalHist(u,dens=TRUE,col="lightcyan")
#'
normalHist<-function(x,dens=FALSE,...){
  m=mean(x)
  stdev=sd(x)
  xn=seq(min(x),max(x),length=200)
  yn=dnorm(xn,m,stdev)
  maxy=1.1*max(yn)
  hist(x, ylim=c(0,maxy),freq=FALSE,...)
  lines(xn,yn,col="red",lwd=2)
  if (dens) lines(density(x),col="blue",lty=2,lwd=2)
}
