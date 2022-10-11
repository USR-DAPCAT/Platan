#' @title                Retorna una data a STRING
#' @description          Retorna una data: "27-09-2022"  a STRING
#' @param  data          una data   tipus  "27-09-2022"
#' @return               Data a STRING
#' @export               data.to.string
#' @examples
#' A<-"27-09-2022"
#' B<-data.to.string(A)
#' B
data.to.string<-function(data) {

  data.string=paste0(lubridate::year(data),
                     stringr::str_pad(lubridate::month(data),2,"left","0"),
                     stringr::str_pad(lubridate::day(data),2,"left","0"))

}
