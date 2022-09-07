#' @title data.to.string
#' @description data passa a string
#' @param data data
#' @return data string
#' @export

data.to.string<-function(data) {

  data.string=paste0(year(data),
                     str_pad(lubridate::month(data),2,"left","0"),
                     str_pad(lubridate::day(data),2,"left","0"))

}
