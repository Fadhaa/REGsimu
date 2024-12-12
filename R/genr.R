#' Title genr
#'
#' @param n sample size generated
#' @return generated data
#' @export
#' @examples generate<-genr()
#' @author Fadhaa Ali



genr<-function(n=100){
  x1<-rnorm(n,0,1);x2<-rnorm(n,0,1);Y<-2+3*x1+4*x2+rnorm(n,0,1)
  data_sim<-data.frame(cbind(Y,x1,x2))
  return(data_sim)}
