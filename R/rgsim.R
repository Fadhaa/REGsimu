#' This is package
#' @param n The sample size
#' @param nv The number of desirable variables
#' @title This to generate datasets according to linear regression model
#' @return list of data and parameters used in simulation
#' @example rgsim(100,3)
#' @author Author Fadhaa Ali
#' @description
#' This is short description
#' @references some references
#' @concept Linear Regression simulation.
#' @source The data comes from simulation.
#' @export
#' @examples n<-100
#' @examples var<-3
#' @examples dat<-rgsim(n,var)
#' @examples print(dat$data)
#' @examples print(dat$par)


rgsim<-function(n,nv){
  X<-c();b<-c();num<-c()
  for (i in 1:nv){
    X<-cbind(X,rnorm(n,0,1))
  }
  X<-cbind(1,X)
  for (i in 0:nv){
    b<-cbind(b,sample(seq(2,7),1))
  }
  for (i in 0:nv){
    num<-c(num,paste(c('X',as.character(i)),collapse=""))
  }
  XX<-as.matrix(X)
  Y<-XX%*%t(b)
  Y<-Y+rnorm(n,0,1)
  YX<-cbind(Y,XX)
  colnames(YX)<-c('Y',num)
  lis<-list(data=YX,par=b)
  return(lis)
}

