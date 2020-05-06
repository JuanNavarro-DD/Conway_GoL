n=20
a=matrix(rbinom((n+2)^2,1,prob=0.3),n+2)

extra_cell <- function(n,a){
  a[1,2:n+1] = a[n+1,2:n+1] #Top line
  a[n+2,2:n+1] = a[2,2:n+1] #Bottom line
  a[2:(n+1),1] = a[2:(n+1),n+1] #Left line
  a[2:(n+1),n+2] = a[2:(n+1),2] #Right line
  
  a[1,1] = a[n+1,n+1]
  a[1,n+2] = a[n+1,2]
  a[n+2,1] = a[2,n+1]
  a[n+2,n+2] = a[2,2]
  
  a
}
neighbours <- function(a){
  n = dim(a)[1]-2
  nbr= a[1:n,1:n] + a[1:n,2:(n+1)] +a[1:n,3:(n+2)]+ #top
    a[2:(n+1),1:n] + a[2:(n+1),3:(n+2)]+ #center
    a[3:(n+2),1:n] + a[3:(n+2),2:(n+1)] + a[3:(n+2),3:(n+2)] #bottom
  
  return(nbr)
}
next_step <- function(a,nbr){
  n = dim(a)[1]-2
  b <- matrix(0,n,n)
  b[a[2:(n+1),2:(n+1)]==1 & (nbr ==2 | nbr == 3)] =1
  b[a[2:(n+1),2:(n+1)]==0 & ( nbr == 3)] =1
  a[2:(n+1),2:(n+1)]<-b
  a<- extra_cell(n,a)
  a
}
plot_mat <- function(a){
  n = dim(a)[1]-2
  par(pty = "s")
  image(a[2:n+1,2:n+1],col = c(3,2),axes=F,frame.plot=T,main="Conways game of life")
}
game <- function(a,n) {
  setwd("~/Documents/Rfiles/T_09/images")
  for (i in 1:30){
    a<-extra_cell(n,a)
    nbr<-neighbours(a)
    a<-next_step(a,nbr)
    name <- paste0("plot",i,".png")
    png(name)
    plot_mat(a)
    Sys.sleep(0.1)
    dev.off()
    
  }
}
game(a,n)