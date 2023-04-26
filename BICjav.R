# BICjav ------------------------------------------------------------------
javitas<-function(output,hossz){
  numpar<-vector()
  for (i in 1:63) {
    numpar[i]<-sum(sokszam[,i])+1
  }
  for (i in 1:63) {
    for (j in 1:63) {
      B<-(numpar[j]*log(hossz))
      output[[1]][[i]][j,1]<-output[[1]][[i]][j,1]-B
    }
  }
  return(output)
}


javitott1260<-javitas(szim1260_szoras1,1260)
javitott252<-javitas(szim252_szoras1,252)
javitott50<-javitas(szim50_szoras1,50)

