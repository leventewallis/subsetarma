szimulacio=function(n,setseedstart,szigma){
  sokszam<-apply(expand.grid(0:1,0:1,0:1,0:1,0:1,0:1), 1, as.vector)
  sokszam<-sokszam[,-1]
  randomkoefek=list()
  egyoutput<-matrix(nrow = 63,ncol = 3)
  outputok<-list()
  diffvekt<-vector()
  egyutthatok<-vector("list", length = 63)
  for (i in 1:63) {
    set.seed((setseedstart-1)+i)
    egyedem<-sokszam[,i]
    egyedemsim<-append(egyedem,c(0,0),after = 3)
    egyedemsim<-append(egyedemsim,c(0,0),after = 8)
    
  
    randomkoefek[[i]]<-c(0.5,runif(sum(egyedem),min=-0.5,max=0.5))
    
    Y<-ARMAsim(egyedemsim,randomkoefek[[i]],n,szigma)
    
    Idosor<-Y$Y
    
    diffszamlalo=0
    for (diffindex in 1:3) {
      stace<-tseries::adf.test(Idosor)
      if (stace$p.value>=0.05) {
        diffszamlalo=diffszamlalo+1
        Idosor<-diff(Y$Y)
      }
    }
    diffvekt[i]<-diffszamlalo
    
    for (j in 1:63) {
      ARMAoutput<-list()
      try(ARMAoutput<-ARMAepit(sokszam[,j],Idosor,0.01))
      try(egyoutput[j,1]<-ARMAoutput[[2]][1])
      try(egyoutput[j,2]<-ARMAoutput[[3]][1])
      try(egyoutput[j,3]<-ARMAoutput[[4]][1])
      try(egyutthatok[[i]][j]<-ARMAoutput[1])
      
    }
    outputok[[i]]<-egyoutput
    egyoutput<-matrix(nrow = 63,ncol = 3)
    
  }
  return(list(outputok,randomkoefek,diffvekt,egyutthatok))
}
sorrend=function(output,szign){
  library(dplyr)
  hanyadikok<-vector()
  sorrendek<-vector()
  for (szamlalo in 1:63) {
    aktualis<-as.data.frame(output[[szamlalo]])
    for (megszamlalo in 1:63) {
      aktualis[megszamlalo,4]<-megszamlalo
      
    }
    if (szign==F) {
      legjobb<-aktualis%>%filter(V2==1)%>%arrange(V1)
    }
    else{
      legjobb<-aktualis%>%filter(V2==1&V3==1)%>%arrange(V1)
    }
    try(hanyadikok[szamlalo]<-which(legjobb$V4==szamlalo))
    sorrendek[szamlalo]<-legjobb[1,4]
    aktualis<-vector()
  }
  return(list(sorrendek,hanyadikok))
}


  
  






























