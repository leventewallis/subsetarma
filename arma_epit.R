ARMAepit=function(egyed,Y,alpha){
  data<-as.data.frame(Y)
  colnames(data)[1] ="Y"
  #egyedb?l k?l?n MA ?s AR r?sz
  hossz<-length(egyed)
  ARresz<-egyed[1:(hossz/2)]
  MAresz<-egyed[-(1:(hossz/2))]
  
  fam<-vector("integer")
  far<-vector("integer")
  koefekszama<-0
  far<-0
  fam<-0
  for (f in 1:hossz) {
    if (f<=(hossz/2)&egyed[f]==1) {
      koefekszama<-koefekszama+1
      far<-f
    }
    if (f>(hossz/2)&egyed[f]==1) {
      koefekszama<-koefekszama+1
      fam<-f-(hossz/2)
    }
  }
  MAXlag<-max(c(far,fam))
  koefek<-rep(0.5,koefekszama+1)
  
  
  tryCatch(

    {
      eredmeny<-optim(koefek,neg_loglikelihood,
                      adatok=data,EgyedARresz=ARresz,EgyedMAresz=MAresz,MaximalisLag=MAXlag,LegnagyobbMA=fam,
                      LegnagyobbAR=far,hessian = T)
    },
    error=function(e) {
      message('An Error Occurred itt')
      eredmeny<-NA
      return(eredmeny)
    }
  )
  
  for (eredmenyszamlalo in 1:3) {
    if (eredmeny$convergence==1) {
      
      eredmeny2<-optim(eredmeny$par,neg_loglikelihood,
                       adatok=data,EgyedARresz=ARresz,EgyedMAresz=MAresz,MaximalisLag=MAXlag,LegnagyobbMA=fam,
                       LegnagyobbAR=far,hessian = T)
      eredmeny<-eredmeny2
    }
  }
  
  #significance es sh
  SH2<-HelpersMG::SEfromHessian(eredmeny$hessian)
  
  
  
  
  datauj<-adatokkiszam(eredmeny$par,adatok=data,ARresz,MAresz,MAXlag,fam,far)

  #valtozok szignifikansak
  Valtszignifvek=c()
  
  for (Valt in 2:length(koefek)) {
    if (2*dt(-abs(eredmeny$par[Valt]/SH2[Valt]),nrow(data)-(sum(egyed)+1))<=alpha) {
      Valtszignifvek<-append(Valtszignifvek,1)
    }
    else{Valtszignifvek<-append(Valtszignifvek,0)}
  }
  valtszignif<-prod(Valtszignifvek)
  
  #BIC
  inform<-2*eredmeny$value+2*(sum(egyed)+1)*log(nrow(data))
  
  
  #feherzaj
  feherzajtest<-lmtest::bgtest(datauj$hiba ~ 1, order =1 )
  feher<-0
  if (feherzajtest$p.value>alpha) {
    feher<-1
  }
  
  
 
  return(list(eredmeny$par,inform,feher,valtszignif))
}
neg_loglikelihood <- function(adatok,koefficiensek,EgyedARresz,EgyedMAresz,MaximalisLag,LegnagyobbMA,LegnagyobbAR){
  ARbecsles<-vector("double",nrow(adatok))
  MAbecsles<-vector("double",nrow(adatok))  
  for (q in 1:nrow(adatok)) {
    if (q<=MaximalisLag) {
      adatok$becsultY[q]<-koefficiensek[1]
    }
    if (q>MaximalisLag) {
      if (LegnagyobbAR!=0) {
        for (a in 1:LegnagyobbAR) {
          koefindex<-2
          if (EgyedARresz[a]==1) {
            ARbecsles[q]<-ARbecsles[q]+adatok$Y[q-a]*koefficiensek[koefindex]
            koefindex<-koefindex+1
          }
        }
      }
      else  koefindex<-2
      if (LegnagyobbMA!=0) {
        for (m in 1:LegnagyobbMA) {
          
          if (EgyedMAresz[m]==1) {
            MAbecsles[q]<-MAbecsles[q]+(adatok$Y[q-m]-adatok$becsultY[q-m])*koefficiensek[koefindex]
            koefindex<-koefindex+1
            
          }
        }
      }
      
      if (LegnagyobbAR>=1&LegnagyobbMA>=1) {
        adatok$becsultY[q]<-ARbecsles[q]+MAbecsles[q]+koefficiensek[1]
      }
      if (LegnagyobbAR>=1&LegnagyobbMA==0) {
        adatok$becsultY[q]<-ARbecsles[q]+koefficiensek[1]
      }
      if (LegnagyobbAR==0&LegnagyobbMA>=1) {
        adatok$becsultY[q]<-MAbecsles[q]+koefficiensek[1]
        
      }
    }
  }
  #Likelihood kezdetben NA
  adatok$Likelihood <- NA
  
  
  for (b in 1:nrow(adatok)) {
    adatok$Likelihood[b] <- dnorm(adatok$Y[b],mean = adatok$becsultY[b],sd=sd(adatok$Y-adatok$becsultY, na.rm = T))
  }
  return(-sum(log(adatok$Likelihood[1:nrow(adatok)])))
}
adatokkiszam <- function(adatok,koefficiensek,EgyedARresz,EgyedMAresz,MaximalisLag,LegnagyobbMA,LegnagyobbAR){
  ARbecsles<-vector("double",nrow(adatok))
  MAbecsles<-vector("double",nrow(adatok))
  hiba<-vector("double",nrow(adatok))
  for (q in 1:nrow(adatok)) {
    if (q<=MaximalisLag) {
      adatok$becsultY[q]<-koefficiensek[1]
    }
    if (q>MaximalisLag) {
      if (LegnagyobbAR!=0) {
        for (a in 1:LegnagyobbAR) {
          koefindex<-2
          if (EgyedARresz[a]==1) {
            ARbecsles[q]<-ARbecsles[q]+adatok$Y[q-a]*koefficiensek[koefindex]
            koefindex<-koefindex+1
          }
        }
      }
      else  koefindex<-2
      if (LegnagyobbMA!=0) {
        for (m in 1:LegnagyobbMA) {
          if (EgyedMAresz[m]==1) {
            MAbecsles[q]<-MAbecsles[q]+(adatok$Y[q-m]-adatok$becsultY[q-m])*koefficiensek[koefindex]
            koefindex<-koefindex+1
            
          }
        }
      }
      
      if (LegnagyobbAR>=1&LegnagyobbMA>=1) {
        adatok$becsultY[q]<-ARbecsles[q]+MAbecsles[q]+koefficiensek[1]
      }
      if (LegnagyobbAR>=1&LegnagyobbMA==0) {
        adatok$becsultY[q]<-ARbecsles[q]+koefficiensek[1]
      }
      if (LegnagyobbAR==0&LegnagyobbMA>=1) {
        adatok$becsultY[q]<-MAbecsles[q]+koefficiensek[1]
        
      }
      
    }
    adatok$hiba<-adatok$Y-adatok$becsultY
  }
  
  return(adatok)
  
}
ARMAsim=function(egyed,koefficiensek,hossz,szoras){
  data<-data.frame(matrix(nrow = hossz, ncol = 2))
  colnames(data)= c("Y","innovacio") 
  ARresz<-egyed[1:5]
  MAresz<-egyed[-(1:5)]
  
  fam<-vector("integer")
  far<-vector("integer")
  koefekszama<-0
  far<-0
  fam<-0
  for (f in 1:10) {
    if (f<=5&egyed[f]==1) {
      koefekszama<-koefekszama+1
      far<-f
    }
    if (f>5&egyed[f]==1) {
      koefekszama<-koefekszama+1
      fam<-f-5
    }
  }
  MAXlag<-max(c(far,fam))
  
  #innovacio
  data$innovacio<- arima.sim(model = list(order = c(0, 0, 0)), n = hossz, mean=koefficiensek[1], sd=szoras)
  
  ARbecsles<-vector("double",hossz)
  MAbecsles<-vector("double",hossz)
  
  
  for (q in 1:hossz) {
    if (q<=MAXlag) {
      data$Y[q]<-data$innovacio[q]
    }
    if (q>MAXlag) {
      if (far!=0) {
        for (a in 1:far) {
          koefindex<-2
          if (ARresz[a]==1) {
            ARbecsles[q]<-ARbecsles[q]+data$Y[q-a]*koefficiensek[koefindex]
            koefindex<-koefindex+1
          }
        }
      }
      else  koefindex<-2
      if (fam!=0) {
        for (m in 1:fam) {
          
          if (MAresz[m]==1) {
            MAbecsles[q]<-MAbecsles[q]+(data$innovacio[q-m])*koefficiensek[koefindex]
            koefindex<-koefindex+1
            
          }
        }
      }
      
      if (far>=1&fam>=1) {
        data$Y[q]<-ARbecsles[q]+MAbecsles[q]+data$innovacio[q]
      }
      if (far>=1&fam==0) {
        data$Y[q]<-ARbecsles[q]+data$innovacio[q]
      }
      if (far==0&fam>=1) {
        data$Y[q]<-MAbecsles[q]+data$innovacio[q]
        
      }
    }
  }
  return(data)
  
  
}

sokszam<-apply(expand.grid(0:1,0:1,0:1,0:1,0:1,0:1), 1, as.vector)


sokszam<-sokszam[,-1]




