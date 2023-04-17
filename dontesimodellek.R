df$hossz<-as.factor(df$hossz)
df$sd<-as.numeric(df$sd)
df$lag1<-as.logical(df$lag1)
df$lag2<-as.logical(df$lag2)
df$lag3<-as.logical(df$lag3)

df$err1<-as.logical(df$err1)
df$err2<-as.logical(df$err2)
df$err3<-as.logical(df$err3)

df$diff<-as.factor(df$diff)

df$eredmeny<-as.factor(df$eredmeny)



library(ggplot2)
ggplot(data = df,aes(x=eredmeny,fill=hossz))+geom_bar() +
  facet_wrap(~hossz,nrow=3)+theme_minimal()+  xlab("Eredmény") +
  ylab("Mennyiség")+labs(fill="Idősor hossza")

df %>%  group_by(hossz,eredmeny) %>%
  summarise(cnt = n()) %>%
  mutate(freq = round(cnt / sum(cnt), 3))




df$eredmeny2<-F

df$eredmeny2[df$eredmeny==1]<-T


modell<-rpart::rpart(data = df,eredmeny~lag1+lag2+lag3+err1+err2+err3+sd+hossz+hany+diff+szakadasos)
rpart.plot(modell,box.palette  = list("Greens","Greys", "Reds"))
summary(modell)


rpart(data = df,type,eredmeny~lag1+lag2+lag3+err1+err2+err3+sd+hossz+hany+diff+szakadasos)

df2<-df
modell2<-rpart::rpart(data = df,method = "class",eredmeny2~lag1+lag2+lag3+err1+err2+err3+sd+hossz+hany+diff+szakadasos)
rpart.plot(modell2)

modell3<-rpart::rpart(data = df2,eredmeny2~lag1+lag2+lag3+err1+err2+err3+sd+hossz+hany+diff+szakadasos)




df2$vlsz<-predict(modell3, newdata = df2) 

library(pROC)
ROC_adatok <- roc(df$eredmeny2, df2$vlsz)
plot(ROC_adatok)
ROC_adatok$auc
#AUC 0,8712






