
head(falad)
summary(falD$cond)
ggplot(falD[falD$site_name=='AD (Deep)',])+geom_point(aes(date_time,avdir))

falad<-falD[falD$site_name=="AD (Deep)",]
falad[falad$avdir>270 | falad$avdir<90,'clAD']<-"North"
falad[falad$avdir>90 & falad$avdir<270,'clAD']<-"South"

falsc<-fal[fal$site_name=="Spring Creek 10 (Deep)",]
falsc[falsc$avdir>115 & falsc$avdir<200,'clSC']<-"South"
falsc[falsc$avdir>200 & falsc$avdir<290,'clSC']<-"West"
falsc[falsc$avdir>290 | falsc$avdir<30,'clSC']<-"North"
falsc[falsc$avdir>30 & falsc$avdir<115,'clSC']<-"East"


new <- merge(falD[,c('site_name','date_time','temp','cond','aspd','avdir')],falad[,c('date_time','clAD')])
new$clAD<-as.factor(new$clAD)

ggplot(new)+geom_point(aes(date_time,aspd,color=clAD))+facet_grid(site_name~., scales='free')

allfal<-rbind(fal,falD)
str(allfal)

m<- merge(allfal,falad[,c('date_time','clAD')],all=T)
m<- merge(m,falsc[,c('date_time','clSC')],all=T)
m$clAD<-as.factor(m$clAD)
m$clSC<-as.factor(m$clSC)

test<-m[m$site_name=='AK (Deep)'|m$site_name=='K (Deep)'|m$site_name=='AD (Deep)'|m$site_name=='Spring Creek 10 (Deep)'|m$site_name=='Revell Sink (Deep)'|m$site_name=="D (Deep)",]

ggplot(test[test$date_time>as.POSIXct('2013-01-01')&test$date_time<as.POSIXct('2013-12-31'),]
       )+geom_point(aes(date_time,aspd,color=clSC),size=0.5)+facet_grid(site_name~.,scales='free')


summary(m$temp[m$site_name=="C (Deep)"])



 
ggplot(test[test$date_time>as.POSIXct('2013-01-01')&test$date_time<as.POSIXct('2013-12-31'),])+
  geom_point(aes(date_time,aspd,color=clSC),size=0.5)+
  facet_grid(site_name~.,scales='free')+
  geom_point(data=rain13,aes(date_time, gallons))

ggplot(rain13)+geom_point(aes(date_time,gallons)








