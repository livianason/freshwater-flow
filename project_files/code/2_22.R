

falad<-fal[fal$site_name=="AD (Deep)",]
falad[falad$avdir>270 | falad$avdir<90,'clAD']<-"North"
falad[falad$avdir>90 & falad$avdir<270,'clAD']<-"South"

falsc<-fal[fal$site_name=="Spring Creek 10 (Deep)",]
falsc[falsc$avdir>115 & falsc$avdir<200,'clSC']<-"South"
falsc[falsc$avdir>200 & falsc$avdir<290,'clSC']<-"West"
falsc[falsc$avdir>290 | falsc$avdir<30,'clSC']<-"North"
falsc[falsc$avdir>30 & falsc$avdir<115,'clSC']<-"East"

falak<-fal[fal$site_name=="AK (Deep)",]
falak[falak$avdir<35 | falak$avdir>340,'clAK']<-"North"
falak[falak$avdir>220 & falak$avdir<340,'clAK']<-"West"
falak[falak$avdir>130 & falak$avdir<220,'clAK']<-"South"
falak[falak$avdir>35 & falak$avdir<130,'clAK']<-"East"

allfal <- merge(fal[,c('site_name','date_time','temp','cond','aspd','avdir')],falad[,c('date_time','clAD')],all=T)
allfal <- merge(allfal,falsc[,c('date_time','clSC')],all=T)
allfal <- merge(allfal,falak[,c('date_time','clAK')],all=T)
allfal$clAD<-as.factor(allfal$clAD)
allfal$clSC<-as.factor(allfal$clSC)
allfal$clAK<-as.factor(allfal$clAK)

ggplot(allfal[allfal$date_time>as.POSIXct('2012-03-12')&allfal$date_time<as.POSIXct('2012-04-01'),])+
  geom_point(aes(date_time,temp,color=clAD),size=0.5)+
  facet_grid(site_name~.,scales='free')

ggplot(allfal[allfal$date_time>as.POSIXct('2012-03-12')&allfal$date_time<as.POSIXct('2012-04-01'),])+
  geom_point(aes(date_time,aspd,color=clAD),size=0.5)+
  facet_grid(site_name~.,scales='free')


ggplot(rain[rain$date_time>as.POSIXct('2012-03-21')&rain$date_time<as.POSIXct('2012-03-24'),])+geom_point(aes(date_time,gallons))+
  geom_point(data=r[r$objectid %in% c(312),],aes(x=date_time,y=gallons,color='r'))

r<-rain[rain$gallons>0.009&rain$date_time>as.POSIXct('2012-03-21 18:00:00')&rain$date_time<as.POSIXct('2012-03-22 12:00:00'),]

#--- DIR
#AD, AK, D, B, K, Rev, Sc

ggplot(fal[fal$site_name=='AK (Deep)',])+geom_point(aes(date_time,avdir))+
  geom_hline(yintercept = 0)+
  geom_hline(yintercept = 360)+
  geom_hline(yintercept = 35)+
  geom_hline(yintercept = 130)+
  geom_hline(yintercept = 340)+
  geom_hline(yintercept = 220)
