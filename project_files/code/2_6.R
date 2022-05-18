

date1<-as.POSIXct('2011-12-22')
date2<-as.POSIXct('2011-12-26')

r1<-rain[rain$date_time>date1 & rain$date_time<date2,]
r1$objectid<-as.factor(r1$objectid)
f1<-fal[fal$site_name=="Revell Sink (Deep)" & fal$date_time>date1 & fal$date_time<date2,]
r1$gallons<-normalize(r1$gallons)
f1$aspd<-rollmean(f1$aspd,25,fill=0)
f1$aspd<-normalize(f1$aspd)

ggplot()+geom_line(data=f1,aes(date_time,aspd))+geom_point(data=r1,aes(date_time,gallons))
 
unique(r1[r1$gallons>0.5,c('objectid')])


#---

date3<-as.POSIXct('2011-12-25')
date4<-as.POSIXct('2011-12-28')

r1<-rain[rain$date_time>date3 & rain$date_time<as.POSIXct('2011-12-27'),]
r1$objectid<-as.factor(r1$objectid)
f1<-fal[fal$site_name=="Revell Sink (Deep)" & fal$date_time>date3 & fal$date_time<date4,]
r1$gallons<-normalize(r1$gallons)
f1$aspd<-rollmean(f1$aspd,25,fill=0)
f1$aspd<-normalize(f1$aspd)

ggplot()+geom_line(data=f1,aes(date_time,aspd))+geom_point(data=r1,aes(date_time,gallons))

bas<-as.data.frame(unique(r1[r1$gallons>0,c('objectid')]))
colnames(bas)<-'gid'
res<-merge(bas,basins[,c('gid','huc','exthuc')])
unique(res$gid[res$huc=='03120001'])

#---
#2/7

zoom<-function(date1,date2,site){
  date1<-as.POSIXct(date1)
  date2<-as.POSIXct(date2)
  date1<-date1-days(2)
  date2<-date2+days(2)
  r1<-rain[]
  r1<-rain[rain$date_time>date1 & rain$date_time<date2,]
  r1$objectid<-as.factor(r1$objectid)
  f1<-fal[fal$site_name==site & fal$date_time>date1 & fal$date_time<date2,]
  r1$gallons<-normalize(r1$gallons)
  f1$aspd<-rollmean(f1$aspd,25,fill=0)
  f1$aspd<-normalize(f1$aspd)
  fr<- merge(f1,r1,by.x='date_time',by.y='date_time')
  return(fr)
}
fr<-zoom('2012-03-14','2012-03-20','AK (Deep)')

p<-ggplot(fr)+geom_line(aes(date_time,aspd))+geom_point(aes(date_time,gallons))
p
bas<-as.data.frame(unique(fr[fr$gallons>0&fr$date_time<as.POSIXct('2012-03-22'),c('objectid')]))
colnames(bas)<-'gid'
res<-merge(bas,basins[,c('gid','huc','exthuc')])
u<-unique(res$gid[res$huc=='03120001'])
paste(as.character(u),collapse=',')
p+geom_point(data=fr[fr$objectid %in% u,],aes(date_time,gallons,color='basins'))

