
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
#fr<-zoom('2011-09-29','2011-10-02','Revell Sink (Deep)')
#fr<-zoom('2011-10-05','2011-10-07','Revell Sink (Deep)')
fr<-zoom('2011-10-13','2011-10-15','Revell Sink (Deep)')

p<-ggplot(fr)+geom_line(aes(date_time,aspd))+geom_point(aes(date_time,gallons))
p
bas<-as.data.frame(unique(fr[fr$gallons>0&fr$date_time<as.POSIXct('2011-10-14 12:00:00')&fr$date_time>as.POSIXct('2011-10-13 20:00:00'),c('objectid')]))
colnames(bas)<-'gid'
res<-merge(bas,basins[,c('gid','huc','exthuc')])
u<-unique(res$gid[res$huc=='03120003'])
paste(as.character(u),collapse=',')
p+geom_point(data=fr[fr$objectid %in% u,],aes(date_time,gallons,color='basins'))


#----




