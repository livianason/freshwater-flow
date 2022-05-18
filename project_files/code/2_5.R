

normalize<- function(data){
  r <- (data-min(data))/(max(data)-min(data))
  return(r)
  }

date1<-as.POSIXct('2012-06-25')
date2<-as.POSIXct('2012-07-15')

falsc<- fal[fal$site_name=='Spring Creek 10 (Deep)' & fal$date_time>date1 & fal$date_time<date2,]
falsc[,'aspdnorm']<-normalize(falsc$aspd)
falsc[,'tempnorm']<-normalize(falsc$temp)
falsc[,'aspdroll']<-rollmean(falsc$aspdnorm,25,fill=NA)

ggplot(falsc,aes(date_time,aspdnorm))+geom_line(
  ) + geom_line(aes(date_time,aspdroll,color='roll')
  ) + geom_line(aes(date_time,tempnorm,color='temp'))

#------

fal1<-dcast(fal, date_time~site_name,value.var = 'aspd')
fal1[is.na(fal1)]<-0
colnames(fal1)<-c('date_time','AK','Rev','SC10')

fal1$AK<-normalize(fal1$AK)
fal1$Rev<-normalize(fal1$Rev)
fal1$SC10<-normalize(fal1$SC10)

fal1$AK<-rollmean(fal1$AK,25,fill=NA)
fal1$Rev<-rollmean(fal1$Rev,25,fill=NA)
fal1$SC10<-rollmean(fal1$SC10,25,fill=NA)

ggplot(fal1[fal1$date_time>as.POSIXct('2012-06-25') & fal1$date_time<as.POSIXct('2012-07-15'),]
           ) + geom_line(aes(date_time,SC10,color='SC')
           ) + geom_line(aes(date_time,Rev,color='Rev')
           ) + geom_line(aes(date_time,AK,color='AK'))
         
#------
fal1<-fal[fal$date_time>date1&fal$date_time<date2,]
fal1<-dcast(fal1, date_time~site_name,value.var = 'aspd')
fal1[is.na(fal1)]<-0
colnames(fal1)<-c('date_time','AK','Rev','SC10')

fal1$AK<-rollmean(fal1$AK,25,fill=0)
fal1$Rev<-rollmean(fal1$Rev,25,fill=0)
fal1$SC10<-rollmean(fal1$SC10,25,fill=0)

fal1$AK<-normalize(fal1$AK)
fal1$Rev<-normalize(fal1$Rev)
fal1$SC10<-normalize(fal1$SC10)



rain1<-rain[rain$date_time>date1 & rain$date_time<date2,]
falrain1<-merge(fal1[,c('date_time','AK','Rev','SC10')],rain1[,c('date_time','gallons')])

falrain1$gallons<-rollmean(falrain1$gallons,25,fill=0)
falrain1$gallons<-normalize(falrain1$gallons)

ggplot(falrain1,aes(x=date_time)
       ) + geom_line(aes(y=AK,color='ak')
       ) + geom_line(aes(y=SC10,color='sc')
       ) + geom_line(aes(y=Rev,color='rev')
       ) + geom_line(aes(y=gallons,color='gallons'))

#------
date3<-as.POSIXct('2013-04-01')
date4<-as.POSIXct('2013-05-20')
r<-rain[rain$date_time>date3&rain$date_time<date4&rain$objectid=='330',]
summary(r$gallons)

r$gallons<-normalize(r$gallons)
r[,'gallonsroll']<-rollmean(r$gallons,25,fill=0)
r$gallonsroll<-normalize(r$gallonsroll)

ggplot(r,aes(date_time,gallonsroll))+geom_point()#+geom_point(data=r,aes(date_time,gallons,color='gal'))


#------

date5<-as.POSIXct('2012-04-08')
date6<-as.POSIXct('2012-04-14')

r1<-rain[rain$date_time>date5&rain$date_time<date6,]
r1$objectid<-as.factor(r1$objectid)
f1<-fal[fal$site_name=="Spring Creek 10 (Deep)"&fal$date_time>date5&fal$date_time<date6,]

r1$gallons<-normalize(r1$gallons)
#r1<-r1[r1$gallons>0.1,]
f1$aspd<-rollmean(f1$aspd,25,fill=0)
f1$aspd<-normalize(f1$aspd)
ggplot()+geom_point(data=r1,aes(date_time,gallons))+geom_line(data=f1,aes(date_time,aspd,color='aspd')
                                                              )+geom_point(data=r1[r1$objectid=='312',],aes(date_time,gallons,color='312'))

length(unique(r1$objectid))

r1[r1$date_time==as.POSIXct('2012-04-10 15:00:00'),]

unique(r1$objectid[r1$gallons>0.2])



