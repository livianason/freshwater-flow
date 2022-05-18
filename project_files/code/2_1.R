date1<-'2013-03-01'
date2<-'2013-04-01'

rain.aug<-rain[rain$date_time>date1 & rain$date_time<date2,]
fal.aug<-fal[fal$date_time>date1 & fal$date_time<date2,]
rain.aug$date_time<-rain.aug$date_time+hours(75)


ggplot(rain.aug[rain.aug$objectid=='330',],aes(date_time,(volume/area)*500))+geom_point()+geom_line(data=fal.aug,aes(date_time,aspd,color=avdir))

