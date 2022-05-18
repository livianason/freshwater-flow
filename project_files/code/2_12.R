

faltemp<-dcast(fal[fal$date_time<as.POSIXct('2012-01-01'),], date_time~site_name,value.var = 'cond',fill=NA)
colnames(faltemp)<-c('date_time','AK','Rev','SC10')
faltemp$AK[is.na(faltemp$AK)]<-min(faltemp$AK[!is.na(faltemp$AK)])-0.1
faltemp$Rev[is.na(faltemp$Rev)]<-min(faltemp$Rev[!is.na(faltemp$Rev)])-0.1
faltemp$SC10[is.na(faltemp$SC10)]<-min(faltemp$SC10[!is.na(faltemp$SC10)])-0.1

faltemp[,'AKroll']<-rollmean(faltemp$AK,25,fill=NA)
faltemp[,'Revroll']<-rollmean(faltemp$Rev,25,fill=NA)
faltemp[,'SCroll']<-rollmean(faltemp$SC10,25,fill=NA)

faltemp$AKroll[is.na(faltemp$AKroll)]<-min(faltemp$AKroll[!is.na(faltemp$AKroll)])
faltemp$Revroll[is.na(faltemp$Revroll)]<-min(faltemp$Revroll[!is.na(faltemp$Revroll)])
faltemp$SCroll[is.na(faltemp$SCroll)]<-min(faltemp$SCroll[!is.na(faltemp$SCroll)])

faltemp[,'AKnorm']<-normalize(faltemp$AKroll)
faltemp[,'Revnorm']<-normalize(faltemp$Revroll)
faltemp[,'SCnorm']<-normalize(faltemp$SCroll)

ggplot(faltemp)+geom_line(aes(date_time,AKnorm,color='AK'))+geom_line(aes(date_time,Revnorm,color='Rev'))+geom_line(aes(date_time,SCnorm,color='SC'))

fal1<-dcast(fal[fal$date_time<as.POSIXct('2012-01-01'),], date_time~site_name,value.var = 'aspd')
fal1[is.na(fal1)]<-0
colnames(fal1)<-c('date_time','AK','Rev','SC10')

fal1$AK<-rollmean(fal1$AK,25,fill=0)
fal1$Rev<-rollmean(fal1$Rev,25,fill=0)
fal1$SC10<-rollmean(fal1$SC10,25,fill=0)
fal1$AK<-normalize(fal1$AK)
fal1$Rev<-normalize(fal1$Rev)
fal1$SC10<-normalize(fal1$SC10)

rain1<-rain[rain$date_time<as.POSIXct('2012-01-01'),]
rain1[,'galroll']<-rollmean(rain1$gallons,25,fill=NA)
rain1[is.na(rain1)]<-0
rain1$galroll<-normalize(rain1$galroll)

#seth code
gwtemp <- data.frame(date_time = rain1$date_time, temp = 20.5)
gwtemp$norm <- (gwtemp$temp-min(faltemp$SC10))/((max(faltemp$SC10)-min(faltemp$SC10)))

ggplot()+geom_line(data=fal1,aes(x=date_time,y=Rev,color='aspd'))+geom_line(data=faltemp,aes(x=date_time,y=Revnorm,color='temp'))+geom_line(data=rain1,aes(x=date_time,y=galroll,color='rain'))
ggplot()+geom_line(data=fal1,aes(x=date_time,y=AK,color='aspd'))+geom_line(data=faltemp,aes(x=date_time,y=AKnorm,color='temp'))+geom_line(data=rain1,aes(x=date_time,y=galroll,color='rain'))

#seth code
ggplot()+geom_line(data=fal1,aes(x=date_time,y=SC10,color='aspd'))+geom_line(data=faltemp,aes(x=date_time,y=SCnorm,color='temp'))+geom_line(data=rain1,aes(x=date_time,y=galroll,color='rain'))+geom_line(data=gwtemp,aes(x=date_time,y=norm), color = 'burlywood')

                                                                                                                                            