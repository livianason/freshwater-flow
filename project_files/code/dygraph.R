library(dygraphs)
library(xts)

#2/1
falrain<-merge(fal[,c('date_time','aspd')],rain[,c('date_time','gallons')],all=T)
falrain$gallons[is.na(falrain$gallons)]<-0

falrain$date_time<-as.POSIXlt(falrain$date_time)

falrain<-xts(falrain[,c(2,3)],order.by=falrain[,1])

dygraph(falrain) %>%
  dyAxis('y',label='ASPD') %>%
  dyAxis('y2',label="Rainfall") %>%
  #dySeries('gallonsroll',axis='y2') %>%
  #dyRoller(rollPeriod = 50) %>%
  dyRangeSelector(dateWindow = c('2012-04-30','2012-05-12'))


#2/2
fal1<-dcast(fal, date_time~site_name,value.var = 'aspd')
colnames(fal1)<-c('date_time','AK','Rev','SC10')
falrain<-merge(fal1[,c('date_time','AK','Rev','SC10')],rain[,c('date_time','gallons')])
falrain[is.na(falrain)]<-0
falrain<-xts(falrain[,c(2,3,4,5)],order.by=falrain[,1])



#2/5
fal1<-dcast(fal, date_time~site_name,value.var = 'aspd')
fal1[is.na(fal1)]<-0
colnames(fal1)<-c('date_time','AK','Rev','SC10')

fal1$AK<-rollmean(fal1$AK,25,fill=0)
fal1$Rev<-rollmean(fal1$Rev,25,fill=0)
fal1$SC10<-rollmean(fal1$SC10,25,fill=0)
fal1$AK<-normalize(fal1$AK)
fal1$Rev<-normalize(fal1$Rev)
fal1$SC10<-normalize(fal1$SC10)
summary(fal1$AK)

r<-rain
r[,'gallonsroll']<-rollmean(r$gallons,25,fill=0)
r$gallonsroll<-normalize(r$gallonsroll)

falrain<-merge(fal1[,c('date_time','AK','Rev','SC10')],r[,c('date_time','gallonsroll')],all=T)
falrain[is.na(falrain)]<-0
falrain<-xts(falrain[,c(2,3,4,5)],order.by=falrain[,1])



     