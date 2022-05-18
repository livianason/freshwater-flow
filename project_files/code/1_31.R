db2 <- dbConnect("PostgreSQL", db = 'x', host = 'x', user ='x', password = 'x')
SQL2 <- "SELECT * FROM y2011.n1p_basins;"
n1pbasins<-dbGetQuery(db2,SQL2)
dbDisconnect(db2)

db <- dbConnect("PostgreSQL", db = 'x', host = 'x', user ='x', password = 'x')
SQL <- "
    SELECT * 
    FROM excel_falmouth
    WHERE date_trunc('day', date_time) 
    BETWEEN '2011-01-01' AND '2011-12-31'
    AND site_name = 'Spring Creek 10 (Deep)';"
fal<-dbGetQuery(db,SQL)
dbDisconnect(db)

n1pbasins$objectid<-as.factor(n1pbasins$objectid)
n1pbasins$gallons<-n1pbasins$volume/n1pbasins$area
ggplot(n1pbasins[n1pbasins$objectid==330 & n1pbasins$date_time<'2011-01-02 06:00:00',],aes(date_time,gallons))+geom_point()

janrain<-n1pbasins[n1pbasins$objectid==330 & n1pbasins$date_time<'2011-02-01 00:00:00',]
janflow<-fal[fal$date_time>'2011-01-01 00:00:00' & fal$date_time<'2011-02-01',]
p<-ggplot(janrain,aes(date_time,gallons*2))+geom_point(aes(color='rain'))
p<-p+geom_point(data=janflow,aes(x=date_time,y=aspd/300))
p
nrow(janrain)

#TEST
testrain<-n1pbasins[n1pbasins$objectid==330 & n1pbasins$date_time<'2011-01-02 06:00:00',]
ggplot(testrain)+geom_point(aes(x=date_time,y=gallons))
test2<-as.data.frame(testrain$gallons*5)
test2$date_time<-testrain$date_time+hours(7)
colnames(test2)[1]<-'gallons'
ggplot(testrain)+geom_point(aes(x=date_time,y=gallons))+geom_point(data=test2,aes(x=date_time,y=gallons,color='rain'))
t<-merge(testrain[,c('date_time','gallons')],test2[,c('date_time','gallons')],by.x='date_time',by.y='date_time',all=T)
t[is.na(t)]<-0

ccf(testrain,test2)
