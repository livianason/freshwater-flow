rev <- data.frame(date_time=fal$date_time[fal$site_name=="Revell Sink (Deep)"],avdir=fal$avdir[fal$site_name=="Revell Sink (Deep)"])
ggplot(rev)+geom_point(aes(x=date_time,y=avdir,color=flow))+geom_hline(yintercept = 300)+geom_hline(yintercept = 170)+geom_hline(yintercept = 75)

rev[rev$avdir>300 | rev$avdir<75,'flow']<-1
rev[rev$avdir>75 & rev$avdir<170,'flow']<-2
rev[rev$avdir>170 & rev$avdir<300,'flow']<-3
rev$flow <- as.factor(rev$flow)

fal1<-merge(fal,rev[,c('date_time','flow')])
ggplot(fal1[fal1$site_name=="Revell Sink (Deep)" & fal1$date_time<as.POSIXct('2012-01-01'),])+geom_point(aes(x=date_time,y=aspd,color=flow))+geom_hline(yintercept =15)

fal2<-fal1[fal1$site_name=="Revell Sink (Deep)"&fal1$aspd<15&fal1$date_time<as.POSIXct('2012-01-01'),]
ggplot(fal2)+geom_point(aes(x=date_time,y=aspd,color=flow))


#---
#clustering test

fal3<-fal1[fal1$site_name=="Revell Sink (Deep)"&fal1$date_time<as.POSIXct('2012-01-01'),]
cl<-kmeans(fal3[,c('avdir','aspd','cond')],3)
fal3[,'clusters']<-as.factor(cl$cluster)
ggplot(fal3)+geom_point(aes(x=date_time,y=aspd,color=flow))

#---
#clustering with SC

sc<-data.frame(date_time=fal$date_time[fal$site_name=="Spring Creek 10 (Deep)"],avdir=fal$avdir[fal$site_name=="Spring Creek 10 (Deep)"])

sc[sc$avdir>115 & sc$avdir<200,'flow']<-1
sc[sc$avdir>200 & sc$avdir<290,'flow']<-2
sc[sc$avdir>290 | sc$avdir<30,'flow']<-3
sc[sc$avdir>30 & sc$avdir<115,'flow']<-4
sc$flow<-as.factor(sc$flow)
fal1<-merge(fal,sc[,c('date_time','flow')])
fal4<-fal1[fal1$site_name=="Spring Creek 10 (Deep)"&fal1$date_time<as.POSIXct('2012-01-01'),]
cl<-kmeans(fal4[,c('avdir','aspd','cond')],3)
fal4[,'clusters']<-as.factor(cl$cluster)
ggplot(fal4)+geom_point(aes(x=date_time,y=aspd,color=clusters))

newfal<-merge(fal,fal4[,c('date_time','flow','clusters')])

ggplot(newfal)+geom_point(aes(x=date_time,y=aspd,color=clusters))+facet_grid(site_name~., scales='free')

#---
# more tests

sc<-data.frame(date_time=fal$date_time[fal$site_name=="Spring Creek 10 (Deep)"],avdir=fal$avdir[fal$site_name=="Spring Creek 10 (Deep)"])

sc[sc$avdir>115 & sc$avdir<200,'flow']<-1
sc[sc$avdir>200 & sc$avdir<290,'flow']<-2
sc[sc$avdir>290 | sc$avdir<30,'flow']<-3
sc[sc$avdir>30 & sc$avdir<115,'flow']<-4
sc$flow<-as.factor(sc$flow)
fal1<-merge(fal,sc[,c('date_time','flow')])
fal4<-fal1[fal1$site_name=="Spring Creek 10 (Deep)"&fal1$date_time<as.POSIXct('2013-01-01')&fal1$date_time>as.POSIXct('2012-01-01'),]
cl<-kmeans(fal4[,c('avdir','aspd','cond')],3)
fal4[,'clusters']<-as.factor(cl$cluster)
newfal<-merge(fal,fal4[,c('date_time','flow','clusters')])

rev<-newfal[newfal$site_name=="Revell Sink (Deep)" & newfal$aspd<20,]
sc<-newfal[newfal$site_name=="Spring Creek 10 (Deep)" & newfal$aspd<100,]
ak<-newfal[newfal$site_name=='AK (Deep)',]
f<-rbind(rev,sc,ak)
ggplot(f)+geom_point(aes(x=date_time,y=aspd,color=clusters))+facet_grid(site_name~., scales='free')


#---
#ak test

ak<-fal[fal$site_name=='AK (Deep)',]
cl<-kmeans(ak[,c('avdir','aspd','cond')],3)
ak[,'cl']<-cl$cluster
ak$cl<-as.factor(ak$cl)
falak<-merge(fal,ak[,c('date_time','cl')])

ggplot(falak[falak$date_time>as.POSIXct('2012-01-01')&falak$date_time<as.POSIXct('2013-01-01'),]
       )+geom_point(aes(x=date_time,y=aspd,color=cl))+facet_grid(site_name~., scales='free')



points3D(ak$avdir,ak$aspd,ak$cond)


