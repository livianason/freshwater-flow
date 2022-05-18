
falsc<-fal[fal$site_name=="Spring Creek 10 (Deep)",]
cl<-kmeans(falsc[,c('temp','cond')],3)
falsc[,'CTcl']<-as.factor(cl$cluster)

falsc[falsc$avdir>115 & falsc$avdir<200,'Dircl']<-"South"
falsc[falsc$avdir>200 & falsc$avdir<290,'Dircl']<-"West"
falsc[falsc$avdir>290 | falsc$avdir<30,'Dircl']<-"North"
falsc[falsc$avdir>30 & falsc$avdir<115,'Dircl']<-"East"

ggplot(falsc)+geom_point(aes(x=date_time,y=aspd,color=CTcl))
ggplot(falsc)+geom_point(aes(x=avdir,y=cond,color=CTcl))                         

falsc[,'temproll']<-rollmean(falsc$temp,25,fill=0)
falsc[,'condroll']<-rollmean(falsc$cond,25,fill=0)
cl<-kmeans(falsc[,c('temproll','condroll')],3)
falsc[,'CTRcl']<-as.factor(cl$cluster)

ggplot(falsc)+geom_point(aes(x=date_time,y=aspd,color=CTRcl))
ggplot(falsc)+geom_point(aes(x=temproll,y=condroll,color=CTRcl))                         


test<-merge(basins[,c('gid','huc')],rain,by.x='gid',by.y='objectid',all.y = T)
test<-test[test$huc=='03120001',]
test<-merge(test[,c('gid','date_time','gallons')],falsc[,c('date_time','temp','aspd','cond','CTRcl','CTcl')],all=T)
test[is.na(test)]<-0

ggplot(test[test$gallons<0.4,])+geom_point(aes(x=date_time,y=gallons,color=CTcl),size=0.5)



#---
ggplot(falsc)+geom_point(aes(x=date_time,y=avdir,color=CTcl))

falcl<-merge(fal,falsc[,c('date_time','CTcl','CTRcl','Dircl')])
ggplot(falcl[!(falcl$site_name=="Spring Creek 10 (Deep)"&falcl$aspd>150),])+geom_point(aes(x=date_time,y=aspd,color=CTRcl),size=0.5)+facet_grid(site_name~.,scales='free')

ggplot(falsc)+geom_point(aes(x=temp,y=cond,color=CTcl),size=0.7)  


ggplot()
