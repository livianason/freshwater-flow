fal1<-fal

fal1$aspd[fal1$site_name=='Spring Creek 10 (Deep)' & (fal1$avdir>315 | fal1$avdir<45)]<- -1*fal1$aspd[fal1$site_name=='Spring Creek 10 (Deep)' & (fal1$avdir>315 | fal1$avdir<45)]

data1<- data.frame(date_time=fal1$date_time[fal1$site_name=='Spring Creek 10 (Deep)'],aspdneg=fal1$aspd[fal1$site_name=='Spring Creek 10 (Deep)'],aspdpos=fal$aspd[fal$site_name=='Spring Creek 10 (Deep)'])

ggplot(data1,aes(x=date_time))+geom_line(aes(y=aspdpos))+geom_line(aes(y=aspdneg,color='neg'))

#---as.POSIXct('2012-01-01')
fal2<-fal[fal$site_name=="Spring Creek 10 (Deep)",]
fal2$avdir[fal2$avdir<75]<- fal2$avdir[fal2$avdir<75]+360
hist(fal2$avdir,breaks=100)
ggplot(fal2)+geom_point(aes(x=date_time,y=avdir))+geom_hline(yintercept = 290)+geom_hline(yintercept = 200)+geom_hline(yintercept = 115)+geom_hline(yintercept = 30)

fal3<-fal[fal$site_name=="AK (Deep)",]
#fal3$avdir[fal3$avdir<180]<- fal3$avdir[fal3$avdir<180]+360

ggplot(fal3)+geom_point(aes(x=date_time,y=avdir))+geom_hline(yintercept = 135)


#---
sc<-data.frame(date_time=fal$date_time[fal$site_name=="Spring Creek 10 (Deep)"],avdir=fal$avdir[fal$site_name=="Spring Creek 10 (Deep)"])

# sc[sc$avdir>115 & sc$avdir<200,'flow']<-1
# sc[sc$avdir>200 & sc$avdir<290,'flow']<-2
# sc[sc$avdir>290 | sc$avdir<30,'flow']<-3
# sc[sc$avdir>30 & sc$avdir<115,'flow']<-4

sc[sc$avdir>115 & sc$avdir<200,'dir']<-"South"
sc[sc$avdir>200 & sc$avdir<290,'dir']<-"West"
sc[sc$avdir>290 | sc$avdir<30,'dir']<-"North"
sc[sc$avdir>30 & sc$avdir<115,'dir']<-"East"
sc$dir <- factor(sc$dir, labels = c('South','West','East','North'), levels = c('South','West','East','North'))

#sc$flow<-as.factor(sc$flow)
sc$dir<-as.factor(sc$dir)
jans.colors <- scale_color_brewer(type = 'qual', palette = 'Pastel2')
livias.colors <- scale_color_manual(values = c('orange','red','green','blue'))
ggplot(sc)+geom_point(aes(x=date_time,y=avdir,color=dir)
                      )+geom_hline(yintercept = 290)+geom_hline(yintercept = 200)+geom_hline(yintercept = 115)+geom_hline(yintercept = 30) + jans.colors + theme_dark()

RColorBrewer::display.brewer.all()
#---

fal4<-merge(fal,sc[,c('date_time','dir')])

ggplot(fal4[fal4$site_name=="Spring Creek 10 (Deep)",])+geom_point(aes(date_time,cond,color=dir),size=0.5,alpha=0.6)
ggplot(fal4, aes(x=date_time, y = aspd, color = dir)) + geom_point(size = 1) + facet_grid(site_name~., scales='free') + livias.colors








ggplot(fal4, aes(x=date_time, y = aspd, color = dir)) + geom_point(size = 1) + facet_grid(site_name~., scales='free') + livias.colors
