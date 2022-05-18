library(DBI)
library(RPostgreSQL)
library(ggplot2)
library(reshape2)
Sys.setenv(TZ='UTC')
db <- dbConnect("PostgreSQL", db = 'x', host = 'x', user ='x', password = 'x')

excel.sc10 <- dbGetQuery(db , 'select * from excel_falmouth WHERE (date_time BETWEEN \'2010-11-30\' AND \'2014-01-23\') AND site_name = \'Spring Creek 10 (Deep)\'')
excel.rev <- dbGetQuery(db , 'select * from excel_falmouth WHERE (date_time BETWEEN \'2010-11-30\' AND \'2014-01-23\') AND site_name = \'Revell Sink (Deep)\'')
excel.ak <- dbGetQuery(db , 'select * from excel_falmouth WHERE (date_time BETWEEN \'2010-11-30\' AND \'2014-01-23\') AND site_name = \'AK (Deep)\'')

excel.sc10["flow"] <- 1
excel.sc10$flow[excel.sc10$avdir<270 & excel.sc10$avdir>90] <- 0
excel.rev["flow"] <- 1
excel.rev$flow[excel.rev$avdir<270 & excel.rev$avdir>90] <- 0
excel.ak["flow"] <- 1
excel.ak$flow[excel.ak$avdir<270 & excel.ak$avdir>90] <- 0

hist(excel.rev$avdir)

db2 <- dbConnect("PostgreSQL", db = 'x', host = 'x', user ='x', password = 'x')
basins2012 <- dbGetQuery(db2,'select * from basins2012')
basins2012$objectid <- as.factor(basins2012$objectid)
basins <- dcast(basins2012, date_time ~ objectid, value.var="gallons")

rev <- excel.rev[excel.rev$date_time>"2011-12-30" & excel.rev$date_time<"2013-01-01",]

widerev <- merge(rev,basins,all=TRUE)

p <- ggplot(data=widerev,aes(x=date_time))
p <- p + geom_point(aes(y=aspd),color='red')
p <- p + geom_point(aes(y=(widerev$'2')/100000000))
p

p2 <- ggplot(data=widerev,aes(x=(widerev$'300'),y=aspd)) + geom_point()
p2

# Use hourly falmouth, and truncated basins
revhourly<-dbGetQuery(db, "SELECT * FROM falmouth_hourly WHERE date_trunc('day', date_time) BETWEEN '2011-12-31' AND '2013-01-01' AND site_name = 'Revell Sink (Deep)';")
basinshourly<-dbGetQuery(db2,'select * from basinshoulry')
basinshourly$objectid <- as.factor(basinshourly$objectid)
basins <- dcast(basinshourly, date_time ~ objectid, mean, value.var="gallons")
widerev <- merge(revhourly,basins,all=TRUE)
widerev<- widerev[-c(1:24),]
widerev[is.na(widerev)] <- 0

p <- ggplot(data=widerev,aes(x=date_time))
p <- p + geom_point(aes(y=aspd),color='red')
p <- p + geom_point(aes(y=(widerev$'2')/100000000))
p

cor(widerev$aspd,widerev$'2',method="pearson",use="complete.obs")

listcors <- function(col){
  corr = cor(widerev$aspd,widerev[,col],method='pearson')
  return(corr)
  }

cors<-list()
for (i in 1:489){
  cors[i] <- listcors(as.character(i))
}

cors.df <- as.data.frame(do.call(rbind, cors))
colnames(cors.df)[1] <- "cor" 
ggplot(cors.df,aes(x=1:489,y=cor)) + geom_point()
