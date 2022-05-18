library(DBI)
library(RPostgreSQL)
library(ggplot2)
library(reshape2)
library(lubridate)
Sys.setenv(TZ='UTC')
library(dygraphs)
library(xts)

db2 <- dbConnect("PostgreSQL", db = 'x', host = 'x', user ='x', password = 'x')
SQL2 <- "SELECT * FROM public.bh11huc2;"
rain11<-dbGetQuery(db2,SQL2)
SQL2 <- "SELECT * FROM public.bh12huc2;"
rain12<-dbGetQuery(db2,SQL2)
SQL2 <- "SELECT * FROM public.bh13huc2;"
rain13<-dbGetQuery(db2,SQL2)
dbDisconnect(db2)
rain<- rbind(rain11,rain12,rain13)
rain$gallons<-rain$volume/rain$area
rain$objectid<-as.factor(rain$objectid)

remove(rain11)
remove(rain12)
remove(rain13)

db2 <- dbConnect("PostgreSQL", db = 'x', host = 'x', user ='x', password = 'x')
SQL3<-"SELECT gid,huc,exthuc,basin FROM public.basins;"
basins<-dbGetQuery(db2,SQL3)
dbDisconnect(db2)

db <- dbConnect("PostgreSQL", db = 'x', host = 'x', user ='x', password = 'x')
SQL <- "
    SELECT * 
    FROM falmouth_hourly
    WHERE date_trunc('day', date_time) 
    BETWEEN '2011-01-01' AND '2013-12-31'
    AND site_name IN ('Spring Creek 10 (Deep)','AK (Deep)','Revell Sink (Deep)','D (Deep)','AD (Deep)','B (Deep)','K (Deep)');"
fal<-dbGetQuery(db,SQL)
dbDisconnect(db)
fal$site_name<-as.factor(fal$site_name)

normalize<- function(data){
  r <- (data-min(data))/(max(data)-min(data))
  return(r)
}

#saveRDS(basins,file="basins.rds")
