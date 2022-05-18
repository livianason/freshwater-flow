library(DBI)
library(RPostgreSQL)
library(ggplot2)
library(reshape2)
library(lubridate)
Sys.setenv(TZ='UTC')
db <- dbConnect("PostgreSQL", db = 'x', host = 'x', user ='x', password = 'x')
db2 <- dbConnect("PostgreSQL", db = 'x', host = 'x', user ='x', password = 'x')

revhourly<-dbGetQuery(db, "SELECT * FROM falmouth_hourly WHERE date_trunc('day', date_time) BETWEEN '2012-01-01' AND '2013-01-01' AND site_name = 'Revell Sink (Deep)';")
basinshourly<-dbGetQuery(db2,"select * from basinshoulry WHERE date_trunc('day', date_time) BETWEEN '2012-01-01' AND '2013-01-01'")
basinshourly$gallons <- basinshourly$volume/basinshourly$area
basinshourly$objectid <- as.factor(basinshourly$objectid)
basins <- dcast(basinshourly, date_time ~ objectid, sum, value.var="gallons")
widerev <- merge(revhourly,basins,all=TRUE) # wasn't here??
#basins[is.na(basins)] <- 0

# offset the rainfall by tau, merge with falmouth, find 489 correlations, find max correlation and id for each tau

listcors <- function(col){
  corr = cor(widerev$aspd,widerev[,col],method='pearson',use="complete.obs")
  return(corr)
}

offset <- function(tau){
  offbasins <- basins
  offbasins$date_time <- offbasins$date_time + hours(tau)
  return(offbasins)
}

maxcor<-list()
maxid<-list()
for (i in 1:24){
  print(i)
  b <- offset(i)
  widerev <- merge(revhourly,b,all=TRUE)
  #widerev[is.na(widerev)] <- 0
  cors<-list()
  for (j in 1:489){
    cors[j] <- listcors(as.character(j))
  }
  maxcor[i] <- max(unlist(cors))
  maxid[i] <- which.max(unlist(cors))
}

maxcorsss <- as.data.frame(cbind(do.call(rbind, maxid),do.call(rbind, maxcor)))

dbWriteTable(db2, 'basin_cors', maxcorsss, overwrite = T)

sql1 <- 'DROP TABLE basin_cors_qgis; CREATE TABLE basin_cors_qgis AS (
SELECT * 
FROM basin_cors
INNER JOIN basins ON ("V1"::integer = objectid));'
dbExecute(db2, sql1)


for (j in 1:489){
  cors[j] <- listcors(as.character(j))
}

cors2 <- data.frame(do.call(rbind, cors))
names(cors2) <- 'cor'
dbWriteTable(db2, 'basin_cors2', cors2, overwrite = T)
sql2 <- 'DROP TABLE basin_cors_qgis2; CREATE TABLE basin_cors_qgis2 AS (
SELECT *
FROM basin_cors2
INNER JOIN basins b ON ("row.names"::integer = objectid));'
dbExecute(db2, sql2)
