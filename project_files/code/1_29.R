
source('./functions.R')

db <- dbConnect("PostgreSQL", db = 'x', host = 'x', user ='x', password = 'x')
db2 <- dbConnect("PostgreSQL", db = 'x', host = 'x', user ='x', password = 'x')

revhourly<-dbGetQuery(db, "SELECT * FROM falmouth_hourly WHERE date_trunc('day', date_time) BETWEEN '2012-05-30' AND '2012-08-31' AND site_name = 'Spring Creek 10 (Deep)';")
basinshourly<-dbGetQuery(db2,"select * from basinshoulry WHERE date_trunc('day', date_time) BETWEEN '2012-05-30' AND '2012-08-31'")
basinshourly$gallons <- basinshourly$volume/basinshourly$area
basinshourly$objectid <- as.factor(basinshourly$objectid)
basins <- dcast(basinshourly, date_time ~ objectid, sum, value.var="gallons")
widerev <- merge(revhourly,basins,all=TRUE) # wasn't here??
widerev[is.na(widerev)] <- 0

offset <- function(tau){
  offbasins <- basins
  offbasins$date_time <- offbasins$date_time + hours(tau)
  return(offbasins)
}

listcors <- function(data,col){
  corr = cor(data$aspd,data[,col],method='pearson',use="complete.obs")
  return(corr)
}

write_to_SQL <- function(cors){
  cors2 <- data.frame(do.call(rbind, cors))
  names(cors2) <- 'cor'
  dbWriteTable(db2, 'basin_cors2', cors2, overwrite = T)
  sql2 <- 'DROP TABLE basin_cors_qgis2; CREATE TABLE basin_cors_qgis2 AS (
  SELECT *
  FROM basin_cors2
  INNER JOIN basins b ON ("row.names"::integer = objectid));'
  dbExecute(db2, sql2)
}


#LAG

c<-ccf(widerev[,'330'],widerev$aspd,lag.max=200,type="correlation")
# max(c$acf)
# 200-which.max(c$acf)
# 
# minus75 <- offset(200-which.max(c$acf))
# w <- merge(revhourly,minus75,all=TRUE)
# w[is.na(w)] <- 0
# cors<-list()
# for (j in 1:489){
#   cors[j] <- listcors(w,as.character(j))
# }
# write_to_SQL(cors)

# Calculate all lags and use average

lags <- list()
for (k in 1:489){
  c<-ccf(widerev[,as.character(k)],widerev$aspd,lag.max=200,type="correlation",plot=F)
  lags[k]<-200-which.max(c$acf)
}
lags <- as.data.frame(do.call(rbind,lags))
summary(lags$V1)

offset_to_SQL <- function(hours){
  minus75 <- offset(hours)
  w <- merge(revhourly,minus75,all=TRUE)
  w[is.na(w)] <- 0
  cors<-list()
  for (j in 1:489){
    cors[j] <- listcors(w,as.character(j))
  }
  write_to_SQL(cors)
}
offset_to_SQL(60)
