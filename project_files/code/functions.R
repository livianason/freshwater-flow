library(DBI)
library(RPostgreSQL)
library(ggplot2)
library(reshape2)
library(lubridate)
Sys.setenv(TZ='UTC')
#db <- dbConnect("PostgreSQL", db = 'x', host = 'x', user ='x', password = 'x')
#db2 <- dbConnect("PostgreSQL", db = 'x', host = 'x', user ='x', password = 'x')

short_mean <- function(x){
  mean(x)
  }
short_site <- function(short){
  switch(short,
         ad = 'AD (Deep)',
         AK = 'AK (Deep)'
  )
}
get_data <- function(sitename,start,end){
  db <- dbConnect("PostgreSQL", db = 'x', host = 'x', user ='x', password = 'x')
  db2 <- dbConnect("PostgreSQL", db = 'x', host = 'x', user ='x', password = 'x')
  SQL <- "
    SELECT * 
    FROM falmouth_hourly 
    WHERE date_trunc('day', date_time) 
    BETWEEN ?start AND ?end
    AND site_name = ?site;"
  
  query <- sqlInterpolate(db, SQL, 
                          site = sitename, 
                          start = start, 
                          end = end)

  falhourly<-dbGetQuery(db,query)
  
  SQL2 <- "
    SELECT * 
    FROM basinshoulry 
    WHERE date_trunc('day', date_time) 
    BETWEEN ?start AND ?end;"
  query2 <- sqlInterpolate(db2, SQL2,
                           start = start, 
                           end = end)
  
  basinshourly <- dbGetQuery(db2,query2)
  basinshourly$gallons <- basinshourly$volume/basinshourly$area
  basinshourly$objectid <- as.factor(basinshourly$objectid)
  basins <- dcast(basinshourly, basinshourly$date_time ~ basinshourly$objectid, sum, value.var="gallons")
  widerev <- merge(falhourly,basins,all=TRUE)
  widerev[is.na(widerev)] <- 0
  dbDisconnect(db)
  dbDisconnect(db2)
  return(widerev)
}

get_windows <- function(data,var,thresh){
  windows <- data[var>thresh,]
  dates<-unique(floor_date(windows$date_time,"month"))
  #windows$month
  
}

widerev<-get_data("Spring Creek 10 (Deep)",'2011-01-01','2011-01-02')
#windows<-get_windows(widerev,widerev$aspd,13)
