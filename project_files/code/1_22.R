library(DBI)
library(RPostgreSQL)
library(ggplot2)

db <- dbConnect("PostgreSQL", db = 'x', host = 'x', user ='x', password = 'x')

falaspd <- dbGetQuery(db , 'select * from rfalmouth WHERE (date_time BETWEEN \'2010-11-30\' AND \'2014-01-23\') AND var = \'aspd\'')
falcond <- dbGetQuery(db , 'select * from rfalmouth WHERE (date_time BETWEEN \'2010-11-30\' AND \'2014-01-23\') AND var = \'cond\'')

fal2012aspd <- falaspd[falaspd$site_name=="Spring Creek 10 (Deep)" & falaspd$date_time>"2011-12-31" & falaspd$date_time<"2013-01-01",]
fal2012cond <- falcond[falcond$site_name=="Spring Creek 10 (Deep)" & falcond$date_time>"2011-12-31" & falcond$date_time<"2013-01-01",]


#Seth Query
excel.sc10 <- dbGetQuery(db , 'select * from excel_falmouth WHERE (date_time BETWEEN \'2010-11-30\' AND \'2014-01-23\') AND site_name = \'Spring Creek 10 (Deep)\'')
excel.rev <- dbGetQuery(db , 'select * from excel_falmouth WHERE (date_time BETWEEN \'2010-11-30\' AND \'2014-01-23\') AND site_name = \'Revell Sink (Deep)\'')
excel.ak <- dbGetQuery(db , 'select * from excel_falmouth WHERE (date_time BETWEEN \'2010-11-30\' AND \'2014-01-23\') AND site_name = \'AK (Deep)\'')

excel.sc10["flow"] <- 1
excel.sc10$flow[excel.sc10$avdir<270 & excel.sc10$avdir>90] <- 0
excel.rev["flow"] <- 1
excel.rev$flow[excel.rev$avdir<270 & excel.rev$avdir>90] <- 0
excel.ak["flow"] <- 1
excel.ak$flow[excel.ak$avdir<270 & excel.ak$avdir>90] <- 0

p <- ggplot()
p <- p + geom_point(data=fal2012aspd,aes(x=date_time,y=val,color="avg. speed"))
p <- p + geom_point(data=fal2012cond,aes(x=date_time,y=val,color="conductivity"))
p <- p + labs(x="Date",y="Value",title="Spring Creek 10 Velocity vs. Conductivity",color="Legend")
p 

hist(fal2012cond$val)
hist(fal2012aspd$val[fal2012aspd$val<80],breaks=30)

# flow patterns
p2 <- ggplot()
p2 <- p2 + geom_point(data=excel.sc10[excel.sc10$date_time>"2011-12-30" & excel.sc10$date_time<"2012-12-30",],aes(x=date_time,y=flow,color="sc10")) 
p2 <- p2 + geom_point(data=excel.rev[excel.rev$date_time>"2011-12-30" & excel.rev$date_time<"2012-12-30",],aes(x=date_time,y=flow,color="rev"))
p2 <- p2 + geom_point(data=excel.ak[excel.ak$date_time>"2011-12-30" & excel.ak$date_time<"2012-12-30",],aes(x=date_time,y=flow,color="ak"))
p2 <- p2 + geom_point(data=n3p2012,aes(x=date_time,y=val/10000,color="rainfall"))
p2

# aspd cond mess
p3 <- ggplot()
p3 <- p3 + geom_point(data=excel.sc10[excel.sc10$date_time>"2011-12-30" & excel.sc10$date_time<"2013-3-30",],aes(x=date_time,y=aspd+400,color=flow)) 
p3 <- p3 + geom_point(data=excel.rev[excel.rev$date_time>"2011-12-30" & excel.rev$date_time<"2013-3-30",],aes(x=date_time,y=aspd+250,color=flow))
p3 <- p3 + geom_point(data=excel.ak[excel.ak$date_time>"2011-12-30" & excel.ak$date_time<"2013-3-30",],aes(x=date_time,y=aspd*5+100,color=flow))
p3 <- p3 + geom_point(data=n3p2012,aes(x=date_time,y=val/50),color='red')
p3 <- p3 + geom_point(data=n3p2013[n3p2013$date_time<"2013-3-30",],aes(x=date_time,y=val/50),color='red')
p3 <- p3 + geom_point(data=excel.rev[excel.rev$date_time>"2011-12-30" & excel.rev$date_time<"2013-3-30",],aes(x=date_time,y=cond*20+220),color='green')
p3 <- p3 + geom_point(data=excel.sc10[excel.sc10$date_time>"2011-12-30" & excel.sc10$date_time<"2013-3-30",],aes(x=date_time,y=cond*5+350),color="purple") 
p3


p3 <- p3 + geom_point(data=n3p2012,aes(x=date_time,y=val/5),color='orange')

