library(DBI)
library(RPostgreSQL)
library(ggplot2)

db <- dbConnect("PostgreSQL", db = 'x', host = 'x', user ='x', password = 'x')
falaspd <- dbGetQuery(db , 'select * from rfalmouth WHERE (date_time BETWEEN \'2010-11-30\' AND \'2014-01-23\') AND var = \'aspd\'')
falcond <- dbGetQuery(db , 'select * from rfalmouth WHERE (date_time BETWEEN \'2010-11-30\' AND \'2014-01-23\') AND var = \'cond\'')

db2 <- dbConnect("PostgreSQL", db = 'x', host = 'x', user ='x', password = 'x')
n3p2011 <- dbGetQuery(db2,'select * from n3p2011')
n3p2012 <- dbGetQuery(db2,'select * from n3p2012')
n3p2013 <- dbGetQuery(db2,'select * from n3p2013')

fal$site_name <- as.factor(fal$site_name)
fal$var <- as.factor(fal$var)
fal$dataset <- as.factor(fal$dataset)

sc2011 <- fal[fal$site_name=="Spring Creek 10 (Deep)" & fal$date_time>"2010-12-31" & fal$date_time<"2012-01-01",]
sc2012 <- fal[fal$site_name=="Spring Creek 10 (Deep)" & fal$date_time>"2011-12-31" & fal$date_time<"2013-01-01",]
sc2013 <- fal[fal$site_name=="Spring Creek 10 (Deep)" & fal$date_time>"2012-12-31" & fal$date_time<"2014-01-01",]


# ggplot(fal, aes(x=date_time, y=val)) + facet_grid(site_name~.,scales = 'free')+ geom_point()  


# Rain vs SC10 PLOT
comb <- ggplot()
comb <- comb + geom_point(data=sc2013,aes(x=date_time,y=val,color="Avg. Speed"))
comb <- comb + geom_point(data=n3p2013,aes(x=date_time,y=val/50,color="Rainfall/50"))
comb <- comb + labs(x="Date",y="Value",title="Spring Creek 10 Velocities in 2013",color="Legend")
comb

ggsave("2013sc10.png",width=25,height=10)

