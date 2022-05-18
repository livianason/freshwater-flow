library(ecp)
db <- dbConnect("PostgreSQL", db = 'x', host = 'x', user ='x', password = 'x')

test.fal <-dbGetQuery(db, "SELECT * FROM falmouth_hourly WHERE date_trunc('day', date_time) BETWEEN '2012-10-01' AND '2012-11-15' AND site_name = 'Spring Creek 10 (Deep)';")
ggplot(test.fal, aes(x=date_time, y = cond)) + geom_line()
nrow(test.fal)
head(test.fal)
test.mat <- as.matrix(test.fal[,c(6)])
head(test.mat)

?e.divisive
test.cluster <- e.divisive(test.mat, min.size = 6, sig.lvl = 0.25)
test.df <- data.frame(date_time = test.fal$date_time, cond = test.fal$cond, cluster = test.cluster$cluster)
test.df$cluster <- factor(test.df$cluster)
ggplot(test.df, aes(x=date_time, y= cond, color = cluster)) + geom_point() + geom_line(size = 0.25)

library(rpart)
library(rpart.plot)


