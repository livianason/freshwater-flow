#falmouth data
fal <- dbGetQuery(db , "select * from rfalmouth WHERE (date_time BETWEEN '2010-11-30' AND '2014-01-23') AND (var = 'aspd' OR var = 'cond') AND (site_name = 'Revell Sink (Deep)' OR site_name = 'Spring Creek 10 (Deep)');")

# plot falmouth
p <- ggplot(fal, aes(x = date_time, y = val, colour = site_name)) + 
  geom_line() + 
  facet_grid(var~., scales = 'free_y')

# tidal data
coops <- dbGetQuery(db , "select * from rcoops WHERE (date_time BETWEEN '2010-11-30' AND '2014-01-23');")

# combine coops tidal and falmouth
all.data <- rbind(fal, coops)

p.coops <- ggplot(all.data, aes(x = date_time, y = val, colour = site_name)) + 
  geom_line() + 
  facet_grid(dataset+var~., scales = 'free_y')


basins.query <- "
SELECT 
'basin_sum'::varchar AS site_name, 
date_time, 
SUM(gallons) as val,
'gallons'::varchar as var,
'Basins'::varchar as dataset FROM y2012.n3p_basins GROUP BY date_time ORDER BY date_time;"


# get the basins n3p
basins.2012 <- dbGetQuery(db2, basins.query)
#combine with falmouth
basins.data <- rbind(fal, basins.2012)

#basic use of facets
p.basins <- ggplot(basins.data, aes(x = date_time, y = val, colour = site_name)) + 
  geom_line(size = 0.25) + 
  facet_grid(dataset+var~., scales = 'free_y')

# can do this, kinda ugly
p.basins <- ggplot(basins.data[basins.data$var != 'cond',], aes(x = date_time, y = val, colour = site_name)) + 
  geom_point() + 
  scale_y_log10()

# window into an time range
p.basins + xlim(c(as.POSIXct("2012-06-01 00:00:00"), as.POSIXct("2013-07-01 00:00:00")))


p.basins + xlim(c(as.POSIXct("2012-07-17 00:00:00"), as.POSIXct("2012-07-25 00:00:00")))

# same as above, using n1p
basins.query <- "
SELECT 
'basin_sum'::varchar AS site_name, 
date_time, 
SUM(gallons) as val,
'gallons'::varchar as var,
'Basins'::varchar as dataset FROM y2012.n1p_basins GROUP BY date_time ORDER BY date_time;"

# get the basins n3p
basins.2012 <- dbGetQuery(db2, basins.query)

#combine with falmouth
basins.data <- rbind(fal, basins.2012)

#
p.basins <- ggplot(basins.data, aes(x = date_time, y = val, colour = site_name)) + 
	geom_line(size = 0.25) + 
	facet_grid(dataset+var~., scales = 'free_y')


fal <- dbGetQuery(db , "select * from excel_falmouth WHERE (date_time BETWEEN '2011-01-01' AND '2011-06-01') AND (site_name = 'Spring Creek 10 (Deep)');")




