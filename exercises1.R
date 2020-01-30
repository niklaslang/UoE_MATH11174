### rivers ###

# minimum/maximum river length and their index in the vector
min(rivers)
which(rivers == min(rivers))

max(rivers)
which(rivers == max(rivers))

# histogram of all integers between 1 and the maximum river length
# that do not appear in the rivers vector

hist(setdiff( seq(0,max(rivers)), rivers))

# mean and standard deviation of the rivers vector

mean <- mean(rivers)
sd <- sd(rivers)

# create a new vector called randomrivers (of the same length as rivers) of normally distributed points 
# according to those parameters

set.seed(1)
randomrivers <- rnorm(n = length(rivers), mean = mean, sd = sd)

# count how many elements in randomrivers are negative (answer: 12)

length(which(randomrivers < 0))

# how many values in randomrivers are more than double than their 
# corresponding element in rivers (answer: 39)

length( which(randomrivers > 2*rivers))

# create a scatter plot of rivers and randomrivers and 
# report the Pearson’s correlation coefficient between the two vectors 
# at 3 significant digits (answer: 0.0892)

plot( rivers, randomrivers)
signif( cor( rivers, randomrivers, method = "pearson"), 3)

### islands ###

# report names and sizes of islands with area in the first quartile

islands[which(islands < quantile(islands, c(.25, .75))[1])]
small.islands <- islands[which(islands < quantile(islands, c(.25, .75))[1])]

# compute the median area in this subset

median(small.islands)

# report sizes of the 10th largest and of the 10th smallest island (answer: 280, 16)

islands[order(islands)[length(islands)-9]]
islands[order(islands)[10]]

# count how many islands have an odd area (answer: 21)

length(which(islands %% 2 == 1))

# create a vector called islands3 which includes only islands with area divisible by 3
# and report its median (answer: 36)
# and interquartile range (answer: 25.5, 244.5)

islands3 <- islands[which(islands %% 3 == 0)]
median(islands3)
quantile(islands3, c(0.25, 0.75))

# report the smallest area in islands3 that is also present in the rivers vector 
# (answer: 306)

min(intersect(islands3, rivers))

# for the areas in islands3 that are not present in rivers report the mean 
# rounded to the first decimal place (answer: 1283.5)

round(mean(setdiff(islands3, rivers)), 1)

round(mean(islands3[!islands3 %in% rivers]), 1)

# note the different results, this is due to the fact that setdiff() 
# implicitly discards any duplicated values, as it treats the vectors as sets


### attitudes ###

# for the “rating” variable, report median, range and interquartile range

median(attitude$rating)
range(attitude$rating)
quantile(attitude$rating, c(.25, .75))

# report the median rating for observations that have above median values for the “raises” variable
# (answer: 71)

median(attitude$rating[attitude$raises > median(attitude$raises)])

# compute the standard deviation for the “advance” variable 
# and compare it to the one computed after removing the extreme values 
# (answer: 10.288706, 8.3864182)

sd(attitude[attitude$advance <= mean(attitude$advance) + 2*sd(attitude$advance) & attitude$advance >= mean(attitude$advance) - 2*sd(attitude$advance),]$advance)

with(attitude, sd(advance))
with(attitude, sd(advance[advance < max(advance) & advance > min(advance)]))

# For each variable in the dataframe, produce histogram and box-plot (using function boxplot()) side by side
# you will need to first specify par(mfrow=c(1,2)) to tell R 
# that you want your image to contain one row and two columns
# Assign correct axis labels as well as plot titles

bins <- seq(20,100,10)

for (j in 1:ncol(attitude)){
  par(mfrow=c(1,2),oma = c(0, 0, 3, 0))
  hist(attitude[,j], breaks=bins, main = "histogram", ylab = "frequency", xlab = paste(colnames(attitude)[j]))
  boxplot(attitude[,j], main = "boxplot", ylab = "percentage of favourable responses", xlab = paste(colnames(attitude)[j]))
  mtext(paste("Distribution of response to:",colnames(attitude)[j] ), outer = TRUE, cex = 1)
  }

### quakes ###

# do a scatter plot of longitude and latitude (set cex=0.5 to decrease the point size), 
# then by using abline() add lines corresponding to median longitude and latitude. 
# Using a different colour, also add lines corresponding to mean longitude and latitude.

par(mfrow=c(1,1),oma = c(0, 0, 0, 0))

plot(quakes$long, quakes$lat, cex = .5, col='blue',
     main = "Location of earthquakes",
     xlab = "longitude",
     ylab = "latitude")

abline(v=median(quakes$long), col='orange')
abline(h=median(quakes$lat), col='orange')
abline(v=mean(quakes$long), col='red')
abline(h=mean(quakes$lat), col='red')

# reate a dataframe called quakes.1sd which contains only points 
# with longitude and latitude that are within one standard deviation from the mean 
# or have earthquake magnitude at least 5.5
# Add these observation to the previous plot using function points(), using yet another colour.

quakes.1sd <- data.frame( subset( quakes, 
                                  (long >= mean(long) - sd(long) & 
                                     long <= mean(long) + sd(long) &
                                     lat >= mean(lat) - sd(lat) &
                                     lat <= mean(lat) + sd(lat)
                                   ) | ( mag >= 5.5 )
                                  ))

points(quakes.1sd$long, quakes.1sd$lat, col='green')

# add a variable to quakes.1sd called “damage” (equation see lab practical pdf)

quakes.1sd$damage <- sqrt(max(quakes.1sd$depth) - quakes.1sd$depth) + 5*quakes.1sd$mag + quakes.1sd$stations^(1/4)

# count the number of observations in quakes.1sd (answer: 585) and the range of variable “damage” rounded to 2 decimal places 
# (answer: 23.17, 58.84)

nrow(quakes.1sd)
round(range(quakes.1sd$damage), 2)

# report the correlation between “damage” and all other variables in quakes.1sd.

for (j in 1:ncol(quakes.1sd)){
  output <- cor(quakes.1sd[,6], quakes.1sd[,j])
  print(paste("Correlation between damages and", colnames(quakes.1sd)[j], "is:", output))
}

# create a dataframe called quakes.40s which contains only points reported by more than 40 stations
# and count how many have a row name of length 3 (answer: 243)
# how many contain the character 7 (answer: 77)
# and how many contain the character 9 but not in the first position (answer: 44)

quakes.40s <- data.frame(subset(quakes, stations > 40))

sum(nchar(rownames(quakes.40s)) == 3)

length(grep(7,rownames(quakes.40s)))

length(grep(9, rownames(quakes.40s))) - length(grep( "^9", rownames(quakes.40s)))

### nottem ###

library(data.table) #standard library for working with data tables
library(lubridate) #makes working with dates a bit easier...

temps.dt <- data.table( temp = c(nottem), year = (c(time(nottem))))[,
                                                                     .(month=format(date_decimal(year+.01), "%b"),
                                                                     Year=format(date_decimal(year+.01), "%Y"), temp)]

# step1: takes nottem time series and variables temp and time and adds them as columns to data table
# time(): creates the vector of decimal times at which a time series was sampled: 1920.083 for Jan 1920
# cycle(): gives the positions in the cycle of each observation: 1920/1 for Jan 1920

# step2: the result is then chained to split the time data into month and year columns
# date_decimal(): converts a decimal to a date
# format() formats an R object for pretty printing

temps.dt <- reshape(temps.dt, idvar = "Year", timevar = "month", direction = "wide")

# step3: convert the data from long into wide format
# reshape( idvar, timevar, direction="wide'): reshapes a data frame into "wide" format with repeated
# measurements in seperate colums
# timevar specifies variable that differentiates multiple records from the same group
# idvar specifies variable that identifies multiple records from the same group

colnames(temps.dt) <- gsub("temp.", "", colnames(temps.dt))

# step4: rename column names
# gsub( string1, string2, vector): replace string1 with string2 in every element of vector

# alternativ approach - very cheap, simple and labour intensive...

temps.dt <- data.table(year = 1920:1939, 
                       jan = nottem[seq(1,229,12)],
                       feb = nottem[seq(2,230,12)],
                       mar = nottem[seq(3,231,12)],
                       apr = nottem[seq(4,232,12)],
                       may = nottem[seq(5,233,12)],
                       jun = nottem[seq(6,234,12)],
                       jul = nottem[seq(7,235,12)],
                       aug = nottem[seq(8,236,12)],
                       sep = nottem[seq(9,237,12)],
                       oct = nottem[seq(10,238,12)],
                       nov = nottem[seq(11,239,12)],
                       dec = nottem[seq(12,240,12)])

# sing the assign by reference := assignment operator, 
# add a column called summer.avg and 
# add the mean temperature of the summer months June to September for each year
# answer: sum of the summer average sum(nottem.dt$summer.avg) equals 1184.7

#temps.dt <- data.table(temps.dt)[ , summer.avg := round(sum(Jun, Jul, Aug, Sep)/4,1), by=year]
#sum(temps.dt$summer.avg)

# or easier (?)

temps.dt[ , summer.avg := rowMeans(.SD), by = .I, .SDcols=colnames(temps.dt[,Jun:Sep])]
sum(temps.dt$summer.avg)

# Using the assign by reference := assignment operator, 
# add a column called june.decade.avg which provides
# the average temperature in june from 1920 to 1929 and 1930 to 1939 
# answer: 56.99 for 20's and 59.09 for 30's

#temps.dt[ , decade1 := year %in% 1920:1929][ , june.decade.avg := mean(Jun), by = decade1]

# or easier (?) but definitely more elegant!

temps.dt[, june.decade.avg := mean(Jun), by=substr(Year, 0, 3)]

# substr(year, 0, 3): selects first three characters from the left of year

temps.dt[as.numeric(Year)%%10 == 0, .(Year, june.decade.avg)] 
