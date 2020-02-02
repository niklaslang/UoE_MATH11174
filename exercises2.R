### state.x77 and USAarrests again ###

# convert to data table format - use the command option to include the rowname as a column

library(data.table)

arrests.dt <- data.table(USArrests, keep.rownames = "State")
states.dt <- data.table(state.x77, keep.rownames = "State")

# find union and intersection of the column names of the two data tables

union(colnames(arrests.dt), colnames(states.dt)) # union
intersect(colnames(arrests.dt), colnames(states.dt))# intersection

# merge the two data tables into a new one called USdata.dt
# count the number of rows and columns of the resulting data table (answer: 50, 15) 
# and the number of missing values in the data table (answer: 0)

USdata.dt <- merge(arrests.dt, states.dt, by.x = "State", by.y = "State", ALL=TRUE)

dim(USdata.dt)
sum(is.na(USdata.dt))

# check that the table is ordered by State, 
# then use the setorder() function to reorder by other columns
# Then return to order by State

all(USdata.dt$State == sort(USdata.dt$State))

USdata.dt <- setorder(USdata.dt, -Population)

USdata.dt <- setorder(USdata.dt, State)

# do a scatter plot of the two “Murder” variables and 
# compute their correlation up to 3 significant digits 
# (answer: 0.934)

with(USdata.dt, plot(Murder.x, Murder.y, 
                     main = "Murder (per 100,000)", 
                     xlab = "dataset1 - USArrests", ylab = "dataset2 - state.x77"))

with(USdata.dt, signif(cor(Murder.x, Murder.y),3))

# add two new variables to USdata.dt: 
# MeanMurder and MaxMurder to store the state-wise average
# and maximum of the two “Murder” variables (hint: look at ?max)
# then remove the two “Murder” variables from the data table

USdata.dt[, MeanMurder := rowMeans(.SD), by=.I, .SDcols=c("Murder.x","Murder.y")]
USdata.dt[, MaxMurder := max(c(Murder.x, Murder.y)), by=State]
USdata.dt[, Murder.x := NULL]
USdata.dt[, Murder.y := NULL]

### airquality ###

# count the number of observed values per column and 
# report the percentage of missing values per column

sapply(airquality, function(z)sum(!is.na(z)))
sapply(airquality, function(z)round((sum(is.na(z))/length(z)*100),1))

# make a copy of the dataframe converting to data table

air.dt <- copy(data.table(airquality))

# impute the missing values to the mean

# generic version - impute all missing values to the mean

air.dt.imp.mean <- sapply( air.dt, function(z)(ifelse(is.na(z), round(mean(z, na.rm=TRUE),1), round(z,1))))

# add new column with imputed values

air.dt[, Ozone.imp.mean := ifelse(is.na(Ozone), round(mean(Ozone, na.rm=TRUE),2), Ozone)]
air.dt[, Solar.R.imp.mean := ifelse(is.na(Solar.R), round(mean(Solar.R, na.rm=TRUE),2), Solar.R)]

# only for columns with imputed values plot side by side 
# histograms of raw (unimputed) and imputed columns.

par(mfrow=c(1,2))
hist(airquality$Ozone, main="Before Imputation", xlab = "Ozone (ppb)")
hist(air.dt.imp.mean[, "Ozone"], main="After Imputation", , xlab = "Ozone (ppb)")

par(mfrow=c(1,2))
hist(airquality$Solar.R, main="Before Imputation", xlab = "Solar radiation (lang)")
hist(air.dt.imp.mean[,"Solar.R"], main="After Imputation", xlab = "Solar radiation (lang)")


par(mfrow=c(2,2))
with(air.dt, hist(Ozone, main="Before Imputation", xlab = "Ozone (ppb)"))
with(air.dt, hist(Ozone.imp.mean, main="After Imputation", , xlab = "Ozone (ppb)"))

with(air.dt, hist(Solar.R, main="Before Imputation", xlab = "Solar radiation (lang)"))
with(air.dt, hist(Solar.R.imp.mean, main="After Imputation", xlab = "Solar radiation (lang)"))


# write function impute.to.monthly.mean(x, month) 
# (where month is a vector of the same length of x) 
# that imputes missing values according to the mean value for each month,
# and repeat the imputation using this function

impute.to.monthly.mean <- function(x, month){
  
  x.imp <- x
  
  for (m in unique(month)){
    month.idx <- which(month == m)
    month.mean <- mean(x[month.idx], na.rm = TRUE)
    na.idx <- is.na(x.imp)
    x.imp[na.idx & month == m] <- month.mean
  }
  
  return(x.imp)

}

air.dt$Ozone.imp.month <- round(with(air.dt, impute.to.monthly.mean(Ozone, Month)),2)
air.dt$Solar.R.imp.month <- round(with(air.dt, impute.to.monthly.mean(Solar.R, Month)),2)

# report maximum absolute difference (maxi |xi −yi|)
# and mean absolute difference (􏰀ni=1 |xi −yi|/n)
# between imputation to the mean and imputation to the monthly mean 
# (answer: Ozone: 18.51, 3.55, Solar.R: 14.07, 0.4)

# maximum absolute difference

with(air.dt, max(Ozone.imp.mean - Ozone.imp.month))
with(air.dt, max(Solar.R.imp.mean - Solar.R.imp.month))

# mean absolute difference

with(air.dt, round(mean(abs(Ozone.imp.mean - Ozone.imp.month)),2))
with(air.dt, round(mean(abs(Solar.R.imp.mean - Solar.R.imp.month)),2))

# for Ozone only, compare graphically the distributions 
# of the unimputed data, the data imputed to the mean,
# and the data imputed to the monthly mean and justify the differences you see

par(mfrow=c(3,1))
with(air.dt, hist(Ozone, main="raw", xlab="Ozone (ppb)"))
with(air.dt, hist(Ozone.imp.mean, main="imputed to overall mean", xlab="Ozone (ppb)"))
with(air.dt, hist(Ozone.imp.month, main="imputed to monthly mean", xlab="Ozone (ppb)"))

# explaination:

# imputed to overall mean vs. raw: all missing values are replaced
# by the overall mean of the variable, hence only the one bar whose
# range includes the overall mean rises
# here bar of range 40 - 60 rose from < 20 to > 50

# imputed to monthly mean vs. raw: all missing values are replaced 
# by the mean of the respective month, hence not only bar rises, but many
# hence we observe that 2 bars rose sue to imputation!

