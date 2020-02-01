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

air.dt <- sapply( air.dt, function(z)(ifelse(is.na(z), round(mean(z, na.rm=TRUE),1), round(z,1))))

# only for columns with imputed values plot side by side 
# histograms of raw (unimputed) and imputed columns.

par(mfrow=c(1,2))
hist(airquality$Ozone, main="Before Imputation", xlab = "Ozone (ppb)")
hist(air.dt[, "Ozone"], main="After Imputation", , xlab = "Ozone (ppb)")

par(mfrow=c(1,2))
hist(airquality$Solar.R, main="Before Imputation", xlab = "Solar radiation (lang)")
hist(air.dt[,"Solar.R"], main="After Imputation", xlab = "Solar radiation (lang)")
