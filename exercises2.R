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

### diab01.txt ###

library(data.table)
library(lubridate)

diab01.dt <- fread("data/diab01.txt", stringsAsFactors = TRUE)

# fit a linear regression model for Y adjusted for age, sex and total cholesterol (TC)

regr.TC <- lm(Y ~ AGE + SEX + TC , data=diab01.dt)

# create a vector called results.table which stores 4 numbers: 
#regression coefficient and p-value for total cholesterol, R^2 and adjusted R^2 of the model

summ.regr <- summary(regr.TC)
results.table <- c(summ.regr$coefficients[4, 1], 
                   summ.regr$coefficients[4, 4],
                   summ.regr$r.squared, 
                   summ.regr$adj.r.squared)
results.table <- round(results.table, 4)
results.table

# build all other possible linear regression models for Y using age, sex and 
# one other predictor at a time
# for each predictor, append a row of results to results.table in the same format as before


for(predictor in c("BMI", "BP", "LDL", "HDL", "GLU")){
  regr.predictor <- lm(paste0("Y ~ AGE + SEX + ", predictor), data=diab01.dt)
  summ.regr.predictor <- summary(regr.predictor)
  results.table <- rbind(results.table, c(summ.regr.predictor$coefficients[4, 1], 
                                          summ.regr.predictor$coefficients[4, 4],
                                          summ.regr.predictor$r.squared, 
                                          summ.regr.predictor$adj.r.squared))
  
  results.table <- round(results.table, 4)
}

# at the end, add row names to results.table to correspond to the predictors used.

colnames(results.table) <- c('coefficients','p.value','r.squared','adj.r.squared')
rownames(results.table) <- c('TC', 'BMI', 'BP', 'LDL', 'HDL', 'GLU')
results.table

# identify the predictor that produces the best performing model (answer: BMI)

# from all significant predictor choose the one with the highest R-squared value
# in general, the higher the R-squared, the better the model fits your data

results.table[, "p-value" < 0.05]

results <- data.frame(results.table)
best.results <- results[results$p.value < 0.05 & results$r.squared == max(signf.results$r.squared),]
best.results

# starting from the set of covariates used in the model with best performance determined above,
# write a loop to fit all possible linear regression models 
# that use one additional predictor
# produce a table of R2 and adjusted R2 of all models you fitted in the loop
# report the adjusted R2 of the model of best fit (answer: 0.252)

results.BMI.models <- vector(mode="numeric", length=0)

for(predictor in c("TC", "BP", "LDL", "HDL", "GLU")){
  regr.predictor <- lm(paste0("Y ~ AGE + SEX + BMI + ", predictor), data=diab01.dt)
  summ.regr.predictor <- summary(regr.predictor)
  results.BMI.models <- rbind(results.BMI.models,
                              c(summ.regr.predictor$r.squared, 
                              summ.regr.predictor$adj.r.squared))
  results.BMI.models <- round(results.BMI.models, 4)
}

colnames(results.BMI.models) <- c('r.squared','adj.r.squared')
rownames(results.BMI.models) <- c("TC", "BP", "LDL", "HDL", "GLU")
results.BMI.models

results.BMI.models[which(results.BMI.models[,2] == max(results.BMI.models[,2])),2]

### birthweight data ###

# explore the dataset and prepare it for analysis

birthweight.dt <- fread("data/birthweight.txt", stringsAsFactors = TRUE)

head(birthweight.dt)
sapply(birthweight.dt, function(z)round((sum(is.na(z))/length(z)*100),1))

summary(birthweight.dt)

num.cols <- which(sapply(birthweight.dt, function(z) (class(z) %in% c("numeric", "integer")))) 

for (col in num.cols) {
  hist(birthweight.dt[[col]], main=colnames(birthweight.dt[, ..col]))
}

birthweight.dt[, age.mother := ifelse(age.mother == 99, NA, age.mother)]
birthweight.dt[, gestation := ifelse(gestation == 999, NA, gestation)]
birthweight.dt[, height.mother := ifelse(height.mother == 999, NA, height.mother)]
birthweight.dt[, prepregnancy.weight := ifelse(prepregnancy.weight == 999, NA, prepregnancy.weight)]
birthweight.dt[, smoker := ifelse(smoker == 999, NA, smoker)]

sapply(birthweight.dt, function(z)round((sum(is.na(z))/length(z)*100),1))
summary(birthweight.dt)

num.cols <- which(sapply(birthweight.dt, function(z) (class(z) %in% c("numeric", "integer")))) 

for (col in num.cols) {
  hist(birthweight.dt[[col]], main=colnames(birthweight.dt[, ..col]))
}

# summarize the distribution of birth weight for babies born to women 
# who smoked during pregnancy and for babies born to women who didn’t
# report the percentage of babies born weighing under
# 2.5kg in the two strata of smoking status (answer: 8.26%, 3.37%)

par(mfrow=c(1,2))
hist(birthweight.dt$birth.weight[which(birthweight.dt$smoker == 1)],
     main = "Smoking Women", xlab = "Birthweight of Babies")
hist(birthweight.dt$birth.weight[which(birthweight.dt$smoker == 0)],
     main = "Non-smoking Women", xlab = "Birthweight of Babies")

length(birthweight.dt$birth.weight[which(birthweight.dt$smoker == 1 & birthweight.dt$birth.weight < 2.5)])/
  length(birthweight.dt$birth.weight[which(birthweight.dt$smoker == 1)])*100

length(birthweight.dt$birth.weight[which(birthweight.dt$smoker == 0 & birthweight.dt$birth.weight < 2.5)])/
  length(birthweight.dt$birth.weight[which(birthweight.dt$smoker == 0)])*100

# fit a linear regression model to establish if there is an association 
# between birth weight and smoking and how much birth weight changes according to smoking

lm.smoking <- lm(birth.weight ~ smoker, data = birthweight.dt)
summary(lm.smoking)
coef(summary(lm.smoking))

# this is not liner regression this is logistic regression
# birthweight decreases by 25% of the mother smoked during pregnancy

# By how much is the average length of gestation different for first born children? 

gestation.firstborn <- birthweight.dt$gestation[which(birthweight.dt$first.born == 1)]
gestation.nonfirstborn <- birthweight.dt$gestation[which(birthweight.dt$first.born == 0)]

mean.gestation.diff <- mean(gestation.firstborn, na.rm = TRUE) - mean(gestation.nonfirstborn, na.rm = TRUE)
mean.gestation.diff

# Is the difference statistically significant? 

gestation.t.test <- t.test(gestation.firstborn, gestation.nonfirstborn)
gestation.t.test$p.value

#yes it is!

# Is the mother’s pre-pregnancy weight associated with length of gestation?

lm.gestation <- lm(prepregnancy.weight ~ gestation , data = birthweight.dt)
summary(lm.gestation)

# yes there is an association, but it is not significant!

# Is birth weight associated with the mother’s pre-pregnancy weight? 
# Is the association independent of height of the mother? 

lm.prepregnancyweight <- lm(birth.weight ~ prepregnancy.weight, data = birthweight.dt)
summary(lm.prepregnancyweight)

# yes there is an association and it is significant!
# BUT the effect size is minimal: around 8g/kg prepregnancyweight

lm.prepregnancyweight.height <- lm(birth.weight ~ prepregnancy.weight + height.mother, data = birthweight.dt)
summary(lm.prepregnancyweight.height)

# there is an significant association between prepregnancy.weight and birth.weight
# even after adjusting for the height of the mother 
# BUT the effect size has decreased eve further: around 4g/kg prepregnancyweight