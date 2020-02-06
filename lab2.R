### data preparation ###

library(data.table)
library(lubridate)

diab01.dt <- fread("data/diab01.txt", stringsAsFactors = TRUE)

# see how different variables are coded
str(diab01.dt)

# summary statistics of all variables
summary(diab01.dt)

# plot each variable against each other
plot(diab01.dt, cex=0.2)

# plot the distributions of the variables
par(mfrow=c(3,2))
hist(diab01.dt$AGE, main="Age")
hist(diab01.dt$BMI, main="BMI")
hist(diab01.dt$BP, main="Blood pressure")
hist(diab01.dt$TC, main="Total cholesterol") > hist(diab01.dt$LDL, main="LDL")
hist(diab01.dt$HDL, main="HDL")

# boxplot to assess outliers
par(mfrow=c(1,2), mar=c(2,3,2,1)) # mar changes margins: bottom, left, top, right 
boxplot(diab01.dt$HDL, main="HDL")
boxplot(diab01.dt$LDL, main="LDL")

# comparing the summary statistics of two subsets of data (stratification)
boxplot(HDL ~ SEX, data=diab01.dt, main="HDL stratified by sex")

# scatterplot of LDL
plot(diab01.dt$LDL, main="Scatter plot of LDL", ylab="LDL")

### missing values ###

head(diab01.dt$TC, n=30) #be aware of the NA values!
table(is.na(diab01.dt$TC)) # and count them!

# computing the mean is not possible because there are missing values
mean(diab01.dt$TC)

# so we tell R to ignore missing values
mean(diab01.dt$TC, na.rm=TRUE)

# but it's not always that easy
diab01.dt.complete <- diab01.dt[ !is.na(BMI) & !is.na(BP) & !is.na(TC) &
                                !is.na(LDL) & !is.na(HDL) & !is.na(GLU)]

table(is.na(diab01.dt.complete))

dim(diab01.dt.complete) # we lost almost a fifth of our values

# another option is imputation

# Take a deep copy of the original data in order to leave the original intact. 
diab01.dt.imputed <- copy(diab01.dt)
diab01.dt.imputed[, BMI := ifelse(is.na(BMI), median(BMI, na.rm=T), BMI)]

### merging datasets ###

diab02.dt <- fread("data/diab02.txt", stringsAsFactors = TRUE)
str(diab01.dt)
str(diab02.dt)

# notabene: column of identifiers is called differently in the new dataset!
length(intersect(diab01.dt$PAT, diab02.dt$ID))

# new patient IDs not in diab01.dt
diab02.dt[!ID %in% diab01.dt$PAT]$ID

# and vice versa: old patient IDs not in diab02.dt
diab01.dt[!PAT %in% diab02.dt$ID]$PAT

# let's merge the two data tables
diab.dt <- merge(diab01.dt, diab02.dt, by.x="PAT", by.y="ID")
dim(diab.dt) # this corresponds to the intersection of patients

#merge(x,y, by.x, by.y): merges data x and y, by identifiers by.x (for x) and by.y.(y)

# we can also specify to keep all observations from the first dataset (setting all.x=TRUE),
# or all observations from the second dataset (setting all.y=TRUE)
# but in this let’s perform a merge so that all observations from both datasets are kept (an outer join)
diab.dt <- merge(diab01.dt, diab02.dt, by.x="PAT", by.y="ID", all=TRUE)
dim(diab.dt)

diab.alt.dt <- merge(diab01.dt, diab02.dt[,.(PAT=ID, MY1=TCH, MY2=LTG)], by="PAT", all=TRUE)

### reusable code ###

# functions

# generic definition of a function

function.name <- function(arg1, arg2) { 
  # body of the function
}

# example: imputing missing values

impute.to.median <- function(x) { 
  
  # check the type of objects we have been given 
  if (!(is.numeric(x) || is.integer(x)))
    return(x)
  # find which values are missing 
  na.idx <- is.na(x)
  # replace NAs with the median computed over the observed values
  x[na.idx] <- median(x, na.rm=TRUE)
  # return the vector with imputed values
  return(x)
}

# test
impute.to.median(diab01.dt$SEX)


# but instead of returning the wrong answer we may want to stop and return 
# an error message rather than returning the wrong answer

mean.vector <- function(x) {
  if (!(is.numeric(x) || is.integer(x)))
    stop("The function works only for numerical vectors")
  return(mean(x))
}

### loops ###

for (col in colnames(diab01.dt)) {
  print(col)
}

# now quickly impute all missing values in a data table by looping over them

table(is.na(diab01.dt))
diab01.dt.imputed <- copy(diab01.dt)

for (cols in colnames(diab01.dt.imputed)) { 
  # using `set()` is a fast way to update huge data tables
  set(diab01.dt.imputed, j = cols, value = impute.to.median(diab01.dt.imputed[[cols]]))
}

table(is.na(diab01.dt.imputed))

# skipping and terminating loops

for (idx in 1:100) {
  if (idx %% 2 == 0) next
  if (idx > 10) break
  cat(idx, "is odd\n")
}

# using sapply instead of a for loop

abs.vals <- sapply(-5:5, abs) 
abs.vals

# when applied to a data table or frame sapply loops over the columns
sapply(diab01.dt, class)

# If the function we want to apply is a composite of other functions, we have two options

# using a named function
num.missing <- function(x) sum(is.na(x)) 
sapply(diab01.dt, num.missing)

# using an anonymous function
sapply(diab01.dt, function(z)sum(is.na(z)))

# comparing the effect of imputation on the distribution of the variables

num.cols <- which(sapply(diab01.dt, function(z) (class(z) %in% c("numeric", "integer")))) 

par(mfrow=c(4,2), # multiple plots in the same image
    mar=c(2,2,2,1)) # bottom, left, top, right margins

for (col in num.cols) {
  plot(density(diab01.dt.imputed[[col]]), col="red", main=colnames(diab01.dt[, ..col]))
  lines(density(diab01.dt[[col]], na.rm=TRUE), col="black", lty=3)
}

### assertions ###

# simple way of making an assertion is by using stopifnot(): 
# in this case, if our condition doesn’t hold, the processing will stop at that point 
# (very helpful when using source() to run a whole script). 
# Note that multiple conditions can be tested simultaneously, 
# in which case the assertion will fail if any of the conditions is not true!

stopifnot(1 == 3)

stopifnot(colnames(diab01.dt) == colnames(diab01.dt.imputed)) # no output since it's true
stopifnot(colnames(diab01.dt) == colnames(diab01.dt.imputed), # check column ordering
          diab01.dt$PAT == diab01.dt.imputed$PAT) # check row ordering

### reference classes ###

library(R6)

# Re-run the regression, storing the result
regr <- lm(Y ~ AGE + SEX + HDL, data=diab01.dt)
 
# Class definition

ModelSummary <- R6Class("ModelSummary", public = list(
    model.store = list(), 
    model.summary.dt = data.table(), 
    model.thresh.set = logical(),
    
    # The initialize (note spelling) is called by new()
    initialize = function(model.in) {
      
      # Input data checks
      stopifnot(length(model.in$coefficients) >= 2)
      
      # Initialise member variables
      self$model.thresh.set = FALSE
      
      # Create a data table summarising the results
      self$model.summary.dt <- data.table(summary(model.in)$coefficients, keep.rownames = TRUE)
      print(self$model.summary.dt)
      
      # Add confidence intervals around the coefficients
      self$model.summary.dt <- cbind(self$model.summar.dt, confint(model.in))
      
      # Reformat the output table
      self$model.summary.dt <- self$model.summary.dt[, .(Name = rn, Estimate = signif(Estimate, 3),
                                 `95% CI` = paste0("(", signif('2.5 %', 3), ", ", signif('97.5 %', 3), ")"), 'P-value' = signif('Pr(>|t|)', 3) )]
      
      # Keep a copy of the original model
      self$model.store <- model.in
    },
    
    # Reformat significance level description
    statssignifdt = function(thresh=0.05){
      stopifnot(is.numeric(thresh))
      
      if(!self$model.thresh.set){
        self$model.summary.dt[,'P-value' := ifelse('P-value' < thresh,
                                paste0("<", thresh), as.character('P-value'))]
        # only allow setting of the threshold once
        self$model.thresh.set=TRUE
      } else {
        cat("\nError: P-value threshold has been set previously!\n\n")
      }
      return(self$model.summary.dt)
    },
    
    # The print method is called if the object is viewed
    
    print = function(){
      if(nrow(self$model.summary.dt) > 1) {
        print(self$model.summar.dt)
      } else {
        cat("No model data.\n")
      }
    
    if(!self$model.thresh.set) {
      cat("P-value threshold not yet set. Run statsignifdt() method.\n")
    }
  })
) 

## fitting linear regression models ###

set.seed(1) # initialize the random number generator 
x <- rnorm(100, mean=50, sd=10)
y <- rnorm(100, mean=75, sd=20)
cor(x, y)
plot(x,y)

# linear regression of variables in the workspace
regr <- lm(y ~ x) 

#extract the regression coefficient
coef(regr) # same as regr$coefficients

#note: if both variables were standardized 
# the regression coefficient for x would correspond to the correlation coefficient

x.std <- x / sd(x)
y.std <- y / sd(y)
coef(lm(y.std ~ x.std))

plot(x, y)
abline(regr) # regression line
abline(h=coef(regr)[1], col="red") # horizontal line at the intercept

# we generally concentrate on the table of hypothesis tests on the regression coefficients

hyp.tests <- coef(summary(regr))
hyp.tests # note that hyp.test is of class matrix

# t value can be computed manually as the ratio of regression coefficients to standard errors
tval <- hyp.tests[, "Estimate"] / hyp.tests[, "Std. Error"]
tval

#
df <- length(y) - 1 - 1 # degrees of freedom with one predictor in the model?
qt(c(0.025, 0.975), df) # quantiles of a t distribution?

2 * pt(abs(tval), df, lower.tail=FALSE) # p-values
#we can confidently reject the null hypothesis that the intercept term is zero
# as its p-value is well below the standard significance threshold of 0.05
# On the other hand, the same doesn’t hold for x

## realistic data ##

#association between age and diabetes severity
diab01.dt <- fread("data/diab01.txt", stringsAsFactors = TRUE)
regr.age <- lm(Y ~ AGE, data=diab01.dt) # specify the data table of covariates 

coef(summary(regr.age))

confint(regr.age)

# let's look at the association between HDL and the outcome

with(diab01.dt, cor(Y, HDL))
with(diab01.dt, cor(Y, HDL, use="pairwise.complete.obs"))

# modest inverse relationship between the two variables

# confirm this with a linear regression model
regr.hdl <- lm(Y ~ HDL, data=diab01.dt) 
summary(regr.hdl)

# Given that the p-value for HDL (0.00132) is below the significance threshold
# we can say that HDL is significantly associated with the diabetes score

# if age and sex were included in the model, would the association change?

regr <- lm(Y ~ AGE + SEX + HDL, data=diab01.dt) 
summary(regr)

# here is a significant association between HDL and our outcome variable 
# even after adjusting for age and sex (effect size has increased!)

## standardized coefficients ##

# dividing each continuous variable by its standard deviation
# produces standardized coefficients
diab01.dt.sd1 <- copy(diab01.dt)

covar.cols <- colnames(diab01.dt.sd1[, -c("PAT", "SEX", "Y")]) # Exclude non covariate columns
diab01.dt.sd1[, (covar.cols) := lapply(.SD, function(x) x / sd(x, na.rm = TRUE)), .SDcols = covar.cols]
summary(lm(Y ~ AGE + SEX + HDL, data=diab01.dt.sd1))

#lapply(): allows to modify the data table in place across column (lists)

# this allows us to state that the effect of one standard deviation change in HDL 
# is almost double than what produced by a change of one standard deviation in age 
# (but in opposite directions)

# note: in real-life analyses we generally work with standardized variables, 
# so that comparing effects sizes of different variables is straightforward. 
# when producing a model for clinical use, we may instead want to report equations 
# that are in the original units (unstandardized), 
# as a clinician will not have access to standard deviations

## design matrix ##

# the design matrix is the matrix of all elements in the predictors and the intercept
X <- model.matrix(~ AGE + SEX + HDL, data=diab01.dt) # same as model.matrix(regr) 
dim(X)

### question: what is the intercept?

# categorial variables are coded as dummy variables
head(diab01.dt[, c("AGE", "SEX")])
head(model.matrix(~ AGE + SEX, data=diab01.dt))

# or a categorical variable with k levels, the design matrix contains 
# k − 1 dummy variables corresponding to all non-reference categories

# most important outputs from a regression model is the vector of fitted values y = Xβ, 
# where X is our design matrix and β are the regression coefficients
# y can be obtained my matrix multiplication!
X <- model.matrix(regr) #design matrix
beta.hat <- coef(regr)  #regression coefs
y.hat <- X %*% beta.hat # matrix multiplication


regr$fitted.values
regr$model # complete dataset used in fitting the linear model
regr$residuals
all.equal(regr$residuals, regr$model$Y - regr$fitted.values)
          
## perfomance measure ##

summ.regr <- summary(regr)
ls(summ.regr) # list all objects that are stored inside the variable        
summ.regr$coefficients

# explanation of each of the terms is given in ?summary.lm

summ.regr$sigma # residual standard error
summ.regr$r.squared # 
summ.regr$adj.r.squared #

# computed manually, haha
rse <- function(residuals, num.predictors)sqrt(sum(residuals^2) / (length(residuals) - num.predictors - 1))
r.squared <- function(y.obs, y.pred)sum((y.pred - mean(y.obs))^2) / sum((y.obs - mean(y.obs))^2)
adj.r.squared <- function(r.squared, n, num.predictors)1 - (1 - r.squared) * (n - 1) / (n - num.predictors - 1)

# visualise the relationship between R^2 and adjusted R^2

x.values <- seq(0.25, 1, by=0.05)
num.predictors <- c(2, 10, 25, 50)

## first do a plot with the first setting of the variables
plot(x.values, adj.r.squared(x.values, 100, 2), ylim=c(-0.1, 1), 
     xlab="R^2", ylab="Adjusted R^2", type="l") # connects points with lines

## then add all other lines
for (i in 2:length(num.predictors)) {
  points(x.values, adj.r.squared(x.values, 100, num.predictors[i]),
  type="l", col=i)
}

# the plot shows that the adjusted R2 is always lower, 
# and decreases faster the more predictors are used in the model
# if the number of predictors is large enough, relative to the sample size, 
# it also possible for an adjusted R2 to go negative