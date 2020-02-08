### load packages ###

library(pROC)
library(caret)

### logistic regression ###

# case-control study includes 1327 women aged 50-81 with hip fractures 
# as well as 3262 randomly selected women in the same age range
# among the cases, 40 women take hormone replacement therapy (HRT); among the controls, 239 women do
# ]does HRT a???ect the probability of having a hip fracture?

# create synthetic dataset with same characteristics

y <- c(rep(1, 1327), rep(0, 3262)) # cases, controls 
hrt <- c(rep(1, 40), rep(0, 1287), # HRT, no HRT in cases
         rep(1, 239), rep(0, 3023))  # HRT, no HRT in control

# fit a logistic regression model

regr.hrt <- glm(y ~ hrt, family="binomial") 
summary(regr.hrt)

# estimated coe???cients are the log-odds: 
# the sign of the regression coe???cient tells us that the use of HRT 
# has a protective e???ect on hip fractures (it reduces their odds)
# however, we need to transform it to an odds ratio in order to 
# better appreciate the magnitude of the e???ect

exp(coef(regr.hrt)[2]) #exponentiat to get the odds ratio

# given that we see an odds ratio of 0.393, 
# we can claim that HRT reduces the odds of having a hip fracture by 60.7 percent

# build 95% CI for the odds ratio

beta <- coef(regr.hrt)[2]
se.beta <- coef(summary(regr.hrt))[2,2]

round(exp(beta + 1.96 * se.beta * c(-1,1)),3)

# confint() uses a t distribution, which is usually better,
# especially for smaller sample sizes. 
# again, the intervals are in the log-odds scale,
# so we need to exponentiat them

or.ci <- round(exp(confint(regr.hrt)),3)
or.ci

# interpretation:  we are 95% con???dent 
# that the true value of the odds ratio is between 0.276 and 0.546
# CI does not include 1 so we can reject the Null hypthesis of no effect!

# transforming the log-odds through the logistic function
# we can compute the baseline probability, 
# that is the probability of an event when all other covariates 
# (in our case, just taking HRT) are zero

baseline.odds <- exp(coef(regr.hrt)[1])
baseline.prob <- baseline.odds / (1 + baseline.odds) #logistic function
round(baseline.prob, 3)


# women in the study who does not take HRT has a 29.9% probability of experiencing 
# a hip fracture

# for a women who does not take HRT?

hrt.odds <- exp(coef(regr.hrt)[1] + coef(regr.hrt)[2])
hrt.prob <- hrt.odds / ( 1 + hrt.odds)
round(hrt.prob, 3)

# note: these do not represent the absolute risk

## fitted values ##

X <- model.matrix(regr.hrt)
y.hat <- as.numeric(X %*% coef(regr.hrt))
all(y.hat == regr.hrt$linear.predictors) # fitted values in the log-odds scale

logistic <- function(z)exp(z)/(exp(z) + 1)
prob.case <- logistic(regr.hrt$linear.predictors)
all(prob.case == regr.hrt$fitted.values) # fitted values in the probability scale

# DO NOT mix the two scales! :)


# Log-Likelihood and derived measures
logLik(regr.hrt)

# deviance of a model is minus twice the log-likelihood 
# (labeled residual deviance by R)
-2*as.numeric(logLik(regr.hrt))

#null model

null.model <- glm(y ~ 1, family="binomial") #only intercept in the model
head(null.model$fitted.values) #same probabilites for all observations

sum(y)/length(y) #proportions of cases

-2*as.numeric(logLik(null.model)) # same as regr$null.deviance

#models with better fit have lower deviance!

# di???erence between two deviances has ??2 distribution with degrees of freedom
# equal to the di???erence in parameters of the two models
# therefore we can use it as a test statistic and 
# compute a p-value under the null hypothesis that the smaller model is better:
# if the p-value < ?? then we reject the null hypothesis and 
# claim that the larger model is signi???cantly better

pchisq(regr.hrt$null.deviance - regr.hrt$deviance, df=1, lower.tail=FALSE)

# AIC: the deviance of the model is penalised
# by the number of parameters estimated in the model
2 * regr.hrt$rank - 2 * as.numeric(logLik(regr.hrt)) 
AIC(regr.hrt)
AIC(null.model)

# the lower the AIC, the better the model

# BIC applies a stronger penalty for the use of additional predictors

BIC(regr.hrt)
BIC(null.model)

#AUC

roc(y, regr.hrt$fitted.values)

# AUC of 0.52 is NOT good
# it  is not very di???erent from a random model 
# at discriminating cases from controls

# but the purpose of this model was not to explain hip fractures based on HRT 
# (or indeed to predict them!), but simply to understand 
# if HRT has an e???ect on the odds of hip fractures!

roc(y, regr.hrt$fitted.values, plot=TRUE, legacy.axes=TRUE) 

###  making predictions ###

# data about subarachnoid hemorrhage (SAH)
# let’s start by fitting a simple model containing only age and gender on the entire dataset

data("aSAH")
asah.all <- glm(outcome ~ age + gender, data=aSAH, family="binomial")

# predict() function allows us to use a fitted regression model 
# (either linear or logistic regression) to make a prediction

y.linpred <- predict(asah.all, newdata = aSAH) # returns linear predictors: log-odds
y.pred <- predict(asah.all, newdata = aSAH, type = "response") # returns predicted probabilites

# predicting the same data that was used for fitting is not very interesting
# we are overestimating the predictive performance of the model
# results are already available in the linear.predictors and fitted.values vectors of the regression object

# to predict the response for a 30 years old male patient
# create a dataframe to contain these data and ask the model make a prediction for it
data.30M <- data.frame(age=30, gender="Male")
predict(asah.all, newdata=data.30M, type="response")

## data partioning and cross-validation ##

# the package caret allows us to call function createDataPartition() 
# which creates a single training/test split
set.seed(1)
train.idx <- createDataPartition(aSAH$outcome, p=0.7)$Resample1

# this creates a list of indices corresponding to the observations in the training set: 
# we can use this to fit the model only on this subset of data
# the subset option of lm() and glm() allows to control the observations 
# that are used to fit the model coefficients without making us create new dataframes to store the subsets

asah.train <- glm(outcome ~ age + gender, data=aSAH, subset=train.idx, family="binomial")

# that we cannot use the deviance or the AIC to compare models fitted on different datasets 
# (or, as in this case, on datasets of different sizes): 
# these quantities depend on the number of observations used in fitting, 
# so a model fitted to a larger dataset will in general have higher deviance and AIC,
# but this is not necessarily an indication of bad quality of fit

# how well do we predict the outcome for the withdrawn observations???

pred.prob <- predict(asah.train, newdata=aSAH, type="response")[-train.idx] # option1
pred.prob <- predict(asah.train, newdata=aSAH[-train.idx, ], type="response") # option2

# plot ROC and calculate corresponding AUC
roc(outcome ~ pred.prob, data=aSAH[-train.idx, ], plot=TRUE)

# the  AUC for this model (0.736) is quite good. 
# unfortunately, given that we created only one partition, 
# it’s  hard to tell if a similar performance would be obtained on a different random training/test split

# cross-validation is a more informative approach:
# it allows us make a prediction for all observations of the dataset
set.seed(1)
num.folds <- 10
folds <- createFolds(aSAH$outcome, k=num.folds) # get indices of the test sets

# note: folds object is a list

# NOW: fitting the model for each training set 
# and predict the outcome for the obserations of the corresponding test sets

# we need to store each fitted model so that they can be used afterwards for prediction or inspection:
# by assigning the output from glm() to res.folds[[fold]] 
# we are effectively adding an element to a list of results

regr.cv <- NULL

for(fold in 1:num.folds){
  train.idx <- setdiff(1:nrow(aSAH), folds[[fold]])
  regr.cv[[fold]] <- glm(outcome ~ age + gender, data=aSAH, subset=train.idx, family="binomial")
}

# another loop to produce the predicted responses for each fold
# for convenience reasons: create a dataframe to store the observed outcome and what we predict from the model

pred.cv <- NULL
auc.cv <- numeric(num.folds)
for (fold in 1:num.folds){
  test.idx <- folds[[fold]]
  pred.cv[[fold]] <- data.frame(obs=aSAH$outcome[test.idx],
                                pred=predict(regr.cv[[fold]], newdata=aSAH, type="response")[test.idx])
  auc.cv[fold] <- roc(obs ~ pred, data=pred.cv[[fold]])$auc
}

# after cross-validation, we can report the expected performance of the model on withdrawn data
# this is slightly smaller than what we obtained when using a single partition
# but we can attach more confidence to this estimate

round(mean(auc.cv), 3)





