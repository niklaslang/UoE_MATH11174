### coronary heart disease ###

library(data.table)
library(pROC)
library(caret)

heart.disease <- fread("data/chdagesex.csv", stringsAsFactors = TRUE)

# logistic regression model to test the association between age and coronary heart disease

m1 <- glm(CHD ~ AGE, data = heart.disease, family = "binomial")

summary(m1)
coef(summary(m1))

# compute the odds ratio for age (answer: 1.06) 

round(exp(coef(summary(m1))[2]),2)

# and the 95% confidence interval (answer: 1.04-1.09)

beta <- coef(summary(m1))[2]
se.beta <- coef(summary(m1))[2,2]

round(exp(beta + 1.96*se.beta*c(-1,1)),3)

# test the association of CHD with age after adjusting for sex (model M2):

m2 <- glm(CHD ~ AGE + SEX, data = heart.disease, family = "binomial")

summary(m2)
coef(summary(m2))

# produce odds ratios and confidence intervals for both covariates 
# (answer: age: 1.06, 1.04-1.09, sex: 2.62, 1.34-5.32)

odds.age <- exp(coef(summary(m2))[2,1])
ci.age <- exp(confint(m2)[2, ])

round(c(odds.age, ci.age), 2)

odds.sex <- exp(coef(summary(m2))[3,1])
ci.sex <- exp(confint(m2)[3,])

round(c(odds.sex, ci.sex),2)

# likelihood ratio test comparing model M1 to model M2 
# and confirm that the addition of sex to the model is significant at α = 0.05 
# by computing a p-value (answer: 0.0047)

signif(pchisq(m1$deviance - m2$deviance, df=1, lower.tail = FALSE),2)

# investigate the effect of sex after stratifying for age ≤ 50 and age > 50
# Report odds ratios and confidence intervals for the two strata 
# (answer: age ≤ 50: 0.62, 0.12-2.62, age > 50: 3.9, 1.81-8.88)

m.young <- glm(CHD ~ SEX, data = heart.disease.young, subset = AGE <= 50, family = "binomial")

odds.young <- exp(coef(summary(m.young))[2,1])
ci.young <- exp(confint(m.young)[2,])

round(c(odds.young, ci.young),2)

m.old <- glm(CHD ~ SEX, data = heart.disease, subset = AGE > 50, family = "binomial")

odds.old <- exp(coef(summary(m.old))[2,1])
ci.old <- exp(confint(m.old)[2,])

round(c(odds.old, ci.old),2)

# create a dataframe agesex containing two columns: AGE with values in the sequence from 1 to 100,
# and SEX created as follows: 
AGE <- seq(1:100)
set.seed(1)
SEX <- factor(rbinom(100, 1, 0.5), labels=c("F", "M"))

agesex <- data.frame(AGE, SEX)

# predict probabilities of CHD for the agesex data according to model M2 
# and plot them using a different colour according to sex.

CHD.predict <- predict(m2, newdata = agesex, type = "response" )

twocolors <- rainbow(2)
plot(agesex$AGE, CHD.predict, pch = 21, bg = twocolors[agesex$SEX], 
    main = "M2: Predictive Perfomance",
    xlab = "Age",
    ylab = "Predicted Probability")
legend("topleft", c("F", "M"), fill = twocolors)

# plot the ROC curves for the two models in the same graph 
# (hint: use option add=TRUE for the second curve) and report their AUCs (answer: 0.734, 0.76)

model1.auc <- roc(heart.disease$CHD, m1$fitted.values)$auc
model2.auc <- roc(heart.disease$CHD, m2$fitted.values)$auc
legend1 <- paste("Model1:", round(model1.auc,3))
legend2 <- paste("Model2:", round(model2.auc,3))


roc(heart.disease$CHD, m1$fitted.values, plot=TRUE, col="green")
roc(heart.disease$CHD, m2$fitted.values, plot=TRUE, col="blue", add=TRUE)
legend("bottomright", title = "AUCs", c(legend1, legend2), 
       fill = c("green","blue"))

# write function glm.cv(formula, data, folds) that 
# given a model formula (an expression of the type outcome ~ predictors), 
# a dataframe containing outcome and predictors, 
# and a set of cross-validation folds produced by createFolds(), 
# fits a logistic regression model in each of the folds and returns a list of fitted models

glm.cv <- function(formula, data, folds){
  
  regr.cv <- NULL
  num.folds <- length(folds)
  
  # fit a logistic regression model in each of the folds
  for(fold in 1:num.folds){
    regr.cv[[fold]] <- glm(formula, data=data[-folds[[fold]], ], family="binomial") 
  }
  
  # returns a list of fitted models
  return(regr.cv)
  
}

# after setting the random seed to 1, generate a set of 10 cross-validation folds 
# and use glm.cv() to cross-validate model M1 and model M2

set.seed(1)
num.f <- 10
folds <- createFolds(heart.disease$CHD, k=num.f)

cross.validation.m1 <- glm.cv(CHD ~ AGE, heart.disease, folds)
cross.validation.m2 <- glm.cv(M2, heart.disease, folds)

# write function predict.cv(regr.cv, data, outcome, folds) 
# where regr.cv is a list of fitted models produced by glm.cv(),
# data is a dataframe of covariates, 
# outcome is the vector of observed outcomes 
# and folds is the set of cross-validation folds: 

# the function should use the model fitted on the training set of each fold 
# to predict the outcome of the corresponding test set
# the function should return a list of dataframes, 
# each containing observed and predicted outcome for the test observations


regr.cv <- function(regr.cv, data, outcome, folds){
  
  num.folds <- length(folds)
  pred.cv <- NULL
  
  # use the model fitted on the training set of each fold
  for(fold in 1:num.folds){
    
    test.idx <- folds[[fold]]
    pred.cv[[fold]] <- data.frame(obs = outcome[test.idx],
                                  pred = predict(regr.cv[[fold]], newdata = data, type="response")[test.idx])
  
  }
  
  # return a list of dataframes containing observed and predicted outcomes
  return(pred.cv)
  
}

# use predict.cv() to make predictions for both model M1 and model M2

pred.cv.m1 <- regr.cv(cross.validation.m1, heart.disease, heart.disease$CHD, folds)
pred.cv.m2 <- regr.cv(cross.validation.m2, heart.disease, heart.disease$CHD, folds)

# using these predictions, compute AUCs for all folds and report the mean cross-validated AUCs
# (answer: 0.756, 0.754)

auc.cv.m1 <- auc.cv.m2 <- numeric(length(folds))

for (fold in 1:length(folds)) {
  auc.cv.m1[fold] <- roc(obs ~ pred, data=pred.cv.m1[[fold]])$auc
  auc.cv.m2[fold] <- roc(obs ~ pred, data=pred.cv.m2[[fold]])$auc
}

round(mean(auc.cv.m1),3)
round(mean(auc.cv.m2),3)


