### correlation plot ###

library(data.table)

# cor() can also be used to compute the correlation of columns of a matrix

diab01.dt <- fread("data/diab01.txt", stringsAsFactors = TRUE)
corr <- cor(diab01.dt[, .SD, .SDcols = sapply(diab01.dt, is.numeric)], use="pairwise.complete")

library(corrplot)
corrplot(corr, diag=FALSE, tl.col="black")

corr.cardiovasc <- cor(diab01.dt[, .SD, .SDcols = c("BMI","BP","TC","LDL")], use="pairwise.complete")
corr <- cor(diab01.dt[, .SD, .SDcols = c("BMI","BP","TC","LDL")], use="pairwise.complete")

# correlation and covariance matrix are better measures of similarity if the data is normal
# try to transform the data so that variables are distributed normally: 
# in most cases, normality can be approximated by taking logs if variables are right-skewed (log-normality)
# or by using a power transform if left-skewed

corrplot(corr, order="hclust", diag=FALSE, tl.col="black",
         title="Correlation matrix (ordered by hierarchical clustering)",
         mar=c(0,1,2,0))

### PCA ###

# principal component analysis on the diab01.dt dataset, 
# even though it’s not too large to require dimensionality reduction

# PCA makes sense only over numerical column, and missing values are not allowed!

diab01.dt.imputed <- copy(diab01.dt)

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

for (cname in colnames(diab01.dt.imputed)) {
  set(diab01.dt.imputed, j = cname,
      value = impute.to.median(diab01.dt.imputed[[cname]]))
}

# PCA can be run by using function prcomp()
# to discover how variables cluster, we need to operate on the transpose of the matrix 
# (obtained by using function t()): make sure that the outcome variable is not included in the PCA analysis
# setting option scale=TRUE is always advised, as it takes into account the fact 
# that variables may be measured in different units

idx.pca <- colnames(diab01.dt.imputed[, .SD, 
                                      .SDcols = sapply(diab01.dt.imputed, is.numeric) & # remove factors
                                      colnames(diab01.dt.imputed) != "Y"] # remove outcome 
                    ) 

pca.vars <- prcomp(t(diab01.dt.imputed[, .SD, .SDcols = idx.pca]))

# the amount of variability explained by the components can be computed 
# bearing in mind that the square root of the eigenvalues is stored in vector sdev of the PCA object
# this is always decreasing, 
# and in this instance the first two components account for most of the variability in the data
# the variance explained by the principal components can be visualized through a scree plot

perc.expl <- pca.vars$sdev^2 / sum(pca.vars$sdev^2)
sum(perc.expl[1:2])
screeplot(pca.vars, main="Scree plot")

# projection of the data on the principal components is given in vector x of the PCA object: 
# plotting this can sometimes reveal some interesting patterns in the data

plot(pca.vars$x[, 1:2], main="Projection of variables on the first 2 PCs")

# what happens if we used PCA to describe the similarity between patients?
# in this case, we operate on the matrix itself, rather than on the transpose

pca.pats <- prcomp(diab01.dt.imputed[, ..idx.pca], scale=TRUE)
plot(pca.pats$x[, 1:2], main="Projection of patients on the first 2 PCs")

# the plot doesn’t suggest any particular difference among patients

# using a colour to denote the outcome (as Y is continuous, we dichotomize it) 
# doesn’t reveal any very clear separation, 
# although possibly for negative values of the two PCs there are more severe cases (high Y),
# while for positive values of both PC there are less severe cases (low Y)

y.bin <- with(diab01.dt.imputed, as.integer(Y > median(Y)))
plot(pca.pats$x[, 1:2], main="Projection of patients on the first 2 PCs", col=2+y.bin, pch=19)
legend("bottomright", legend=c("Low Y", "High Y"), col=2:3, lwd=4)

### Subset selection ###

# stepwise selection can be executed through function stepAIC() provided by the MASS package 
# (a simplified version, called step() doesn’t require an additional package)

library(MASS)

# the function requires an initial model, and adds or removes one predictor at a time 
# (according to what is specified in the direction parameter) 
# until no improvement in AIC can be produced

# a special symbol that can be used in formulas is “.”, which represents all columns in the dataframe
# in order to use it, we need to remove the patient identifiers from the columns, 
# otherwise they would be turned into dummy variables and create collinearities

full.model <- lm(Y ~ ., data=diab01.dt.imputed[, !"PAT"]) # use all variables except PAT
sel.back <- stepAIC(full.model, direction="back") # backward elimination

# he object produced by stepAIC() is identical to one produced by lm() or glm()
# and it corresponds to the final model
# to see the results of fitting the model we can use summary()

summary(sel.back)

# suppose that we wanted both age and sex in the selected model 
# (these are two of the most common confounders, so usually we want to have them in all models):
# this can be controlled by the scope parameter, 
# which allows to use either a fitted model or just a formula to describe the smallest model allowed 
# (that is, which predictors should never be eliminated)

sel.back <- stepAIC(full.model, scope=list(lower=~ AGE + SEX), direction="back")

# we could now try forward selection on the same dataset
# when going forward, the scope parameter must always be specified to indicate the upper model,
# that is which variables should be considered in the selection process 
# (when going backward this is implied by the initial model, 
# which by definition includes all variables of potential interest)

null.model <- lm(Y ~ 1, data=diab01.dt.imputed) # only include the intercept
sel.forw <- stepAIC(null.model, scope=list(upper=full.model), direction="forward")

# forward selection chose a sparser model than backward elimination,
# but this is not guaranteed to happen at all times

# the step() function also allows to run stepwise selection by setting direction="both".

init.both <- lm(Y ~ AGE + SEX, data=diab01.dt.imputed)
sel.both <- step(init.both, scope=list(upper=full.model), direction="both")

# remember that this process identified the model that best fits our dataset: 
# although the backward elimination model has lower AIC than the other models,
# we do not yet know which model generalises better

### regularisation approaches ###

# ridge regression, lasso and elastic net are implemented in the glmnet package

library(glmnet)

# two main functions: glmnet() and cv.glmnet()

# the first fits a regularised model for a series of values of the penalty parameter λ 
# (by default 100, but it may be truncated for small datasets)

# the second, run an internal cross-validation to identify 
# which specific setting of λ performs better when predicting the observations in the test set

# neither function accepts formulas to define models, but expects matrices and vectors as input

# use the following function to facilitate the tranformation of a dataframe to a matrix
# as expected by the glmnet package

prepare.glmnet <- function(data, formula=~ .) {
  ## create the design matrix to deal correctly with factor variables, ## without losing rows containing NAs
  old.opts <- options(na.action='na.pass')
  x <- model.matrix(formula, data)
  options(old.opts)
  ## remove the intercept column, as glmnet will add one by default x <- x[, -match("(Intercept)", colnames(x))]
  return(x)
}

# by default, the function uses all existing columns in the dataframe
# however, we do not want the outcome variable to be in the matrix of predictors: 
# so we first remove it, then convert the rest of the dataframe to a matrix

ydiab01.dt <- diab01.dt.imputed$Y # store the outcome separately 
xdiab01.dt <- prepare.glmnet(diab01.dt.imputed[,!"PAT"], ~ . - Y) # exclude the outcome

# now we are finally ready to fit the first regularised model: 
# by default, function glmnet() will fit a linear regression with lasso penalty
# to change it to ridge regression, set the alpha option to 0.

fit.lasso <- glmnet(xdiab01.dt, ydiab01.dt) # same as setting alpha=1
fit.ridge <- glmnet(xdiab01.dt, ydiab01.dt, alpha=0)

# to see the trajectories of the coefficients for the various choices of λ 
# it’s enough to use plot() on the fitted objects

par(mfrow=c(1,2), mar=c(4,4,5,2))
plot(fit.lasso, main="Lasso trajectories") 
plot(fit.ridge, main="Ridge trajectories")

# the x-axis indicates the L1 norm of the regression coefficients: 
# when the penalty parameter λ is at its maximum value all coefficients are zero 
# (null model)
# by decreasing the strength of the penalty term, 
# the coefficients are allowed to increase
# the numbers at the top of the plot count the number of nonzero variables: 
# note how for ridge all predictors become very soon nonzero, 
# while for lasso this happens in a staggered way

# the model coefficients depend on the choice of λ: 
# they can be found in the fields a0 (for intercepts) and beta (for predictors)
# of the fitted objects, while the value of the corresponding penalty factor
# is stored in the lambda field
# assuming that we were interested in the 10-th value of λ, 
# we could retrieve the corresponding model coefficients by subsetting

idx <- 10
lambda10 <- fit.lasso$lambda[idx]

fit.lasso$a0[idx] #intercept
fit.lasso$beta[, idx] #coefficients

# note that because of the regularization term, 
# we are not able to produce estimates of the standard error 
# and consequently p-values

# the predict() method works in a similar way as for linear and logistic regression
# however, unless otherwise specified through the s option, 
# the returned values correspond again to all settings of λ
# also, there are a few more types of values that can be obtained 
# through this function (see ?predict.glmnet for more details)

predict(fit.lasso, newx=xdiab01.dt, type="response")
predict(fit.lasso, newx=xdiab01.dt, type="response", s=lambda10)
predict(fit.lasso, type="nonzero") # indices of nonzero elements for each lambda 
predict(fit.lasso, type="coefficients") # regression coefficients for each lambda


# in most cases we are interested only in a specific setting of the penalty parameter,
# ideally one which will be most effective in prediction
# to identify it, we can use function cv.glmnet() to perform cross-validation:
# this happens within the function, so we do not need to create a specific set 
# of folds for that (although we may do if we were to learn or tune other parameters)

fit.cv.lasso <- cv.glmnet(xdiab01.dt, ydiab01.dt)
fit.cv.ridge <- cv.glmnet(xdiab01.dt, ydiab01.dt, alpha=0)

# lotting the cross-validation curve allows us to inspect 
# how prediction errors vary according to the amount of shrinkage applied

par(mfrow=c(1,2), mar=c(4,4,5,2))
plot(fit.cv.lasso, main="Lasso") 
plot(fit.cv.ridge, main="Ridge")

# the plot displays the mean cross-validated error in red 
# with bars corresponding to standard errors
# the leftmost dotted line in each plot corresponds to the λ 
# that minimizes the error (lambda.min in the fitted object); 
# the dotted line to the right corresponds to the largest value of λ 
# such that the error is within one standard error from the minimum 
# (fit.lasso$lambda.1se in the fitted object)

# he curves obtained depend on the choice of the error measure
# used in cross-validation
# by default, cv.glmnet() uses the mean square error for linear regression
# and deviance for logistic regression
# however, these can be changed to mean absolute error (for linear regression)
# or to AUC or classification error (for logistic regression) 
# by setting the appropriate choice of the type.measure option (see ?cv.glmnet)

# now that we know a good choice of the penalty parameter, 
# we can use that in prediction 
# (although here, since we have hadn’t created cross-validation folds,
# we do not have any new data to predict)

predict(fit.cv.lasso, newx=xdiab01.dt, type="response", s=fit.cv.lasso$lambda.min)

# note that inside the object produced by cv.glmnet() 
# there is a field called glmnet.fit which effectively stores 
# what would have been created by using glmnet(): 
# this is where the regression coefficients for all values of λ are stored
