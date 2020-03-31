library(mlr3)
library(mlr3learners)
library(mlr3viz)
library(glmnet)
library(gradDescent)
library(ggthemes)
library(tidyverse)
library(paradox)
library(mlr3tuning)

# Step 1 read data ---------
housing <- read.table("http://archive.ics.uci.edu/ml/machine-learning-databases/housing/housing.data")

# housing variable names 
names(housing)
names(housing) <- c("crim","zn","indus","chas","nox","rm","age","dis","rad","tax","ptratio","b","lstat","medv")

# Data description -----------
#    1. CRIM      per capita crime rate by town
#    2. ZN        proportion of residential land zoned for lots over 25,000 sq.ft.
#    3. INDUS     proportion of non-retail business acres per town
#    4. CHAS      Charles River dummy variable (= 1 if tract bounds river; 0 otherwise)
#    5. NOX       nitric oxides concentration (parts per 10 million)
#    6. RM        average number of rooms per dwelling
#    7. AGE       proportion of owner-occupied units built prior to 1940
#    8. DIS       weighted distances to five Boston employment centres
#    9. RAD       index of accessibility to radial highways
#    10. TAX      full-value property-tax rate per $10,000
#    11. PTRATIO  pupil-teacher ratio by town
#    12. B        1000(Bk - 0.63)^2 where Bk is the proportion of blacks by town
#    13. LSTAT    % lower status of the population
#    14. MEDV     Median value of owner-occupied homes in $1000's

# begin problem set ----------
# first, copy data transformation from ps9
housing$lmedv <- log(housing$medv)
housing$medv <- NULL # drop median value
formula<- as.formula(lmedv ~ .^3 +
                       poly(crim,6) +
                       poly(zn,6) +
                       poly(indus,6) +
                       poly(nox,6) +
                       poly(rm,6) +
                       poly(age,6) +
                       poly(dis,6) +
                       poly(rad,6) +
                       poly(tax,6) +
                       poly(ptratio,6) +
                       poly(b,6) +
                       poly(lstat,6))
#now replace the intercept column by the response since MLR will do
#"y ~ ." and get the intercept by default
mod_matrix <- data.frame(model.matrix(formula,housing))

mod_matrix [,1] = housing$lmedv
colnames(mod_matrix)[1] = "lmedv"
#check data 
head(mod_matrix) 

# Split the data into test/train
train_set = sample(task_housing$nrow, 0.80 * task_housing$nrow)
test_set = setdiff(seq_len(task_housing$nrow), train_set)


# Define the task:
task_housing <- TaskRegr$new(id = "housing", backend = mod_matrix, target = "lmedv")
print(task_housing)

# OLS --------
learner <- lrn("regr.rpart")
learner$train(task_housing, row_ids = test_set)
prediction = learner$predict(task_housing, row_ids = test_set)
prediction_insample = learner$predict(task_housing, train_set) # ???

#Set resampling strategy (6-fold cross validation)
rcv <- rsmp("cv", folds = 6)
rcv$instantiate(task_housing)
rcv$test_set(1)
rcv$train_set(1)

rr = resample(task_housing, learner, rcv, store_models = TRUE)
rr$aggregate(msr("regr.rmse"))
# RMSE after resampling the learner = 0.20735
autoplot(rr$prediction())

# Not sure if these are valid ------
RMSE(prediction$truth, prediction$response)
# RMSE out of sample = 0.13136 ??
RMSE(prediction_insample$truth, prediction_insample$response)
# RMSE in sample = 0.2554 ????

# LASSO ---------------------------------------
learner_glm <- lrn("regr.glmnet")
learner_glm$train(task_housing, train_set)
learner_glm$model

prediction_glm <- learner_glm$predict(task_housing, test_set)
head(as.data.table(prediction_glm))

rr_glm <- resample(task_housing, learner_glm, rcv, store_models = TRUE)
rr_glm$aggregate(msr("regr.rmse"))
RMSE(rr_glm$prediction()$truth, rr_glm$prediction()$response)

# RMSE of GLM model = 0.1650

autoplot(prediction_glm) + theme_economist()

# Tune parameters

tune_params <- ParamSet$new(list(
  ParamDbl$new("lambda", lower = 0, upper = 1),
  ParamDbl$new("alpha", lower = 1, upper = 1)
))

measure = msr("regr.rmse")

evals20 = term("evals", n_evals = 20)

tuning_instance = TuningInstance$new(
  task = task_housing,
  learner = learner_glm,
  resampling = rcv,
  measures = measure,
  param_set = tune_params,
  terminator = evals20
)
tuning_instance

tuner = tnr("random_search")

tuning_results <- tuner$tune(tuning_instance)