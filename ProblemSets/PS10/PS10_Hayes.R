library(tidyverse)
library(e1071)
library(nnet)
library(kknn)
library(mlr3)
library(mlr3viz)
library(mlr3tuning)
library(mlr3learners)
library(glmnet)
library(keras)
library(ggthemes)
library(paradox)


set.seed(100)

income <- read.table("http://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data")
names(income) <- c("age","workclass","fnlwgt","education","education.num","marital.status","occupation","relationship","race","sex","capital.gain","capital.loss","hours","native.country","high.earner")

# From UC Irvine's website (http://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.names)
#   age: continuous.
#   workclass: Private, Self-emp-not-inc, Self-emp-inc, Federal-gov, Local-gov, State-gov, Without-pay, Never-worked.
#   fnlwgt: continuous.
#   education: Bachelors, Some-college, 11th, HS-grad, Prof-school, Assoc-acdm, Assoc-voc, 9th, 7th-8th, 12th, Masters, 1st-4th, 10th, Doctorate, 5th-6th, Preschool.
#   education-num: continuous.
#   marital-status: Married-civ-spouse, Divorced, Never-married, Separated, Widowed, Married-spouse-absent, Married-AF-spouse.
#   occupation: Tech-support, Craft-repair, Other-service, Sales, Exec-managerial, Prof-specialty, Handlers-cleaners, Machine-op-inspct, Adm-clerical, Farming-fishing, Transport-moving, Priv-house-serv, Protective-serv, Armed-Forces.
#   relationship: Wife, Own-child, Husband, Not-in-family, Other-relative, Unmarried.
#   race: White, Asian-Pac-Islander, Amer-Indian-Eskimo, Other, Black.
#   sex: Female, Male.
#   capital-gain: continuous.
#   capital-loss: continuous.
#   hours-per-week: continuous.
#   native-country: United-States, Cambodia, England, Puerto-Rico, Canada, Germany, Outlying-US(Guam-USVI-etc), India, Japan, Greece, South, China, Cuba, Iran, Honduras, Philippines, Italy, Poland, Jamaica, Vietnam, Mexico, Portugal, Ireland, France, Dominican-Republic, Laos, Ecuador, Taiwan, Haiti, Columbia, Hungary, Guatemala, Nicaragua, Scotland, Thailand, Yugoslavia, El-Salvador, Trinadad&Tobago, Peru, Hong, Holand-Netherlands.

######################
# Clean up the data
######################
# Drop unnecessary columns
income$native.country <- NULL
income$fnlwgt         <- NULL
# Make sure continuous variables are coded as such
income$age            <- as.numeric(income$age)
income$hours          <- as.numeric(income$hours)
income$education.num  <- as.numeric(income$education.num)
income$capital.gain   <- as.numeric(income$capital.gain)
income$capital.loss   <- as.numeric(income$capital.loss)
# Combine levels of categorical variables that currently have too many levels
levels(income$education) <- list(Advanced = c("Masters,","Doctorate,","Prof-school,"), Bachelors = c("Bachelors,"), "Some-college" = c("Some-college,","Assoc-acdm,","Assoc-voc,"), "HS-grad" = c("HS-grad,","12th,"), "HS-drop" = c("11th,","9th,","7th-8th,","1st-4th,","10th,","5th-6th,","Preschool,"))
levels(income$marital.status) <- list(Married = c("Married-civ-spouse,","Married-spouse-absent,","Married-AF-spouse,"), Divorced = c("Divorced,","Separated,"), Widowed = c("Widowed,"), "Never-married" = c("Never-married,"))
levels(income$race) <- list(White = c("White,"), Black = c("Black,"), Asian = c("Asian-Pac-Islander,"), Other = c("Other,","Amer-Indian-Eskimo,"))
levels(income$workclass) <- list(Private = c("Private,"), "Self-emp" = c("Self-emp-not-inc,","Self-emp-inc,"), Gov = c("Federal-gov,","Local-gov,","State-gov,"), Other = c("Without-pay,","Never-worked,","?,"))
levels(income$occupation) <- list("Blue-collar" = c("?,","Craft-repair,","Farming-fishing,","Handlers-cleaners,","Machine-op-inspct,","Transport-moving,"), "White-collar" = c("Adm-clerical,","Exec-managerial,","Prof-specialty,","Sales,","Tech-support,"), Services = c("Armed-Forces,","Other-service,","Priv-house-serv,","Protective-serv,"))

# Break up the data:
n <- nrow(income)

task_income <- TaskClassif$new(id = "income", backend = income, target = "high.earner")

train_set <- sample(task_income$nrow, size = .80 * task_income$nrow)
test_set  <- setdiff(seq_len(task_income$nrow), train_set)

# Learners --------
learner_rpart = lrn("classif.rpart")
learner_glm = lrn("classif.glmnet")
learner_kknn = lrn("classif.kknn")
learner_bayes = lrn("classif.naive_bayes")
learner_svm = lrn("classif.svm")
# How to use nnet with mlr3????
learner_nnet = lrn("classif.nnet")
mlr_learners$get("classif.nnet")


learner_rpart$train(task_income, row_ids = train_set)
# Doesnt work: unsupported feature type: factor
learner_glm$train(task_income, row_ids = train_set)
learner_kknn$train(task_income, row_ids = train_set)
learner_bayes$train(task_income, row_ids = train_set)
# Also doesn't work
# learner_svm$train(task_income, row_ids = train_set)

prediction_rpart = learner_rpart$predict(task_income, row_ids = test_set)
autoplot(prediction_rpart)

prediction_kknn = learner_kknn$predict(task_income, row_ids = test_set)
autoplot(prediction_kknn)
# performs better than rpart even untuned

prediction_bayes = learner_bayes$predict(task_income, row_ids = test_set)

autoplot(prediction_bayes) + 
  scale_fill_economist() + 
  labs(title = "Naive Bayes Prediction")



## Set Tuners -------
tune_tree = ParamSet$new(list(
  ParamDbl$new("cp", lower = 0.001, upper = 0.2),
  ParamInt$new("minsplit", lower = 10, upper = 50),
  ParamInt$new("minbucket", lower = 5, upper = 50)
))

tune_glm = ParamSet$new(list(
  ParamDbl$new("lambda", lower = 0, upper = 3),
  ParamDbl$new("alpha", lower = 0, upper = 1)
))

tune_nnet = ParamSet$new(list(
  ParamInt$new("size", lower = 1, upper = 10),
  ParamDbl$new("decay", lower = 0.1, upper = 0.5),
  ParamInt$new("maxit", lower = 1000, upper = 1000)
))

tune_kknn = ParamSet$new(list(
  ParamInt$new("k", lower = 1, upper = 30)
))





# Tune models ----------

resampling = rsmp("cv", folds = 3L)
resampling$instantiate(task_income)
resampling$iters
evals30 = term("evals", n_evals = 30)

instance_rpart = TuningInstance$new(
  task = task_income,
  learner = lrn("classif.rpart"),
  resampling = resampling,
  measures = msr("classif.fbeta"),
  param_set = tune_tree,
  terminator = evals30
)

tuner = tnr("random_search")

tuner$tune(instance_rpart)
learner_rpart$param_set$values = instance_rpart$result$params
learner_rpart$train(task_income, row_ids = train_set)
prediction_rpart = learner_rpart$predict(task_income, row_ids = test_set)



# GLM Tuning ---------

resampling_holdout = rsmp("holdout")
resampling_holdout$instantiate(task_income)
measure = msr("classif.fbeta")
evals20 = term("evals", n_evals = 30)
tuner = tnr("random_search")


instance_glm = TuningInstance$new(
  task = task_income,
  learner = learner_glm,
  resampling = resampling,
  measures = msr("classif.fbeta"),
  param_set = tune_glm,
  terminator = evals20
)

tuner$tune(instance_glm)

# compare results
instance_rpart$result$perf
instance_glm$result$perf


# K NN -------------

# tuning
tuner = tnr("random_search")

instance_kknn = TuningInstance$new(
  task = task_income,
  learner = learner_kknn,
  resampling = resampling,
  measures = msr("classif.fbeta"),
  param_set = tune_kknn,
  terminator = evals30
)

tuner$tune(instance_kknn)
instance_kknn$result$params
learner_kknn$param_set$values
learner_rpart$param_set$values

learner_kknn$param_set$values = instance_kknn$result$params
learner_kknn$train(task_income, row_ids = train_set)
prediction_kknn = learner_kknn$predict(task_income, row_ids = test_set)
autoplot(prediction_kknn) +
  scale_fill_economist() +
  labs(title = "KKNN Prediction")

autoplot(prediction_rpart) +
  scale_fill_economist() +
  labs(title = "Tree Prediction")
