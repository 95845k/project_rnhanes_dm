

# load necessary libraries
library(readr)
library(dplyr)
library(tableone)
library(rpart)
library(bnlearn)
library(e1071)
library(randomForest)
library(ROCR)
library(ggplot2)


# create model data
model.data <- read_csv("https://raw.githubusercontent.com/95845k/project_rnhanes_dm/master/prd_code_data_transform.csv")


# re-class variable from integer to numeric
model.data$age <- as.numeric(model.data$age)


# subset numeric columns
model.data.num <- model.data[, which(sapply(model.data, FUN = class) == "numeric")]


# subset non-numeric columns and re-class to factor  
model.data.fac <- data.frame(sapply(model.data[, which(sapply(model.data, FUN = class) != "numeric")], 
                                    FUN = function(x) { as.factor(as.character(x)) }))


# re-combine dataframe
model.data <- cbind(model.data.fac, model.data.num)


# remove duplicate information
rm("model.data.fac", "model.data.num")


# rename dataframe
model.data.df <- model.data


# create tibble
model.data <- model.data %>% tbl_df


# set seed for replication of random sampling
set.seed(123)


# add index for checks
model.data$index <- 1:nrow(model.data)


# create shuffled dataset
model.data.shuffle <- sample_n(tbl = model.data, size = nrow(model.data))


# set train percentage
train.percent <- 0.70


# create training dataset
model.data.train <- model.data.shuffle[1:floor(nrow(model.data.shuffle) * train.percent), ]


# create testing dataset
model.data.test <- model.data.shuffle[ceiling(nrow(model.data.shuffle) * train.percent):
                                      nrow(model.data.shuffle), ]


# set validation percentage
validate.percent <- 0.30


# create training-base dataset
model.data.train.base <- model.data.train[1:floor(nrow(model.data.train) * (1 - validate.percent)), ]


# create training-validation dataset
model.data.train.validate <- model.data.train[ceiling(nrow(model.data.train) * (1 - validate.percent)):
                                        nrow(model.data.train), ]


# exclude non-model variables
model.data.train.base <- model.data.train.base %>% select(-index)
model.data.train.validate <- model.data.train.validate %>% select(-index)
model.data.test <- model.data.test %>% select(-index)
model.data.shuffle <- model.data.shuffle %>% select(-index)


# create vector of table one variables
tableone.vars <- model.data.train.base %>% select(-outcome) %>% names

# create table one stratified by outcome
tableone.tbl1 <- CreateTableOne(data = model.data.train.base, 
                                vars = tableone.vars, 
                                strata = "outcome")

# create csv-ready version of table one
tableone.p <- print(tableone.tbl1, showAllLevels = T, quote = F, noSpaces = T, printToggle = F)

# write to csv for further formatting
##write.csv(tableone.p, "tableone.csv")

## Logit
# create logit model from training-base dataset
model.logit <- glm(data = model.data.train.base, outcome ~ ., family = binomial(link =  "logit"))

# make predictions on training-validation dataset
pred.logit <- predict(object = model.logit, newdata = model.data.train.validate, type = "response")

# create dataframe of outcome and prediction probability
pred.logit <- data.frame(outcome = model.data.train.validate$outcome, prediction.prob = pred.logit)

# add prediction column
pred.logit <- pred.logit %>% mutate(prediction = prediction.prob > 0.50)

# add reordered outcome and prediction columns
pred.logit <- pred.logit %>% transform(outcome.reorder = plyr::mapvalues(outcome == "1", 
                                                                         from = c(TRUE, FALSE),
                                                                         to = c("1.TRUE", "2.FALSE")),
                                       prediction.reorder = plyr::mapvalues(prediction, 
                                                                            from = c(TRUE, FALSE),
                                                                            to = c("1.TRUE", "2.FALSE")))

# create confusion matrix
confmatrix.logit <- with(pred.logit, table(prediction.reorder, outcome.reorder))

# calculate accuracy
{
  if (dim(confmatrix.logit)[1] == 1 & row.names(confmatrix.logit)[1] == "2.FALSE") {
    acc.logit <- (0 + confmatrix.logit[1, 2]) / sum(colSums(confmatrix.logit))
  }
  else { 
    acc.logit <- (confmatrix.logit[1, 1] + confmatrix.logit[2, 2]) / sum(colSums(confmatrix.logit))
  }
}

# calculate error
error.logit <- 1 - acc.logit

# calculate confidence interval for accuracy
acc.ci.logit <- 1.96 * (sqrt((error.logit * acc.logit) / nrow(model.data.train.validate)))

# retrieve roc performance information
roc.logit <- with(pred.logit, prediction(predictions = prediction.prob, labels = outcome)) %>%
  performance("tpr", "fpr")

# retrieve precision recall performance information
precrec.logit <- with(pred.logit, prediction(predictions = prediction.prob, labels = outcome)) %>%
  performance("prec", "rec")

# gather roc data
roc.data.logit <- data.frame(tpr = roc.logit@y.values[[1]], fpr = roc.logit@x.values[[1]])

# gather precision recall data
precrec.data.logit <- data.frame(prec = precrec.logit@y.values[[1]], rec = precrec.logit@x.values[[1]])


## Decision Tree
# create decision tree model from training-base dataset
model.dtree <- rpart(data = model.data.train.base, outcome ~ ., method = "class")

# make predictions on training-validation dataset
pred.dtree <- predict(object = model.dtree, newdata = model.data.train.validate, type = "prob")

# create dataframe of outcome and prediction probability
pred.dtree <- data.frame(outcome = model.data.train.validate$outcome, prediction.prob = pred.dtree[, 2])

# add prediction column
pred.dtree <- pred.dtree %>% mutate(prediction = prediction.prob > 0.50)

# add reordered outcome and prediction columns
pred.dtree <- pred.dtree %>% transform(outcome.reorder = plyr::mapvalues(outcome == "1", 
                                                                         from = c(TRUE, FALSE),
                                                                         to = c("1.TRUE", "2.FALSE")),
                                       prediction.reorder = plyr::mapvalues(prediction, 
                                                                            from = c(TRUE, FALSE),
                                                                            to = c("1.TRUE", "2.FALSE")))

# create confusion matrix
confmatrix.dtree <- with(pred.dtree, table(prediction.reorder, outcome.reorder))

# calculate accuracy
{
  if (dim(confmatrix.dtree)[1] == 1 & row.names(confmatrix.dtree)[1] == "2.FALSE") {
    acc.dtree <- (0 + confmatrix.dtree[1, 2]) / sum(colSums(confmatrix.dtree))
  }
  else { 
    acc.dtree <- (confmatrix.dtree[1, 1] + confmatrix.dtree[2, 2]) / sum(colSums(confmatrix.dtree))
  }
}

# calculate error
error.dtree <- 1 - acc.dtree

# calculate confidence interval for accuracy
acc.ci.dtree <- 1.96 * (sqrt((error.dtree * acc.dtree) / nrow(model.data.train.validate)))

# retrieve roc performance information
roc.dtree <- with(pred.dtree, prediction(predictions = prediction.prob, labels = outcome)) %>%
  performance("tpr", "fpr")

# retrieve precision recall performance information
precrec.dtree <- with(pred.dtree, prediction(predictions = prediction.prob, labels = outcome)) %>%
  performance("prec", "rec")

# gather roc data
roc.data.dtree <- data.frame(tpr = roc.dtree@y.values[[1]], fpr = roc.dtree@x.values[[1]])

# gather precision recall data
precrec.data.dtree <- data.frame(prec = precrec.dtree@y.values[[1]], rec = precrec.dtree@x.values[[1]])



## create data for bnlearn algorithms
# discretize shuffle dataset to ensure same buckets are created
model.data.shuffle.d <- discretize(model.data.shuffle)

# create training dataset
model.data.train.d <- model.data.shuffle.d[1:floor(nrow(model.data.shuffle.d) * train.percent), ]

# create testing dataset
model.data.test.d <- model.data.shuffle.d[ceiling(nrow(model.data.shuffle.d) * train.percent):
                                            nrow(model.data.shuffle.d), ]
# create training-base dataset
model.data.train.base.d <- model.data.train.d[1:floor(nrow(model.data.train.d) * (1 - validate.percent)), ]

# create training-validation dataset
model.data.train.validate.d <- model.data.train.d[ceiling(nrow(model.data.train.d) * (1 - validate.percent)):
                                                    nrow(model.data.train.d), ]


## Naive Bayes
# create naive bayes model from training-base dataset
model.nb <- naive.bayes(x = model.data.train.base.d, training = "outcome")

# fit naive bayes model to training dataset
fitted.nb <- bn.fit(x = model.nb, data = model.data.train.base.d)

# make predictions on training-validation dataset
pred.nb <- predict(object = fitted.nb, data = model.data.train.validate.d)

# retrieve prediction probabilities
pred.prob.nb <- predict(object = fitted.nb, data = model.data.train.validate.d, prob = T) %>% 
  attr("prob") %>% t() %>% tbl_df() %>% .[[2]]


# create dataframe of outcome and prediction probability
pred.nb <- data.frame(outcome = model.data.train.validate.d$outcome, 
                      prediction.prob = pred.prob.nb,
                      prediction = pred.nb)

# add reordered outcome and prediction columns
pred.nb <- pred.nb %>% transform(outcome.reorder = plyr::mapvalues(outcome == "1", 
                                                                   from = c(TRUE, FALSE),
                                                                   to = c("1.TRUE", "2.FALSE")),
                                 prediction.reorder = plyr::mapvalues(prediction == "1", 
                                                                      from = c(TRUE, FALSE),
                                                                      to = c("1.TRUE", "2.FALSE")))

# create confusion matrix
confmatrix.nb <- with(pred.nb, table(prediction.reorder, outcome.reorder))

# calculate accuracy
{
  if (dim(confmatrix.nb)[1] == 1 & row.names(confmatrix.nb)[1] == "2.FALSE") {
    acc.nb <- (0 + confmatrix.nb[1, 2]) / sum(colSums(confmatrix.nb))
  }
  else { 
    acc.nb <- (confmatrix.nb[1, 1] + confmatrix.nb[2, 2]) / sum(colSums(confmatrix.nb))
  }
}

# calculate error
error.nb <- 1 - acc.nb

# calculate confidence interval for accuracy
acc.ci.nb <- 1.96 * (sqrt((error.nb * acc.nb) / nrow(model.data.train.validate)))

# retrieve roc performance information
roc.nb <- with(pred.nb, prediction(predictions = prediction.prob, labels = outcome)) %>%
  performance("tpr", "fpr")

# retrieve precision recall performance information
precrec.nb <- with(pred.nb, prediction(predictions = prediction.prob, labels = outcome)) %>%
  performance("prec", "rec")

# gather roc data
roc.data.nb <- data.frame(tpr = roc.nb@y.values[[1]], fpr = roc.nb@x.values[[1]])

# gather precision recall data
precrec.data.nb <- data.frame(prec = precrec.nb@y.values[[1]], rec = precrec.nb@x.values[[1]])



## Tree Augmented Naive Bayes
# create tree augmented naive bayes model from training-base dataset
model.tan <- tree.bayes(x = model.data.train.base.d, training = "outcome")

# fit tree augmented naive bayes model to training dataset
fitted.tan <- bn.fit(x = model.tan, data = model.data.train.base.d)

# make predictions on training-validation dataset
pred.tan <- predict(object = fitted.tan, data = model.data.train.validate.d)

# retrieve prediction probabilities
pred.prob.tan <- predict(object = fitted.tan, data = model.data.train.validate.d, prob = T) %>% 
  attr("prob") %>% t() %>% tbl_df() %>% .[[2]]

# create dataframe of outcome and prediction probability
pred.tan <- data.frame(outcome = model.data.train.validate.d$outcome, 
                       prediction.prob = pred.prob.tan,
                       prediction = pred.tan)

# add reordered outcome and prediction columns
pred.tan <- pred.tan %>% transform(outcome.reorder = plyr::mapvalues(outcome == "1", 
                                                                     from = c(TRUE, FALSE),
                                                                     to = c("1.TRUE", "2.FALSE")),
                                   prediction.reorder = plyr::mapvalues(prediction == "1", 
                                                                        from = c(TRUE, FALSE),
                                                                        to = c("1.TRUE", "2.FALSE")))

# create confusion matrix
confmatrix.tan <- with(pred.tan, table(prediction.reorder, outcome.reorder))

# calculate accuracy
{
  if (dim(confmatrix.tan)[1] == 1 & row.names(confmatrix.tan)[1] == "2.FALSE") {
    acc.tan <- (0 + confmatrix.tan[1, 2]) / sum(colSums(confmatrix.tan))
  }
  else { 
    acc.tan <- (confmatrix.tan[1, 1] + confmatrix.tan[2, 2]) / sum(colSums(confmatrix.tan))
  }
}

# calculate error
error.tan <- 1 - acc.tan

# calculate confidence interval for accuracy
acc.ci.tan <- 1.96 * (sqrt((error.tan * acc.tan) / nrow(model.data.train.validate)))

# retrieve roc performance information
roc.tan <- with(pred.tan, prediction(predictions = prediction.prob, labels = outcome)) %>%
  performance("tpr", "fpr")

# retrieve precision recall performance information
precrec.tan <- with(pred.tan, prediction(predictions = prediction.prob, labels = outcome)) %>%
  performance("prec", "rec")

# gather roc data
roc.data.tan <- data.frame(tpr = roc.tan@y.values[[1]], fpr = roc.tan@x.values[[1]])

# gather precision recall data
precrec.data.tan <- data.frame(prec = precrec.tan@y.values[[1]], rec = precrec.tan@x.values[[1]])


## Support Vector Machines
# create svm model from training-base dataset
model.svm <- svm(data = model.data.train.base, outcome ~ ., probability = T)

# make predictions on training-validation dataset
pred.svm <- predict(object = model.svm, newdata = model.data.train.validate, probability = T)

# retrieve prediction probabilities
pred.prob.svm <- pred.svm %>% attr("prob") %>% tbl_df() %>% .[[2]]

# create dataframe of outcome and prediction probability
pred.svm <- data.frame(outcome = model.data.train.validate.d$outcome, 
                       prediction.prob = pred.prob.svm,
                       prediction = pred.svm)


# add reordered outcome and prediction columns
pred.svm <- pred.svm %>% transform(outcome.reorder = plyr::mapvalues(outcome == "1", 
                                                                     from = c(TRUE, FALSE),
                                                                     to = c("1.TRUE", "2.FALSE")),
                                   prediction.reorder = plyr::mapvalues(prediction == "1", 
                                                                        from = c(TRUE, FALSE),
                                                                        to = c("1.TRUE", "2.FALSE")))

# create confusion matrix
confmatrix.svm <- with(pred.svm, table(prediction.reorder, outcome.reorder))

# calculate accuracy
{
  if (dim(confmatrix.svm)[1] == 1 & row.names(confmatrix.svm)[1] == "2.FALSE") {
    acc.svm <- (0 + confmatrix.svm[1, 2]) / sum(colSums(confmatrix.svm))
  }
  else { 
    acc.svm <- (confmatrix.svm[1, 1] + confmatrix.svm[2, 2]) / sum(colSums(confmatrix.svm))
  }
}

# calculate error
error.svm <- 1 - acc.svm

# calculate confidence interval for accuracy
acc.ci.svm <- 1.96 * (sqrt((error.svm * acc.svm) / nrow(model.data.train.validate)))

# retrieve roc performance information
roc.svm <- with(pred.svm, prediction(predictions = prediction.prob, labels = outcome)) %>%
  performance("tpr", "fpr")

# retrieve precision recall performance information
precrec.svm <- with(pred.svm, prediction(predictions = prediction.prob, labels = outcome)) %>%
  performance("prec", "rec")

# gather roc data
roc.data.svm <- data.frame(tpr = roc.svm@y.values[[1]], fpr = roc.svm@x.values[[1]])

# gather precision recall data
precrec.data.svm <- data.frame(prec = precrec.svm@y.values[[1]], rec = precrec.svm@x.values[[1]])


## Random Forest
# create rf model from training-base dataset
model.rf <- randomForest(data = model.data.train.base, outcome ~ .)

# make predictions on training-validation dataset
pred.rf <- predict(object = model.rf, newdata = model.data.train.validate, type = "prob")

# create dataframe of outcome and prediction probability
pred.rf <- data.frame(outcome = model.data.train.validate$outcome, prediction.prob = pred.rf[, 2])

# add prediction column
pred.rf <- pred.rf %>% mutate(prediction = prediction.prob > 0.50)

# add reordered outcome and prediction columns
pred.rf <- pred.rf %>% transform(outcome.reorder = plyr::mapvalues(outcome == "1", 
                                                                   from = c(TRUE, FALSE),
                                                                   to = c("1.TRUE", "2.FALSE")),
                                 prediction.reorder = plyr::mapvalues(prediction, 
                                                                      from = c(TRUE, FALSE),
                                                                      to = c("1.TRUE", "2.FALSE")))

# create confusion matrix
confmatrix.rf <- with(pred.rf, table(prediction.reorder, outcome.reorder))

# calculate accuracy
{
  if (dim(confmatrix.rf)[1] == 1 & row.names(confmatrix.rf)[1] == "2.FALSE") {
    acc.rf <- (0 + confmatrix.rf[1, 2]) / sum(colSums(confmatrix.rf))
  }
  else { 
    acc.rf <- (confmatrix.rf[1, 1] + confmatrix.rf[2, 2]) / sum(colSums(confmatrix.rf))
  }
}

# calculate error
error.rf <- 1 - acc.rf

# calculate confidence interval for accuracy
acc.ci.rf <- 1.96 * (sqrt((error.rf * acc.rf) / nrow(model.data.train.validate)))

# retrieve roc performance information
roc.rf <- with(pred.rf, prediction(predictions = prediction.prob, labels = outcome)) %>%
  performance("tpr", "fpr")

# retrieve precision recall performance information
precrec.rf <- with(pred.rf, prediction(predictions = prediction.prob, labels = outcome)) %>%
  performance("prec", "rec")

# gather roc data
roc.data.rf <- data.frame(tpr = roc.rf@y.values[[1]], fpr = roc.rf@x.values[[1]])

# gather precision recall data
precrec.data.rf <- data.frame(prec = precrec.rf@y.values[[1]], rec = precrec.rf@x.values[[1]])



## All Models
# create accuracy, confidence interval table
vec.logit <- c("logit", acc.logit - acc.ci.logit, acc.logit, acc.logit + acc.ci.logit)
vec.dtree <- c("dtree", acc.dtree - acc.ci.dtree, acc.dtree, acc.dtree + acc.ci.dtree)
vec.nb <- c("nb", acc.nb - acc.ci.nb, acc.nb, acc.nb + acc.ci.nb)
vec.tan <- c("tan-nb", acc.tan - acc.ci.tan, acc.tan, acc.tan + acc.ci.tan)
vec.svm <- c("svm", acc.svm - acc.ci.svm, acc.svm, acc.svm + acc.ci.svm)
vec.rf <- c("rf", acc.rf - acc.ci.rf, acc.rf, acc.rf + acc.ci.rf)
df.acc <- data.frame(rbind(vec.logit, vec.dtree, vec.nb, vec.tan, vec.svm, vec.rf), stringsAsFactors = F)
names(df.acc) <- c("model", "acc_low_c.i.", "acc", "acc_high_c.i.")
df.acc$acc_low_c.i. <- round(as.numeric(df.acc$acc_low_c.i.), 5)
df.acc$acc <- round(as.numeric(df.acc$acc), 5)
df.acc$acc_high_c.i. <- round(as.numeric(df.acc$acc_high_c.i.), 5)
df.acc <- arrange(df.acc, desc(acc_low_c.i.))

# create ROC curves for machine learning models
roc.data.logit$model <- "Logistic Regression"
roc.data.dtree$model <- "Decision Tree"
roc.data.nb$model <- "Naive Bayes"
roc.data.tan$model <- "Tree Augmented Naive Bayes"
roc.data.svm$model <- "Support Vector Machines"
roc.data.rf$model <- "Random Forest"
roc.data.all <- rbind(roc.data.logit, roc.data.dtree, roc.data.nb, roc.data.tan, roc.data.svm, roc.data.rf)
roc.plot <- ggplot(data = roc.data.all, mapping = aes(y = tpr, x = fpr, color = model)) + 
  geom_line() + labs(y = "True Positive Rate", x = "False Positive Rate") +
  ggtitle("ROC Curves") + scale_color_discrete(name = "Model")

# create precision-recall curves for machine learning models
precrec.data.logit$model <- "Logistic Regression"
precrec.data.dtree$model <- "Decision Tree"
precrec.data.nb$model <- "Naive Bayes"
precrec.data.tan$model <- "Tree Augmented Naive Bayes"
precrec.data.svm$model <- "Support Vector Machines"
precrec.data.rf$model <- "Random Forest"
precrec.data.all <- rbind(precrec.data.logit, precrec.data.dtree, 
                          precrec.data.nb, precrec.data.tan,
                          precrec.data.svm, precrec.data.rf)
precrec.plot <- ggplot(data = precrec.data.all, mapping = aes(y = prec, x = rec, color = model)) + 
  geom_line() + labs(y = "Precision", x = "Recall") +
  ggtitle("Precision Recall Curves") + scale_color_discrete(name = "Model")

