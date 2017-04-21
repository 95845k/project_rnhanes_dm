
# check integer factors
table(model.data$outcome, model.data.fac$outcome)
table(model.data$fam.hist.diabetes, model.data.fac$fam.hist.diabetes)
table(model.data$high.blood.pressure, model.data.fac$high.blood.pressure)
table(model.data$high.cholesterol, model.data.fac$high.cholesterol)
table(model.data$smoker, model.data.fac$smoker)

# check shuffling
head(model.data$index, 10)
head(model.data.shuffle$index, 10)
tail(model.data$index, 10)
tail(model.data.shuffle$index, 10)

# check row for train/test data entirety
nrow(model.data) - nrow(model.data.train) - nrow(model.data.test)

# check train/test data uniqueness
left_join(x = model.data.train[, c("index", "outcome")],
          y = model.data.test[, c("index", "outcome")],
          by = "index") %>% select(outcome.y) %>% is.na() %>% unique()

left_join(x = model.data.test[, c("index", "outcome")],
          y = model.data.train[, c("index", "outcome")],
          by = "index") %>% select(outcome.y) %>% is.na() %>% unique()

# check row for train-base/train-validate data entirety
nrow(model.data.train) - nrow(model.data.train.base) - nrow(model.data.train.validate)

# check train-base/train-validate data uniqueness
left_join(x = model.data.train.base[, c("index", "outcome")],
          y = model.data.train.validate[, c("index", "outcome")],
          by = "index") %>% select(outcome.y) %>% is.na() %>% unique()

left_join(x = model.data.train.validate[, c("index", "outcome")],
          y = model.data.train.base[, c("index", "outcome")],
          by = "index") %>% select(outcome.y) %>% is.na() %>% unique()

# check train/validate/test data percentages
round(nrow(model.data.train)/nrow(model.data), 2)
round(nrow(model.data.test)/nrow(model.data), 2)
round(nrow(model.data.train.base)/nrow(model.data.train), 2)
round(nrow(model.data.train.validate)/nrow(model.data.train), 2)
round(nrow(model.data.train.base)/nrow(model.data), 2)
round(nrow(model.data.train.validate)/nrow(model.data), 2)

# check percent of records with outcome equal to true
round(sum(model.data.train$outcome == 1) / nrow(model.data.train) * 100, 1)
round(sum(model.data.test$outcome == 1) / nrow(model.data.test) * 100, 1)
round(sum(model.data.train.base$outcome == 1) / nrow(model.data.train.base) * 100, 1)
round(sum(model.data.train.validate$outcome == 1) / nrow(model.data.train.validate) * 100, 1)

# check reordered columns
with(pred.logit, table(outcome.reorder, outcome))
with(pred.logit, table(prediction.reorder, prediction))
with(pred.dtree, table(outcome.reorder, outcome))
with(pred.dtree, table(prediction.reorder, prediction))
with(pred.nb, table(outcome.reorder, outcome))
with(pred.nb, table(prediction.reorder, prediction))
with(pred.tan, table(outcome.reorder, outcome))
with(pred.tan, table(prediction.reorder, prediction))
with(pred.svm, table(outcome.reorder, outcome))
with(pred.svm, table(prediction.reorder, prediction))
with(pred.rf, table(outcome.reorder, outcome))
with(pred.rf, table(prediction.reorder, prediction))

# check prediction 
table(pred.dtree$prediction, predict(object = model.dtree, newdata = model.data.train.validate, type = "class"))
with(pred.nb, table(prediction, prediction.prob > 0.50))
with(pred.tan, table(prediction, prediction.prob > 0.50))
with(pred.svm, table(prediction, prediction.prob > 0.50))
with(pred.rf, table(prediction, prediction.prob > 0.50))

# create simple plots
roc.logit %>% plot
precrec.logit %>% plot
roc.dtree %>% plot
precrec.dtree %>% plot
roc.nb %>% plot
precrec.nb %>% plot
roc.tan %>% plot
precrec.tan %>% plot

# highest accuracy
round(max((acc.logit + acc.ci.logit), (acc.dtree + acc.ci.dtree), 
          (acc.nb + acc.ci.nb), (acc.tan + acc.ci.tan)) * 100, 3)

# lowest accuracy
round(min((acc.logit - acc.ci.logit), (acc.dtree - acc.ci.dtree), 
                     (acc.nb - acc.ci.nb), (acc.tan - acc.ci.tan)) * 100, 3)



