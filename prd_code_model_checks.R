
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

# check confusion matrices
confmatrix.logit
confmatrix.dtree
confmatrix.nb
confmatrix.tan
confmatrix.svm
confmatrix.rf

# check roc plot
roc.plot

# check precision-recall plot
precrec.plot

# calculate auc
auc.logit <- with(pred.logit, prediction(predictions = prediction.prob, labels = outcome)) %>%
  performance("auc")
auc.logit@y.values

auc.nb <- with(pred.nb, prediction(predictions = prediction.prob, labels = outcome)) %>%
  performance("auc")
auc.nb@y.values

auc.tan <- with(pred.tan, prediction(predictions = prediction.prob, labels = outcome)) %>%
  performance("auc")
auc.tan@y.values

# check logistic regression summary
model.logit %>% summary
df.logit <- data.frame(cbind(row.names(coef(summary(model.logit))), coef(summary(model.logit))))
names(df.logit) <- c("variable", "estimate", "std.error", "z.value", "p.value")
df.logit$estimate <- round(as.numeric(as.character(df.logit$estimate)), 5)
df.logit$exp.estimate <- round(exp(df.logit$estimate), 5)
df.logit$p.value <- round(as.numeric(as.character(df.logit$p.value)), 5)
df.logit %>% tbl_df %>% select(variable, exp.estimate, p.value) %>% filter(p.value < 0.05)

model.select.logit <- glm(data = model.data.train.base, outcome ~ age + race + education +
                                                                  fam.hist.diabetes + high.blood.pressure +
                                                                  waist.size,
                          family = binomial(link =  "logit"))

model.select.logit %>% summary  
df.select.logit <- data.frame(cbind(row.names(coef(summary(model.select.logit))), coef(summary(model.select.logit))))
names(df.select.logit) <- c("variable", "estimate", "std.error", "z.value", "p.value")
df.select.logit$estimate <- round(as.numeric(as.character(df.select.logit$estimate)), 5)
df.select.logit$exp.estimate <- round(exp(df.select.logit$estimate), 5)
df.select.logit$p.value <- round(as.numeric(as.character(df.select.logit$p.value)), 5)
df.select.logit %>% tbl_df %>% select(variable, exp.estimate, p.value) %>% filter(p.value < 0.05)
df.select.logit %>% tbl_df %>% select(variable, exp.estimate, p.value) %>% filter(p.value < 0.05)  

# check logit threshold values
data.frame(tpr = roc.logit@y.values[[1]], 
           fpr = roc.logit@x.values[[1]], 
           cutoff = roc.logit@alpha.values[[1]]) %>% 
  mutate(metric = round(tpr / fpr, 5)) %>% 
  arrange(desc(metric)) %>% 
  filter(tpr > 0.75, fpr < 0.305)


