
# summarize
model.data %>% summary

# check pregnancy filter
with(model.data, table(preg.label, preg.filter))
model.data %>% select(preg.label, preg.filter) %>% summary

# check diabetes filter
with(model.data, table(dm.label, dm.filter))
model.data %>% select(dm.label, dm.filter) %>% summary

# check glucose filter
with(model.data, table(gluc1.label, gluc1.filter))
with(model.data, table(gluc2.label, gluc2.filter))
with(model.data, table(gluc3.label, gluc3.filter))
with(model.data, table(as.factor(gluc.filter)))

# count down from model.data.import to model.data
sum(model.data.unfiltered$age.value < 20) #38,298 --> 43,793
sum(model.data.unfiltered$age.value >= 20 & model.data.unfiltered$preg.label == "Yes", na.rm = T) #1,416 --> 42,377
sum(model.data.unfiltered$age.value >= 20 & is.na(model.data.unfiltered$dm.label)) #3 --> 42,374
sum(model.data.unfiltered$age.value >= 20 & model.data.unfiltered$dm.label != "No", na.rm = T) #5,782 
sum(model.data.unfiltered$age.value >= 20 & model.data.unfiltered$dm.label != "No" &
      model.data.unfiltered$preg.label == "Yes", na.rm = T) #28; 5,782 - 28 = 5,754 --> 36,620
sum(model.data.unfiltered$age.value >= 20 & (is.na(model.data.unfiltered$dm.label) == F) & 
      (is.na(model.data.unfiltered$gluc1.label) & 
         is.na(model.data.unfiltered$gluc2.label) & 
         is.na(model.data.unfiltered$gluc3.label))) #4,234

sum(model.data.unfiltered$age.value >= 20 & (is.na(model.data.unfiltered$dm.label) == F) & 
      (is.na(model.data.unfiltered$gluc1.label) & 
         is.na(model.data.unfiltered$gluc2.label) & 
         is.na(model.data.unfiltered$gluc3.label)) &
      model.data.unfiltered$dm.label != "No", na.rm = T) #552

sum(model.data.unfiltered$age.value >= 20 & (is.na(model.data.unfiltered$dm.label) == F) & 
      (is.na(model.data.unfiltered$gluc1.label) & 
         is.na(model.data.unfiltered$gluc2.label) & 
         is.na(model.data.unfiltered$gluc3.label)) &
      model.data.unfiltered$preg.label == "Yes", na.rm = T) #157

sum(model.data.unfiltered$age.value >= 20 & (is.na(model.data.unfiltered$dm.label) == F) & 
      (is.na(model.data.unfiltered$gluc1.label) & 
         is.na(model.data.unfiltered$gluc2.label) & 
         is.na(model.data.unfiltered$gluc3.label)) &
      model.data.unfiltered$preg.label == "Yes" & 
      model.data.unfiltered$dm.label != "No", na.rm = T) # 2; 4,234 - 552 - 157 + 2 = 3,527 --> 33,093

# check glucose labels
with(model.data, table(gluc1.label, lab1.diabetes))
with(model.data, table(gluc2.label, lab2.diabetes))
with(model.data, table(gluc3.label, lab3.diabetes))

# check outcome variable
with(model.data, table(outcome, dm.label))
with(model.data, table(outcome, lab.diabetes))
with(model.data, table(outcome, dm.label, lab.diabetes))

# summarize model data
model.data %>% summary

# check gender categories
with(model.data, table(gender.label, gender.cat))

# check race categories
with(model.data, table(race.label, race.cat))

# check education categories
with(model.data, table(educ.label, educ.cat))

# check family history relabel
with(model.data, table(hist.label, hist.cat))

# check blood pressure relabel
with(model.data, table(bp.label, bp.cat))

# check cholesterol relabel
with(model.data, table(chol.label, chol.cat))

# check smoking relabel
with(model.data, table(smoke.label, smoke.cat))
with(model.data, table(smoke.cat, outcome))

# check age categories
model.data %>% select(age.value, age.cat) %>% group_by(age.cat) %>% 
  summarise(min.age = min(age.value), max.age = max(age.value))

# check bmi categories
model.data %>% select(bmi.value, bmi.cat) %>% group_by(bmi.cat) %>% 
  summarise(min.bmi = min(bmi.value), max.bmi = max(bmi.value))

# check sleep values
summary(as.factor(as.character(model.data$sleep.value)))
summary(as.factor(model.data$sleep.value))

# summarize model data
model.data %>% summary
model.data$educ.label %>% summary
# education 47 NAs (Don't Know 32, Refused 14) [0.1%]
# fam.hist.diabetes 671 NAs (Don't Know 668, Refused 2) [2%; just actually don't know]
# bmi.category 546 NAs -- straight up
# waist.size 1,338 NAs -- straight up
# high.blood.pressure 159 NAs (Don't Know 45) [0.1%; maybe just actually don't know]
# high.cholesterol 8,255 NAs (Don't Know 168) [0.5%; maybe just actually don't know]
# alcohol.num.drinks 12,137 NAs (Don't Know 34, Refused 2) [0.1%]
# smoker 17,736 NAs (Refused 2) [0.0%]
# sleep.num.hours 11,291 NAs (Don't Know 40, Refused 2) [0.1%]

# summarize complete cases model data
model.data.cc %>% summary

# summarize imputed model data
model.data.imp.c1 %>% summary
model.data.imp.c2 %>% summary
model.data.imp.c3 %>% summary

# check assigned imputed values
column.check <- sample(imputed.vars$variable, size = 1)
record.check <- sample(which(is.na(model.data[, column.check]) == T), size = 1)
rbind(
  column.check,
  record.check,
  original.value = model.data[record.check, column.check], 
  imp.1 = as.character(model.data.imp.c1[record.check, column.check]), 
  imp.2 = as.character(model.data.imp.c2[record.check, column.check]), 
  imp.3 = as.character(model.data.imp.c3[record.check, column.check]),
  imp.assign = as.character(model.data.imp[record.check, column.check]))






