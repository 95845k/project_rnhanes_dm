

# load necessary libraries
library(readr)
library(dplyr)
library(mice)


# create model data
model.data <- read_csv("https://raw.githubusercontent.com/95845k/project_rnhanes_dm/master/prd_code_data_extract.csv")


# re-class variable from character to numeric
model.data$sleep.value <- as.numeric(model.data$sleep.value)


# subset numeric columns
model.data.num <- model.data[, which(sapply(model.data, FUN = class) != "character")]


# re-class variables from character to factor  
model.data.fac <- data.frame(sapply(model.data[, which(sapply(model.data, FUN = class) == "character")], 
                         FUN = as.factor))


# re-combine dataframe
model.data <- cbind(model.data.fac, model.data.num)


# remove duplicate information
rm("model.data.fac", "model.data.num")


# preserve model.data.unfiltered
model.data.unfiltered <- model.data


# reset dataframe
##model.data <- model.data.unfiltered


# filter out age < 20
model.data <- model.data %>% filter(age.value >= 20)


# filter out pregnant women
model.data$preg.filter <- as.character(model.data$preg.label)
model.data[which(is.na(model.data$preg.filter) == T), "preg.filter"] <- "Missing"
model.data$preg.filter <- as.factor(model.data$preg.filter)
model.data <- model.data %>% filter(preg.filter != "Yes")


# filter to only no diabetes dx for first part of outcome variable
model.data$dm.filter <- as.character(model.data$dm.label)
model.data[which(is.na(model.data$dm.filter) == T), "dm.filter"] <- "Missing"
model.data$dm.filter <- as.factor(model.data$dm.filter)
model.data <- model.data %>% filter(dm.filter == "No")


# filter out nas for second part of outcome variable
model.data$gluc1.filter <- as.character(model.data$gluc1.label)
model.data[which(is.na(model.data$gluc1.filter) == T), "gluc1.filter"] <- "Missing"
model.data$gluc1.filter <- as.factor(model.data$gluc1.filter)
model.data$gluc2.filter <- as.character(model.data$gluc2.label)
model.data[which(is.na(model.data$gluc2.filter) == T), "gluc2.filter"] <- "Missing"
model.data$gluc2.filter <- as.factor(model.data$gluc2.filter)
model.data$gluc3.filter <- as.character(model.data$gluc3.label)
model.data[which(is.na(model.data$gluc3.filter) == T), "gluc3.filter"] <- "Missing"
model.data$gluc3.filter <- as.factor(model.data$gluc3.filter)

model.data <- model.data %>% mutate(
  gluc1.filter = as.numeric(as.character(plyr::mapvalues(
    gluc1.filter,
    from = c("No Diabetes", "Pre-Diabetes", "Diabetes", "Missing"),
    to = c(0, 0, 0, 1)))),
  gluc2.filter = as.numeric(as.character(plyr::mapvalues(
    gluc2.filter,
    from = c("No Diabetes", "Pre-Diabetes", "Diabetes", "Missing"),
    to = c(0, 0, 0, 1)))),
  gluc3.filter = as.numeric(as.character(plyr::mapvalues(
    gluc3.filter,
    from = c("No Diabetes", "Pre-Diabetes", "Diabetes", "Missing"),
    to = c(0, 0, 0, 1)))))


model.data <- model.data %>% mutate(
  gluc.filter = 
    rowSums(model.data[, c("gluc1.filter", "gluc2.filter", "gluc3.filter")])
)

model.data <- model.data %>% filter(gluc.filter != 3)


## create primary outcome variable: predict undiagnosed diabetes
# reassign lab data to 1s and 0s
model.data <- model.data %>% mutate(
  lab1.diabetes = as.numeric(as.character(plyr::mapvalues(
    gluc1.label,
    from = c("No Diabetes", "Pre-Diabetes", "Diabetes"),
    to = c(0, 0, 1)))),
  lab2.diabetes = as.numeric(as.character(plyr::mapvalues(
    gluc2.label,
    from = c("No Diabetes", "Pre-Diabetes", "Diabetes"),
    to = c(0, 0, 1)))),
  lab3.diabetes = as.numeric(as.character(plyr::mapvalues(
    gluc3.label,
    from = c("No Diabetes", "Pre-Diabetes", "Diabetes"),
    to = c(0, 0, 1)))))


# sum 1s and 0s 
model.data <- model.data %>% mutate(
  lab.diabetes = 
    rowSums(model.data[, c("lab1.diabetes", "lab2.diabetes", "lab3.diabetes")], na.rm = T)
)


# create undiagnosed diabetes outcome variable 
model.data$outcome <- "0"
model.data[model.data$dm.label == "No" & model.data$lab.diabetes > 0, "outcome"] <- "1"
model.data$outcome <- as.factor(model.data$outcome)


# relabel race/ethnicity
model.data <- model.data %>% mutate(race.cat = 
                                      plyr::mapvalues(race.label,
                                                      from = c("Mexican American", "Other Hispanic", 
                                                               "Non-Hispanic White", "Non-Hispanic Black",
                                                               "Other Race - Including Multi-Racial"),
                                                      to = c("Hispanic", "Hispanic",
                                                             "White", "Black",
                                                             "Other")))


# categorize education
model.data <- model.data %>% mutate(educ.cat = 
                                      plyr::mapvalues(educ.label,
                                                      from = c("Less Than 9th Grade", 
                                                               "9-11th Grade (Includes 12th grade with no diploma)",
                                                               "High School Grad/GED or Equivalent", 
                                                               "Some College or AA degree",
                                                               "College Graduate or above",
                                                               "Refused", "Don't Know"),
                                                      to = c("1-No HS Degree", "1-No HS Degree",
                                                             "2-HS Degree", "2-HS Degree",
                                                             "3-College Degree",
                                                             NA, NA)))


# relabel family history of diabetes
model.data <- model.data %>% mutate(hist.cat = 
                                      as.factor(plyr::mapvalues(hist.label,
                                                                from = c("Yes", "No", "Refused", "Don't know"),
                                                                to = c("1", "0", NA, NA))))


# relabel blood pressure
model.data <- model.data %>% mutate(bp.cat = 
                                      as.factor(plyr::mapvalues(bp.label,
                                                                from = c("Yes", "No", "Don't know"),
                                                                to = c("1", "0", NA))))


# relabel cholesterol
model.data <- model.data %>% mutate(chol.cat = 
                                      as.factor(plyr::mapvalues(chol.label,
                                                                from = c("Yes", "No", "Refused", "Don't know"),
                                                                to = c("1", "0", NA, NA))))


# relabel smoking
model.data <- model.data %>% mutate(smoke.cat = 
                                      as.factor(plyr::mapvalues(smoke.label,
                                                                from = c("Don't know", "Every day", "Not at all",
                                                                         "Refused", "Some days"),
                                                                to = c(NA, "1", "0", NA, "1"))))


# categorize age
model.data$age.cat <- with(model.data, 
                           cut(age.value, 
                               breaks = c(0, 35, 45, 55, 65, 75, 200), 
                               right = F,
                               labels = c("20-34", "35-44", "45-54", 
                                          "55-64", "65-74", ">75")))


# categorize bmi
model.data <- model.data %>% mutate(bmi.cat = 
                                      cut(bmi.value, 
                                          breaks = c(0, 24.999, 30, 10000),
                                          right = F,
                                          labels = c("1-Normal", "2-Overweight", "3-Obese")))


# create model feature dataset
model.data <- model.data %>% select(outcome, age.cat, gender.label, race.cat, educ.cat,
                                    hist.cat, bmi.cat, waist.value, bp.cat, chol.cat, 
                                    alc.value, smoke.cat, sleep.value)


# rename features
names(model.data) <- c("outcome", "age", "gender", "race", "education",
                       "fam.hist.diabetes", "bmi.category", "waist.size", "high.blood.pressure", "high.cholesterol",
                       "alcohol.num.drinks", "smoker", "sleep.num.hours")


# create complete cases dataset
model.data.cc <- model.data[complete.cases(model.data), ]


# impute missing values
model.data.imp <- mice(data = model.data, m = 3, maxit = 3, seed = 0)


# complete imputed datasets
model.data.imp.c1 <- complete(model.data.imp, 1)
model.data.imp.c2 <- complete(model.data.imp, 2)
model.data.imp.c3 <- complete(model.data.imp, 3)


## pool imputed datasets and determine value for each record
# select imputed variables
imputed.vars <- data.frame(missing = sapply(model.data, FUN = function(x) { sum(is.na(x)) }))
imputed.vars <- data.frame(cbind(variable = as.character(row.names(imputed.vars)), missing = imputed.vars))
imputed.vars$variable <- as.character(imputed.vars$variable)
non.imputed.vars <- imputed.vars %>% filter(missing == 0) %>% select(variable)
imputed.vars <- imputed.vars %>% filter(missing > 0) %>% select(variable)


# create mode function
# http://stackoverflow.com/questions/2547402/is-there-a-built-in-function-for-finding-the-mode
fun.mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}


# combine completed imputed datasets for each variable and assign class/value for each record
fun.impute.assign <- function(imputed.vars) {
  for (i in 1:nrow(imputed.vars)) {
    var.name = imputed.vars$variable[i]
    if (class(model.data.imp.c1[ , var.name]) == "factor") {
      df <- data.frame(t(cbind(
        as.character(model.data.imp.c1[ , var.name]), 
        as.character(model.data.imp.c2[ , var.name]), 
        as.character(model.data.imp.c3[ , var.name]))), stringsAsFactors = F)
      df <- data.frame(var.name = sapply(df, FUN = fun.mode))
      }
    else {
      df <- data.frame(t(cbind(
        model.data.imp.c1[ , var.name], 
        model.data.imp.c2[ , var.name], 
        model.data.imp.c3[ , var.name])))
      df <- data.frame(var.name = sapply(df, FUN = function(x) { round(mean(x), 1) }))
      }
    if (i == 1) {
      imputed.vars.df <- df
      names(imputed.vars.df) <- imputed.vars$variable[1]
      }
    else {
      imputed.vars.df <- cbind(imputed.vars.df, df)
      names(imputed.vars.df)[i] <- imputed.vars$variable[i]
      }
    }
  return(imputed.vars.df)
}
  
model.data.imp.assign <- fun.impute.assign(imputed.vars = imputed.vars)


# re-join data for complete model.data dataset
model.data.imp <- model.data[, non.imputed.vars$variable]
model.data.imp <- cbind(model.data.imp, model.data.imp.assign)


# copy and rename final model dataset
model.data.final <- model.data.imp


# export final model dataset
write.csv(model.data.final, "prd_code_data_transform.csv")


# export complete cases model dataset
write.csv(model.data.cc, "prd_code_data_transform_cc.csv")





