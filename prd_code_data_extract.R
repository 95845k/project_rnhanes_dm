

# load necessary libraries
library(RNHANES)
library(dplyr)
library(stringr)


# create file object for file search
files <- nhanes_data_files()


### ----------------------------------
### DIABETES
# search for diabetes files
dm.search <- nhanes_search(files, "diabetes")


# function to retrieve, subset, and transform diabetes data
fun.dm.data <- 
        function(search) {
        for (i in 1:nrow(search)) {
                df.data.load <- nhanes_load_data(search$data_file_name[i], search$cycle[i])
                df.data.load <- df.data.load %>% select(SEQN, cycle, DIQ010) %>%
                                        mutate(SEQN = as.factor(as.character(SEQN)),
                                               cycle = as.factor(as.character(cycle)),
                                                join.id = paste(SEQN, cycle, sep = " | "),
                                                dm.factor = as.factor(as.character(DIQ010)),
                                                dm.label = as.factor(plyr::mapvalues(DIQ010,
                                                            from = c(1, 2, 3, 7, 9),
                                                            to = c("Yes", "No", "Borderline", "Refused", "Don't know"))))
                if (i == 1) {
                        df.data <- df.data.load
                }
                else {
                        df.data <- rbind(df.data, df.data.load)
                }
        }
        return(df.data)
        }


# retrieve, subset, and transform diabetes data
dm.data <- arrange(fun.dm.data(dm.search), cycle, SEQN)


### ----------------------------------
### PLASMA FASTING GLUCOSE
# search for plasma fasting glucose files
gluc1.search <- arrange(nhanes_search(files, "plasma fasting glucose"), cycle)


# function to retrieve, subset, and transform plasma fasting glucose data
fun.gluc1.data <- 
        function(search) {
                for (i in 1:nrow(search)) {
                        df.data.load <- nhanes_load_data(search$data_file_name[i], search$cycle[i])
                        df.data.load <- df.data.load %>% select(SEQN, cycle, LBXGLU) %>%
                                mutate(SEQN = as.factor(as.character(SEQN)),
                                       cycle = as.factor(as.character(cycle)),
                                       join.id = paste(SEQN, cycle, sep = " | "),
                                       gluc1.value = LBXGLU,
                                       gluc1.label = cut(LBXGLU, 
                                                        breaks = c(0, 99.9, 125.9, 10000),
                                                        labels = c("No Diabetes", "Pre-Diabetes", "Diabetes")))
                        if (i == 1) {
                                df.data <- df.data.load
                        }
                        else {
                                df.data <- rbind(df.data, df.data.load)
                        }
                }
                return(df.data)
        }


# retrieve, subset, and transform plasma fasting glucose data
gluc1.data <- arrange(fun.gluc1.data(gluc1.search), cycle, SEQN)


### ----------------------------------
### GLYCOHEMOGLOBIN
# search for glycohemoglobin files
gluc2.search <- arrange(nhanes_search(files, "glycohemoglobin"), cycle)


# filter glycohemoglobin files
gluc2.search <- gluc2.search %>% filter(data_file_description == "Glycohemoglobin")


# function to retrieve, subset, and transform glycohemoglobin data
fun.gluc2.data <- 
  function(search) {
    for (i in 1:nrow(search)) {
      df.data.load <- nhanes_load_data(search$data_file_name[i], search$cycle[i])
      df.data.load <- df.data.load %>% select(SEQN, cycle, LBXGH) %>%
        mutate(SEQN = as.factor(as.character(SEQN)),
               cycle = as.factor(as.character(cycle)),
               join.id = paste(SEQN, cycle, sep = " | "),
               gluc2.value = LBXGH,
               gluc2.label = cut(LBXGH, 
                                 breaks = c(0, 5.69, 6.49, 10000),
                                 labels = c("No Diabetes", "Pre-Diabetes", "Diabetes")))
      if (i == 1) {
        df.data <- df.data.load
      }
      else {
        df.data <- rbind(df.data, df.data.load)
      }
    }
    return(df.data)
  }


# retrieve, subset, and transform glycohemoglobin data
gluc2.data <- arrange(fun.gluc2.data(gluc2.search), cycle, SEQN)


### ----------------------------------
### ORAL GLUCOSE
# search for oral glucose files
gluc3.search <- arrange(nhanes_search(files, "oral glucose"), cycle)


# function to retrieve, subset, and transform oral glucose data
fun.gluc3.data <- 
  function(search) {
    for (i in 1:nrow(search)) {
      df.data.load <- nhanes_load_data(search$data_file_name[i], search$cycle[i])
      df.data.load <- df.data.load %>% select(SEQN, cycle, LBXGLT) %>%
        mutate(SEQN = as.factor(as.character(SEQN)),
               cycle = as.factor(as.character(cycle)),
               join.id = paste(SEQN, cycle, sep = " | "),
               gluc3.value = LBXGLT,
               gluc3.label = cut(LBXGLT, 
                                 breaks = c(0, 139.9, 199.9, 10000),
                                 labels = c("No Diabetes", "Pre-Diabetes", "Diabetes")))
      if (i == 1) {
        df.data <- df.data.load
      }
      else {
        df.data <- rbind(df.data, df.data.load)
      }
    }
    return(df.data)
  }


# retrieve, subset, and transform oral glucose data
gluc3.data <- arrange(fun.gluc3.data(gluc3.search), cycle, SEQN)


### ----------------------------------
### MEDICAL CONDITIONS/FAMILY HISTORY
# search for medical conditions files
hist.search <- arrange(nhanes_search(files, "medical conditions"), cycle)


# function to retrieve, subset, and transform family history data
fun.hist.data <- 
        function(search) {
                for (i in 1:nrow(search)) {
                        df.data.load <- nhanes_load_data(search$data_file_name[i], search$cycle[i])
                        if (i < 4) {
                        df.data.load <- df.data.load %>% select(SEQN, cycle, MCQ250A) %>%
                                mutate(SEQN = as.factor(as.character(SEQN)),
                                       cycle = as.factor(as.character(cycle)),
                                       join.id = paste(SEQN, cycle, sep = " | "),
                                       hist.factor = as.factor(as.character(MCQ250A)),
                                       hist.label = as.factor(plyr::mapvalues(MCQ250A,
                                                                            from = c(1, 2, 7, 9),
                                                                            to = c("Yes", "No", "Refused", "Don't know"))))
                        }
                        else {
                                df.data.load <- df.data.load %>% select(SEQN, cycle, MCQ300C) %>%
                                        mutate(SEQN = as.factor(as.character(SEQN)),
                                               cycle = as.factor(as.character(cycle)),
                                               join.id = paste(SEQN, cycle, sep = " | "),
                                               hist.factor = as.factor(as.character(MCQ300C)),
                                               hist.label = as.factor(plyr::mapvalues(MCQ300C,
                                                                                      from = c(1, 2, 7, 9),
                                                                                      to = c("Yes", "No", "Refused", "Don't know"))))
                                names(df.data.load)[3] <- "MCQ250A"
                                }
                        if (i == 1) {
                                df.data <- df.data.load
                        }
                        else {
                                df.data <- rbind(df.data, df.data.load)
                        }
                }
                return(df.data)
        }


# retrieve, subset, and transform family history data
hist.data <- arrange(fun.hist.data(hist.search), cycle, SEQN)


### ----------------------------------
### DEMOGRAPHICS
# search for demographic files
demo.search <- arrange(nhanes_search(files, "demographic"), cycle)


# remove california demographic files
demo.search <- demo.search %>% filter(data_file != "RDC Only")


# function to retrieve, subset, and transform demographic data
fun.demo.data <- 
        function(search) {
                for (i in 1:nrow(search)) {
                        df.data.load <- nhanes_load_data(search$data_file_name[i], search$cycle[i])
                        df.data.load <- df.data.load %>% 
                                        select(SEQN, SDDSRVYR, 
                                               RIDAGEYR, RIAGENDR, RIDRETH1, DMDEDUC2, RIDEXPRG) %>%
                                        mutate(SEQN = as.factor(as.character(SEQN)),
                                                cycle.factor = as.factor(as.character(SDDSRVYR)),
                                                cycle = as.factor(plyr::mapvalues(SDDSRVYR,
                                                                                      from = c(1, 2, 3, 4, 5, 6, 7, 8),
                                                                                      to = c("1999-2000", "2001-2002",
                                                                                             "2003-2004", "2005-2006",
                                                                                             "2007-2008", "2009-2010",
                                                                                             "2011-2012", "2013-2014"))),
                                                join.id = paste(SEQN, cycle, sep = " | "), 
                                                age.value = RIDAGEYR,
                                                gender.factor = as.factor(as.character(RIAGENDR)),
                                                gender.label = as.factor(plyr::mapvalues(RIAGENDR,
                                                                              from = c(1, 2),
                                                                              to = c("Male", "Female"))),
                                                race.factor = as.factor(as.character(RIDRETH1)),
                                                race.label = as.factor(plyr::mapvalues(RIDRETH1,
                                                                              from = c(1, 2, 3, 4, 5),
                                                                              to = c("Mexican American", "Other Hispanic",
                                                                                     "Non-Hispanic White", "Non-Hispanic Black",
                                                                                     "Other Race - Including Multi-Racial"))),
                                                educ.factor = as.factor(as.character(DMDEDUC2)),
                                                educ.label = as.factor(plyr::mapvalues(DMDEDUC2,
                                                                            from = c(1, 2, 3, 4, 5, 7, 9),
                                                                            to = c("Less Than 9th Grade", 
                                                                                   "9-11th Grade (Includes 12th grade with no diploma)",
                                                                                   "High School Grad/GED or Equivalent", 
                                                                                   "Some College or AA degree",
                                                                                   "College Graduate or above",
                                                                                   "Refused", "Don't Know"))),
                                               preg.factor = as.factor(RIDEXPRG),
                                               preg.label = as.factor(plyr::mapvalues(RIDEXPRG,
                                                                                       from = c(1, 2, 3),
                                                                                       to = c("Yes", "No", "Uncertain"))))

                        if (i == 1) {
                                df.data <- df.data.load
                        }
                        else {
                                df.data <- rbind(df.data, df.data.load)
                        }
                }
                return(df.data)
        }


# retrieve, subset, and transform demographic data
demo.data <- arrange(fun.demo.data(demo.search), cycle, SEQN)


### ----------------------------------
### BODY MEASURES
# search for body measures files
body.search <- arrange(nhanes_search(files, "body measures"), cycle)


# remove arthritis file
body.search <- body.search %>% filter(data_file_name != "ARX_F")


# function to retrieve, subset, and transform body measures data
fun.body.data <- 
        function(search) {
                for (i in 1:nrow(search)) {
                        df.data.load <- nhanes_load_data(search$data_file_name[i], search$cycle[i])
                        df.data.load <- df.data.load %>% select(SEQN, cycle, BMXBMI, BMXWAIST) %>%
                                mutate(SEQN = as.factor(as.character(SEQN)),
                                       cycle = as.factor(as.character(cycle)),
                                       join.id = paste(SEQN, cycle, sep = " | "),
                                       bmi.value = BMXBMI,
                                       waist.value = BMXWAIST)
                        if (i == 1) {
                                df.data <- df.data.load
                        }
                        else {
                                df.data <- rbind(df.data, df.data.load)
                        }
                }
                return(df.data)
        }


# retrieve, subset, and transform body measures data
body.data <- arrange(fun.body.data(body.search), cycle, SEQN)


### ----------------------------------
### SLEEP
# search for sleep files
sleep.search <- arrange(nhanes_search(files, "sleep"), cycle)


# function to retrieve, subset, and transform sleep data
fun.sleep.data <- 
  function(search) {
    for (i in 1:nrow(search)) {
      df.data.load <- nhanes_load_data(search$data_file_name[i], search$cycle[i])
      df.data.load <- df.data.load %>% select(SEQN, cycle, SLD010H) %>%
        mutate(SEQN = as.factor(as.character(SEQN)),
               cycle = as.factor(as.character(cycle)),
               join.id = paste(SEQN, cycle, sep = " | "),
               sleep.value = SLD010H)

      if (i == 1) {
        df.data <- df.data.load
      }
      else {
        df.data <- rbind(df.data, df.data.load)
      }
    }
    df.data$sleep.value[df.data$sleep.value == 77 | df.data$sleep.value == 99] <- NA
    return(df.data)
  }


# retrieve, subset, and transform sleep data
sleep.data <- arrange(fun.sleep.data(sleep.search), cycle, SEQN)


### ----------------------------------
### BLOOD PRESSURE & CHOLESTEROL
# search for blood pressure files
bp.search <- arrange(nhanes_search(files, "blood pressure"), cycle)


# remove examination files
bp.search <- bp.search %>% filter(component == "questionnaire")


# function to retrieve, subset, and transform blood pressure data
fun.bp.data <- 
  function(search) {
    for (i in 1:nrow(search)) {
      df.data.load <- nhanes_load_data(search$data_file_name[i], search$cycle[i])
      df.data.load <- df.data.load %>% select(SEQN, cycle, BPQ020, BPQ080) %>%
        mutate(SEQN = as.factor(as.character(SEQN)),
               cycle = as.factor(as.character(cycle)),
               join.id = paste(SEQN, cycle, sep = " | "),
               bp.factor = as.factor(as.character(BPQ020)),
               bp.label = as.factor(plyr::mapvalues(BPQ020,
                                                      from = c(1, 2, 7, 9),
                                                      to = c("Yes", "No", "Refused", "Don't know"))),
               chol.factor = as.factor(as.character(BPQ080)),
               chol.label = as.factor(plyr::mapvalues(BPQ080,
                                                     from = c(1, 2, 7, 9),
                                                     to = c("Yes", "No", "Refused", "Don't know"))))

      if (i == 1) {
        df.data <- df.data.load
      }
      else {
        df.data <- rbind(df.data, df.data.load)
      }
    }
    return(df.data)
  }


# retrieve, subset, and transform blood pressure data
bp.data <- arrange(fun.bp.data(bp.search), cycle, SEQN)


### ----------------------------------
### SMOKING
# search for smoking files
smoke.search <- arrange(nhanes_search(files, "smoking"), cycle)

# filter smoking files
smoke.search <- smoke.search[str_length(smoke.search$data_file_name) <= 5, ]

# function to retrieve, subset, and transform smoking data
fun.smoke.data <- 
  function(search) {
    for (i in 1:nrow(search)) {
      df.data.load <- nhanes_load_data(search$data_file_name[i], search$cycle[i])
      df.data.load <- df.data.load %>% select(SEQN, cycle, SMQ040) %>%
        mutate(SEQN = as.factor(as.character(SEQN)),
               cycle = as.factor(as.character(cycle)),
               join.id = paste(SEQN, cycle, sep = " | "),
               smoke.factor = as.factor(as.character(SMQ040)),
               smoke.label = as.factor(plyr::mapvalues(SMQ040,
                                                    from = c(1, 2, 3, 7, 9),
                                                    to = c("Every day", "Some days", 
                                                           "Not at all", "Refused", "Don't know"))))
      if (i == 1) {
        df.data <- df.data.load
      }
      else {
        df.data <- rbind(df.data, df.data.load)
      }
    }
    return(df.data)
  }


# retrieve, subset, and transform smoking data
smoke.data <- arrange(fun.smoke.data(smoke.search), cycle, SEQN)


### ----------------------------------
### ALCOHOL
# search for alcohol files
alc.search <- arrange(nhanes_search(files, "alcohol"), cycle)

# filter alcohol files
alc.search <- alc.search %>% filter(is.na(data_file_name) == F & str_length(data_file_name) <= 5)


# function to retrieve, subset, and transform alcohol data
fun.alc.data <- 
  function(search) {
    for (i in 1:nrow(search)) {
      df.data.load <- nhanes_load_data(search$data_file_name[i], search$cycle[i])
      df.data.load <- df.data.load %>% select(SEQN, cycle, ALQ130) %>%
        mutate(SEQN = as.factor(as.character(SEQN)),
               cycle = as.factor(as.character(cycle)),
               join.id = paste(SEQN, cycle, sep = " | "),
               alc.value = ALQ130)
      
      if (i == 1) {
        df.data <- df.data.load
      }
      else {
        df.data <- rbind(df.data, df.data.load)
      }
    }
    df.data$alc.value[df.data$alc.value == 77 | df.data$alc.value == 99 | 
                        df.data$alc.value == 777 | df.data$alc.value == 999] <- NA
    return(df.data)
  }


# retrieve, subset, and transform alcohol data
alc.data <- arrange(fun.alc.data(alc.search), cycle, SEQN)


### ----------------------------------
### COMBINE DATA
# gather data objects
data.object.vec <- ls()
data.object.vec <- data.object.vec[grep(".data", data.object.vec)]
data.object.vec <- data.object.vec[-grep("^fun.", data.object.vec)]


# gather unique ids
for (i in 1:length(data.object.vec)) {
  join.id.object <- get(data.object.vec[i])
  join.id.load <- data.frame(join.id.object$join.id, stringsAsFactors = F)
  if (i == 1) {
    join.id.data <- join.id.load
  }
  else {
    join.id.data <- rbind(join.id.data, join.id.load)
  }
}
join.id.data <- data.frame(join.id = unique(join.id.data$join.id), stringsAsFactors = F)


# join data
all.dm.data <- left_join(join.id.data, dm.data[, c("join.id", "dm.label")], by = "join.id")
all.dm.data <- left_join(all.dm.data, gluc1.data[, c("join.id", "gluc1.label")], by = "join.id")
all.dm.data <- left_join(all.dm.data, gluc2.data[, c("join.id", "gluc2.label")], by = "join.id")
all.dm.data <- left_join(all.dm.data, gluc3.data[, c("join.id", "gluc3.label")], by = "join.id")
all.dm.data <- left_join(all.dm.data, demo.data[, c("join.id", "age.value", "gender.label", 
                                              "race.label", "educ.label", "preg.label")], 
                                              by = "join.id")
all.dm.data <- left_join(all.dm.data, hist.data[, c("join.id", "hist.label")], by = "join.id")
all.dm.data <- left_join(all.dm.data, body.data[, c("join.id", "bmi.value", "waist.value")], by = "join.id")
all.dm.data <- left_join(all.dm.data, bp.data[, c("join.id", "bp.label", "chol.label")], by = "join.id")
all.dm.data <- left_join(all.dm.data, alc.data[, c("join.id", "alc.value")], by = "join.id")
all.dm.data <- left_join(all.dm.data, smoke.data[, c("join.id", "smoke.label")], by = "join.id")
all.dm.data <- left_join(all.dm.data, sleep.data[, c("join.id", "sleep.value")], by = "join.id")


# export csv of all.dm.data
write.csv(all.dm.data, "prd_code_data_extract.csv", row.names = F)


