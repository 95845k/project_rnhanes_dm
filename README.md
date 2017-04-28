# project_rnhanes_dm

project_rnhanes_dm is the repository for the final project for class 95845.

the r code has three components: extract, transform, and model. It is set up this way so that the secondary on the project does not have to download the data in extract or impute missing values in transform. The secondary can jump right to the modeling portion.

the extract component contains three files:

	(1) prd_code_data_extract.R
	(2) prd_code_data_extract_checks.R
	(3) prd_code_data_extract.csv

the transform component contains four files:

	(1) prd_code_data_transform.R
	(2) prd_code_data_transform_checks.R
	(3) prd_code_data_transform_cc.csv
	(4) prd_code_data_transform.csv

the model component contains two files:

	(1) prd_code_model.R
	(2) prd_code_model_checks.R

files ending in .R are R script files. file ending in \_checks.R are the associated check files where code checks are stored.

prd_code_data_extract.R requires the following R libraries:

	library(RNHANES)
	library(dplyr)
	library(stringr)

prd_code_data_transform.R requires the following R libraries:

	library(readr)
	library(dplyr)
	library(mice)

prd_code_model.R requires the following R libraries:

	library(readr)
	library(tableone)
	library(dplyr)
	library(rpart)
	library(bnlearn)
	library(e1071)
	library(randomForest)
	library(ROCR)
	library(ggplot2)

Each .R file can run independently. The final dataset from extract is uploaded as a csv and is used as the initial dataset in transform. The final dataset from transform is uploaded as a csv and is used as the modeling dataset in model. The complete cases (\_cc) dataset is not used at this time. The runtime on prd_code_data_extract.R is about 5 minutes with the downloads accounting for most of the time. The runtime on prd_code_data_transform.R is about 10 minutes with the imputation accounting for the most time. The runtime on prd_code_model.R is about 5 minutes with svm accounting for the most time.

If the secondary would like to jump right to the modeling component of the project, run prd_code_model.R.

The paper is prd_paper.pdf, which was built from prd_paper.tex. NOTE: The .tex file will not be able to be compiled. The packages and figures are not uploaded to github. The .tex is shown just to prove that I did in fact use "TeX" for this report. 


