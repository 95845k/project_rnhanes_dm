# project_rnhanes_dm

project_rnhanes_dm is the repository for the final project for class 95845.

the r code has three components: extract, transform, and model.

the extract component contains three files: 

  	(1) prd_code_data_extract.R
  	(2) prd_code_data_extract_checks.R
  	(3) prd_code_data_extract.csv

the transform component contains four files: 

	(1) prd_code_data_transform.R
	(2) prd_code_data_transform_checks.R
	(3) prd_code_data_transform_cc.csv
	(4) prd_code_data_transform.csv
	
the model component has yet to be built.

files ending in .R are R script files. file ending in \_checks.R are the associated check files where code checks are stored.

prd_code_data_extract.R requires the following R libraries:
		
		library(RNHANES)
		library(dplyr)
		library(stringr)
		
prd_code_data_transform.R requires the following R libraries:
		
		library(readr)
		library(dplyr)
		library(mice)
		
		
Each .R file can run independently of the other. The final dataset from extract is uploaded as a csv and is used as the initial dataset in transform. The final datasets from transform are uploaded as csvs and are used as the modeling datasets in model. 


