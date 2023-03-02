# Replication package for "Predicting individual-level longevity with statistical and machine learning methods" 

## Folder structure

### 1_raw_data

  * The raw Health and Retirement Study (HRS) data used in this study are publicly available and can be downloaded [here](https://hrsdata.isr.umich.edu/data-products/gateway-harmonized-hrs#:~:text=These%20harmonized%20data%20sets%20allow,RAND%20HRS%20Longitudinal%20data%20file). 
   

### 2_long_data

  * **hrs_long.rds** - Raw HRS data converted from wide to long format
  * **make_long_data.R** - Script to produce hrs_long.rds

### 3_cleaned_variables

  * **hrs_long_cleaned.rds** - HRS data after cleaning all variables 
  * **HRS_cleaning_all.R** - Script to produce hrs_long_cleaned.rds
  * **HRS_variables_list.R** - List of all variables, levels, data types
  
### 4_modelling data

  * **train.rds** - Training dataset for training all models (60% of full sample)
  * **test.rds** - Test dataset for evaluating all models (40% of full sample)
  * **make_modelling_data.R** script to produce train.rds and test.rds
  
### 5_predictions

  * **base_static_models/**
    * [ari to fill in files and scripts here]
    
  * **advanced_static_models/**
    * [elnura to fill in files and scripts here]
    
  * **time_varying_models/**
    * [Nick to fill in files and scripts here]
    
### 6_expectations

  * [Everyone to fill in scripts that produce results for the subjective predictability]
  
  ### 7_outputs

  * [Everyone to fill in scripts that produce figures/tables based on predictions]
    
