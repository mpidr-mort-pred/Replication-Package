# Replication material for "The Limits of Predicting Individual-Level Longevity" 

## Folder structure

### 1_raw_data

  * The raw harmonized Health and Retirement Study (HRS) data used in this study are publicly available and can be downloaded [here](https://hrsdata.isr.umich.edu/data-products/gateway-harmonized-hrs#:~:text=These%20harmonized%20data%20sets%20allow,RAND%20HRS%20Longitudinal%20data%20file) 
   
### 2_long_data

  * **make_long_data.R** - Script to convert the Raw HRS data from wide to long format, saved as *hrs_long.rds*
  * **HRS_cleaning_all.R** - Script to clean the data, select the variables on interest, and produce *hrs_long_cleaned.rds*
  * **HRS_variables_list.R** - List of all variables, levels, data types included in the analyses
  
### 3_cleaned_variables

 * **hrs_long_cleaned.rds** - Output of *HRS_cleaning_all.R*
 
### 4_modelling data

  * **make_modelling_data.R** - Script to produce the training data *train.rds* and the testing data *test.rds*
  * **train.rds** - Training dataset for training all models (60% of full sample). Output of *make_modelling_data.R*
  * **test.rds** - Test dataset for evaluating all models (40% of full sample). Output of *make_modelling_data.R*
  
### 5_predictions

  * **models.ipynb** - 
  * **models.R** - 
    
  * **curves** - Folder that contains the predicted survival curves  
  * **model_objects** - Folder that contains the predicted model objects
  * **oversampled** - Folder that contains the predicted survival curves and model objects for the oversampled data
  
### 6_evaluation

  * **evaluate_predictions.ipynb** - Script to compute the prediction accuracy metrics 
  * **evaluation_visualization.R** - Script to visualize the prediction accuracy metrics 
  * **variable_importance.py** - Script to calculate variable importance for the nonparametric models
  * **all**, **educl**, **gender**, **race** - Folders that contains the prediction accuracy metrics for the main test dataset, and the oversampled dataset for education level, gender, and race and ethnicity
 
 ### 7_Expectations

  * **expec_final.R** - Script to compute subjective survival probability 
  
### 8_outputs

  * Figures included in the paper
    
