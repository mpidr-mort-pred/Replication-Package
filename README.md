# Replication material for "Predicting individual-level longevity with statistical and machine learning methods" 

**Date of the last update**: 2023-03-02

## MPIDR Working Paper

**Title**: Predicting individual-level longevity with statistical and machine learning methods

**Authors**: Badolato, L., Decter-Frain, A. G., Irons, N., Miranda, M. L., Walk, E., Zhalieva, E., Alexander, M., Basellini, U., Zagheni, E.

**DOI**: DOI:10.4054/MPIDR-WP-2023-008

**Abstract**:
Individual-level mortality prediction is a fundamental challenge with implications for people and societies. Accurate longevity predictions improve life planning, targeting of high-risk individuals, and organization of social interventions, policies, and public spending. Demographers and actuaries have been primarily concerned with mortality modeling and prediction at a macro level, leveraging strong regularities in mortality rates over age, sex, space, and time. Besides clinical settings, individual-level mortality predictions have been largely overlooked and have remained a challenging task. We model and predict individual-level lifespan using data from the US Health and Retirement Study, a nationally representative longitudinal survey of people over 50 years of age. We estimate 12 statistical and machine learning survival analysis models using over 150 predictors measuring behavioral, biological, demographic, health, and social indicators. Extending previous research on inequalities in mortality and morbidity, we investigate inequalities in individual mortality prediction by gender, race and ethnicity, and education. Machine learning and traditional models report comparable accuracy and relatively high discriminative performance, particularly when including time-varying information (best mean Area Under the Curve = 0.87). However, the models and predictors used fail to account for a majority of lifespan heterogeneity at the individual level. We observe consistent inequalities in mortality predictability and risk discrimination, with lower prediction accuracy for men, non-Hispanic Blacks, and low-educated individuals. In addition, people in these groups show lower accuracy in their subjective predictions of their own lifespan. Finally, we see minimal variation in the top features across groups, with variables related to habits, health history, and finances being relevant predictors. Our results assess how well mortality can be predicted from representative surveys, providing baselines and guidance for future research across countries.

## Folder structure

### 1_raw_data

  * The raw Health and Retirement Study (HRS) data used in this study are publicly available and can be downloaded [here](https://hrsdata.isr.umich.edu/data-products/gateway-harmonized-hrs#:~:text=These%20harmonized%20data%20sets%20allow,RAND%20HRS%20Longitudinal%20data%20file). 
   

### 2_long_data

  * **make_long_data.R** - Script to convert the Raw HRS data from wide to long format, saved as *hrs_long.rds*

### 3_cleaned_variables

  * **HRS_cleaning_all.R** - Script to clean the data, select the variables on interest, and produce *hrs_long_cleaned.rds*
  * **HRS_variables_list.R** - List of all variables, levels, data types included in the analyses
  
### 4_modelling data

  * **train.rds** - Training dataset for training all models (60% of full sample)
  * **test.rds** - Test dataset for evaluating all models (40% of full sample)
  * **make_modelling_data.R** script to produce the training data *train.rds* and the testing data *test.rds*
  
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
    
