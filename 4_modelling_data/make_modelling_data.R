suppressPackageStartupMessages({
  library(tidyverse)
  library(dtplyr)
  library(missForest)
  library(doParallel)
})

in_file <- "3_cleaned_variables/hrs_long_cleaned.rds"

out_path <- "4_modelling_data"

hrs <- readRDS(in_file)

# Make all the survival analysis variables

# Event type
hrs <- hrs %>% 
  mutate(event_type = ifelse(!is.na(r_dyear), 1, 0)) 
sum(is.na(hrs$event_type))

# Get cohort number variable
hrs <- hrs %>%
  mutate(first_wave = case_when(
    inw1 == 1 ~ 1,
    inw1 == 0 & inw2 == 1 ~ 2,
    inw1 == 0 & inw2 == 0 & inw3 == 1 ~ 3,
    inw1 == 0 & inw2 == 0 & inw3 == 0 & inw4 == 1 ~ 4,
    inw1 == 0 & inw2 == 0 & inw3 == 0 & inw4 == 0 & inw5 == 1 ~ 5,
    inw1 == 0 & inw2 == 0 & inw3 == 0 & inw4 == 0 & inw5 == 0 & inw6 == 1 ~ 6,
    inw1 == 0 & inw2 == 0 & inw3 == 0 & inw4 == 0 & inw5 == 0 & inw6 == 0 & 
      inw7 == 1 ~ 7,
    inw1 == 0 & inw2 == 0 & inw3 == 0 & inw4 == 0 & inw5 == 0 & inw6 == 0 & 
      inw7 == 0 & inw8 == 1 ~ 8,
    inw1 == 0 & inw2 == 0 & inw3 == 0 & inw4 == 0 & inw5 == 0 & inw6 == 0 & 
      inw7 == 0 & inw8 == 0 & inw9 == 1 ~ 9,
    inw1 == 0 & inw2 == 0 & inw3 == 0 & inw4 == 0 & inw5 == 0 & inw6 == 0 & 
      inw7 == 0 & inw8 == 0 & inw9 == 0 & inw10 == 1 ~ 10,
    inw1 == 0 & inw2 == 0 & inw3 == 0 & inw4 == 0 & inw5 == 0 & inw6 == 0 & 
      inw7 == 0 & inw8 == 0 & inw9 == 0 & inw10 == 0 & inw11 == 1 ~ 11,
    inw1 == 0 & inw2 == 0 & inw3 == 0 & inw4 == 0 & inw5 == 0 & inw6 == 0 & 
      inw7 == 0 & inw8 == 0 & inw9 == 0 & inw10 == 0 & inw11 == 0 &
      inw12 == 1 ~ 12,
    inw1 == 0 & inw2 == 0 & inw3 == 0 & inw4 == 0 & inw5 == 0 & inw6 == 0 & 
      inw7 == 0 & inw8 == 0 & inw9 == 0 & inw10 == 0 & inw11 == 0 &
      inw12 == 0 & inw13 == 1 ~ 13,
    inw1 == 0 & inw2 == 0 & inw3 == 0 & inw4 == 0 & inw5 == 0 & inw6 == 0 & 
      inw7 == 0 & inw8 == 0 & inw9 == 0 & inw10 == 0 & inw11 == 0 &
      inw12 == 0 & inw13 == 0 & inw14 == 1 ~ 14
  ))
sum(is.na(hrs$first_wave))

# Last wave
hrs <- hrs %>%
  mutate(last_wave = case_when(
    inw14 == 1 ~ 14,
    inw13 == 1 ~ 13,
    inw12 == 1 ~ 12,
    inw11 == 1 ~ 11,
    inw10 == 1 ~ 10,
    inw9 == 1 ~ 9,
    inw8 == 1 ~ 8,
    inw7 == 1 ~ 7,
    inw6 == 1 ~ 6,
    inw5 == 1 ~ 5,
    inw4 == 1 ~ 4,
    inw3 == 1 ~ 3,
    inw2 == 1 ~ 2,
    inw1 == 1 ~ 1
  ))
sum(is.na(hrs$last_wave))

# Interview date
hrs <- hrs %>%
  mutate(iwdate = case_when(
    is.na(r_iwendm) ~ as.numeric(r_iwendy) + .5,
    TRUE ~ as.numeric(r_iwendy) + as.numeric(r_iwendm) / 12,
  ))
sum(is.na(hrs$iwdate))

hrs <- hrs %>%
  group_by(hhidpn) %>%
  mutate(
    # Exit date
    last_date = max(iwdate, na.rm = TRUE),
    # Entry date
    first_date = min(iwdate, na.rm = TRUE)
  ) %>%
  ungroup()
sum(is.na(hrs$last_date))
sum(is.na(hrs$first_date))

hrs <- hrs %>% 
  mutate(
    birth_date = case_when(
      is.na(r_bmonth) ~ as.numeric(r_byear) + .5,
      TRUE ~ as.numeric(r_byear) + as.numeric(r_bmonth) / 12,
    ),
    death_date = case_when(
      is.na(r_dmonth) ~ as.numeric(r_dyear) + .5,
      TRUE ~ as.numeric(r_dyear) + as.numeric(r_dmonth) / 12,
    ))

# hrs <- hrs %>%
#   mutate(
#     age_entry = first_date - birth_date,
#     age_exit 
#   )

hrs <- hrs %>%
  mutate(
    # age
    age = iwdate - birth_date,
    
    # age at death
    age_death = death_date - birth_date,
    
    # age at last interview
    last_age = last_date - birth_date,
    
    # define follow-up date, which is either time of death 
    # or date of last interview, if death did not occur
    exit_date = case_when(
      event_type == 1 ~ death_date,
      event_type == 0 ~ last_date
    ),
    # define follow-up age, which is either age at death 
    # or age at last interview, if death did not occur
    exit_age = case_when(
      event_type == 1 ~ age_death,
      event_type == 0 ~ last_age
    ),
    # define follow-up time, which is either time at death or
    # time at last interview, where time starts at first interview
    exit_time = exit_date - first_date
  )

# Remove people whose exit_age is less than 50
r <- nrow(hrs)
hrs <- hrs %>%
  filter(exit_age > 50, exit_time >= 0)
r - nrow(hrs)    

hrs <- hrs %>%
  mutate(
    # Get death status by wave
    death_status = case_when(
      event_type == 0 ~ 0,
      wave == 14 ~ 0,
      wave == 13 & inw14 == 0 ~ 1,
      wave == 13 & inw14 == 1 ~ 0,
      wave == 12 & inw13 == 0 ~ 1,
      wave == 12 & inw13 == 1 ~ 0,
      wave == 11 & (inw12 == 1 | inw13 == 1) ~ 0,
      wave == 11 & inw12 == 0 ~ 1,
      wave == 10 & (inw11 == 1 | inw12 == 1 | inw13 == 1) ~ 0,
      wave == 10 & inw11 == 0 ~ 1,
      wave == 9 & (inw10 == 1 | inw11 == 1 | inw12 == 1 | inw13 == 1) ~ 0,
      wave == 9 & inw10 == 0 ~ 1,
      wave == 8 & (inw9 == 1 | inw10 == 1 |inw11 == 1 | inw12 == 1 |  inw13 == 1) ~ 0,
      wave == 8 & inw9 == 0 ~ 1,
      wave == 7 & (inw8 == 1 | inw9 == 1 | inw10 == 1 |inw11 == 1 |  inw12 == 1 | inw13 == 1) ~ 0,
      wave == 7 & inw8 == 0 ~ 1,
      wave == 6 & (inw7 == 1 | inw8 == 1 | inw9 == 1 | inw10 == 1 | inw11 == 1 | inw12 == 1 | inw13 == 1) ~ 0,
      wave == 6 & inw7 == 0 ~ 1,
      wave == 5 & (inw6 == 1 | inw7 == 1 | inw8 == 1 | inw9 == 1 |  inw10 == 1 |inw11 == 1 | inw12 == 1 |  inw13 == 1) ~ 0,
      wave == 5 & inw6 == 0 ~ 1,
      wave == 4 & (inw5 == 1 | inw6 == 1 | inw7 == 1 | inw8 == 1 | inw9 == 1 | inw10 == 1 |inw11 == 1 | inw12 == 1 |  inw13 == 1) ~ 0,
      wave == 4 & inw5 == 0 ~ 1,
      wave == 3 & (inw4 == 1 | inw5 == 1 | inw6 == 1 | inw7 == 1 |  inw8 == 1 | inw9 == 1 | inw10 == 1 |  inw11 == 1 | inw12 == 1 | inw13 == 1) ~ 0,
      wave == 3 & inw4 == 0 ~ 1,
      wave == 2 & (inw3 == 1 | inw4 == 1 | inw5 == 1 | inw6 == 1 | inw7 == 1 | inw8 == 1 | inw9 == 1 | inw10 == 1 | inw11 == 1 | inw12 == 1 | inw13 == 1) ~ 0,
      wave == 2 & inw3 == 0 ~ 1,
      wave == 1 & (inw2 == 1 | inw3 == 1 | inw4 == 1 | inw5 == 1 | inw6 == 1 | inw7 == 1 | inw8 == 1 | inw9 == 1 | inw10 == 1 |inw11 == 1 | inw12 == 1 | inw13 == 1) ~ 0,
      wave == 1 & inw2 == 0 ~ 1,
      
    )
  )
sum(is.na(hrs$death_status))

# Get start and stop variables
hrs <- hrs %>%
  group_by(hhidpn) %>%
  arrange(iwdate) %>%
  mutate(
    start = iwdate - birth_date,
    stop = lead(iwdate) - birth_date,
    stop = ifelse(is.na(stop), exit_age, stop)
  ) %>%
  ungroup()

# Remove wave 14
hrs <- hrs %>%
  filter(wave != 14, exit_time > 0)

# Convert all categorical variables to factor, all id vars to character
cat_vars <- c('cidimde3','cidisymp','lideal3','lstsf3','rested','s_rxpsych','witwill',
              'wlifein','wtrust', 'lifeins_fam', 'will_fam','ftrhlp','mealhlp','medhlp',
              'moneyhlp','phonehlp','shophlp','rcany','gender','race','bcohort',
              'mstat','relig','vetrn','bplace','momliv','dadliv', 'evbrn','smokev',
              'lhchild','dadgrela','pabused','chshlt','dadoccup','cage', 'lvwith', 'hometyp',
              'mobese','traumatic_events','rural', 'educl','citizen','balance','adlfive',
              'kidu14', 'kidu6', 'urinai','cancre_cancrst','hipe','hrtrhme','angine','cancre',
              'diabe','catrcte','conhrtfe','hrtatte','stroke','hrtsrge',
              'hchole','gender_hystere','jointre','osteoe','shingle','glaucoma',
              'lowermob','ncatrct','pneushte','hrtrhm','catrct','conhrtf',
              'hrtatt','sight','hearing','fatigue','shnglshte','uppermob',
              'painlv','lunglmt','limimpar','work_jdealpplb','work_jdiffa',
              'work_jenjwrka','work_jrsleft','work_lookwrkpf','work_satjob',
              'work','unemp','dcsxori','gcaany','kcnt','pcnt','relgwk','rfcnt','socwk','unfair',
              'everdiv','everwid', 'evermrg')
cat_vars <- c(paste0("r_", cat_vars), paste0("s_", cat_vars), paste0("h_", cat_vars))

outcome_vars <- c('event_type', 'exit_age', 'exit_date', 'wave', 'death_status',
                  'start', 'stop', 'exit_time', 'iwdate', 'birth_date')

char_vars <- c('hhidpn', 'hhid', 'pn', 'inw1', 'inw2', 'inw3', 'inw4', 'inw5',
               'inw6', 'inw7', 'inw8', 'inw9', 'inw10', 'inw11', 'inw12',
               'inw13', 'inw14', 'wave')

# Relabel and correctly type all variables
hrs <- hrs %>%
  rename_with(~ paste0(., "_cat"), any_of(cat_vars)) %>%
  rename_with(~ paste0(., '_outcome'), contains(outcome_vars)) 

# Replace NAs with -1 for cat variables
#hrs <- hrs %>%
#  mutate(across(ends_with("_cat"), ~ ifelse(is.na(.), '-1', .))) 

hrs <- hrs %>%
  mutate(across(ends_with("_cat"), ~ as.factor(.)),
         across(any_of(char_vars), ~ as.character(.)))

# Get static data where each person comes from the last wave
# ----------------------------------------------------------
# hrs_static <- hrs %>%
#   filter(wave_outcome == last_wave_outcome) %>%
#   filter(!is.na(exit_age_outcome), !is.na(event_type_outcome))

# Remove unneeded variables
hrs <- hrs %>%
  select(
    -contains('year'),
    -contains('month'),
    -contains('wend'),
    -contains('inw'),
    -last_date,
    -first_date,
    -death_date,
    -last_age,
    -age_death,
    -r_agem_b,
    -r_agey_b,
    -s_cage_cat
  )

# Filter so age > 50, age is nonmissing
hrs <- hrs %>%
  filter(age > 50, !is.na(age)) 

# Check obs per wave
table(hrs$wave_outcome, exclude = FALSE)

# Split into predictors and outcomes/other
X <- hrs %>%
  select(-contains("outcome"), -hhidpn, -age)
rest <- hrs %>%
  select(hhidpn, age, contains("outcome"))

# Remove variables with more than 33% missing
missing_pcts <- X %>% 
  summarise(across(everything(), ~ sum(is.na(.x))/n())) %>%
  pivot_longer(everything(), names_to = 'varname', values_to = 'pctna')
to_remove <- missing_pcts %>%
  filter(pctna > .33) %>%
  .$varname

X <- X %>% 
  select(-any_of(to_remove))

# Also remove variables that have been back-imputed
X <- X %>%
  select(-r_financh, -r_chshlt_cat, -r_dadoccup_cat, -r_citizen_cat,
         -r_mfstyr, -r_diabe_diagdiab)

# Recombine data
all <- cbind(rest, X)

to_plot <- missing_pcts %>%
  filter(pctna > .1) %>%
  .$varname

all %>%
  select(-contains("outcome"), wave_outcome, where(is.numeric)) %>%
  group_by(wave_outcome) %>%
  summarize(across(everything(), ~sum(is.na(.))),
            n = n()) %>%
  pivot_longer(cols = -any_of(c("n", "wave_outcome"))) %>%
  filter(name %in% to_plot) %>%
  mutate(pct_missing = value / n) %>%
  ggplot(aes(x = wave_outcome, y = name, fill = pct_missing)) +
    geom_tile()

# Train test split
set.seed(101010)
ids <- unique(rest$hhidpn)
N <- length(ids)
train_ids <- sample(ids, round(N*.6), replace = FALSE)

train <- all %>%
  filter(hhidpn %in% train_ids)
test <- all %>%
  filter(!(hhidpn %in% train_ids))

X_train <- train %>%
  select(-contains("outcome"), -hhidpn, -age)
y_train <- train %>%
  select(hhidpn, age, contains("outcome"))

X_test <- test %>%
  select(-contains("outcome"), -hhidpn, -age)
y_test <- test %>%
  select(hhidpn, age, contains("outcome"))

# Look at how much stuff needs imputing
train %>% 
  summarize(across(everything(), ~sum(is.na(.)))) %>% 
  pivot_longer(everything()) %>% 
  filter(value > 0) %>% 
  arrange(desc(value))

test %>% 
  summarize(across(everything(), ~sum(is.na(.)))) %>% 
  pivot_longer(everything()) %>% 
  filter(value > 0) %>% 
  arrange(desc(value))

# Impute on train dataset
hhidpns <- unique(train$hhidpn)
ids <- split(hhidpns, sample(10, length(hhidpns), replace = TRUE))

all_imputed_train <- map(
  ids,
  function(id_set) {
    message("Imputing next subset of ids")
    subset <- train %>%
      filter(hhidpn %in% id_set)
    to_impute <- subset %>%
      select(-contains("outcome"), -hhidpn, -age)
    other <- subset %>%
      select(hhidpn, age, contains("outcome"))
    
    options(na.action = "na.pass")
    #to_impute_subvars_matrix <- model.matrix(~ ., to_impute)[,-1]
    cl <- makePSOCKcluster(100)
    registerDoParallel(cl)
    imputed <- missForest(
      to_impute, 
      verbose = TRUE,
      parallelize = 'variables',
      ntree = 100,
      mtry = 4
    )
    stopCluster(cl)
    
    out <- cbind(
      other,
      as.data.frame(imputed$ximp)
    )
    return(out)
  }
) %>% bind_rows()

# --------------------

# Impute on test dataset
hhidpns <- unique(test$hhidpn)
ids <- split(hhidpns, sample(10, length(hhidpns), replace = TRUE))

all_imputed_test <- map(
  ids,
  function(id_set) {
    message("Imputing next subset of ids")
    subset <- test %>%
      filter(hhidpn %in% id_set)
    to_impute <- subset %>%
      select(-contains("outcome"), -hhidpn, -age)
    other <- subset %>%
      select(hhidpn, age, contains("outcome"))
    
    options(na.action = "na.pass")
    #to_impute_subvars_matrix <- model.matrix(~ ., to_impute)[,-1]
    cl <- makePSOCKcluster(100)
    registerDoParallel(cl)
    imputed <- missForest(
      to_impute, 
      verbose = TRUE,
      parallelize = 'variables',
      ntree = 100,
      mtry = 4
    )
    stopCluster(cl)
    
    out <- cbind(
      other,
      as.data.frame(imputed$ximp)
    )
    return(out)
  }
) %>% bind_rows()

# --------------------

# Confirm columns have the same order
all_imputed_test <- all_imputed_test %>%
  select(all_of(colnames(all_imputed_train)))
all(names(all_imputed_test) == names(all_imputed_train))

saveRDS(all_imputed_train, file.path(out_path, "train_imputed.rds"))
saveRDS(all_imputed_test, file.path(out_path, "test_imputed.rds"))

#all_imputed_train <- readRDS(file.path(out_path, "train_imputed.rds"))
#all_imputed_test <- readRDS(file.path(out_path, "test_imputed.rds"))

# Split into predictors and the rest
X_test_imputed <- all_imputed_test %>%
  select(-contains("outcome"), -hhidpn, -age)
y_test_imputed <- all_imputed_test %>%
  select(hhidpn, age, contains("outcome"))

X_train_imputed <- all_imputed_train %>%
  select(-contains("outcome"), -hhidpn, -age)
y_train_imputed <- all_imputed_train %>%
  select(hhidpn, age, contains("outcome"))

# Rescale train and test predictors
X_train_imputed_scaled <-  X_train_imputed %>%
  mutate(across(-contains("_cat"), ~ scale(.) %>% as.vector()))
X_test_imputed_scaled <-  X_test_imputed %>%
  mutate(across(-contains("_cat"), ~ scale(.) %>% as.vector()))


# Make final train and test datasets
all_train_imputed = cbind(y_train_imputed, X_train_imputed_scaled)
all_test_imputed = cbind(y_test_imputed, X_test_imputed_scaled)

# Save out datasets for modelling
saveRDS(all_train_imputed, file.path(out_path, "all_train_imputed_scaled_2.rds"))
saveRDS(all_test_imputed, file.path(out_path, "all_test_imputed_scaled_2.rds"))

# Save out unaltered train and test sets
saveRDS(train, file.path(out_path, "train_info.rds"))
saveRDS(test, file.path(out_path, "test_info.rds"))

