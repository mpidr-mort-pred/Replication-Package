
# Clear the environment and set the working directory
rm(list = ls())

# Install and load the required packages
listofpackages = c('dplyr', 'ggplot2', 'ggpubr', 'purrr', 'tidyverse', 'haven',
                   'stringr', 'viridis', 'grid', 'scales', 'forcats')
for (j in listofpackages){
  if(sum(installed.packages()[, 1] == 0)) {
    install.packages(j)
  }
  library(j, character.only = T)
}


# Subjective survival probability data
df <- read_dta('1_raw_data/randhrs1992_2018v1.dta')

cols <- df %>%
  dplyr::select(hhidpn, contains('liv75')) %>%
  dplyr::select(-starts_with('s')) %>%
  mutate(across(everything(), ~ as.character(ifelse(is.na(.), na_tag(.), .))))

varnames <- names(cols)
varnames <- gsub('__', '_', varnames)

for (i in 14:1) {
  target_r <- paste0('^r', i)
  varnames <- gsub(target_r, paste0('w', i, '__r_'), varnames)
}

names(cols) <- varnames

cols_long <- cols %>%
  pivot_longer(cols = -hhidpn, names_to = c('wave_outcome', 'varname'), 
               names_sep = '__r_', values_to = 'value') %>%
  pivot_wider(id_cols = any_of(c('hhidpn', 'wave_outcome')), 
              names_from = 'varname', values_from = 'value') %>%
  mutate(wave_outcome = as.numeric(gsub('w', '', wave_outcome))) %>% 
  dplyr::select(c(hhidpn, first_wave_outcome = wave_outcome, liv75)) %>% 
  replace(. == 'a' |
            . == 'd' |
            . == 'j' |
            . == 'm' |
            . == 'q' |
            . == 'r' |
            . == 's' , NA) %>% 
  mutate(liv75 = as.numeric(liv75)/100)

#saveRDS(cols_long, "7_expectations/expectations.rds")
#cols_long <- readRDS('7_expectations/expectations.rds')

# Merge subjective survival probability with test set dataset 
data <- readRDS('4_modelling_data/minus_backimpd/all_test_imputed_scaled_2.rds') %>% 
  filter(first_wave_outcome == wave_outcome) %>% 
  dplyr::select(c(hhidpn, age, first_wave_outcome, exit_age_outcome, 
                  event_type_outcome, r_gender_cat, r_race_cat, 
                  r_educl_cat)) %>% 
  left_join(cols_long,c('hhidpn','first_wave_outcome')) %>% 
  mutate(outcome_75 = as.numeric(case_when(exit_age_outcome <= 75 & 
                                             event_type_outcome == 1 ~ 0,
                                           exit_age_outcome > 75  ~ 1))) %>% 
  rename(Subjective = liv75)

data$r_gender <- factor(data$r_gender_cat, levels = c('0','1'),
                        labels = c('Men','Women'))

data$r_race <- factor(data$r_race_cat, levels = c('1','2','3'),
                      labels = c('Non-Hispanic white','Non-Hispanic Black',
                                 'Hispanic'))

data$r_education <- factor(data$r_educl_cat, levels = c('1','2','3'),
                           labels = c('Low education','Middle education',
                                      'High education'))

# Survival probabilities
data <- data %>% 
  left_join(data.frame(read.csv('5_predictions/minus_backimpd/curves/cph_age.csv')) %>% 
              filter(age %in% c(75)) %>% 
              gather('hhidpn','Cox',-age) %>% 
              mutate(hhidpn = str_replace(hhidpn, 'X', '')) %>% 
              dplyr::select(-age),
            'hhidpn') %>% 
  left_join(data.frame(read.csv('5_predictions/minus_backimpd/curves/cphreduced_age.csv')) %>% 
              dplyr::select(-X) %>% 
              filter(age %in% c(75)) %>% 
              gather('hhidpn','CoxReduced',-age) %>% 
              mutate(hhidpn = str_replace(hhidpn, 'X', '')) %>% 
              dplyr::select(-age),
            'hhidpn') %>% 
  left_join(data.frame(read.csv('5_predictions/minus_backimpd/curves/gmp_age.csv')) %>% 
              dplyr::select(-X) %>% 
              filter(age %in% c(75)) %>% 
              gather('hhidpn','Gompertz',-age) %>% 
              mutate(hhidpn = str_replace(hhidpn, 'X', '')) %>% 
              dplyr::select(-age),
            'hhidpn') %>% 
  left_join(data.frame(read.csv('5_predictions/minus_backimpd/curves/km_age.csv')) %>% 
              dplyr::select(-X) %>% 
              filter(age %in% c(75)) %>% 
              gather('hhidpn','KM',-age) %>% 
              mutate(hhidpn = str_replace(hhidpn, 'X', '')) %>% 
              dplyr::select(-age),
            'hhidpn') %>% 
  left_join(data.frame(read.csv('5_predictions/minus_backimpd/curves/pchazard_time_age.csv')) %>% 
              dplyr::select(-X) %>% 
              filter(time %in% c(75)) %>% 
              gather('hhidpn','DeepPCH',-time) %>% 
              mutate(hhidpn = str_replace(hhidpn, 'X', '')) %>% 
              dplyr::select(-time),
            'hhidpn') %>% 
  left_join(data.frame(read.csv('5_predictions/minus_backimpd/curves/deephit_time_age.csv')) %>% 
              dplyr::select(-X) %>% 
              filter(time %in% c(75)) %>% 
              gather('hhidpn','DeepHit',-time) %>% 
              mutate(hhidpn = str_replace(hhidpn, 'X', '')) %>% 
              dplyr::select(-time),
            'hhidpn') %>% 
  left_join(data.frame(read.csv('5_predictions/minus_backimpd/curves/deepsurv_time_age.csv')) %>% 
              dplyr::select(-X) %>% 
              filter(time %in% c(75)) %>% 
              gather('hhidpn','DeepSurv',-time) %>% 
              mutate(hhidpn = str_replace(hhidpn, 'X', '')) %>% 
              dplyr::select(-time),
            'hhidpn') %>% 
  left_join(data.frame(read.csv('5_predictions/minus_backimpd/curves/gbd_age.csv')) %>% 
              filter(age %in% c(75)) %>% 
              gather('hhidpn','GradBoost',-age) %>% 
              mutate(hhidpn = str_replace(hhidpn, 'X', '')) %>% 
              dplyr::select(-age),
            'hhidpn') %>% 
  left_join(data.frame(read.csv('5_predictions/minus_backimpd/curves/cphnet_age.csv')) %>% 
              filter(age %in% c(75)) %>% 
              gather('hhidpn','CoxNet',-age) %>% 
              mutate(hhidpn = str_replace(hhidpn, 'X', '')) %>% 
              dplyr::select(-age),
            'hhidpn') %>% 
  left_join(data.frame(read.csv('5_predictions/minus_backimpd/curves/rsf_age.csv')) %>% 
              filter(age %in% c(75)) %>% 
              gather('hhidpn','RSF',-age) %>% 
              mutate(hhidpn = str_replace(hhidpn, 'X', '')) %>% 
              dplyr::select(-age),
            'hhidpn') %>% 
  left_join(data.frame(read.csv('5_predictions/minus_backimpd/curves/cph_tv_age.csv')) %>% 
              dplyr::select(-X) %>% 
              filter(age %in% c(75)) %>% 
              gather('hhidpn','Cox-TV',-age) %>% 
              mutate(hhidpn = str_replace(hhidpn, 'X', '')) %>% 
              dplyr::select(-age),
            'hhidpn') %>% 
  left_join(data.frame(read.csv('5_predictions/minus_backimpd/curves/rrf_age.csv')) %>% 
              dplyr::select(-X) %>% 
              filter(age %in% c(75)) %>% 
              gather('hhidpn','RRF-TV',-age) %>% 
              mutate(hhidpn = str_replace(hhidpn, 'X', '')) %>% 
              dplyr::select(-age),
            'hhidpn')

cols <- c('Subjective','Cox', 'CoxReduced', 'Gompertz','KM','DeepPCH','DeepHit','DeepSurv',
          'GradBoost','CoxNet','RSF','Cox-TV','RRF-TV')

data[cols] <- sapply(data[cols], as.numeric)

data <- data %>% pivot_longer(all_of(cols)) %>% 
        select('hhidpn', 'age', 'exit_age_outcome', 'event_type_outcome', 
               'r_education', 'outcome_75', 'r_gender', 'r_race', 
               'name', 'value') %>% 
        rename(model = name, surv_prob = value, outcome = outcome_75)


#saveRDS(data, "expec_brier.rds")

# plot subjective survival prob at 75 by gender, race, and education
data %>% 
  filter(model == 'Subjective') %>% 
  ggplot() +
  geom_histogram(aes(x = surv_prob, y = stat(density)), 
                 binwidth=0.01, color="black") + 
  theme_bw() +
  facet_grid(~r_gender) +
  scale_x_continuous(minor_breaks = seq(0, 1, 0.10), 
                     breaks = seq(0, 1, 0.10), limits = c(0,1) ) +
  labs(x = "Subjective survival prob at 75", y = "Density")

data %>% 
  filter(model == 'Subjective') %>% 
  ggplot() +
  geom_histogram(aes(x = surv_prob, y = stat(density)), 
                 binwidth=0.01, color="black") + 
  theme_bw() +
  facet_grid(~r_race) +
  scale_x_continuous(minor_breaks = seq(0, 1, 0.10), 
                     breaks = seq(0, 1, 0.10), limits = c(0,1) ) +
  labs(x = "Subjective survival prob at 75", y = "Density")

data %>% 
  filter(model == 'Subjective') %>% 
  ggplot() +
  geom_histogram(aes(x = surv_prob, y = stat(density)), 
                 binwidth=0.01, color="black") + 
  theme_bw() +
  facet_grid(~r_education) +
  scale_x_continuous(minor_breaks = seq(0, 1, 0.10), 
                     breaks = seq(0, 1, 0.10), limits = c(0,1) ) +
  labs(x = "Subjective survival prob at 75", y = "Density")

data %>%  
  filter(model == 'Subjective') %>% 
  select('surv_prob') %>% 
  map(table, exclude = NULL)


data[data$model=="Cox", "model"] <- "Cox Full"
data[data$model=="CoxReduced", "model"] <- "Cox Reduced"

# Computing errors
data <- data %>% mutate(sqerr = (surv_prob - outcome)^2,  #squared errors
                        err = (surv_prob - outcome),      #errors
                        model = fct_relevel(model, 'RRF-TV', 'RSF','GradBoost', 'DeepSurv', 'DeepPCH' , 'DeepHit',
                                            'Gompertz', 'Cox-TV', 'CoxNet', 'Cox Full', 
                                            'KM', 'Cox Reduced', 
                                            'Subjective'))

# mse by gender 
mse_gender <- data %>% group_by(r_gender, model) %>% 
              summarize(mse = mean(sqerr, na.rm=TRUE)) %>% 
              mutate(cat = 'Gender') %>% 
              rename(group = r_gender)

# mse by race 
mse_race <- data %>% group_by(r_race, model) %>% 
              summarize(mse = mean(sqerr, na.rm=TRUE)) %>% 
              mutate(cat = 'Race') %>% 
              rename(group = r_race)

# mse by education 
mse_education <- data %>% group_by(r_education, model) %>% 
              summarize(mse = mean(sqerr, na.rm=TRUE)) %>% 
              mutate(cat = 'Education') %>% 
              rename(group = r_education)


mse_df <- rbind(mse_gender, mse_education, mse_race)
view(mse_df)


# mean error by gender 
me_gender <- data %>% group_by(r_gender, model) %>% 
  summarize(me = mean(err, na.rm=TRUE)) %>% 
  mutate(cat = 'Gender')  %>% 
  rename(group = r_gender)

# mean error by race 
me_race <- data %>% group_by(r_race, model) %>% 
  summarize(me = mean(err, na.rm=TRUE)) %>% 
  mutate(cat = 'Race') %>% 
  rename(group = r_race)

# mean error by education 
me_education <- data %>% group_by(r_education, model) %>% 
  summarize(me = mean(err, na.rm=TRUE)) %>% 
  mutate(cat = 'Education') %>% 
  rename(group = r_education)

me_df <- rbind(me_gender, me_education, me_race)
view(me_df)

mse_df$model_cat <- case_when(mse_df$model == "Cox Reduced"  |
                                mse_df$model == "KM" ~ "1",
                              mse_df$model == "Cox Full" |
                                mse_df$model == "CoxNet" |
                                mse_df$model == "Cox-TV" |
                                mse_df$model == "Gompertz" ~ "2",
                              mse_df$model == "DeepHit" |
                                mse_df$model == "DeepPCH" |
                                mse_df$model == "DeepSurv" |
                                mse_df$model == "GradBoost" |
                                mse_df$model == "RSF" |
                                mse_df$model == "RRF-TV" ~ "3", 
                              mse_df$model == "Subjective" ~ "4")


a <- mse_df %>% 
  filter(cat == "Gender") %>% 
  ggplot() +
  geom_point(aes(x = model, y = mse, color = group, shape = model_cat), alpha=.6, size = 2) +
  geom_hline(yintercept=0.25, linetype="dashed", color = "black", alpha = 0.8) +
  theme_bw() + 
  coord_flip() +
  scale_y_continuous(minor_breaks = seq(0, 0.35, 0.05), 
                     breaks = seq(0, 0.35, 0.05), limits = c(0.05, 0.36)) +
  labs(title = "a. Gender", x = "", y = "MSE at age 75") +
  scale_color_manual(values = c("navy", "red", "#999999")) +
  scale_shape_manual(values = c(15, 19, 17, 3), guide = "none") +
  theme(plot.title = element_text(size=10, hjust = 0.5, face = "bold"),
      axis.text.x = element_text(size = 8.5),
      axis.title.x = element_text(size = 9),
      legend.title = element_blank(),
      legend.position=c('bottom'),
      legend.background = element_blank(),
      #axis.title.y=element_blank(),
      #axis.text.y=element_blank(),
      #axis.ticks.y=element_blank(),
      legend.box.margin=margin(-10,0,0.5,0),
      plot.margin=unit(c(0.2,0.1,0.2,0.2), "cm")) +
  guides(color=guide_legend(override.aes = list(shape = 15, size =4)))


b <- mse_df %>% 
  filter(cat == "Race") %>% 
  mutate(group = case_when(group == "Non-Hispanic white" ~ "Whites", 
                           group == "Non-Hispanic Black" ~ "Blacks", 
                           group == "Hispanic" ~ "Hisp.")) %>% 
  ggplot() +
  geom_point(aes(x = model, y = mse, color = fct_relevel(group, "Hisp.", "Blacks", "Whites"), shape = model_cat), alpha=.6, size = 2) +
  geom_hline(yintercept=0.25, linetype="dashed", color = "black", alpha = 0.8) +
  theme_bw() + 
  coord_flip() +
  scale_y_continuous(minor_breaks = seq(0, 0.35, 0.05), 
                     breaks = seq(0, 0.35, 0.05), limits = c(0.05, 0.36)) +
  labs(title = "b. Race and Ethnicity", x = "", y = "MSE at age 75") +
  scale_color_manual(values = c("navy", "red", "#666666")) +
  scale_shape_manual(values = c(15, 19, 17, 3), guide = "none") +
  theme(plot.title = element_text(size=10, hjust = 0.5, face = "bold"),
        axis.text.x = element_text(size = 8.5),
        axis.title.x = element_text(size = 9),
        legend.title = element_blank(),
        legend.position=c('bottom'),
        legend.background = element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.box.margin=margin(-10,0,0.5,0),
        plot.margin=unit(c(0.2,0.1,0.2,0.2), "cm")) +
  guides(color=guide_legend(override.aes = list(shape = 15, size =4)))


c <- mse_df %>% 
  filter(cat == "Education") %>% 
  mutate(group = case_when(group == "Low education" ~ "Low", 
                           group == "Middle education" ~ "Middle", 
                           group == "High education" ~ "High")) %>% 
  ggplot() +
  geom_point(aes(x = model, y = mse, color = fct_relevel(group, "Low", "Middle", "High"), shape = model_cat), alpha=.6, size = 2) +
  geom_hline(yintercept=0.25, linetype="dashed", color = "black", alpha = 0.8) +
  theme_bw() + 
  coord_flip() +
  scale_y_continuous(minor_breaks = seq(0, 0.35, 0.05), 
                     breaks = seq(0, 0.35, 0.05), limits = c(0.05, 0.35)) +
  labs(title = "c. Education", x = "", y = "MSE at age 75") +
  scale_color_manual(values = c("navy", "red", "#666666")) +
  scale_shape_manual(values = c(15, 19, 17, 3), guide = "none") +
  theme(plot.title = element_text(size=10, hjust = 0.5, face = "bold"),
        axis.text.x = element_text(size = 8.5),
        axis.title.x = element_text(size = 9),
        legend.title = element_blank(),
        legend.position=c('bottom'),
        legend.background = element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.box.margin=margin(-10,0,0.5,0),
        plot.margin=unit(c(0.2,0.1,0.2,0.2), "cm")) +
  guides(color=guide_legend(override.aes = list(shape = 15, size =4)))

plot1 <- ggarrange(a, b, c, ncol=3, 
                   widths = c(4, 3, 3))

plot1

ggsave("8_outputs/figures/plot_subjective_mse.pdf", width = 8, height = 4.4)

# Mean error

me_df$model_cat <- case_when(me_df$model == "KM" |
                                me_df$model == "Cox Reduced" ~ "1",
                                me_df$model == "Cox Full" |
                                me_df$model == "CoxNet" |
                                me_df$model == "Cox-TV" |
                                me_df$model == "Gompertz" ~ "2",
                                me_df$model == "DeepHit" |
                                me_df$model == "DeepPCH" |
                                me_df$model == "DeepSurv" |
                                me_df$model == "GradBoost" |
                                me_df$model == "RSF" |
                                me_df$model == "RRF-TV" ~ "3", 
                                me_df$model == "Subjective" ~ "4")

d <- me_df %>% 
  filter(cat == "Gender") %>% 
  ggplot() +
  geom_point(aes(x = model, y = me, color = group, shape = model_cat), alpha=.6, size = 2) +
  geom_hline(yintercept=0, linetype="dashed", color = "black", alpha = 0.8) +
  theme_bw() + 
  coord_flip() +
  scale_y_continuous(minor_breaks = seq(-0.15, 0.15, 0.05), 
                     breaks = seq(-0.15, 0.15, 0.05), limits = c(-0.15, 0.15), 
                     labels = comma) +
  labs(title = "a. Gender", x = "", y = "ME at age 75") +
  scale_color_manual(values = c("navy", "red", "#999999")) +
  scale_shape_manual(values = c(15, 19, 17, 3), guide = "none") +
  theme(plot.title = element_text(size=10, hjust = 0.5, face = "bold"),
        axis.text.x = element_text(size = 8.5),
        axis.title.x = element_text(size = 9),
        legend.title = element_blank(),
        legend.position=c('bottom'),
        legend.background = element_blank(),
        #axis.title.y=element_blank(),
        #axis.text.y=element_blank(),
        #axis.ticks.y=element_blank(),
        legend.box.margin=margin(-10,0,0.5,0),
        plot.margin=unit(c(0.2,0.1,0.2,0.2), "cm")) +
  guides(color=guide_legend(override.aes = list(shape = 15, size =4)))


e <- me_df %>% 
  filter(cat == "Race") %>% 
  mutate(group = case_when(group == "Non-Hispanic white" ~ "Whites", 
                           group == "Non-Hispanic Black" ~ "Blacks", 
                           group == "Hispanic" ~ "Hisp.")) %>% 
  ggplot() +
  geom_point(aes(x = model, y = me, color = fct_relevel(group, "Hisp.", "Blacks", "Whites"), shape = model_cat), alpha=.6, size = 2) +
  geom_hline(yintercept=0, linetype="dashed", color = "black", alpha = 0.8) +
  theme_bw() + 
  coord_flip() +
  scale_y_continuous(minor_breaks = seq(-0.15, 0.15, 0.05), 
                     breaks = seq(-0.15, 0.15, 0.05), limits = c(-0.15, 0.15), 
                     labels = comma) +
  labs(title = "b. Race and Ethnicity", x = "", y = "ME at age 75") +
  scale_color_manual(values = c("navy", "red", "#666666")) +
  scale_shape_manual(values = c(15, 19, 17, 3), guide = "none") +
  theme(plot.title = element_text(size=10, hjust = 0.5, face = "bold"),
        axis.text.x = element_text(size = 8.5),
        axis.title.x = element_text(size = 9),
        legend.title = element_blank(),
        legend.position=c('bottom'),
        legend.background = element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.box.margin=margin(-10,0,0.5,0),
        plot.margin=unit(c(0.2,0.1,0.2,0.2), "cm")) +
  guides(color=guide_legend(override.aes = list(shape = 15, size =4)))


f <- me_df %>% 
  filter(cat == "Education") %>% 
  mutate(group = case_when(group == "Low education" ~ "Low", 
                           group == "Middle education" ~ "Middle", 
                           group == "High education" ~ "High")) %>% 
  ggplot() +
  geom_point(aes(x = model, y = me, color = fct_relevel(group, "Low", "Middle", "High"), shape = model_cat), alpha=.6, size = 2) +
  geom_hline(yintercept=0, linetype="dashed", color = "black", alpha = 0.8) +
  theme_bw() + 
  coord_flip() +
  scale_y_continuous(minor_breaks = seq(-0.15, 0.15, 0.05), 
                     breaks = seq(-0.15, 0.15, 0.05), limits = c(-0.15, 0.15), 
                     labels = comma) +
  labs(title = "c. Education", x = "", y = "ME at age 75") +
  scale_color_manual(values = c("navy", "red", "#666666")) +
  scale_shape_manual(values = c(15, 19, 17, 3), guide = "none") +
  theme(plot.title = element_text(size=10, hjust = 0.5, face = "bold"),
        axis.text.x = element_text(size = 8.5),
        axis.title.x = element_text(size = 9),
        legend.title = element_blank(),
        legend.position=c('bottom'),
        legend.background = element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.box.margin=margin(-10,0,0.5,0),
        plot.margin=unit(c(0.2,0.1,0.2,0.2), "cm")) +
  guides(color=guide_legend(override.aes = list(shape = 15, size =4)))

plot2 <- ggarrange(d, e, f, ncol=3, 
                   widths = c(4, 3, 3))

plot2

ggsave("8_outputs/figures/plot_subjective_me.pdf", width = 9, height = 3.5)

# graph calibration 


data <- data %>% filter(model != "KM")  

# gender
data_cal_gender <- data %>%
  mutate(prob_group = case_when(surv_prob <= 0.1 ~ 0.05,
                                surv_prob <= 0.2 ~ 0.15,
                                surv_prob <= 0.3 ~ 0.25,
                                surv_prob <= 0.4 ~ 0.35,
                                surv_prob <= 0.5 ~ 0.45,
                                surv_prob <= 0.6 ~ 0.55,
                                surv_prob <= 0.7 ~ 0.65,
                                surv_prob <= 0.8 ~ 0.75,
                                surv_prob <= 0.9 ~ 0.85,
                                surv_prob <= 1 ~ 0.95)) %>% 
  group_by(prob_group, model, r_gender) %>% 
  summarize(true_proportion = mean(outcome, na.rm=TRUE)) %>% 
  mutate(cat = 'Gender') %>% 
  rename(group = r_gender) 

# race 
data_cal_race <- data %>%
  mutate(prob_group = case_when(surv_prob <= 0.1 ~ 0.05,
                                surv_prob <= 0.2 ~ 0.15,
                                surv_prob <= 0.3 ~ 0.25,
                                surv_prob <= 0.4 ~ 0.35,
                                surv_prob <= 0.5 ~ 0.45,
                                surv_prob <= 0.6 ~ 0.55,
                                surv_prob <= 0.7 ~ 0.65,
                                surv_prob <= 0.8 ~ 0.75,
                                surv_prob <= 0.9 ~ 0.85,
                                surv_prob <= 1 ~ 0.95)) %>% 
  group_by(prob_group, model, r_race) %>% 
  summarize(true_proportion = mean(outcome, na.rm=TRUE)) %>% 
  mutate(cat = 'Race') %>% 
  rename(group = r_race) 

# education 
data_cal_education <- data %>%
  mutate(prob_group = case_when(surv_prob <= 0.1 ~ 0.05,
                                surv_prob <= 0.2 ~ 0.15,
                                surv_prob <= 0.3 ~ 0.25,
                                surv_prob <= 0.4 ~ 0.35,
                                surv_prob <= 0.5 ~ 0.45,
                                surv_prob <= 0.6 ~ 0.55,
                                surv_prob <= 0.7 ~ 0.65,
                                surv_prob <= 0.8 ~ 0.75,
                                surv_prob <= 0.9 ~ 0.85,
                                surv_prob <= 1 ~ 0.95)) %>% 
  group_by(prob_group, model, r_education) %>% 
  summarize(true_proportion = mean(outcome, na.rm=TRUE))  %>% 
  mutate(cat = 'Education') %>% 
  rename(group = r_education) 

data_cal <- rbind(data_cal_gender, data_cal_race, data_cal_education)

data_cal <- data_cal %>% mutate(subjective = case_when(model == "Subjective" ~ "1", 
                                                       model != "Subjective" ~ "0"))

#palette.colors(palette = "Tableau 10")
col_pal = c("#4E79A7", "#F28E2B", "#E15759", "#76B7B2", "#59A14F", "#EDC948", 
            "#B07AA1", "#FF9DA7", "#9C755F", "#BAB0AC", "hotpink", "red")

data_cal$model_cat <- case_when(data_cal$model == "Cox Reduced" ~ "1",
                                data_cal$model == "Cox Full" |
                                data_cal$model == "CoxNet" |
                                data_cal$model == "Cox-TV" |
                                data_cal$model == "Gompertz" ~ "2",
                                data_cal$model == "DeepHit" |
                                data_cal$model == "DeepPCH" |
                                data_cal$model == "DeepSurv" |
                                data_cal$model == "GradBoost" |
                                data_cal$model == "RSF" |
                                data_cal$model == "RRF-TV" ~ "3", 
                                data_cal$model == "Subjective" ~ "4")


data_cal <- data_cal %>% mutate(model = fct_relevel(model, 'Cox Reduced',
                                                    'Cox Full', 'CoxNet', 'Cox-TV', 'Gompertz',
                                                    'DeepHit','DeepPCH' , 'DeepSurv', 'GradBoost' , 'RSF', 'RRF-TV',
                                                    'Subjective'))

override.shape <- c(15, 19, 19, 19, 19, 17, 17, 17, 17, 17, 17, 3)

# Gender
g1 <- data_cal %>% 
  filter(group == "Men") %>% 
  ggplot() + 
  geom_point(aes(x = prob_group, y = true_proportion, color = model, 
                 alpha = subjective, shape = model_cat ),  size = 1.5) +
  geom_line(aes(x = prob_group, y = true_proportion, color = model, 
                size = subjective, alpha = subjective)) +
  geom_abline(intercept = 0, slope = 1, linetype = 'dashed') +
  theme_bw() + 
  scale_x_continuous(minor_breaks = seq(0, 1, 0.1), breaks = seq(0, 1, 0.2), 
                     limits = c(0, 1)) +
  scale_y_continuous(minor_breaks = seq(0, 1, 0.1), breaks = seq(0, 1, 0.2), 
                     limits = c(0, 1)) +
  labs(title = "Men", x = "", y = "True proportion alive") +
  scale_color_manual(values = col_pal) +
  scale_size_manual(values = c(0.4, 0.8), guide = 'none') +
  scale_alpha_manual(values = c(0.6, 0.95), guide = 'none') +
  scale_shape_manual(values = c(15, 19, 17, 3), guide = "none") +
  theme(plot.title = element_text(size=11, hjust = 0.5),
        axis.text.x = element_text(size = 8.5),
        axis.text.y = element_text(size = 8.5),
        legend.title = element_blank(),
        legend.position=c('bottom'),
        legend.background = element_blank(),
        legend.box.margin=margin(-10,0,0.5,0),
        plot.margin=unit(c(0.2,0.02,-0.45,0.2), "cm")) +
  guides(color=guide_legend(override.aes = list(shape = override.shape),nrow=2,byrow=TRUE)) 


g2 <- data_cal %>% 
  filter(group == "Women") %>% 
  ggplot() + 
  geom_point(aes(x = prob_group, y = true_proportion, color = model, 
                 alpha = subjective, shape = model_cat ),  size = 1.5) +
  geom_line(aes(x = prob_group, y = true_proportion, color = model, 
                size = subjective, alpha = subjective)) +
  geom_abline(intercept = 0, slope = 1, linetype = 'dashed') +
  theme_bw() + 
  scale_x_continuous(minor_breaks = seq(0, 1, 0.1), breaks = seq(0, 1, 0.2), 
                     limits = c(0, 1)) +
  scale_y_continuous(minor_breaks = seq(0, 1, 0.1), breaks = seq(0, 1, 0.2), 
                     limits = c(0, 1)) +
  labs(title = "Women", x = "", y = "") +
  scale_color_manual(values = col_pal) +
  scale_size_manual(values = c(0.4, 0.8), guide = 'none') +
  scale_alpha_manual(values = c(0.6, 0.95), guide = 'none') +
  scale_shape_manual(values = c(15, 19, 17, 3), guide = "none") +
  theme(plot.title = element_text(size=11, hjust = 0.5),
        axis.text.x = element_text(size = 8.5),
        axis.text.y = element_text(size = 8.5),
        legend.title = element_blank(),
        legend.position=c('bottom'),
        legend.background = element_blank(),
        legend.box.margin=margin(-10,0,0.5,0),
        plot.margin=unit(c(0.2,0.2,-0.45,0.02), "cm")) +
  guides(color=guide_legend(override.aes = list(shape = override.shape),nrow=2,byrow=TRUE)) 

plot_gender <- ggarrange(g1, g2, NA, 
                         ncol=3,
                         common.legend = TRUE,
                         legend = "none",
                         widths = c(3.05, 3, 3))

plot_gender <- annotate_figure(plot_gender, top = text_grob("a. Gender", face = "bold", size = 11))

plot_gender


# Race
h1 <- data_cal %>% 
  filter(group == "Hispanic") %>% 
  ggplot() + 
  geom_point(aes(x = prob_group, y = true_proportion, color = model, 
                 alpha = subjective, shape = model_cat ),  size = 1.5) +
  geom_line(aes(x = prob_group, y = true_proportion, color = model, 
                size = subjective, alpha = subjective)) +
  geom_abline(intercept = 0, slope = 1, linetype = 'dashed') +
  theme_bw() + 
  scale_x_continuous(minor_breaks = seq(0, 1, 0.1), breaks = seq(0, 1, 0.2), 
                     limits = c(0, 1)) +
  scale_y_continuous(minor_breaks = seq(0, 1, 0.1), breaks = seq(0, 1, 0.2), 
                     limits = c(0, 1)) +
  labs(title = "Hispanics", x = "", y = "True proportion alive") +
  scale_color_manual(values = col_pal) +
  scale_size_manual(values = c(0.4, 0.8), guide = 'none') +
  scale_alpha_manual(values = c(0.6, 0.95), guide = 'none') +
  scale_shape_manual(values = c(15, 19, 17, 3), guide = "none") +
  theme(plot.title = element_text(size=11, hjust = 0.5),
        axis.text.x = element_text(size = 8.5),
        axis.text.y = element_text(size = 8.5),
        legend.title = element_blank(),
        legend.position=c('bottom'),
        legend.background = element_blank(),
        legend.box.margin=margin(-10,0,0.5,0),
        plot.margin=unit(c(0.2,0.02,-0.45,0.2), "cm")) +
  guides(color=guide_legend(override.aes = list(shape = override.shape),nrow=2,byrow=TRUE)) 


h2 <- data_cal %>% 
  filter(group == "Non-Hispanic Black") %>% 
  ggplot() + 
  geom_point(aes(x = prob_group, y = true_proportion, color = model, 
                 alpha = subjective, shape = model_cat ),  size = 1.5) +
  geom_line(aes(x = prob_group, y = true_proportion, color = model, 
                size = subjective, alpha = subjective)) +
  geom_abline(intercept = 0, slope = 1, linetype = 'dashed') +
  theme_bw() + 
  scale_x_continuous(minor_breaks = seq(0, 1, 0.1), breaks = seq(0, 1, 0.2), 
                     limits = c(0, 1)) +
  scale_y_continuous(minor_breaks = seq(0, 1, 0.1), breaks = seq(0, 1, 0.2), 
                     limits = c(0, 1)) +
  labs(title = "NH Blacks", x = "", y = "") +
  scale_color_manual(values = col_pal) +
  scale_size_manual(values = c(0.4, 0.8), guide = 'none') +
  scale_alpha_manual(values = c(0.6, 0.95), guide = 'none') +
  scale_shape_manual(values = c(15, 19, 17, 3), guide = "none") +
  theme(plot.title = element_text(size=11, hjust = 0.5),
        axis.text.x = element_text(size = 8.5),
        axis.text.y = element_text(size = 8.5),
        legend.title = element_blank(),
        legend.position=c('bottom'),
        legend.background = element_blank(),
        legend.box.margin=margin(-10,0,0.5,0),
        plot.margin=unit(c(0.2,0.02,-0.45,0.02), "cm")) +
  guides(color=guide_legend(override.aes = list(shape = override.shape),nrow=2,byrow=TRUE)) 

h3 <- data_cal %>% 
  filter(group == "Non-Hispanic white") %>% 
  ggplot() + 
  geom_point(aes(x = prob_group, y = true_proportion, color = model, 
                 alpha = subjective, shape = model_cat ),  size = 1.5) +
  geom_line(aes(x = prob_group, y = true_proportion, color = model, 
                size = subjective, alpha = subjective)) +
  geom_abline(intercept = 0, slope = 1, linetype = 'dashed') +
  theme_bw() + 
  scale_x_continuous(minor_breaks = seq(0, 1, 0.1), breaks = seq(0, 1, 0.2), 
                     limits = c(0, 1)) +
  scale_y_continuous(minor_breaks = seq(0, 1, 0.1), breaks = seq(0, 1, 0.2), 
                     limits = c(0, 1)) +
  labs(title = "NH Whites", x = "", y = "") +
  scale_color_manual(values = col_pal) +
  scale_size_manual(values = c(0.4, 0.8), guide = 'none') +
  scale_alpha_manual(values = c(0.6, 0.95), guide = 'none') +
  scale_shape_manual(values = c(15, 19, 17, 3), guide = "none") +
  theme(plot.title = element_text(size=11, hjust = 0.5),
        axis.text.x = element_text(size = 8.5),
        axis.text.y = element_text(size = 8.5),
        legend.title = element_blank(),
        legend.position=c('bottom'),
        legend.background = element_blank(),
        legend.box.margin=margin(-10,0,0.5,0),
        plot.margin=unit(c(0.2,0.2,-0.45,0.02), "cm")) +
  guides(color=guide_legend(override.aes = list(shape = override.shape),nrow=2,byrow=TRUE)) 

plot_race <- ggarrange(h1, h2, h3, 
                         ncol=3,
                         common.legend = TRUE,
                         legend = "none",
                         widths = c(3.05, 3, 3))

plot_race <- annotate_figure(plot_race, top = text_grob("b. Race and Ethnicity", face = "bold", size = 11))

plot_race


# Education
i1 <- data_cal %>% 
  filter(group == "Low education") %>% 
  ggplot() + 
  geom_point(aes(x = prob_group, y = true_proportion, color = model, 
                 alpha = subjective, shape = model_cat ),  size = 1.5) +
  geom_line(aes(x = prob_group, y = true_proportion, color = model, 
                size = subjective, alpha = subjective)) +
  geom_abline(intercept = 0, slope = 1, linetype = 'dashed') +
  theme_bw() + 
  scale_x_continuous(minor_breaks = seq(0, 1, 0.1), breaks = seq(0, 1, 0.2), 
                     limits = c(0, 1)) +
  scale_y_continuous(minor_breaks = seq(0, 1, 0.1), breaks = seq(0, 1, 0.2), 
                     limits = c(0, 1)) +
  labs(title = "Low", x = "Predicted survival probability", y = "True proportion alive") +
  scale_color_manual(values = col_pal) +
  scale_size_manual(values = c(0.4, 0.8), guide = 'none') +
  scale_alpha_manual(values = c(0.6, 0.95), guide = 'none') +
  scale_shape_manual(values = c(15, 19, 17, 3), guide = "none") +
  theme(plot.title = element_text(size=11, hjust = 0.5),
        axis.text.x = element_text(size = 8.5),
        axis.text.y = element_text(size = 8.5),
        legend.spacing.y = unit(0, "mm"), 
        legend.title = element_blank(),
        legend.position=c('bottom'),
        legend.background = element_blank(),
        legend.box.margin=margin(-10,0,0.5,0),
        plot.margin=unit(c(0.2,0.02,0.2,0.2), "cm")) +
  guides(color=guide_legend(override.aes = list(shape = override.shape),nrow=2,byrow=TRUE)) 


i2 <- data_cal %>% 
  filter(group == "Middle education") %>% 
  ggplot() + 
  geom_point(aes(x = prob_group, y = true_proportion, color = model, 
                 alpha = subjective, shape = model_cat ),  size = 1.5) +
  geom_line(aes(x = prob_group, y = true_proportion, color = model, 
                size = subjective, alpha = subjective)) +
  geom_abline(intercept = 0, slope = 1, linetype = 'dashed') +
  theme_bw() + 
  scale_x_continuous(minor_breaks = seq(0, 1, 0.1), breaks = seq(0, 1, 0.2), 
                     limits = c(0, 1)) +
  scale_y_continuous(minor_breaks = seq(0, 1, 0.1), breaks = seq(0, 1, 0.2), 
                     limits = c(0, 1)) +
  labs(title = "Middle", x = "Predicted survival probability", y = "") +
  scale_color_manual(values = col_pal) +
  scale_size_manual(values = c(0.4, 0.8), guide = 'none') +
  scale_alpha_manual(values = c(0.6, 0.95), guide = 'none') +
  scale_shape_manual(values = c(15, 19, 17, 3), guide = "none") +
  theme(plot.title = element_text(size=11, hjust = 0.5),
        axis.text.x = element_text(size = 8.5),
        legend.spacing.y = unit(0, "mm"), 
        axis.text.y = element_text(size = 8.5),
        legend.title = element_blank(),
        legend.position=c('bottom'),
        legend.background = element_blank(),
        legend.box.margin=margin(-10,0,0.5,0),
        plot.margin=unit(c(0.2,0.02,0.2,0.02), "cm")) +
  guides(color=guide_legend(override.aes = list(shape = override.shape),nrow=2,byrow=TRUE)) 

i3 <- data_cal %>% 
  filter(group == "High education") %>% 
  ggplot() + 
  geom_point(aes(x = prob_group, y = true_proportion, color = model, 
                 alpha = subjective, shape = model_cat ),  size = 1.5) +
  geom_line(aes(x = prob_group, y = true_proportion, color = model, 
                size = subjective, alpha = subjective)) +
  geom_abline(intercept = 0, slope = 1, linetype = 'dashed') +
  theme_bw() + 
  scale_x_continuous(minor_breaks = seq(0, 1, 0.1), breaks = seq(0, 1, 0.2), 
                     limits = c(0, 1)) +
  scale_y_continuous(minor_breaks = seq(0, 1, 0.1), breaks = seq(0, 1, 0.2), 
                     limits = c(0, 1)) +
  labs(title = "High", x = "Predicted survival probability", y = "") +
  scale_color_manual(values = col_pal) +
  scale_size_manual(values = c(0.4, 0.8), guide = 'none') +
  scale_alpha_manual(values = c(0.6, 0.95), guide = 'none') +
  scale_shape_manual(values = c(15, 19, 17, 3), guide = "none") +
  theme(plot.title = element_text(size=11, hjust = 0.5),
        axis.text.x = element_text(size = 8.5),
        axis.text.y = element_text(size = 8.5),
        legend.spacing.y = unit(0, "mm"), 
        legend.title = element_blank(),
        legend.position=c('bottom'),
        legend.background = element_blank(),
        legend.box.margin=margin(-10,0,0.5,0),
        plot.margin=unit(c(0.2,0.2,0.2,0.02), "cm")) +
  guides(color=guide_legend(override.aes = list(shape = override.shape),nrow=2,byrow=TRUE)) 

plot_education <- ggarrange(i1, i2, i3, 
                       ncol=3,
                       common.legend = TRUE,
                       legend = "bottom",
                       widths = c(3.05, 3, 3))

plot_education <- annotate_figure(plot_education, top = text_grob("c. Education", face = "bold", size = 11))

plot_education


ggarrange(plot_gender, 
          plot_race,
          plot_education, 
          ncol=1,
          heights = c(2.96, 3, 3.9))

ggsave("8_outputs/figures/plot_calibration.pdf", width = 7.5, height = 8.1)


end 

