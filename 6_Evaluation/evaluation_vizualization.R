
# Clear the environment 
rm(list = ls())

# Install and load the required packages
listofpackages = c('dplyr', 'ggplot2', 'ggpubr', 'purrr', 'tidyverse', 'haven',
                   'stringr', 'viridis', 'grid', 'scales', 'ggthemes')
for (j in listofpackages){
  if(sum(installed.packages()[, 1] == 0)) {
    install.packages(j)
  }
  library(j, character.only = T)
}

# Load brier score data
data_brier <- read.csv("6_evaluation/minus_backimpd/all/brier.csv") %>% 
  select(-X) %>% 
  pivot_longer(!time, names_to = "model", values_to = "value")
  

data_brier$value <- as.numeric(data_brier$value)
data_brier$cat = 'Time-dependent Brier score'

data_brier$model_cat <- case_when(data_brier$model == "KM" ~ "1",
                                  data_brier$model == "CoxReduced" ~ "1",
                                  data_brier$model == "Cox" |
                                  data_brier$model == "CoxNet" |
                                  data_brier$model == "Cox.TV" |
                                  data_brier$model == "Gompertz" ~ "2",
                                  data_brier$model == "DeepHit" |
                                  data_brier$model == "DeepPCH" |
                                  data_brier$model == "DeepSurv" |
                                  data_brier$model == "GradBoost" |
                                  data_brier$model == "RSF" |
                                  data_brier$model == "RRF.TV" ~ "3")

data_brier[data_brier$model=="CoxReduced", "model"] <- "Cox Reduced"
data_brier[data_brier$model=="Cox", "model"] <- "Cox Full"
data_brier[data_brier$model=="DeepPCH", "model"] <- "DeepPCH"
data_brier[data_brier$model=="Cox.TV", "model"] <- "Cox-TV"
data_brier[data_brier$model=="RRF.TV", "model"] <- "RRF-TV"

# Load AUC data
data_auc <- read.csv("6_evaluation/minus_backimpd/all/auc.csv") %>% 
  select(-X) %>% 
  pivot_longer(!time, names_to = "model", values_to = "value")

data_auc$value <- as.numeric(data_auc$value)
data_auc$cat = 'Area Under the Curve'

data_auc$model_cat <- case_when(data_auc$model == "KM" ~ "1",
                                data_auc$model == "CoxReduced" ~ "1",
                                data_auc$model == "Cox" |
                                  data_auc$model == "CoxNet" |
                                  data_auc$model == "Cox.TV" |
                                  data_auc$model == "Gompertz" ~ "2",
                                  data_auc$model == "DeepHit" |
                                  data_auc$model == "DeepPCH" |
                                  data_auc$model == "DeepSurv" |
                                  data_auc$model == "GradBoost" |
                                  data_auc$model == "RSF" |
                                  data_auc$model == "RRF.TV" ~ "3")

data_auc[data_auc$model=="CoxReduced", "model"] <- "Cox Reduced"
data_auc[data_auc$model=="Cox", "model"] <- "Cox Full"
data_auc[data_auc$model=="DeepPCH", "model"] <- "DeepPCH"
data_auc[data_auc$model=="Cox.TV", "model"] <- "Cox-TV"
data_auc[data_auc$model=="RRF.TV", "model"] <- "RRF-TV"

override.shape <- c(15, 15, 19, 19, 19, 19, 17, 17, 17, 17, 17, 17)


level_order_model <- c("Cox Reduced", 'KM',
                       'Cox Full', 'CoxNet', 'Cox-TV', 'Gompertz',
                       'DeepHit','DeepPCH' , 'DeepSurv', 'GradBoost' , 'RSF', 'RRF-TV')

data_brier$model <- factor(data_brier$model, level = level_order_model)
data_auc$model <- factor(data_auc$model, level = level_order_model)

#palette.colors(palette = "Tableau 10")
col_pal = c("#4E79A7", "#666666", "#F28E2B", "#E15759", "#76B7B2", "#59A14F", "#EDC948", 
            "#B07AA1", "#FF9DA7", "#9C755F", "#BAB0AC", "hotpink") 

a = data_brier %>% 
  ggplot() + 
  geom_point(aes(x= time, y = value, color = model, shape = model_cat), size = 1.5) +
  geom_line(aes(x= time, y = value, color = model), size = 0.8) +
  geom_hline(yintercept=0, linetype="dashed", color = "black", alpha = 0.8) +
  theme_bw() + 
  scale_x_continuous(minor_breaks = seq(0, 25, 5), breaks = seq(0, 25, 5), limits = c(0, 25)) +
  scale_y_continuous(minor_breaks = seq(0, 0.25, 0.05), breaks = seq(0, 0.25, 0.05), limits = c(0, 0.26)) +
  labs(title = "a. Time-dependent Brier Score", x = "Time in the study", y = "") +
  scale_color_manual(values = col_pal) +
  scale_shape_manual(values = c(15, 19, 17), guide = "none") +
  theme(plot.title = element_text(size=11, hjust = 0.5, face = "bold"),
        axis.text.x = element_text(size = 10.5),
        axis.title.x = element_text(size = 11.5),
        axis.text.y = element_text(size = 10.5),
        axis.title.y = element_text(size = 11.5),
        legend.text=element_text(size=10),
        legend.position = "bottom", 
        legend.title = element_blank(),
        legend.key.size = unit(0.4, 'cm'),
        legend.margin=margin(0,0,0,0),
        #plot.margin=unit(c(-0.5,0.5,0,0.5), "cm"),
        legend.box.margin=margin(0.5,-1,0.5,-1)) +
  guides(color=guide_legend(override.aes = list(shape = override.shape),nrow=2,byrow=TRUE)) 

b = data_auc %>% 
  ggplot() + 
  geom_point(aes(x= time, y = value, color = model, shape = model_cat), size = 1.5) +
  geom_line(aes(x= time, y = value, color = model), size = 0.8) +
  geom_hline(yintercept=1, linetype="dashed", color = "black", alpha = 0.8) +
  theme_bw() + 
  scale_x_continuous(minor_breaks = seq(0, 25, 5), breaks = seq(0, 25, 5), limits = c(0, 25)) +
  scale_y_continuous(minor_breaks = seq(0.5, 1, .05), breaks = seq(0.5, 1, .05), limits = c(0.78, 1)) +
  labs(title = "b. Area Under the Curve (AUC)", x = "Time in the study", y = "") +
  scale_color_manual(values = col_pal) +
  scale_shape_manual(values = c(15, 19, 17), guide = "none") +
  theme(plot.title = element_text(size=11, hjust = 0.5, face = "bold"),
        axis.text.x = element_text(size = 10.5),
        axis.title.x = element_text(size = 11.5),
        axis.text.y = element_text(size = 10.5),
        axis.title.y = element_text(size = 11.5),
        legend.text=element_text(size=10),
        legend.position = "bottom", legend.title = element_blank(),
        legend.key.size = unit(0.4, 'cm'),
        legend.margin=margin(0,0,0,0),
        #plot.margin=unit(c(-0.5,0.5,0,0.5), "cm"),
        legend.box.margin=margin(0.5,-1,0.5,-1)) +
  guides(color=guide_legend(override.aes = list(shape = override.shape),nrow=2,byrow=TRUE)) 

# theme(legend.title = element_blank(),
#         legend.spacing.y = unit(-1, "mm"), 
#         legend.key.size = unit(0.4, 'cm'),
#         legend.position=c(0.65,0.25),
#         legend.background = element_blank(),
#         legend.box.background = element_rect(colour = "black")) +
#   guides(color=guide_legend(nrow=4,byrow=TRUE))

ggarrange(a, b, 
          ncol=2,
          common.legend = TRUE,
          legend = "bottom")


ggsave("8_outputs/figures/plot_evaluation.pdf", width = 8.5, height = 4)


# Load brier score data by gender 
data_brier_men <- read.csv("6_evaluation/minus_backimpd/all/int_brier_gender_men.csv") %>% 
  rename(model = X, value = IBS)

data_brier_men$gender <- "Men"

data_brier_women <- read.csv("6_evaluation/minus_backimpd/all/int_brier_gender_women.csv") %>% 
  rename(model = X, value = IBS)

data_brier_women$gender <- "Women"

data_brier_gender <- rbind(data_brier_men, data_brier_women)
data_brier_gender$index = "Integrated Brier Score"

# Load mean AUC data by gender 
data_auc_men <- read.csv("6_evaluation/minus_backimpd/all/mean_auc_gender_men.csv") %>% 
  rename(model = X, value = Mean.AUC)

data_auc_men$gender <- "Men"

data_auc_women <- read.csv("6_evaluation/minus_backimpd/all/mean_auc_gender_women.csv") %>% 
  rename(model = X, value = Mean.AUC)

data_auc_women$gender <- "Women"

data_auc_gender <- rbind(data_auc_men, data_auc_women)
data_auc_gender$index = "Mean AUC"

data_gender <- rbind(data_brier_gender, data_auc_gender)

data_gender$model_cat <- case_when(data_gender$model == "KM" ~ "1",
                                   data_gender$model == "CoxReduced" ~ "1",
                                data_gender$model == "Cox" |
                                  data_gender$model == "CoxNet" |
                                  data_gender$model == "Cox-TV" |
                                  data_gender$model == "Gompertz" ~ "2",
                                data_gender$model == "DeepHit" |
                                  data_gender$model == "DeepPCH" |
                                  data_gender$model == "DeepSurv" |
                                  data_gender$model == "GradBoost" |
                                  data_gender$model == "RSF" |
                                  data_gender$model == "RRF-TV" ~ "3")


data_gender[data_gender$model=="CoxReduced", "model"] <- "Cox Reduced"
data_gender[data_gender$model=="Cox", "model"] <- "Cox Full"
data_gender[data_gender$model=="DeepPCH", "model"] <- "DeepPCH"
data_gender[data_gender$model=="RRF.TV", "model"] <- "RRF-TV"

level_order <- c('RRF-TV', 'RSF','GradBoost', 'DeepSurv', 'DeepPCH' , 'DeepHit',
                 'Gompertz', 'Cox-TV', 'CoxNet', 'Cox Full', 
                 'KM', 'Cox Reduced')


a1 <- data_gender %>% 
  filter(index == "Integrated Brier Score", model != "CIF-TV") %>% 
  ggplot() + 
  geom_point(aes(x=factor(model, level = level_order), y=value, color = gender, shape = model_cat), alpha=.6, size = 2) +
  geom_hline(yintercept=0, linetype="dashed", color = "black", alpha = 0.8) +
  scale_y_continuous(minor_breaks = seq(0, 0.3, 0.05), breaks = seq(0, 0.3, 0.05), limits = c(0, 0.25)) +
  theme_bw() + 
  coord_flip() +
  labs(title = "Int. Brier Score", x = "", y = "", color="                    ") +
  scale_color_manual(values = c("navy", "red")) +
  scale_shape_manual(values = c(15, 19, 17), guide = "none") +
  theme(plot.title = element_text(size=11, hjust = 0.5),
        axis.text.x = element_text(size = 10.5),
        axis.title.x = element_text(size = 11.5),
        axis.text.y = element_text(size = 10.5),
        axis.title.y = element_text(size = 11.5),
        legend.text=element_text(size=10),
        legend.title = element_text(),
        legend.position=c('bottom'),
        legend.background = element_blank(),
        legend.box.margin=margin(-20,0,0.5,0),
        plot.margin=unit(c(0.2,0.2,0.2,0.2), "cm")) +
  guides(color=guide_legend(override.aes = list(shape = 15, size =4)))


a2 <-data_gender %>% 
  filter(index == "Mean AUC", model != "CIF-TV", model != "KM") %>% 
  ggplot() + 
  geom_point(aes(x=factor(model, level = level_order), y=value, color = gender, shape = model_cat), alpha=.6, size = 2) +
  geom_hline(yintercept=1, linetype="dashed", color = "black", alpha = 0.8) +
  scale_y_continuous(minor_breaks = seq(0.5, 1, 0.05), breaks = seq(0.5, 1, 0.05), limits = c(0.75, 1)) +
  theme_bw() + 
  coord_flip() +
  labs(title = "Mean AUC", x = "", y = "", color="                    ") +
  scale_color_manual(values = c("navy", "red")) +
  scale_shape_manual(values = c(15, 19, 17), guide = "none") +
  theme(plot.title = element_text(size=11, hjust = 0.5),
        axis.text.x = element_text(size = 10.5),
        axis.title.x = element_text(size = 11.5),
        axis.text.y = element_text(size = 10.5),
        axis.title.y = element_text(size = 11.5),
        legend.text=element_text(size=10),
        legend.title = element_text(),
        legend.position=c('bottom'),
        legend.background = element_blank(),
        legend.box.margin=margin(-20,0,0.5,0),
        plot.margin=unit(c(-0.3,0.2,0.2,0.2), "cm")) +
  guides(color=guide_legend(override.aes = list(shape = 15, size =4)))

plot_gender <- ggarrange(a1, a2, 
          ncol=1,
          common.legend = TRUE,
          legend = "bottom")

plot_gender <- annotate_figure(plot_gender, top = text_grob("                      a. Gender", face = "bold", size = 11))

plot_gender


#plot_gender

# Race and ethnicity

# Load brier score data by race
data_brier_white <- read.csv("6_evaluation/minus_backimpd/all/int_brier_race_white.csv") %>% 
  rename(model = X, value = IBS)
data_brier_white$race <- "NH Whites"

data_brier_black <- read.csv("6_evaluation/minus_backimpd/all/int_brier_race_black.csv") %>% 
  rename(model = X, value = IBS)
data_brier_black$race <- "NH Blacks"

data_brier_hispanic <- read.csv("6_evaluation/minus_backimpd/all/int_brier_race_hispanic.csv") %>% 
  rename(model = X, value = IBS)
data_brier_hispanic$race <- "Hispanics"

data_brier_race <- rbind(data_brier_white, data_brier_black, data_brier_hispanic )
data_brier_race$index = "Integrated Brier Score"

# Load AUC data by race
data_auc_white <- read.csv("6_evaluation/minus_backimpd/all/mean_auc_race_white.csv") %>% 
  rename(model = X, value = Mean.AUC)
data_auc_white$race <- "NH Whites"

data_auc_black <- read.csv("6_evaluation/minus_backimpd/all/mean_auc_race_black.csv") %>% 
  rename(model = X, value = Mean.AUC)
data_auc_black$race <- "NH Blacks"

data_auc_hispanic <- read.csv("6_evaluation/minus_backimpd/all/mean_auc_race_hispanic.csv") %>% 
  rename(model = X, value = Mean.AUC)
data_auc_hispanic$race <- "Hispanics"

data_auc_race <- rbind(data_auc_white, data_auc_black, data_auc_hispanic )
data_auc_race$index = "Mean AUC"

data_race <- rbind(data_brier_race, data_auc_race)

data_race$model_cat <- case_when(data_race$model == "KM" |
                                 data_race$model == "CoxReduced" ~ "1",
                                   data_race$model == "Cox" |
                                     data_race$model == "CoxNet" |
                                     data_race$model == "Cox-TV" |
                                     data_race$model == "Gompertz" ~ "2",
                                   data_race$model == "DeepHit" |
                                     data_race$model == "DeepPCH" |
                                     data_race$model == "DeepSurv" |
                                     data_race$model == "GradBoost" |
                                     data_race$model == "RSF" |
                                     data_race$model == "RRF-TV" ~ "3")

data_race[data_race$model=="CoxReduced", "model"] <- "Cox Reduced"
data_race[data_race$model=="Cox", "model"] <- "Cox Full"
data_race[data_race$model=="DeepPCH", "model"] <- "DeepPCH"
data_race[data_race$model=="RRF.TV", "model"] <- "RRF-TV"

b1 <- data_race %>% 
  filter(index == "Integrated Brier Score", model != "CIF-TV") %>% 
  mutate(race = case_when(race == "NH Whites" ~ "Whites", 
                          race == "NH Blacks" ~ "Blacks", 
                          race == "Hispanics" ~ "Hisp.")) %>% 
  ggplot() + 
  geom_point(aes(x=factor(model, level = level_order), y=value, color = fct_relevel(race, "Hisp.", "Blacks", "Whites"), shape = model_cat), alpha=0.6, size = 2) +
  geom_hline(yintercept=0, linetype="dashed", color = "black", alpha = 0.8) +
  scale_y_continuous(minor_breaks = seq(0, 0.3, 0.05), breaks = seq(0, 0.3, 0.05), limits = c(0, 0.25)) +
  theme_bw() + 
  coord_flip() +
  labs(title = "Int. Brier Score", x = "", y = "") +
  scale_color_manual(values = c("navy", "red", "#666666")) +
  scale_shape_manual(values = c(15, 19, 17), guide = "none") +
  theme(plot.title = element_text(size=11, hjust = 0.5),
        axis.text.x = element_text(size = 10.5),
        axis.title.x = element_text(size = 11.5),
        legend.text=element_text(size=10),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.title = element_blank(),
        legend.position=c('bottom'),
        legend.background = element_blank(),
        legend.box.margin=margin(-20,0,0.5,0),
        plot.margin=unit(c(0.2,0.2,0.2,0.2), "cm")) +
  guides(color=guide_legend(override.aes = list(shape = 15, size =4)))


b2 <- data_race %>% 
  filter(index == "Mean AUC", model != "CIF-TV", model != "KM") %>% 
  mutate(race = case_when(race == "NH Whites" ~ "Whites", 
                           race == "NH Blacks" ~ "Blacks", 
                           race == "Hispanics" ~ "Hisp.")) %>% 
  ggplot() + 
  geom_point(aes(x=factor(model, level = level_order), y=value, color = fct_relevel(race, "Hisp.", "Blacks", "Whites"), shape = model_cat), alpha=0.6, size = 2) +
  geom_hline(yintercept=1, linetype="dashed", color = "black", alpha = 0.8) +
  scale_y_continuous(minor_breaks = seq(0.5, 1, 0.05), breaks = seq(0.5, 1, 0.05), limits = c(0.75, 1)) +
  theme_bw() + 
  coord_flip() +
  labs(title = "Mean AUC", x = "", y = "") +
  scale_color_manual(values = c( "navy", "red", "#666666")) +
  scale_shape_manual(values = c(15, 19, 17), guide = "none") +
  theme(plot.title = element_text(size=11, hjust = 0.5),
        axis.text.x = element_text(size = 10.5),
        axis.title.x = element_text(size = 11.5),
        legend.text=element_text(size=10),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.title = element_blank(),
        legend.position=c('bottom'),
        legend.background = element_blank(),
        legend.box.margin=margin(-20,0,0.5,0),
        plot.margin=unit(c(-0.3,0.2,0.2,0.2), "cm")) +
  guides(color=guide_legend(override.aes = list(shape = 15, size =4)))



plot_race <- ggarrange(b1, b2, 
                         ncol=1,
                         common.legend = TRUE,
                         legend = "bottom")

plot_race <- annotate_figure(plot_race, top = text_grob("b. Race and ethnicity", face = "bold", size = 11))

#plot_race 


# Education

# Load brier score data by education
data_brier_high <- read.csv("6_evaluation/minus_backimpd/all/int_brier_educl_high.csv") %>% 
  rename(model = X, value = IBS)
data_brier_high$education <- "High"

data_brier_middle <- read.csv("6_evaluation/minus_backimpd/all/int_brier_educl_medium.csv") %>% 
  rename(model = X, value = IBS)
data_brier_middle$education <- "Middle"

data_brier_low <- read.csv("6_evaluation/minus_backimpd/all/int_brier_educl_low.csv") %>% 
  rename(model = X, value = IBS)
data_brier_low$education <- "Low"

data_brier_education <- rbind(data_brier_high, data_brier_middle, data_brier_low)
data_brier_education$index = "Integrated Brier Score"

# Load mean AUC score data by education
data_auc_high <- read.csv("6_evaluation/minus_backimpd/all/mean_auc_educl_high.csv") %>% 
  rename(model = X, value = Mean.AUC)
data_auc_high$education <- "High"

data_auc_middle <- read.csv("6_evaluation/minus_backimpd/all/mean_auc_educl_medium.csv") %>% 
  rename(model = X, value = Mean.AUC)
data_auc_middle$education <- "Middle"

data_auc_low <- read.csv("6_evaluation/minus_backimpd/all/mean_auc_educl_low.csv") %>% 
  rename(model = X, value = Mean.AUC)
data_auc_low$education <- "Low"

data_auc_education <- rbind(data_auc_high, data_auc_middle, data_auc_low)
data_auc_education$index = "Mean AUC"


data_education <- rbind(data_brier_education, data_auc_education)

data_education$model_cat <- case_when(data_education$model == "KM" |
                                      data_education$model == "CoxReduced" ~ "1",
                                   data_education$model == "Cox" |
                                     data_education$model == "CoxNet" |
                                     data_education$model == "Cox-TV" |
                                     data_education$model == "Gompertz" ~ "2",
                                   data_education$model == "DeepHit" |
                                     data_education$model == "DeepPCH" |
                                     data_education$model == "DeepSurv" |
                                     data_education$model == "GradBoost" |
                                     data_education$model == "RSF" |
                                     data_education$model == "RRF-TV" ~ "3")

data_education[data_education$model=="CoxReduced", "model"] <- "Cox Reduced"
data_education[data_education$model=="Cox", "model"] <- "Cox Full"
data_education[data_education$model=="DeepPCH", "model"] <- "DeepPCH"
data_education[data_education$model=="RRF.TV", "model"] <- "RRF-TV"

level_order <- c('RRF-TV', 'RSF','GradBoost', 'DeepSurv', 'DeepPCH' , 'DeepHit',
                 'Gompertz', 'Cox-TV', 'CoxNet', 'Cox Full', 
                 'KM','Cox Reduced')

data_education$education <- factor(data_education$education, level = c("Low", "Middle", "High"))

c1 <- data_education %>% 
  filter(index == "Integrated Brier Score", model != "CIF-TV") %>% 
  ggplot() + 
  geom_point(aes(x=factor(model, level = level_order), y=value, color = education, shape = model_cat), alpha=.6, size = 2) +
  geom_hline(yintercept=0, linetype="dashed", color = "black", alpha = 0.8) +
  scale_y_continuous(minor_breaks = seq(0, 0.3, 0.05), breaks = seq(0, 0.3, 0.05), limits = c(0, 0.25)) +
  theme_bw() + 
  coord_flip() +
  labs(title = "Int. Brier Score", x = "", y = "") +
  scale_color_manual(values = c("navy", "red", "#666666")) +
  scale_shape_manual(values = c(15, 19, 17), guide = "none") +
  theme(plot.title = element_text(size=11, hjust = 0.5),
        axis.text.x = element_text(size = 10.5),
        axis.title.x = element_text(size = 11.5),
        legend.text=element_text(size=10),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.title = element_blank(),
        legend.position=c('bottom'),
        legend.background = element_blank(),
        legend.box.margin=margin(-20,0,0.5,0),
        plot.margin=unit(c(0.2,0.2,0.2,0.2), "cm")) +
  guides(color=guide_legend(override.aes = list(shape = 15, size =4)))


c2 <- data_education %>% 
  filter(index == "Mean AUC", model != "CIF-TV", model != "KM") %>% 
  ggplot() + 
  geom_point(aes(x=factor(model, level = level_order), y=value, color = education, shape = model_cat), alpha=.6, size = 2) +
  geom_hline(yintercept=1, linetype="dashed", color = "black", alpha = 0.8) +
  scale_y_continuous(minor_breaks = seq(0.5, 1, 0.05), breaks = seq(0.5, 1, 0.05), limits = c(0.75, 1)) +
  theme_bw() + 
  coord_flip() +
  labs(title = "Mean AUC", x = "", y = "") +
  scale_color_manual(values = c("navy", "red", "#666666")) +
  scale_shape_manual(values = c(15, 19, 17), guide = "none") +
  theme(plot.title = element_text(size=11, hjust = 0.5),
        axis.text.x = element_text(size = 10.5),
        axis.title.x = element_text(size = 11.5),
        legend.text=element_text(size=10),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.title = element_blank(),
        legend.position=c('bottom'),
        legend.background = element_blank(),
        legend.box.margin=margin(-20,0,0.5,0),
        plot.margin=unit(c(-0.3,0.2,0.2,0.2), "cm")) +
  guides(color=guide_legend(override.aes = list(shape = 15, size =4)))

plot_education <- ggarrange(c1, c2, 
                       ncol=1,
                       common.legend = TRUE,
                       legend = "bottom")

plot_education <- annotate_figure(plot_education, top = text_grob("c. Education", face = "bold", size = 11))

#plot_education


ggarrange(plot_gender, plot_race, plot_education, 
          ncol=3,
          widths = c(4.5, 3, 3))


ggsave("8_outputs/figures/plot_evaluation_groups.pdf", width = 8.3, height = 7)


#Horizontal plot for presentation


# gender

a1 <- data_gender %>% 
  filter(index == "Integrated Brier Score", model != "CIF-TV") %>% 
  ggplot() + 
  geom_point(aes(x=factor(model, level = level_order), y=value, color = gender, shape = model_cat), alpha=.6, size = 2) +
  geom_hline(yintercept=0, linetype="dashed", color = "black", alpha = 0.8) +
  scale_y_continuous(minor_breaks = seq(0, 0.3, 0.05), breaks = seq(0, 0.3, 0.05), limits = c(0, 0.25)) +
  theme_bw() + 
  coord_flip() +
  labs(title = "Int. Brier Score", x = "", y = "", color="                    ") +
  scale_color_manual(values = c("navy", "red")) +
  scale_shape_manual(values = c(15, 19, 17), guide = "none") +
  theme(plot.title = element_text(size=11, hjust = 0.5),
        axis.text.x = element_text(size = 8.5),
        axis.text.y = element_text(size = 8.5),
        legend.title = element_text(),
        legend.position=c('bottom'),
        legend.background = element_blank(),
        legend.box.margin=margin(-20,0,0.5,0),
        plot.margin=unit(c(0.2,0.1,0.2,0.2), "cm")) +
  guides(color=guide_legend(override.aes = list(shape = 15, size =4)))


a2 <-data_gender %>% 
  filter(index == "Mean AUC", model != "CIF-TV") %>% 
  ggplot() + 
  geom_point(aes(x=factor(model, level = level_order), y=value, color = gender, shape = model_cat), alpha=.6, size = 2) +
  geom_hline(yintercept=1, linetype="dashed", color = "black", alpha = 0.8) +
  scale_y_continuous(minor_breaks = seq(0.5, 1, 0.05), breaks = seq(0.5, 1, 0.05), limits = c(0.75, 1)) +
  theme_bw() + 
  coord_flip() +
  labs(title = "Mean AUC", x = "", y = "", color="                    ") +
  scale_color_manual(values = c("navy", "red")) +
  scale_shape_manual(values = c(15, 19, 17), guide = "none") +
  theme(plot.title = element_text(size=11, hjust = 0.5),
        axis.text.x = element_text(size = 8.5),
        legend.title = element_blank(),
        legend.spacing.y = unit(0, "mm"), 
        legend.position=c('bottom'),
        legend.background = element_blank(), 
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.box.margin=margin(-20,0,0.5,0),
        plot.margin=unit(c(0.2,0.2,0.2,0.1), "cm")) +
  guides(color=guide_legend(override.aes = list(shape = 15, size =4)))


plot_gender <- ggarrange(a1, a2, 
                         ncol=2,
                         common.legend = TRUE,
                         legend = "bottom",
                         widths = c(4.5, 3))

plot_gender <- annotate_figure(plot_gender, top = text_grob("                            a. Gender", face = "bold", size = 11))


# race and ethnicity
b1 <- data_race %>% 
  filter(index == "Integrated Brier Score", model != "CIF-TV") %>% 
  ggplot() + 
  geom_point(aes(x=factor(model, level = level_order), y=value, color = race, shape = model_cat), alpha=0.6, size = 2) +
  geom_hline(yintercept=0, linetype="dashed", color = "black", alpha = 0.8) +
  scale_y_continuous(minor_breaks = seq(0, 0.3, 0.05), breaks = seq(0, 0.3, 0.05), limits = c(0, 0.25)) +
  theme_bw() + 
  coord_flip() +
  labs(title = "Int. Brier Score", x = "", y = "") +
  scale_color_manual(values = c("navy", "red", "#666666")) +
  scale_shape_manual(values = c(15, 19, 17), guide = "none") +
  theme(plot.title = element_text(size=11, hjust = 0.5),
        axis.text.x = element_text(size = 8.5),
        legend.title = element_blank(),
        legend.position=c('bottom'),
        legend.background = element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.box.margin=margin(-20,0,0.5,0),
        plot.margin=unit(c(0.2,0.1,0.2,0.2), "cm")) +
  guides(color=guide_legend(override.aes = list(shape = 15, size =4)))


b2 <- data_race %>% 
  filter(index == "Mean AUC", model != "CIF-TV") %>% 
  ggplot() + 
  geom_point(aes(x=factor(model, level = level_order), y=value, color = race, shape = model_cat), alpha=0.6, size = 2) +
  geom_hline(yintercept=1, linetype="dashed", color = "black", alpha = 0.8) +
  scale_y_continuous(minor_breaks = seq(0.5, 1, 0.05), breaks = seq(0.5, 1, 0.05), limits = c(0.75, 1)) +
  theme_bw() + 
  coord_flip() +
  labs(title = "Mean AUC", x = "", y = "") +
  scale_color_manual(values = c( "navy", "red", "#666666")) +
  scale_shape_manual(values = c(15, 19, 17), guide = "none") +
  theme(plot.title = element_text(size=11, hjust = 0.5),
        axis.text.x = element_text(size = 8.5),
        legend.title = element_blank(),
        legend.spacing.y = unit(0, "mm"), 
        legend.position=c('bottom'),
        legend.background = element_blank(), 
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.box.margin=margin(-20,0,0.5,0),
        plot.margin=unit(c(0.2,0.2,0.2,0.1), "cm")) +
  guides(color=guide_legend(override.aes = list(shape = 15, size =4)))


plot_race <- ggarrange(b1, b2, 
                       ncol=2,
                       common.legend = TRUE,
                       legend = "bottom",
                       widths = c(3, 3))

plot_race <- annotate_figure(plot_race, top = text_grob("b. Race and ethnicity", face = "bold", size = 11))

# education

c1 <- data_education %>% 
  filter(index == "Integrated Brier Score", model != "CIF-TV") %>% 
  ggplot() + 
  geom_point(aes(x=factor(model, level = level_order), y=value, color = education, shape = model_cat), alpha=.6, size = 2) +
  geom_hline(yintercept=0, linetype="dashed", color = "black", alpha = 0.8) +
  scale_y_continuous(minor_breaks = seq(0, 0.3, 0.05), breaks = seq(0, 0.3, 0.05), limits = c(0, 0.25)) +
  theme_bw() + 
  coord_flip() +
  labs(title = "Int. Brier Score", x = "", y = "") +
  scale_color_manual(values = c("navy", "red", "#666666")) +
  scale_shape_manual(values = c(15, 19, 17), guide = "none") +
  theme(plot.title = element_text(size=11, hjust = 0.5),
        axis.text.x = element_text(size = 8.5),
        legend.title = element_blank(),
        legend.position=c('bottom'),
        legend.background = element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.box.margin=margin(-20,0,0.5,0),
        plot.margin=unit(c(0.2,0.1,0.2,0.2), "cm")) +
  guides(color=guide_legend(override.aes = list(shape = 15, size =4)))


c2 <- data_education %>% 
  filter(index == "Mean AUC", model != "CIF-TV") %>% 
  ggplot() + 
  geom_point(aes(x=factor(model, level = level_order), y=value, color = education, shape = model_cat), alpha=.6, size = 2) +
  geom_hline(yintercept=1, linetype="dashed", color = "black", alpha = 0.8) +
  scale_y_continuous(minor_breaks = seq(0.5, 1, 0.05), breaks = seq(0.5, 1, 0.05), limits = c(0.75, 1)) +
  theme_bw() + 
  coord_flip() +
  labs(title = "Mean AUC", x = "", y = "") +
  scale_color_manual(values = c("navy", "red", "#666666")) +
  scale_shape_manual(values = c(15, 19, 17), guide = "none") +
  theme(plot.title = element_text(size=11, hjust = 0.5),
        axis.text.x = element_text(size = 8.5),
        legend.title = element_blank(),
        legend.spacing.y = unit(0, "mm"), 
        legend.position=c('bottom'),
        legend.background = element_blank(), 
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.box.margin=margin(-20,0,0.5,0),
        plot.margin=unit(c(0.2,0.2,0.2,0.1), "cm")) +
  guides(color=guide_legend(override.aes = list(shape = 15, size =4)))


plot_education <- ggarrange(c1, c2, 
                            ncol=2,
                            common.legend = TRUE,
                            legend = "bottom",
                            widths = c(3, 3))

plot_education <- annotate_figure(plot_education, top = text_grob("c. Education", face = "bold", size = 11))

#plot_education


ggarrange(plot_gender, plot_race, plot_education, 
          ncol=3,
          widths = c(8, 6, 6))


ggsave("8_outputs/figures/plot_evaluation_groups_horizontal.pdf", width = 12, height = 4)

# Plot for oversampling, appendix 

# Clear the environment 
rm(list = ls())

level_order_model <- c("Cox Reduced", 'KM', 
                       'Cox Full', 'CoxNet', 'Cox-TV', 'Gompertz',
                       'DeepHit','DeepPCH' , 'DeepSurv', 'GradBoost' , 'RSF', 'RRF-TV')

col_pal = c("#4E79A7", "#666666", "#F28E2B", "#E15759", "#76B7B2", "#59A14F", "#EDC948", 
            "#B07AA1", "#FF9DA7", "#9C755F", "#BAB0AC", "hotpink")


# Load brier score data by gender 
data_brier_men <- read.csv("6_evaluation/minus_backimpd/gender/int_brier_gender_men.csv") %>% 
  rename(model = X, value = IBS)

data_brier_men$gender <- "Men"

data_brier_women <- read.csv("6_evaluation/minus_backimpd/gender/int_brier_gender_women.csv") %>% 
  rename(model = X, value = IBS)

data_brier_women$gender <- "Women"

data_brier_gender <- rbind(data_brier_men, data_brier_women)
data_brier_gender$index = "Integrated Brier Score"

# Load mean AUC data by gender 
data_auc_men <- read.csv("6_evaluation/minus_backimpd/gender/mean_auc_gender_men.csv") %>% 
  rename(model = X, value = Mean.AUC)

data_auc_men$gender <- "Men"

data_auc_women <- read.csv("6_evaluation/minus_backimpd/gender/mean_auc_gender_women.csv") %>% 
  rename(model = X, value = Mean.AUC)

data_auc_women$gender <- "Women"

data_auc_gender <- rbind(data_auc_men, data_auc_women)
data_auc_gender$index = "Mean AUC"

data_gender <- rbind(data_brier_gender, data_auc_gender)

data_gender$model_cat <- case_when(data_gender$model == "KM" |
                                   data_gender$model == "CoxReduced" ~ "1",
                                   data_gender$model == "Cox" |
                                     data_gender$model == "CoxNet" |
                                     data_gender$model == "Cox.TV" |
                                     data_gender$model == "Gompertz" ~ "2",
                                   data_gender$model == "DeepHit" |
                                     data_gender$model == "DeepPCH" |
                                     data_gender$model == "DeepSurv" |
                                     data_gender$model == "GradBoost" |
                                     data_gender$model == "RSF" |
                                     data_gender$model == "RRF-TV" ~ "3")

data_gender[data_gender$model=="CoxReduced", "model"] <- "Cox Reduced"
data_gender[data_gender$model=="Cox", "model"] <- "Cox Full"
data_gender[data_gender$model=="DeepPCH", "model"] <- "DeepPCH"
data_gender[data_gender$model=="RRF.TV", "model"] <- "RRF-TV"

level_order <- c('RRF-TV', 'RSF','GradBoost', 'DeepSurv', 'DeepPCH' , 'DeepHit',
                 'Gompertz', 'Cox-TV', 'CoxNet', 'Cox Full', 
                 'KM', 'Cox Reduced')


a1 <- data_gender %>% 
  filter(index == "Integrated Brier Score", model != "CIF-TV", model != "CoxLasso-TV",
         model != "DeepSurv") %>% 
  ggplot() + 
  geom_point(aes(x=factor(model, level = level_order), y=value, color = gender, shape = model_cat), alpha=.6, size = 2) +
  geom_hline(yintercept=0, linetype="dashed", color = "black", alpha = 0.8) +
  scale_y_continuous(minor_breaks = seq(0, 0.3, 0.05), breaks = seq(0, 0.3, 0.05), limits = c(0, 0.25)) +
  theme_bw() + 
  coord_flip() +
  labs(title = "Int. Brier Score", x = "", y = "", color="                    ") +
  scale_color_manual(values = c("navy", "red")) +
  scale_shape_manual(values = c(15, 19, 17), guide = "none") +
  theme(plot.title = element_text(size=11, hjust = 0.5),
        axis.text.x = element_text(size = 10.5),
        axis.title.x = element_text(size = 11.5),
        axis.text.y = element_text(size = 10.5),
        axis.title.y = element_text(size = 11.5),
        legend.text=element_text(size=10),
        legend.title = element_text(),
        legend.position=c('bottom'),
        legend.background = element_blank(),
        legend.box.margin=margin(-20,0,0.5,0),
        plot.margin=unit(c(0.2,0.2,0.2,0.2), "cm")) +
  guides(color=guide_legend(override.aes = list(shape = 15, size =4)))


a2 <-data_gender %>% 
  filter(index == "Mean AUC", model != "CIF-TV", model != "KM", model != "CoxLasso-TV",
         model != "DeepSurv") %>% 
  ggplot() + 
  geom_point(aes(x=factor(model, level = level_order), y=value, color = gender, shape = model_cat), alpha=.6, size = 2) +
  geom_hline(yintercept=1, linetype="dashed", color = "black", alpha = 0.8) +
  scale_y_continuous(minor_breaks = seq(0.5, 1, 0.05), breaks = seq(0.5, 1, 0.05), limits = c(0.75, 1)) +
  theme_bw() + 
  coord_flip() +
  labs(title = "Mean AUC", x = "", y = "", color="                    ") +
  scale_color_manual(values = c("navy", "red")) +
  scale_shape_manual(values = c(15, 19, 17), guide = "none") +
  theme(plot.title = element_text(size=11, hjust = 0.5),
        axis.text.x = element_text(size = 10.5),
        axis.title.x = element_text(size = 11.5),
        axis.text.y = element_text(size = 10.5),
        axis.title.y = element_text(size = 11.5),
        legend.text=element_text(size=10),
        legend.title = element_text(),
        legend.position=c('bottom'),
        legend.background = element_blank(),
        legend.box.margin=margin(-20,0,0.5,0),
        plot.margin=unit(c(-0.3,0.2,0.2,0.2), "cm")) +
  guides(color=guide_legend(override.aes = list(shape = 15, size =4)))

plot_gender <- ggarrange(a1, a2, 
                         ncol=1,
                         common.legend = TRUE,
                         legend = "bottom")

plot_gender <- annotate_figure(plot_gender, top = text_grob("                         a. Gender (overs.)", face = "bold", size = 11))

plot_gender


# Load brier score data by race
data_brier_white <- read.csv("6_evaluation/minus_backimpd/race/int_brier_race_white.csv") %>% 
  rename(model = X, value = IBS)
data_brier_white$race <- "NH Whites"

data_brier_black <- read.csv("6_evaluation/minus_backimpd/race/int_brier_race_black.csv") %>% 
  rename(model = X, value = IBS)
data_brier_black$race <- "NH Blacks"

data_brier_hispanic <- read.csv("6_evaluation/minus_backimpd/race/int_brier_race_hispanic.csv") %>% 
  rename(model = X, value = IBS)
data_brier_hispanic$race <- "Hispanics"

data_brier_race <- rbind(data_brier_white, data_brier_black, data_brier_hispanic )
data_brier_race$index = "Integrated Brier Score"

# Load AUC data by race
data_auc_white <- read.csv("6_evaluation/minus_backimpd/race/mean_auc_race_white.csv") %>% 
  rename(model = X, value = Mean.AUC)
data_auc_white$race <- "NH Whites"

data_auc_black <- read.csv("6_evaluation/minus_backimpd/race/mean_auc_race_black.csv") %>% 
  rename(model = X, value = Mean.AUC)
data_auc_black$race <- "NH Blacks"

data_auc_hispanic <- read.csv("6_evaluation/minus_backimpd/race/mean_auc_race_hispanic.csv") %>% 
  rename(model = X, value = Mean.AUC)
data_auc_hispanic$race <- "Hispanics"

data_auc_race <- rbind(data_auc_white, data_auc_black, data_auc_hispanic )
data_auc_race$index = "Mean AUC"

data_race <- rbind(data_brier_race, data_auc_race)

data_race$model_cat <- case_when(data_race$model == "KM" |
                                 data_race$model == "CoxReduced" ~ "1",
                                   data_race$model == "Cox" |
                                     data_race$model == "CoxNet" |
                                     data_race$model == "Cox.TV" |
                                     data_race$model == "Gompertz" ~ "2",
                                   data_race$model == "DeepHit" |
                                     data_race$model == "DeepPCH" |
                                     data_race$model == "DeepSurv" |
                                     data_race$model == "GradBoost" |
                                     data_race$model == "RSF" |
                                     data_race$model == "RRF-TV" ~ "3")

data_race[data_race$model=="CoxReduced", "model"] <- "Cox Reduced"
data_race[data_race$model=="Cox", "model"] <- "Cox Full"
data_race[data_race$model=="DeepPCH", "model"] <- "DeepPCH"
data_race[data_race$model=="RRF.TV", "model"] <- "RRF-TV"

b1 <- data_race %>% 
  filter(index == "Integrated Brier Score", model != "CIF-TV", model != "CoxLasso-TV",
         model != "DeepSurv") %>% 
  mutate(race = case_when(race == "NH Whites" ~ "Whites", 
                          race == "NH Blacks" ~ "Blacks", 
                          race == "Hispanics" ~ "Hisp.")) %>% 
  ggplot() + 
  geom_point(aes(x=factor(model, level = level_order), y=value, color = fct_relevel(race, "Hisp.", "Blacks", "Whites"), shape = model_cat), alpha=0.6, size = 2) +
  geom_hline(yintercept=0, linetype="dashed", color = "black", alpha = 0.8) +
  scale_y_continuous(minor_breaks = seq(0, 0.3, 0.05), breaks = seq(0, 0.3, 0.05), limits = c(0, 0.25)) +
  theme_bw() + 
  coord_flip() +
  labs(title = "Int. Brier Score", x = "", y = "") +
  scale_color_manual(values = c("navy", "red", "#666666")) +
  scale_shape_manual(values = c(15, 19, 17), guide = "none") +
  theme(plot.title = element_text(size=11, hjust = 0.5),
        axis.text.x = element_text(size = 10.5),
        axis.title.x = element_text(size = 11.5),
        legend.text=element_text(size=10),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.title = element_blank(),
        legend.position=c('bottom'),
        legend.background = element_blank(),
        legend.box.margin=margin(-20,0,0.5,0),
        plot.margin=unit(c(0.2,0.2,0.2,0.2), "cm")) +
  guides(color=guide_legend(override.aes = list(shape = 15, size =4)))


b2 <- data_race %>% 
  filter(index == "Mean AUC", model != "CIF-TV", model != "KM", model != "CoxLasso-TV",
         model != "DeepSurv") %>% 
  mutate(race = case_when(race == "NH Whites" ~ "Whites", 
                          race == "NH Blacks" ~ "Blacks", 
                          race == "Hispanics" ~ "Hisp.")) %>% 
  ggplot() + 
  geom_point(aes(x=factor(model, level = level_order), y=value, color = fct_relevel(race, "Hisp.", "Blacks", "Whites"), shape = model_cat), alpha=0.6, size = 2) +
  geom_hline(yintercept=1, linetype="dashed", color = "black", alpha = 0.8) +
  scale_y_continuous(minor_breaks = seq(0.5, 1, 0.05), breaks = seq(0.5, 1, 0.05), limits = c(0.75, 1)) +
  theme_bw() + 
  coord_flip() +
  labs(title = "Mean AUC", x = "", y = "") +
  scale_color_manual(values = c( "navy", "red", "#666666")) +
  scale_shape_manual(values = c(15, 19, 17), guide = "none") +
  theme(plot.title = element_text(size=11, hjust = 0.5),
        axis.text.x = element_text(size = 10.5),
        axis.title.x = element_text(size = 11.5),
        legend.text=element_text(size=10),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.title = element_blank(),
        legend.position=c('bottom'),
        legend.background = element_blank(),
        legend.box.margin=margin(-20,0,0.5,0),
        plot.margin=unit(c(-0.3,0.2,0.2,0.2), "cm")) +
  guides(color=guide_legend(override.aes = list(shape = 15, size =4)))



plot_race <- ggarrange(b1, b2, 
                       ncol=1,
                       common.legend = TRUE,
                       legend = "bottom")

plot_race <- annotate_figure(plot_race, top = text_grob("b. Race and ethnicity (overs.)", face = "bold", size = 11))


# Education

# Load brier score data by education
data_brier_high <- read.csv("6_evaluation/minus_backimpd/educl/int_brier_educl_high.csv") %>% 
  rename(model = X, value = IBS)
data_brier_high$education <- "High"

data_brier_middle <- read.csv("6_evaluation/minus_backimpd/educl/int_brier_educl_medium.csv") %>% 
  rename(model = X, value = IBS)
data_brier_middle$education <- "Middle"

data_brier_low <- read.csv("6_evaluation/minus_backimpd/educl/int_brier_educl_low.csv") %>% 
  rename(model = X, value = IBS)
data_brier_low$education <- "Low"

data_brier_education <- rbind(data_brier_high, data_brier_middle, data_brier_low)
data_brier_education$index = "Integrated Brier Score"

# Load mean AUC score data by education
data_auc_high <- read.csv("6_evaluation/minus_backimpd/educl/mean_auc_educl_high.csv") %>% 
  rename(model = X, value = Mean.AUC)
data_auc_high$education <- "High"

data_auc_middle <- read.csv("6_evaluation/minus_backimpd/educl/mean_auc_educl_medium.csv") %>% 
  rename(model = X, value = Mean.AUC)
data_auc_middle$education <- "Middle"

data_auc_low <- read.csv("6_evaluation/minus_backimpd/educl/mean_auc_educl_low.csv") %>% 
  rename(model = X, value = Mean.AUC)
data_auc_low$education <- "Low"

data_auc_education <- rbind(data_auc_high, data_auc_middle, data_auc_low)
data_auc_education$index = "Mean AUC"


data_education <- rbind(data_brier_education, data_auc_education)

data_education$model_cat <- case_when(data_education$model == "KM" |
                                      data_education$model == "CoxReduced" ~ "1",
                                   data_education$model == "Cox" |
                                     data_education$model == "CoxNet" |
                                     data_education$model == "Cox.TV" |
                                     data_education$model == "Gompertz" ~ "2",
                                   data_education$model == "DeepHit" |
                                     data_education$model == "DeepPCH" |
                                     data_education$model == "DeepSurv" |
                                     data_education$model == "GradBoost" |
                                     data_education$model == "RSF" |
                                     data_education$model == "RRF-TV" ~ "3")

data_education[data_education$model=="CoxReduced", "model"] <- "Cox Reduced"
data_education[data_education$model=="Cox", "model"] <- "Cox Full"
data_education[data_education$model=="DeepPCH", "model"] <- "DeepPCH"
data_education[data_education$model=="RRF.TV", "model"] <- "RRF-TV"

level_order <- c('RRF-TV', 'RSF','GradBoost', 'DeepSurv', 'DeepPCH' , 'DeepHit',
                 'Gompertz', 'Cox-TV', 'CoxNet', 'Cox Full', 
                 'KM','Cox Reduced')

data_education$education <- factor(data_education$education, level = c("Low", "Middle", "High"))

c1 <- data_education %>% 
  filter(index == "Integrated Brier Score", model != "CIF-TV", model != "CoxLasso-TV",
         model != "DeepSurv") %>% 
  ggplot() + 
  geom_point(aes(x=factor(model, level = level_order), y=value, color = education, shape = model_cat), alpha=.6, size = 2) +
  geom_hline(yintercept=0, linetype="dashed", color = "black", alpha = 0.8) +
  scale_y_continuous(minor_breaks = seq(0, 0.3, 0.05), breaks = seq(0, 0.3, 0.05), limits = c(0, 0.25)) +
  theme_bw() + 
  coord_flip() +
  labs(title = "Int. Brier Score", x = "", y = "") +
  scale_color_manual(values = c("navy", "red", "#666666")) +
  scale_shape_manual(values = c(15, 19, 17), guide = "none") +
  theme(plot.title = element_text(size=11, hjust = 0.5),
        axis.text.x = element_text(size = 10.5),
        axis.title.x = element_text(size = 11.5),
        legend.text=element_text(size=10),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.title = element_blank(),
        legend.position=c('bottom'),
        legend.background = element_blank(),
        legend.box.margin=margin(-20,0,0.5,0),
        plot.margin=unit(c(0.2,0.2,0.2,0.2), "cm")) +
  guides(color=guide_legend(override.aes = list(shape = 15, size =4)))


c2 <- data_education %>% 
  filter(index == "Mean AUC", model != "CIF-TV", model != "KM", model != "CoxLasso-TV",
         model != "DeepSurv") %>% 
  ggplot() + 
  geom_point(aes(x=factor(model, level = level_order), y=value, color = education, shape = model_cat), alpha=.6, size = 2) +
  geom_hline(yintercept=1, linetype="dashed", color = "black", alpha = 0.8) +
  scale_y_continuous(minor_breaks = seq(0.5, 1, 0.05), breaks = seq(0.5, 1, 0.05), limits = c(0.75, 1)) +
  theme_bw() + 
  coord_flip() +
  labs(title = "Mean AUC", x = "", y = "") +
  scale_color_manual(values = c("navy", "red", "#666666")) +
  scale_shape_manual(values = c(15, 19, 17), guide = "none") +
  theme(plot.title = element_text(size=11, hjust = 0.5),
        axis.text.x = element_text(size = 10.5),
        axis.title.x = element_text(size = 11.5),
        legend.text=element_text(size=10),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.title = element_blank(),
        legend.position=c('bottom'),
        legend.background = element_blank(),
        legend.box.margin=margin(-20,0,0.5,0),
        plot.margin=unit(c(-0.3,0.2,0.2,0.2), "cm")) +
  guides(color=guide_legend(override.aes = list(shape = 15, size =4)))

plot_education <- ggarrange(c1, c2, 
                            ncol=1,
                            common.legend = TRUE,
                            legend = "bottom")

plot_education <- annotate_figure(plot_education, top = text_grob("c. Education (overs.)", face = "bold", size = 11))


ggarrange(plot_gender, plot_race, plot_education, 
          ncol=3,
          widths = c(4.5, 3, 3))

ggsave("8_outputs/figures/plot_evaluation_groups_oversampling.pdf", width = 8.3, height = 6.5)


# end
