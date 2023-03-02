
# clear the environment and set the working directory
rm(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# install and load the required packages
listofpackages = c("dplyr", "tidyverse", "haven", "purrr")
for (j in listofpackages){
  if(sum(installed.packages()[, 1] == 0)) {
    install.packages(j)
  }
  library(j, character.only = T)
}

# load data: harmonized hrs in the long format

data <- readRDS("hrs_long.rds")
colnames(data) = sub('^ra', 'r_', colnames(data))


# DATA CLEANING

# GROUP: CHILDHOOD

# pwarm	r parental warmth summary mean score - first reported (includes MOMATT, 
# MOMEFT, MOMGRELA) (continuous) r
data %>% select(ends_with('pwarm')) %>% map(table, exclude = NULL )

# lhchild	r summary count for life history childhood stress items: divorced before 
# 16, difficult living arrangement, death of a parent before 16 (0-3) r, s
data %>% select(ends_with('lhchild')) %>% map(table, exclude = NULL )

# dadgrela:	before age 18 r had a good relationship with father (0-5) r,s
data %>% select(contains('dadgrela')) %>% map(table, exclude = NULL )

data <- data %>% mutate(r_dadgrela = replace(r_dadgrela, r_dadgrela == "n", 0))
data <- data %>% mutate(s_dadgrela = replace(s_dadgrela, s_dadgrela == "n", 0))

# pabused:	before age 18 r was physically abused by parent (0,1) r,s
data %>% select(contains('pabused')) %>% map(table, exclude = NULL )

# financh	financial condition while s was growing up (1-3) r,s
data %>% select(contains('financh')) %>% map(table, exclude = NULL )

# chshlt	S childhood health status (1-5) r,s
data %>% select(contains('chshlt')) %>% map(table, exclude = NULL )

# dadoccup	father's occupation at age 16 (0-3) r,s
# .s father not around, .n father never worked
data %>% select(contains('dadoccup')) %>% map(table, exclude = NULL )

data <- data %>% mutate(r_ftrhlp = replace(r_dadoccup, r_dadoccup == "s", 0))
data <- data %>% mutate(r_ftrhlp = replace(r_dadoccup, r_dadoccup == "n", 0))

data <- data %>% mutate(s_ftrhlp = replace(s_dadoccup, s_dadoccup == "s", 0))
data <- data %>% mutate(s_ftrhlp = replace(s_dadoccup, s_dadoccup == "n", 0))


# GROUP: HABITS 

# cage	r cage summary: drinkcut + drinkcr + drinkbd + drinknr (0-4) 
data %>% select(contains('cage')) %>% map(table, exclude = NULL )

data <- data %>% mutate(r_cage = replace(r_cage, r_cage == "n", 0))
data <- data %>% mutate(s_cage = replace(s_cage, s_cage == "n", 0))

# binged	r number days binge drinks (continuous) r,s
data %>% select(contains('binged')) %>% map(table, exclude = NULL )

data <- data %>% mutate(r_binged = replace(r_binged, r_binged == "n", 0))
data <- data %>% mutate(s_binged = replace(s_binged, s_binged == "n", 0))

# interact these with whether they smoke
# smokef	r # cigarettes/day (continuous) r,s
data %>% select(contains('smokef')) %>% map(table, exclude = NULL )

data <- data %>% mutate(r_smokef = replace(r_smokef, r_smokef == "n", 0))
data <- data %>% mutate(s_smokef = replace(s_smokef, s_smokef == "n", 0))

# quitsmok	r age quit smoking (continuous) r,s
data %>% select(contains('quitsmok')) %>% map(table, exclude = NULL )

# strtsmok	r age started smoking (continuous) r,s
data %>% select(contains('strtsmok')) %>% map(table, exclude = NULL )

data <- rename(data,c('r_smokev_smokef'='r_smokef', 's_smokev_smokef'='s_smokef',
                      'r_smokev_quitsmok'='r_quitsmok', 's_smokev_quitsmok'='s_quitsmok',
                      'r_smokev_strtsmok'='r_strtsmok', 's_smokev_strtsmok'='s_strtsmok'))

# GROUP: DEMOGRAPHIC

# lvwith	living arrangement (1-5) h
data %>% select(contains('lvwith')) %>% map(table, exclude = NULL )

# hometyp	type of home r lives in (1-7) r,s
data %>% select(contains('hometyp')) %>% map(table, exclude = NULL )

# SUMMARY VARIABLE

# chdeathe	r ever experienced death of own child (0,1) r,s
data %>% select(contains('chdeathe')) %>% map(table, exclude = NULL )

data <- data %>% 
  mutate(r_chdeathe = replace(r_chdeathe, r_chdeathe == "c" |
                                r_chdeathe == "m" |
                                r_chdeathe == "n" , NA))

data <- data %>% 
  mutate(s_chdeathe = replace(s_chdeathe, s_chdeathe == "c" |
                                s_chdeathe == "m" |
                                s_chdeathe == "n" |
                                s_chdeathe == "v" , NA)) %>% 
  mutate(s_chdeathe = replace(s_chdeathe, s_chdeathe == "u", 0))

# lifethe	r ever experienced life-threatening illness or accident in life (0,1) r,s
data %>% select(contains('lifethe')) %>% map(table, exclude = NULL )

data <- data %>% 
  mutate(r_lifethe = replace(r_lifethe, r_lifethe == "c" |
                               r_lifethe == "m" |
                               r_lifethe == "n" , NA))

data <- data %>% 
  mutate(s_lifethe = replace(s_lifethe, s_lifethe == "c" |
                               s_lifethe == "m" |
                               s_lifethe == "n" |
                               s_lifethe == "v" , NA)) %>% 
  mutate(s_lifethe = replace(s_lifethe, s_lifethe == "u", 0))

# lifethcse	r ever experienced life-threatening illness or accident of spouse (0,1) r,s
data %>% select(contains('lifethcse')) %>% map(table, exclude = NULL )

data <- data %>% 
  mutate(r_lifethcse = replace(r_lifethcse, r_lifethcse == "c" |
                                 r_lifethcse == "m" |
                                 r_lifethcse == "n" , NA))

data <- data %>% 
  mutate(s_lifethcse = replace(s_lifethcse, s_lifethcse == "c" |
                                 s_lifethcse == "m" |
                                 s_lifethcse == "n" |
                                 s_lifethcse == "v" , NA)) %>% 
  mutate(s_lifethcse = replace(s_lifethcse, s_lifethcse == "u", 0))

# nadise	r ever experienced natural disaster (0,1) r,s
data %>% select(contains('nadise')) %>% map(table, exclude = NULL )

data <- data %>% 
  mutate(r_nadise = replace(r_nadise, r_nadise == "c" |
                              r_nadise == "m" |
                              r_nadise == "n" , NA))

data <- data %>% 
  mutate(s_nadise = replace(s_nadise, s_nadise == "c" |
                              s_nadise == "m" |
                              s_nadise == "n" |
                              s_nadise == "v" , NA)) %>% 
  mutate(s_nadise = replace(s_nadise, s_nadise == "u", 0))

# padrug	r ever experienced parent drunk or use drugs that it caused problems (0,1) r,s
data %>% select(contains('padrug')) %>% map(table, exclude = NULL )

data <- data %>% 
  mutate(r_padrug = replace(r_padrug, r_padrug == "c" |
                              r_padrug == "m" |
                              r_padrug == "n" , NA))

data <- data %>% 
  mutate(s_padrug = replace(s_padrug, s_padrug == "c" |
                              s_padrug == "m" |
                              s_padrug == "n" |
                              s_padrug == "v" , NA)) %>% 
  mutate(s_padrug = replace(s_padrug, s_padrug == "u", 0))

# attacke	r ever experienced physical attack or assault in life (0,1) r,s
data %>% select(contains('attacke')) %>% map(table, exclude = NULL )

data <- data %>% 
  mutate(r_attacke = replace(r_attacke, r_attacke == "c" |
                               r_attacke == "m" |
                               r_attacke == "n" , NA))

data <- data %>% 
  mutate(s_attacke = replace(s_attacke, s_attacke == "c" |
                               s_attacke == "m" |
                               s_attacke == "n" |
                               s_attacke == "v" , NA)) %>% 
  mutate(s_attacke = replace(s_attacke, s_attacke == "u", 0))

# drugcse	r ever experienced spouse, partner, or child addicted to drugs or a (0,1) r,s
data %>% select(contains('drugcse')) %>% map(table, exclude = NULL )

data <- data %>% 
  mutate(r_drugcse = replace(r_drugcse, r_drugcse == "c" |
                               r_drugcse == "m" |
                               r_drugcse == "n" , NA))

data <- data %>% 
  mutate(s_drugcse = replace(s_drugcse, s_drugcse == "c" |
                               s_drugcse == "m" |
                               s_drugcse == "n" |
                               s_drugcse == "v" , NA)) %>% 
  mutate(s_drugcse = replace(s_drugcse, s_drugcse == "u", 0))

# combate	r ever fired weapon or was fired on in combat (0,1) r,s
data %>% select(contains('combate')) %>% map(table, exclude = NULL )

data <- data %>% 
  mutate(r_combate = replace(r_combate, r_combate == "c" |
                               r_combate == "m" |
                               r_combate == "n" , NA))

data <- data %>% 
  mutate(s_combate = replace(s_combate, s_combate == "c" |
                               s_combate == "m" |
                               s_combate == "n" |
                               s_combate == "v" , NA)) %>% 
  mutate(s_combate = replace(s_combate, s_combate == "u", 0))

d <- data %>% select(c('r_chdeathe','s_chdeathe','r_lifethe','s_lifethe',
                       'r_lifethcse','s_lifethcse','r_nadise','s_nadise',
                       'r_padrug','s_padrug','r_attacke','s_attacke',
                       'r_drugcse','s_drugcse','r_combate','s_combate'))

d <- as.data.frame(sapply(d, as.numeric))

d[is.na(d)] <- 0

data$r_traumatic_events <- d$r_chdeathe + d$r_lifethe + d$r_lifethcse + d$r_nadise + 
  d$r_padrug + d$r_attacke + d$r_drugcse + d$r_combate

data$s_traumatic_events <- d$s_chdeathe + d$s_lifethe + d$s_lifethcse + d$s_nadise + 
  d$s_padrug + d$s_attacke + d$s_drugcse + d$s_combate

# rural	lives in rural or urban (0,1) h
data %>% select(contains('rural')) %>% map(table, exclude = NULL )

# dadeducl	father harmonized education level (1-3) r,s
# .s father not around / alive
data %>% select(contains('dadeducl')) %>% map(table, exclude = NULL )

data <- data %>% mutate(r_dadeducl = replace(r_dadeducl, r_dadeducl == "s", 0))
data <- data %>% mutate(s_dadeducl = replace(s_dadeducl, s_dadeducl == "s", 0))

# momeducl	mother harmonized education level (1-3) r,s
data %>% select(contains('momeducl')) %>% map(table, exclude = NULL )

data <- data %>% mutate(r_momeducl = replace(r_momeducl, r_momeducl == "s", 0))
data <- data %>% mutate(s_momeducl = replace(s_momeducl, s_momeducl == "s", 0))

# kidu14	number children/grandchildren in hh under age 14 (0-8) h
data %>% select(contains('kidu14')) %>% map(table, exclude = NULL )

# kidu6	number children/grandchildren in hh under age 6 (0-5) h
data %>% select(contains('kidu6')) %>% map(table, exclude = NULL )

# depndntn	number dependents (continuous) h
data %>% select(contains('depndntn')) %>% map(table, exclude = NULL )

# grchild	number of grandchildren (continuous) h
data %>% select(contains('_grchild')) %>% map(table, exclude = NULL )

# arriage	r age started to live in U.S. (continuous) r,s
data %>% select(contains('arriage')) %>% map(table, exclude = NULL )

# educl	r harmonized education level (1-3) r,s
data %>% select(contains('_educl')) %>% map(table, exclude = NULL )

# mheight	r height measurement in meters (continuous) r,s
data %>% select(contains('mheight')) %>% map(table, exclude = NULL )

# mbmi	r measured Body Mass Index=kg/m2 (continuous) r,s
data %>% select(ends_with('_mbmi')) %>% map(table, exclude = NULL )

# mweight	r weight measurement in kilograms (continuous) r,s
data %>% select(ends_with('_mweight')) %>% map(table, exclude = NULL )

# mobese	r whether measured obese (0,1) r,s
data %>% select(contains('mobese')) %>% map(table, exclude = NULL )

# mcrtyr	r year of current marriage (continuous) r,s
# interact with if married
data %>% select(contains('mcrtyr')) %>% map(table, exclude = NULL )

# data <- rename(data,c('r_mcrtyr'='r_smokev_mcrtyr', 's_mcrtyr' = 's_smokev_mcrtyr'))

# citizen	whether s has U.S. citizenship (0,1) r,s
data %>% select(contains('citizen')) %>% map(table, exclude = NULL )


# GROUP: PHYSICAL HEALTH

# balance	r balance test summary score (1-4) r,s
data %>% select(contains('balance')) %>% map(table, exclude = NULL )

# SUMMARY VARIABLE

# rxbldthn	r takes blood thinning meds to prevent clots (0,1) r,s
data %>% select(contains('rxbldthn')) %>% map(table, exclude = NULL )

data <- data %>% 
  mutate(r_rxbldthn = replace(r_rxbldthn, r_rxbldthn == "d" |
                                r_rxbldthn == "m" |
                                r_rxbldthn == "r" , NA)) 

data <- data %>% 
  mutate(s_rxbldthn = replace(s_rxbldthn, s_rxbldthn == "d" |
                                s_rxbldthn == "m" |
                                s_rxbldthn == "r" |
                                s_rxbldthn == "v" , NA)) %>% 
  mutate(s_rxbldthn = replace(s_rxbldthn, s_rxbldthn == "u", 0))

# rxdiabi	r takes insulin for diabetes (0,1) r,s
data %>% select(contains('rxdiabi')) %>% map(table, exclude = NULL )

data <- data %>% 
  mutate(r_rxdiabi = replace(r_rxdiabi, r_rxdiabi == "d" |
                               r_rxdiabi == "m" |
                               r_rxdiabi == "r" |
                               r_rxdiabi == "s" , NA))

data <- data %>% 
  mutate(s_rxdiabi = replace(s_rxdiabi, s_rxdiabi == "d" |
                               s_rxdiabi == "m" |
                               s_rxdiabi == "r" |
                               s_rxdiabi == "s" |
                               s_rxdiabi == "v" , NA)) %>% 
  mutate(s_rxdiabi = replace(s_rxdiabi, s_rxdiabi == "u", 0))

# rxangina	r takes meds for angina (0,1) r,s
data %>% select(ends_with('rxangina')) %>% map(table, exclude = NULL )

# rxarthr	r takes meds for arthritis (0,1) r,s
data %>% select(contains('rxarthr')) %>% map(table, exclude = NULL )

data <- data %>% 
  mutate(r_rxarthr = replace(r_rxarthr, r_rxarthr == "d" |
                               r_rxarthr == "m" |
                               r_rxarthr == "q" |
                               r_rxarthr == "r" |
                               r_rxarthr == "s" , NA))

data <- data %>% 
  mutate(s_rxarthr = replace(s_rxarthr, s_rxarthr == "d" |
                               s_rxarthr == "m" |
                               s_rxarthr == "q" |
                               s_rxarthr == "r" |
                               s_rxarthr == "s" |
                               s_rxarthr == "v" , NA)) %>% 
  mutate(s_rxarthr = replace(s_rxarthr, s_rxarthr == "u", 0))

# rxbreath	r takes meds for breathing problem (0,1) r,s
data %>% select(contains('rxbreath')) %>% map(table, exclude = NULL)

data <- data %>% 
  mutate(r_rxbreath = replace(r_rxbreath, r_rxbreath == "d" |
                                r_rxbreath == "m" |
                                r_rxbreath == "r" , NA))

data <- data %>% 
  mutate(s_rxbreath = replace(s_rxbreath, s_rxbreath == "d" |
                                s_rxbreath == "m" |
                                s_rxbreath == "r" |
                                s_rxbreath == "v" , NA)) %>% 
  mutate(s_rxbreath = replace(s_rxbreath, s_rxbreath == "u", 0))

# rxchol	r takes meds for cholesterol (0,1) r,s
data %>% select(contains('rxchol')) %>% map(table, exclude = NULL )

data <- data %>% 
  mutate(r_rxchol = replace(r_rxchol, r_rxchol == "d" |
                              r_rxchol == "m" |
                              r_rxchol == "r" , NA))

data <- data %>% 
  mutate(s_rxchol = replace(s_rxchol, s_rxchol == "d" |
                              s_rxchol == "m" |
                              s_rxchol == "r" |
                              s_rxchol == "v" , NA)) %>% 
  mutate(s_rxchol = replace(s_rxchol, s_rxchol == "u", 0))

# rxchf	r takes meds for congestive heart failure (0,1) r,s
data %>% select(contains('rxchf')) %>% map(table, exclude = NULL )

data <- data %>% 
  mutate(r_rxchf = replace(r_rxchf, r_rxchf == "d" |
                             r_rxchf == "m" |
                             r_rxchf == "q" |
                             r_rxchf == "r" |
                             r_rxchf == "s" , NA))

data <- data %>% 
  mutate(s_rxchf = replace(s_rxchf, s_rxchf == "d" |
                             s_rxchf == "m" |
                             s_rxchf == "q" |
                             s_rxchf == "r" |
                             s_rxchf == "s" |
                             s_rxchf == "v" , NA)) %>% 
  mutate(s_rxchf = replace(s_rxchf, s_rxchf == "u", 0))

# rxdepres	r takes meds for depression or anxiety (0,1) r,s
data %>% select(contains('rxdepres')) %>% map(table, exclude = NULL )

data <- data %>% 
  mutate(r_rxdepres = replace(r_rxdepres, r_rxdepres == "d" |
                                r_rxdepres == "m" |
                                r_rxdepres == "r" , NA))

data <- data %>% 
  mutate(s_rxdepres = replace(s_rxdepres, s_rxdepres == "d" |
                                s_rxdepres == "m" |
                                s_rxdepres == "r" |
                                s_rxdepres == "v" , NA)) %>% 
  mutate(s_rxdepres = replace(s_rxdepres, s_rxdepres == "u", 0))

# rxdiab	r takes meds for diabetes (0,1) r,s
data %>% select(ends_with('rxdiab')) %>% map(table, exclude = NULL )

data <- data %>% 
  mutate(r_rxdiab = replace(r_rxdiab, r_rxdiab == "d" |
                               r_rxdiab == "m" |
                               r_rxdiab == "r" |
                               r_rxdiab == "s" , NA))

data <- data %>% 
  mutate(s_rxdiab = replace(s_rxdiab, s_rxdiab == "d" |
                               s_rxdiab == "m" |
                               s_rxdiab == "r" |
                               s_rxdiab == "s" |
                               s_rxdiab == "v" , NA)) %>% 
  mutate(s_rxdiab = replace(s_rxdiab, s_rxdiab == "u", 0))
 
# rxhrtat	r takes meds for heart attack (0,1) r,s
data %>% select(contains('rxhrtat')) %>% map(table, exclude = NULL )

data <- data %>% 
  mutate(r_rxhrtat = replace(r_rxhrtat, r_rxhrtat == "d" |
                               r_rxhrtat == "m" |
                               r_rxhrtat == "r" |
                               r_rxhrtat == "s" , NA))

data <- data %>% 
  mutate(s_rxhrtat = replace(s_rxhrtat, s_rxhrtat == "d" |
                               s_rxhrtat == "m" |
                               s_rxhrtat == "r" |
                               s_rxhrtat == "s" |
                               s_rxhrtat == "v" , NA)) %>% 
  mutate(s_rxhrtat = replace(s_rxhrtat, s_rxhrtat == "u", 0))

# rxheart	r takes meds for heart problems (0,1) r,s
data %>% select(contains('rxheart')) %>% map(table, exclude = NULL )

data <- data %>% 
  mutate(r_rxheart = replace(r_rxheart, r_rxheart == "d" |
                               r_rxheart == "m" |
                               r_rxheart == "r" |
                               r_rxheart == "s" , NA))

data <- data %>% 
  mutate(s_rxheart = replace(s_rxheart, s_rxheart == "d" |
                               s_rxheart == "m" |
                               s_rxheart == "r" |
                               s_rxheart == "s" |
                               s_rxheart == "v" , NA)) %>% 
  mutate(s_rxheart = replace(s_rxheart, s_rxheart == "u", 0))

# rxhibp	r takes meds for high blood pressure (0,1) r,s
data %>% select(contains('rxhibp')) %>% map(table, exclude = NULL )

data <- data %>% 
  mutate(r_rxhibp = replace(r_rxhibp, r_rxhibp == "d" |
                              r_rxhibp == "m" |
                              r_rxhibp == "q" |
                              r_rxhibp == "r" |
                              r_rxhibp == "s" , NA))

data <- data %>% 
  mutate(s_rxhibp = replace(s_rxhibp, s_rxhibp == "d" |
                              s_rxhibp == "m" |
                              s_rxhibp == "q" |
                              s_rxhibp == "r" |
                              s_rxhibp == "s" |
                              s_rxhibp == "v" , NA)) %>% 
  mutate(s_rxhibp = replace(s_rxhibp, s_rxhibp == "u", 0))

# rxlung	r takes meds for lung condition (0,1) r,s
data %>% select(contains('rxlung')) %>% map(table, exclude = NULL )

data <- data %>% 
  mutate(r_rxlung = replace(r_rxlung, r_rxlung == "d" |
                              r_rxlung == "m" |
                              r_rxlung == "q" |
                              r_rxlung == "r" |
                              r_rxlung == "s" , NA))

data <- data %>% 
  mutate(s_rxlung = replace(s_rxlung, s_rxlung == "d" |
                              s_rxlung == "m" |
                              s_rxlung == "q" |
                              s_rxlung == "r" |
                              s_rxlung == "s" |
                              s_rxlung == "v" , NA)) %>% 
  mutate(s_rxlung = replace(s_rxlung, s_rxlung == "u", 0))

# rxmemry	r takes meds for memory problem (0,1) r,s
data %>% select(contains('rxmemry')) %>% map(table, exclude = NULL )

data <- data %>% 
  mutate(r_rxmemry = replace(r_rxmemry, r_rxmemry == "d" |
                               r_rxmemry == "m" |
                               r_rxmemry == "r" , NA))

data <- data %>% 
  mutate(s_rxmemry = replace(s_rxmemry, s_rxmemry == "d" |
                               s_rxmemry == "m" |
                               s_rxmemry == "r" |
                               s_rxmemry == "v" , NA)) %>% 
  mutate(s_rxmemry = replace(s_rxmemry, s_rxmemry == "u", 0))

# rxpain	r takes meds for pain (0,1) r,s
data %>% select(contains('rxpain')) %>% map(table, exclude = NULL )

data <- data %>% 
  mutate(r_rxpain = replace(r_rxpain, r_rxpain == "d" |
                              r_rxpain == "m" |
                              r_rxpain == "r" , NA))

data <- data %>% 
  mutate(s_rxpain = replace(s_rxpain, s_rxpain == "d" |
                              s_rxpain == "m" |
                              s_rxpain == "r" |
                              s_rxpain == "v" , NA)) %>% 
  mutate(s_rxpain = replace(s_rxpain, s_rxpain == "u", 0))

# rxstom	r takes meds for stomach (0,1) r,s
data %>% select(contains('rxstom')) %>% map(table, exclude = NULL )

data <- data %>% 
  mutate(r_rxstom = replace(r_rxstom, r_rxstom == "d" |
                              r_rxstom == "m" |
                              r_rxstom == "r" , NA))

data <- data %>% 
  mutate(s_rxstom = replace(s_rxstom, s_rxstom == "d" |
                              s_rxstom == "m" |
                              s_rxstom == "r" |
                              s_rxstom == "v" , NA)) %>% 
  mutate(s_rxstom = replace(s_rxstom, s_rxstom == "u", 0))

# rxstrok	r takes meds for stroke (0,1) r,s
data %>% select(contains('rxstrok')) %>% map(table, exclude = NULL )

data <- data %>% 
  mutate(r_rxstrok = replace(r_rxstrok, r_rxstrok == "d" |
                               r_rxstrok == "m" |
                               r_rxstrok == "q" |
                               r_rxstrok == "r" |
                               r_rxstrok == "s" , NA))

data <- data %>% 
  mutate(s_rxstrok = replace(s_rxstrok, s_rxstrok == "d" |
                               s_rxstrok == "m" |
                               s_rxstrok == "q" |
                               s_rxstrok == "r" |
                               s_rxstrok == "s" |
                               s_rxstrok == "v" , NA)) %>% 
  mutate(s_rxstrok = replace(s_rxstrok, s_rxstrok == "u", 0))

# rxslp	r takes meds to sleep (0,1) r,s
data %>% select(contains('rxslp')) %>% map(table, exclude = NULL )

data <- data %>% 
  mutate(r_rxslp = replace(r_rxslp, r_rxslp == "d" |
                             r_rxslp == "m" |
                             r_rxslp == "r" , NA))

data <- data %>% 
  mutate(s_rxslp = replace(s_rxslp, s_rxslp == "d" |
                             s_rxslp == "m" |
                             s_rxslp == "r" |
                             s_rxslp == "v" , NA)) %>% 
  mutate(s_rxslp = replace(s_rxslp, s_rxslp == "u", 0))

# rxdiabo	r takes oral meds for diabetes (0,1) r,s
data %>% select(contains('rxdiabo')) %>% map(table, exclude = NULL )

data <- data %>% 
  mutate(r_rxdiabo = replace(r_rxdiabo, r_rxdiabo == "d" |
                               r_rxdiabo == "m" |
                               r_rxdiabo == "r" |
                               r_rxdiabo == "s" , NA))

data <- data %>% 
  mutate(s_rxdiabo = replace(s_rxdiabo, s_rxdiabo == "d" |
                               s_rxdiabo == "m" |
                               s_rxdiabo == "r" |
                               s_rxdiabo == "s" |
                               s_rxdiabo == "v" , NA)) %>% 
  mutate(s_rxdiabo = replace(s_rxdiabo, s_rxdiabo == "u", 0))

dat <- data %>% select(c('r_rxbldthn','s_rxbldthn','r_rxdiabi','s_rxdiabi','r_rxarthr',
                       's_rxarthr','r_rxbreath','s_rxbreath','r_rxchol','s_rxchol',
                       'r_rxchf','s_rxchf','r_rxdepres','s_rxdepres','r_rxdiab',
                       's_rxdiab','r_rxhrtat','s_rxhrtat','r_rxheart','s_rxheart',
                       'r_rxhibp','s_rxhibp','r_rxlung','s_rxlung','r_rxmemry',
                       's_rxmemry','r_rxpain','s_rxpain','r_rxstom','s_rxstom',
                       'r_rxstrok','s_rxstrok','r_rxslp','s_rxslp','r_rxdiabo','s_rxdiabo'))

dat <- as.data.frame(sapply(dat, as.numeric))

dat[is.na(dat)] <- 0

data$r_sum_med <- dat$r_rxbldthn + dat$r_rxdiabi + #dat$r_rxangina + 
  dat$r_rxarthr + dat$r_rxbreath + dat$r_rxchol + dat$r_rxchf + dat$r_rxdepres + 
  dat$r_rxdiab + dat$r_rxhrtat + dat$r_rxheart + dat$r_rxhibp + dat$r_rxlung + 
  dat$r_rxmemry + dat$r_rxpain + dat$r_rxstom + dat$r_rxstrok + dat$r_rxslp + dat$r_rxdiabo

data$s_sum_med <- dat$s_rxbldthn + dat$s_rxdiabi + #dat$s_rxangina + 
  dat$s_rxarthr + dat$s_rxbreath + dat$s_rxchol + dat$s_rxchf + dat$s_rxdepres + 
  dat$s_rxdiab + dat$s_rxhrtat + dat$s_rxheart + dat$s_rxhibp + dat$s_rxlung + 
  dat$s_rxmemry + dat$s_rxpain + dat$s_rxstom + dat$s_rxstrok + dat$s_rxslp + dat$s_rxdiabo

# adlfive	r 5-item adl summary/0-5:  In Wave 1, ADLFIVE is the sum of BATH + DRESS 
# + EAT + BED + v9034 (toilet), where "1.not at all diff" is counted as 0, and "2.a 
# little diff", "3.somewhat diff", and "4.very diff/can't do" are counted as 1. The difficulty 
# using the toilet variable, v9034, is only asked to a subset of Wave 1 respondents. 
#Starting in Wave 2, ADLFIVE is the sum of BATHA + DRESSA + EATA + BEDA + TOILTA. 
# ADLFIVE is calculated as long as at least one of the comprising ADL measures is not missing. 
# (0-5) r,s
data %>% select(contains('adlfive')) %>% map(table, exclude = NULL )

# lstmnspd	r age last menstrual period (continuous) r,s
# interact with gender
data %>% select(contains('lstmnspd')) %>% map(table, exclude = NULL )

data <- rename(data,c('r_gender_lstmnspd'='r_lstmnspd', 's_gender_lstmnspd'='s_lstmnspd'))

# reccancr	r age most recent cancer diagnosis (continuous) r,s
data %>% select(contains('reccancr')) %>% map(table, exclude = NULL )

data <- rename(data,c('r_cancre_reccancr'='r_reccancr', 's_cancre_reccancr'='s_reccancr'))

# rechrtatt	r age most recent heart attack (continuous) r,s
data %>% select(contains('rechrtatt')) %>% map(table, exclude = NULL )

data <- rename(data,c('r_hrtatte_rechrtatt'='r_rechrtatt', 's_hrtatte_rechrtatt'='s_rechrtatt'))

# stroke	r ever stroke (0,1) r,s
data %>% select(contains('stroke')) %>% map(table, exclude = NULL )

# recstrok	r age most recent stroke (continuous) r,s
data %>% select(contains('recstrok')) %>% map(table, exclude = NULL )

data <- rename(data,c('r_stroke_recstrok'='r_recstrok', 's_stroke_recstrok'='s_recstrok'))

# urinai	r any urinary incontinence (0,1) r,s
data %>% select(contains('urinai')) %>% map(table, exclude = NULL )

# diasto	r average blood pressure measure (diastolic) 2 & 3 (continuous) r,s
data %>% select(contains('diasto')) %>% map(table, exclude = NULL )

# systo	r average blood pressure measure (systolic) 2 & 3 (continuous) r,s
data %>% select(contains('systo')) %>% map(table, exclude = NULL )

# pulse	r average pulse measure 2 & 3 (continuous) r,s
data %>% select(contains('pulse')) %>% map(table, exclude = NULL )

# cancrst	r cancer status (1-3) r,s
data %>% select(contains('cancrst')) %>% map(table, exclude = NULL )

data <- rename(data,c('r_cancre_cancrst'='r_cancrst', 's_cancre_cancrst'='s_cancrst'))

# cancre r ever had cancer (0,1) r,s
data %>% select(contains('cancre')) %>% map(table, exclude = NULL )

# diabe r ever had diabetes (0,1) r,s
data %>% select(contains('diabe')) %>% map(table, exclude = NULL )

# hipe	r ever fractured hip (0,1) r,s
data %>% select(contains('hipe')) %>% map(table, exclude = NULL )

# hrtrhme	r ever had abnormal heart rhythm (0,1) r,s
data %>% select(contains('hrtrhme')) %>% map(table, exclude = NULL )

# angine	r ever had angina (0,1) r,s
data %>% select(contains('angine')) %>% map(table, exclude = NULL )

# catrcte	r ever had cataract surgery (0,1) r,s
data %>% select(contains('catrcte')) %>% map(table, exclude = NULL )

# conhrtfe	r ever had congestive heart failure (0,1) r,s
data %>% select(contains('conhrtfe')) %>% map(table, exclude = NULL )

# hrtatte	r ever had heart attack (0,1) r,s
data %>% select(contains('hrtatte')) %>% map(table, exclude = NULL )

# hrtsrge	r ever had heart surgery
#.x doesn't have condition
data %>% select(contains('hrtsrge')) %>% map(table, exclude = NULL )

data <- data %>% mutate(r_hrtsrge = replace(r_hrtsrge, r_hrtsrge == "x", 0))
data <- data %>% mutate(s_hrtsrge = replace(s_hrtsrge, s_hrtsrge == "x", 0))

# hchole	r ever had high cholesterol (0,1) r,s
data %>% select(contains('hchole')) %>% map(table, exclude = NULL )

# hystere	r ever had hysterectomy (0,1) r,s
# interact with gender
data %>% select(contains('hystere')) %>% map(table, exclude = NULL )

data <- rename(data,c('r_gender_hystere'='r_hystere', 's_gender_hystere'='s_hystere'))

# jointre	r ever had joint replaced (0,1) r,s
data %>% select(contains('jointre')) %>% map(table, exclude = NULL )

# osteoe	r ever had osteoporosis (0,1) r,s
data %>% select(contains('osteoe')) %>% map(table, exclude = NULL )

# shingle	r ever had shingles (0,1) r,s
data %>% select(contains('shingle')) %>% map(table, exclude = NULL )

# glaucoma	r ever treated for glaucoma (0,1) r,s
data %>% select(contains('glaucoma')) %>% map(table, exclude = NULL )

# lowermob	r lower body mobility summary:  In Wave 1, LOWERMOB is the sum of WALK1 
# + CLIMS + CHAIR, where "1.yes" and "2.can't do" are counted as 1. Starting in 
# Wave 2, LOWERMOB is the sum of WALK1A + CLIMSA + CHAIRA + STOOPA.
#(0-4) r,s
data %>% select(contains('lowermob')) %>% map(table, exclude = NULL )

data <- data %>% mutate(r_lowermob = replace(r_lowermob, r_lowermob == "x", 0))
data <- data %>% mutate(s_lowermob = replace(s_lowermob, s_lowermob == "x", 0))

# ncatrct	r number eyes cataract surg this wave (1,2) r,s
data %>% select(contains('ncatrct')) %>% map(table, exclude = NULL )

data <- data %>% mutate(r_ncatrct = replace(r_ncatrct, r_ncatrct == "x", 0))
data <- data %>% mutate(s_ncatrct = replace(s_ncatrct, s_ncatrct == "x", 0))

# pneushte	r pneumonia vaccine ever (0,1) r,s
data %>% select(contains('pneushte')) %>% map(table, exclude = NULL )

# hrtrhm	r reports abnormal heart rhythm since last wave (0,1) r,s
data %>% select(ends_with('hrtrhm')) %>% map(table, exclude = NULL )

# catrct	r reports cataract surgery this wave (0,1) r,s
data %>% select(ends_with('_catrct')) %>% map(table, exclude = NULL )

# conhrtf	r reports congestive heart failure since last wave (0,1) r,s
data %>% select(ends_with('conhrtf')) %>% map(table, exclude = NULL )

# hrtatt	r reports heart attack since last wave (0,1) r,s
data %>% select(ends_with('_hrtatt')) %>% map(table, exclude = NULL )

# sight	r self-rated eyesight (1-6) r,s
data %>% select(contains('_sight')) %>% map(table, exclude = NULL )

# hearing	r self-rated hearing (1-5) r,s
data %>% select(contains('hearing')) %>% map(table, exclude = NULL )

# fatigue	r severe fatigue (0,1) r,s
data %>% select(contains('fatigue')) %>% map(table, exclude = NULL )

# shnglshte	r shingles vaccine ever (0,1) r,s
data %>% select(contains('shnglshte')) %>% map(table, exclude = NULL )

# uppermob	r upper body mobility summary: In Wave 1, UPPERMOB is the sum of ARMS 
# + LIFT + DIME, where "1.yes" and "2.can't do" are counted as 1. Starting in Wave 2, 
# UPPERMOB is the sum of ARMSA + LIFTA + DIMEA (0-3) r,s
#.x doesn't have condition
data %>% select(contains('uppermob')) %>% map(table, exclude = NULL )

data <- data %>% mutate(r_uppermob = replace(r_uppermob, r_uppermob == "x", 0))
data <- data %>% mutate(s_uppermob = replace(s_uppermob, s_uppermob == "x", 0))

# painlv	r usual level of pain (0-3) r,s
data %>% select(contains('painlv')) %>% map(table, exclude = NULL )

# lunglmt	r whether lung condition limits activity (0,1) r,s
data %>% select(contains('lunglmt')) %>% map(table, exclude = NULL )

data <- data %>% mutate(r_lunglmt = replace(r_lunglmt, r_lunglmt == "x", 0))
data <- data %>% mutate(s_lunglmt = replace(s_lunglmt, s_lunglmt == "x", 0))

# diaghrtr	s age first diagnosed with abnormal heart rhythm (continuous) r,s
data %>% select(contains('diaghrtr')) %>% map(table, exclude = NULL )

data <- rename(data,c('r_hrtrhme_diaghrtr'='r_diaghrtr', 's_hrtrhme_diaghrtr'='s_diaghrtr'))

# diagangin	s age first diagnosed with angina (continuous) r,s
data %>% select(contains('diagangin')) %>% map(table, exclude = NULL )

data <- rename(data,c('r_angine_diagangin'='r_diagangin', 's_angine_diagangin'='s_diagangin'))

# diagchf	s age first diagnosed with congestive heart failure (continuous) r,s
data %>% select(contains('diagchf')) %>% map(table, exclude = NULL )

data <- rename(data,c('r_conhrtfe_diagchf'='r_diagchf', 's_conhrtfe_diagchf'='s_diagchf'))

# diagdiab	s age first diagnosed with diabetes (continuous) r,s
data %>% select(contains('diagdiab')) %>% map(table, exclude = NULL )

data <- rename(data,c('r_diabe_diagdiab'='r_diagdiab', 's_diabe_diagdiab'='s_diagdiab'))

# frhrtatt	s age first heart attack (continuous) r,s
data %>% select(contains('frhrtatt')) %>% map(table, exclude = NULL )

data <- rename(data,c('r_hrtatte_frhrtatt'='r_frhrtatt', 's_hrtatte_frhrtatt'='s_frhrtatt'))

# limimpar	whether r limited in any way due to impairment (0,1) r,s
data %>% select(contains('limimpar')) %>% map(table, exclude = NULL )


# GROUP: MENTAL HEALTH

# cidimde3 : r,s CIDI probable major depressive episode (3+ symp) (0,1)
# if .s it means that it was asked in a previous wave and not asked again,
# think about how to impute this
data %>% select(contains('cidimde3')) %>% map(table, exclude = NULL)

# cidisymp : r,s CIDI total symptom score (0-7)
data %>% select(contains('cidisymp')) %>% map(table, exclude = NULL)

# lideal3	: r,s	Life is close to ideal 3-point
data %>% select(contains('lideal3')) %>% map(table, exclude = NULL)

# lstsf3 : r,s Satisfied with life 3-point
# Asked only from wave 7 
data %>% select(contains('lstsf3')) %>% map(table, exclude = NULL)

# rested : r,s feels rested when wakes up (1-3)
# Asked starting from wave 6
data %>% select(contains('rested')) %>% map(table, exclude = NULL)

# rxpsych :	r,s	takes meds for psych condition (0,1)
data %>% select(contains('rxpsych')) %>% map(table, exclude = NULL)

# trpsych :	r,s	receives psychological treatment (0,1)
data %>% select(contains('trpsych')) %>% map(table, exclude = NULL)


# GROUP: SUPPORT

# ftrhlp : r,s	anyone able to help r with future adl needs (0,1)
# NA: if .h put 1 (someone is currently helping the respondent)
data %>% select(contains('ftrhlp')) %>% map(table, exclude = NULL)

data <- data %>% mutate(r_ftrhlp = replace(r_ftrhlp, r_ftrhlp == "h", 1))
data <- data %>% mutate(r_ftrhlp = replace(r_ftrhlp, r_ftrhlp == "n", 1))

data <- data %>% mutate(s_ftrhlp = replace(s_ftrhlp, s_ftrhlp == "h", 1))
data <- data %>% mutate(s_ftrhlp = replace(s_ftrhlp, s_ftrhlp == "n", 1))

# mealhlp :	r,s	whether anyone helps r with meal preparation (0,1)
# NA if .x put 0, respondents does not need help and if .j put 0, 
# respondents don't do the activity
data %>% select(contains('mealhlp')) %>% map(table, exclude = NULL)

data <- data %>% mutate(r_mealhlp = replace(r_mealhlp, r_mealhlp == "x", 0))
data <- data %>% mutate(r_mealhlp = replace(r_mealhlp, r_mealhlp == "j", 0))

data <- data %>% mutate(s_mealhlp = replace(s_mealhlp, s_mealhlp == "x", 0))
data <- data %>% mutate(s_mealhlp = replace(s_mealhlp, s_mealhlp == "j", 0))

# medhlp : r,s	whether anyone helps r with taking medication (0,1)
# NA if .x put 0, respondents does not need help and if .j put 0, 
# respondents don't do the activity
data %>% select(contains('medhlp')) %>% map(table, exclude = NULL)

data <- data %>% mutate(r_medhlp = replace(r_medhlp, r_medhlp == "x", 0))
data <- data %>% mutate(r_medhlp = replace(r_medhlp, r_medhlp == "j", 0))

data <- data %>% mutate(s_medhlp = replace(s_medhlp, s_medhlp == "x", 0))
data <- data %>% mutate(s_medhlp = replace(s_medhlp, s_medhlp == "j", 0))

# moneyhlp : r,s	whether anyone helps r with managing money (0,1)
# NA if .x put 0, respondents does not need help and if .j put 0, 
# respondents don't do the activity
data %>% select(contains('moneyhlp')) %>% map(table, exclude = NULL)

data <- data %>% mutate(r_moneyhlp = replace(r_moneyhlp, r_moneyhlp == "x", 0))
data <- data %>% mutate(r_moneyhlp = replace(r_moneyhlp, r_moneyhlp == "j", 0))

data <- data %>% mutate(s_moneyhlp = replace(s_moneyhlp, s_moneyhlp == "x", 0))
data <- data %>% mutate(s_moneyhlp = replace(s_moneyhlp, s_moneyhlp == "j", 0))

# phonehlp : r,s	whether anyone helps r with phone calls (0,1)
# NA if .x put 0, respondents does not need help and if .j put 0, 
# respondents don't do the activity
data %>% select(contains('phonehlp')) %>% map(table, exclude = NULL)

data <- data %>% mutate(r_phonehlp = replace(r_phonehlp, r_phonehlp == "x", 0))
data <- data %>% mutate(r_phonehlp = replace(r_phonehlp, r_phonehlp == "j", 0))

data <- data %>% mutate(s_phonehlp = replace(s_phonehlp, s_phonehlp == "x", 0))
data <- data %>% mutate(s_phonehlp = replace(s_phonehlp, s_phonehlp == "j", 0))

# shophlp	: r,s	whether anyone helps r with grocery shopping (0,1)
# NA if .x put 0, no difficulties
data %>% select(contains('shophlp')) %>% map(table, exclude = NULL)

data <- data %>% mutate(r_shophlp = replace(r_shophlp, r_shophlp == "x", 0))
data <- data %>% mutate(r_shophlp = replace(r_shophlp, r_shophlp == "j", 0))

data <- data %>% mutate(s_shophlp = replace(s_shophlp, s_shophlp == "x", 0))
data <- data %>% mutate(s_shophlp = replace(s_shophlp, s_shophlp == "j", 0))

# rcany	: r,s	receives any care for adls/iadls (0,1)
# NA if .x put 0, respondents has no difficulties for any adls/iadls
data %>% select(contains('rcany')) %>% map(table, exclude = NULL)

data <- data %>% mutate(r_rcany = replace(r_rcany, r_rcany == "x", 0))
data <- data %>% mutate(s_rcany = replace(s_rcany, s_rcany == "x", 0))

# rccarehr : r,s	hours/day kids/grandkids help r with adls/iadls
# RCCAREHR is the sum of hours per day for all children or grandchildren helpers, and so values can be 
# over 24 hours (>=0, continuous integer)
# NA if .m put 0: not helped 
# NA if .x or .h put 0, no difficulties or not helped
data %>% select(contains('rccarehr')) %>% map(table, exclude = NULL)

data <- data %>% mutate(r_rccarehr = replace(r_rccarehr, r_rccarehr == "m", 0))
data <- data %>% mutate(r_rccarehr = replace(r_rccarehr, r_rccarehr == "x", 0))
data <- data %>% mutate(r_rccarehr = replace(r_rccarehr, r_rccarehr == "h", 0))
data <- data %>% mutate(s_rccarehr = replace(s_rccarehr, s_rccarehr == "m", 0))
data <- data %>% mutate(s_rccarehr = replace(s_rccarehr, s_rccarehr == "x", 0))
data <- data %>% mutate(s_rccarehr = replace(s_rccarehr, s_rccarehr == "h", 0))
data <- data %>% mutate(s_rccarehr = replace(s_rccarehr, s_rccarehr == "u", 0))

data <- data %>% 
  mutate(r_rccarehr = replace(r_rccarehr, r_rccarehr == "d" | 
                                r_rccarehr == "f" |
                                r_rccarehr == "q" |
                                r_rccarehr == "r" |
                                r_rccarehr == "v" , NA))

data <- data %>% 
  mutate(s_rccarehr = replace(s_rccarehr, s_rccarehr == "d" | 
                                s_rccarehr == "f" |
                                s_rccarehr == "q" |
                                s_rccarehr == "r" |
                                s_rccarehr == "v" , NA))

data$r_rccarehr <- case_when(data$r_rccarehr >= 48 ~ 48,TRUE ~ as.numeric(data$r_rccarehr))
data$s_rccarehr <- case_when(data$s_rccarehr >= 48 ~ 48,TRUE ~ as.numeric(data$s_rccarehr))
data %>% select(contains('rccarehr')) %>% map(table, exclude = NULL)

# rccaren	: r,s	# kids/grandkids who help r with adls/iadls ((>=0, continuous integer))
# NA if .x or .h put 0, no difficulties or not helped
data %>% select(contains('rccaren')) %>% map(table, exclude = NULL)

data <- data %>% mutate(r_rccaren = replace(r_rccaren, r_rccaren == "x", 0))
data <- data %>% mutate(r_rccaren = replace(r_rccaren, r_rccaren == "h", 0))
data <- data %>% mutate(s_rccaren = replace(s_rccaren, s_rccaren == "x", 0))
data <- data %>% mutate(s_rccaren = replace(s_rccaren, s_rccaren == "h", 0))
data <- data %>% mutate(s_rccaren = replace(s_rccaren, s_rccaren == "u", 0))

data <- data %>% 
  mutate(r_rccaren = replace(r_rccaren, r_rccaren == "d" | 
                               r_rccaren == "f" |
                               r_rccaren == "m" |
                               r_rccaren == "q" |
                               r_rccaren == "r" |
                               r_rccaren == "v" , NA))

data <- data %>% 
  mutate(s_rccaren = replace(s_rccaren, s_rccaren == "d" | 
                               s_rccaren == "f" |
                               s_rccaren == "m" |
                               s_rccaren == "q" |
                               s_rccaren == "r" |
                               s_rccaren == "v" , NA))

# rfcarehr : r,s	hours/day non-relatives help r with adls/iadls
# sum of hours per day for all non-relatives helpers, and so values can be 
# over 24 hours (>=0, continuous integer)
# NA if .x or .h put 0, no difficulties or not helped
data %>% select(contains('rfcarehr')) %>% map(table, exclude = NULL)

data <- data %>% mutate(r_rfcarehr = replace(r_rfcarehr, r_rfcarehr == "m", 0))
data <- data %>% mutate(r_rfcarehr = replace(r_rfcarehr, r_rfcarehr == "x", 0))
data <- data %>% mutate(r_rfcarehr = replace(r_rfcarehr, r_rfcarehr == "h", 0))
data <- data %>% mutate(s_rfcarehr = replace(s_rfcarehr, s_rfcarehr == "m", 0))
data <- data %>% mutate(s_rfcarehr = replace(s_rfcarehr, s_rfcarehr == "x", 0))
data <- data %>% mutate(s_rfcarehr = replace(s_rfcarehr, s_rfcarehr == "h", 0))
data <- data %>% mutate(s_rfcarehr = replace(s_rfcarehr, s_rfcarehr == "u", 0))

data <- data %>% 
  mutate(r_rfcarehr = replace(r_rfcarehr, r_rfcarehr == "d" | 
                                r_rfcarehr == "e" |
                                r_rfcarehr == "f" |
                                r_rfcarehr == "q" |
                                r_rfcarehr == "r" |
                                r_rfcarehr == "w" |
                                r_rfcarehr == "v" , NA))

data <- data %>% 
  mutate(s_rfcarehr = replace(s_rfcarehr, s_rfcarehr == "d" | 
                                s_rfcarehr == "e" |
                                s_rfcarehr == "f" |
                                s_rfcarehr == "q" |
                                s_rfcarehr == "r" |
                                s_rfcarehr == "w" |
                                s_rfcarehr == "v" , NA))

data$r_rfcarehr <- case_when(data$r_rfcarehr >= 48 ~ 48,TRUE ~ as.numeric(data$r_rfcarehr))
data$s_rfcarehr <- case_when(data$s_rfcarehr >= 48 ~ 48,TRUE ~ as.numeric(data$s_rfcarehr))
data %>% select(contains('rfcarehr')) %>% map(table, exclude = NULL)

# rfcaren	: r,s	# non-relatives who help r with adls/iadls (>=0, continuous integer)
# NA if .x or .h put 0, no difficulties or not helped
data %>% select(contains('rfcaren')) %>% map(table, exclude = NULL)

data <- data %>% mutate(r_rfcaren = replace(r_rfcaren, r_rfcaren == "x", 0))
data <- data %>% mutate(r_rfcaren = replace(r_rfcaren, r_rfcaren == "h", 0))
data <- data %>% mutate(s_rfcaren = replace(s_rfcaren, s_rfcaren == "x", 0))
data <- data %>% mutate(s_rfcaren = replace(s_rfcaren, s_rfcaren == "h", 0))
data <- data %>% mutate(s_rfcaren = replace(s_rfcaren, s_rfcaren == "u", 0))

data <- data %>% 
  mutate(r_rfcaren = replace(r_rfcaren, r_rfcaren == "d" | 
                               r_rfcaren == "f" |
                               r_rfcaren == "m" |
                               r_rfcaren == "q" |
                               r_rfcaren == "r" |
                               r_rfcaren == "v" , NA))

data <- data %>% 
  mutate(s_rfcaren = replace(s_rfcaren, s_rfcaren == "d" | 
                               s_rfcaren == "f" |
                               s_rfcaren == "m" |
                               s_rfcaren == "q" |
                               s_rfcaren == "r" |
                               s_rfcaren == "v" , NA))

# rpfcarehr :	r,s	hours/day paid professionals help r with adls/iadls
# sum of hours per day for all paid professionals, and so values can be 
# over 24 hours (>=0, continuous integer)
# NA if .x or .h put 0, no difficulties or not helped
data %>% select(contains('rpfcarehr')) %>% map(table, exclude = NULL)

data <- data %>% mutate(r_rpfcarehr = replace(r_rpfcarehr, r_rpfcarehr == "m", 0))
data <- data %>% mutate(r_rpfcarehr = replace(r_rpfcarehr, r_rpfcarehr == "x", 0))
data <- data %>% mutate(r_rpfcarehr = replace(r_rpfcarehr, r_rpfcarehr == "h", 0))
data <- data %>% mutate(s_rpfcarehr = replace(s_rpfcarehr, s_rpfcarehr == "m", 0))
data <- data %>% mutate(s_rpfcarehr = replace(s_rpfcarehr, s_rpfcarehr == "x", 0))
data <- data %>% mutate(s_rpfcarehr = replace(s_rpfcarehr, s_rpfcarehr == "h", 0))
data <- data %>% mutate(s_rpfcarehr = replace(s_rpfcarehr, s_rpfcarehr == "u", 0))

data <- data %>% 
  mutate(r_rpfcarehr = replace(r_rpfcarehr, r_rpfcarehr == "d" | 
                                 r_rpfcarehr == "e" |
                                 r_rpfcarehr == "f" |
                                 r_rpfcarehr == "q" |
                                 r_rpfcarehr == "r" |
                                 r_rpfcarehr == "v" , NA))

data <- data %>% 
  mutate(s_rpfcarehr = replace(s_rpfcarehr, s_rpfcarehr == "d" | 
                                 s_rpfcarehr == "e" |
                                 s_rpfcarehr == "f" |
                                 s_rpfcarehr == "q" |
                                 s_rpfcarehr == "r" |
                                 s_rpfcarehr == "v" , NA))

data$r_rpfcarehr <- case_when(data$r_rpfcarehr >= 48 ~ 48,TRUE ~ as.numeric(data$r_rpfcarehr))
data$s_rpfcarehr <- case_when(data$s_rpfcarehr >= 48 ~ 48,TRUE ~ as.numeric(data$s_rpfcarehr))
data %>% select(contains('rpfcarehr')) %>% map(table, exclude = NULL)

# rpfcaren : r,s	# paid professionals who help r with adls/iadls (>=0, continuous integer)
# NA if .x or .h put 0, no difficulties or not helped
data %>% select(contains('rpfcaren')) %>% map(table, exclude = NULL)

data <- data %>% mutate(r_rpfcaren = replace(r_rpfcaren, r_rpfcaren == "x", 0))
data <- data %>% mutate(r_rpfcaren = replace(r_rpfcaren, r_rpfcaren == "h", 0))
data <- data %>% mutate(s_rpfcaren = replace(s_rpfcaren, s_rpfcaren == "x", 0))
data <- data %>% mutate(s_rpfcaren = replace(s_rpfcaren, s_rpfcaren == "h", 0))
data <- data %>% mutate(s_rpfcaren = replace(s_rpfcaren, s_rpfcaren == "u", 0))

data <- data %>% 
  mutate(r_rpfcaren = replace(r_rpfcaren, r_rpfcaren == "d" | 
                                r_rpfcaren == "f" |
                                r_rpfcaren == "m" |
                                r_rpfcaren == "q" |
                                r_rpfcaren == "r" |
                                r_rpfcaren == "v" , NA))

data <- data %>% 
  mutate(s_rpfcaren = replace(s_rpfcaren, s_rpfcaren == "d" | 
                                s_rpfcaren == "f" |
                                s_rpfcaren == "m" |
                                s_rpfcaren == "q" |
                                s_rpfcaren == "r" |
                                s_rpfcaren == "v" , NA))

# rrcarehr :	r,s	hours/day relatives help r with adls/iadls
# sum of hours per day for all relatives, and so values can be 
# over 24 hours (>=0, continuous integer)
# NA if .x or .h put 0, no difficulties or not helped
data %>% select(contains('rrcarehr')) %>% map(table, exclude = NULL)

data <- data %>% mutate(r_rrcarehr = replace(r_rrcarehr, r_rrcarehr == "m", 0))
data <- data %>% mutate(r_rrcarehr = replace(r_rrcarehr, r_rrcarehr == "x", 0))
data <- data %>% mutate(r_rrcarehr = replace(r_rrcarehr, r_rrcarehr == "h", 0))
data <- data %>% mutate(s_rrcarehr = replace(s_rrcarehr, s_rrcarehr == "m", 0))
data <- data %>% mutate(s_rrcarehr = replace(s_rrcarehr, s_rrcarehr == "x", 0))
data <- data %>% mutate(s_rrcarehr = replace(s_rrcarehr, s_rrcarehr == "h", 0))
data <- data %>% mutate(s_rrcarehr = replace(s_rrcarehr, s_rrcarehr == "u", 0))

data <- data %>% 
  mutate(r_rrcarehr = replace(r_rrcarehr, r_rrcarehr == "d" | 
                                r_rrcarehr == "e" |
                                r_rrcarehr == "f" |
                                r_rrcarehr == "q" |
                                r_rrcarehr == "r" |
                                r_rrcarehr == "v" , NA))

data <- data %>% 
  mutate(s_rrcarehr = replace(s_rrcarehr, s_rrcarehr == "d" | 
                                s_rrcarehr == "e" |
                                s_rrcarehr == "f" |
                                s_rrcarehr == "q" |
                                s_rrcarehr == "r" |
                                s_rrcarehr == "v" , NA))

data$r_rrcarehr <- case_when(data$r_rrcarehr >= 48 ~ 48,TRUE ~ as.numeric(data$r_rrcarehr))
data$s_rrcarehr <- case_when(data$s_rrcarehr >= 48 ~ 48,TRUE ~ as.numeric(data$s_rrcarehr))
data %>% select(contains('rrcarehr')) %>% map(table, exclude = NULL)

# rrcaren	: r,s	# relatives who help r with adls/iadls (>=0, continuous integer)
# NA if .x or .h put 0, no difficulties or not helped
data %>% select(contains('rrcaren')) %>% map(table, exclude = NULL)

data <- data %>% mutate(r_rrcaren = replace(r_rrcaren, r_rrcaren == "x", 0))
data <- data %>% mutate(r_rrcaren = replace(r_rrcaren, r_rrcaren == "h", 0))
data <- data %>% mutate(s_rrcaren = replace(s_rrcaren, s_rrcaren == "x", 0))
data <- data %>% mutate(s_rrcaren = replace(s_rrcaren, s_rrcaren == "h", 0))
data <- data %>% mutate(s_rrcaren = replace(s_rrcaren, s_rrcaren == "u", 0))

data <- data %>% 
  mutate(r_rrcaren = replace(r_rrcaren, r_rrcaren == "d" | 
                               r_rrcaren == "f" |
                               r_rrcaren == "m" |
                               r_rrcaren == "q" |
                               r_rrcaren == "r" |
                               r_rrcaren == "v" , NA))

data <- data %>% 
  mutate(s_rrcaren = replace(s_rrcaren, s_rrcaren == "d" | 
                               s_rrcaren == "f" |
                               s_rrcaren == "m" |
                               s_rrcaren == "q" |
                               s_rrcaren == "r" |
                               s_rrcaren == "v" , NA))

# rscarehr : r,s	hours/day spouse helps r with adls/iadls
# sum of hours per day for all spouses, and so values can be 
# over 24 hours (>=0, continuous integer)
# NA if .x or .h put 0, no difficulties or not helped
data %>% select(contains('rscarehr')) %>% map(table, exclude = NULL)

data <- data %>% mutate(r_rscarehr = replace(r_rscarehr, r_rscarehr == "m", 0))
data <- data %>% mutate(r_rscarehr = replace(r_rscarehr, r_rscarehr == "x", 0))
data <- data %>% mutate(r_rscarehr = replace(r_rscarehr, r_rscarehr == "h", 0))
data <- data %>% mutate(s_rscarehr = replace(s_rscarehr, s_rscarehr == "m", 0))
data <- data %>% mutate(s_rscarehr = replace(s_rscarehr, s_rscarehr == "x", 0))
data <- data %>% mutate(s_rscarehr = replace(s_rscarehr, s_rscarehr == "h", 0))
data <- data %>% mutate(s_rscarehr = replace(s_rscarehr, s_rscarehr == "u", 0))

data <- data %>% 
  mutate(r_rscarehr = replace(r_rscarehr, r_rscarehr == "d" | 
                                r_rscarehr == "e" |
                                r_rscarehr == "f" |
                                r_rscarehr == "q" |
                                r_rscarehr == "r" |
                                r_rscarehr == "v" , NA))

data <- data %>% 
  mutate(s_rscarehr = replace(s_rscarehr, s_rscarehr == "d" | 
                                s_rscarehr == "e" |
                                s_rscarehr == "f" |
                                s_rscarehr == "q" |
                                s_rscarehr == "r" |
                                s_rscarehr == "v" , NA))

# rufcarehr :	r,s	hours/day unpaid professionals help r with adls/iadls
# sum of hours per day for all unpaid professionals, and so values can be 
# over 24 hours (>=0, continuous integer)
# NA if .x or .h put 0, no difficulties or not helped
data %>% select(contains('rufcarehr')) %>% map(table, exclude = NULL)

data <- data %>% mutate(r_rufcarehr = replace(r_rufcarehr, r_rufcarehr == "m", 0))
data <- data %>% mutate(r_rufcarehr = replace(r_rufcarehr, r_rufcarehr == "x", 0))
data <- data %>% mutate(r_rufcarehr = replace(r_rufcarehr, r_rufcarehr == "h", 0))
data <- data %>% mutate(s_rufcarehr = replace(s_rufcarehr, s_rufcarehr == "m", 0))
data <- data %>% mutate(s_rufcarehr = replace(s_rufcarehr, s_rufcarehr == "x", 0))
data <- data %>% mutate(s_rufcarehr = replace(s_rufcarehr, s_rufcarehr == "h", 0))
data <- data %>% mutate(s_rufcarehr = replace(s_rufcarehr, s_rufcarehr == "u", 0))

data <- data %>% 
  mutate(r_rufcarehr = replace(r_rufcarehr, r_rufcarehr == "d" | 
                                 r_rufcarehr == "e" |
                                 r_rufcarehr == "f" |
                                 r_rufcarehr == "q" |
                                 r_rufcarehr == "r" |
                                 r_rufcarehr == "v" , NA))

data <- data %>% 
  mutate(s_rufcarehr = replace(s_rufcarehr, s_rufcarehr == "d" | 
                                 s_rufcarehr == "e" |
                                 s_rufcarehr == "f" |
                                 s_rufcarehr == "q" |
                                 s_rufcarehr == "r" |
                                 s_rufcarehr == "v" , NA))

data$r_rufcarehr <- case_when(data$r_rufcarehr >= 48 ~ 48,TRUE ~ as.numeric(data$r_rufcarehr))
data$s_rufcarehr <- case_when(data$s_rufcarehr >= 48 ~ 48,TRUE ~ as.numeric(data$s_rufcarehr))
data %>% select(contains('rufcarehr')) %>% map(table, exclude = NULL)

# rufcaren : r,s	# unpaid professionals who help r with adls/iadls (>=0, continuous integer)
# NA if .x or .h put 0, no difficulties or not helped
data %>% select(contains('rufcaren')) %>% map(table, exclude = NULL)

data <- data %>% mutate(r_rufcaren = replace(r_rufcaren, r_rufcaren == "x", 0))
data <- data %>% mutate(r_rufcaren = replace(r_rufcaren, r_rufcaren == "h", 0))
data <- data %>% mutate(s_rufcaren = replace(s_rufcaren, s_rufcaren == "x", 0))
data <- data %>% mutate(s_rufcaren = replace(s_rufcaren, s_rufcaren == "h", 0))
data <- data %>% mutate(s_rufcaren = replace(s_rufcaren, s_rufcaren == "u", 0))

data <- data %>% 
  mutate(r_rufcaren = replace(r_rufcaren, r_rufcaren == "d" | 
                                r_rufcaren == "f" |
                                r_rufcaren == "m" |
                                r_rufcaren == "q" |
                                r_rufcaren == "r" |
                                r_rufcaren == "v" , NA))

data <- data %>% 
  mutate(s_rufcaren = replace(s_rufcaren, s_rufcaren == "d" | 
                                s_rufcaren == "f" |
                                s_rufcaren == "m" |
                                s_rufcaren == "q" |
                                s_rufcaren == "r" |
                                s_rufcaren == "v" , NA))

# create adl_hour_fam : r,s, hours/day family members help r with adls/iadls (>=0, continuous integer)
# cap 50+

data$r_rccarehr <- as.numeric(data$r_rccarehr)
data$r_rrcarehr <- as.numeric(data$r_rrcarehr)
data$r_rscarehr <- as.numeric(data$r_rscarehr)
data$s_rccarehr <- as.numeric(data$s_rccarehr)
data$s_rrcarehr <- as.numeric(data$s_rrcarehr)
data$s_rscarehr <- as.numeric(data$s_rscarehr)

data$r_adl_hour_fam <- ifelse(is.na(data$r_rccarehr) & is.na(data$r_rrcarehr) &
                                is.na(data$r_rscarehr), NA,
                              rowSums(data[,c('r_rccarehr', 'r_rrcarehr', 'r_rscarehr')], na.rm = TRUE))

data$r_adl_hour_fam <- case_when(data$r_adl_hour_fam >= 48 ~ 48,TRUE ~ as.numeric(data$r_adl_hour_fam))
data %>% select(contains('r_adl_hour_fam')) %>% map(table, exclude = NULL)

data$s_adl_hour_fam <- ifelse(is.na(data$s_rccarehr) & is.na(data$s_rrcarehr) &
                                is.na(data$s_rscarehr), NA,
                              rowSums(data[,c('s_rccarehr', 's_rrcarehr', 's_rscarehr')], na.rm = TRUE))

data$s_adl_hour_fam <- case_when(data$s_adl_hour_fam >= 48 ~ 48,TRUE ~ as.numeric(data$s_adl_hour_fam))
data %>% select(contains('s_adl_hour_fam')) %>% map(table, exclude = NULL)

# create adl_fam : r, s # family members help r with adls/iadls (>=0, continuous integer)
# cap 10+

data$r_rccaren <- as.numeric(data$r_rccaren)
data$r_rrcaren <- as.numeric(data$r_rrcaren)
data$r_rscaren <- as.numeric(data$r_rscaren)

data$s_rccaren <- as.numeric(data$s_rccaren)
data$s_rrcaren <- as.numeric(data$s_rrcaren)
data$s_rscaren <- as.numeric(data$s_rscaren)

data$r_adl_fam <- ifelse(is.na(data$r_rccaren) & is.na(data$r_rrcaren) &
                           is.na(data$r_rscaren), NA,
                         rowSums(data[,c('r_rccaren', 'r_rrcaren', 'r_rscaren')], na.rm = TRUE))

data %>% select(contains('r_adl_fam')) %>% map(table, exclude = NULL)

data$s_adl_fam <- ifelse(is.na(data$s_rccaren) & is.na(data$s_rrcaren) &
                           is.na(data$s_rscaren), NA,
                         rowSums(data[,c('s_rccaren', 's_rrcaren', 's_rscaren')], na.rm = TRUE))
data %>% select(contains('s_adl_fam')) %>% map(table, exclude = NULL)

# create adl_hour_nf : r,s, hours/day non family members help r with adls/iadls (>=0, continuous integer)
# p 50+ 

data$r_rfcarehr <- as.numeric(data$r_rfcarehr)
data$r_rpfcarehr <- as.numeric(data$r_rpfcarehr)
data$r_rufcarehr <- as.numeric(data$r_rufcarehr)

data$s_rfcarehr <- as.numeric(data$s_rfcarehr)
data$s_rpfcarehr <- as.numeric(data$s_rpfcarehr)
data$s_rufcarehr <- as.numeric(data$s_rufcarehr)

data$r_adl_hour_nf <- ifelse(is.na(data$r_rfcarehr) & is.na(data$r_rpfcarehr) &
                               is.na(data$r_rufcarehr), NA,
                             rowSums(data[,c('r_rfcarehr', 'r_rpfcarehr', 'r_rufcarehr')], na.rm = TRUE))

data$r_adl_hour_nf <- case_when(data$r_adl_hour_nf >= 48 ~ 48,TRUE ~ as.numeric(data$r_adl_hour_nf))
data %>% select(contains('r_adl_hour_nf')) %>% map(table, exclude = NULL)

data$s_adl_hour_nf <- ifelse(is.na(data$s_rfcarehr) & is.na(data$s_rpfcarehr) &
                               is.na(data$s_rufcarehr), NA,
                             rowSums(data[,c('s_rfcarehr', 's_rpfcarehr', 's_rufcarehr')], na.rm = TRUE))

data$s_adl_hour_nf <- case_when(data$s_adl_hour_nf >= 48 ~ 48,TRUE ~ as.numeric(data$s_adl_hour_nf))
data %>% select(contains('s_adl_hour_nf')) %>% map(table, exclude = NULL)

# create adl_nf : r, s # non family members help r with adls/iadls (>=0, continuous integer)
data$r_rfcaren <- as.numeric(data$r_rfcaren)
data$r_rpfcaren <- as.numeric(data$r_rpfcaren)
data$r_rufcaren <- as.numeric(data$r_rufcaren)

data$s_rfcaren <- as.numeric(data$s_rfcaren)
data$s_rpfcaren <- as.numeric(data$s_rpfcaren)
data$s_rufcaren <- as.numeric(data$s_rufcaren)

data$r_adl_nf <- ifelse(is.na(data$r_rfcaren) & is.na(data$r_rpfcaren) &
                          is.na(data$r_rufcaren), NA,
                        rowSums(data[,c('r_rfcaren', 'r_rpfcaren', 'r_rufcaren')], na.rm = TRUE))
data %>% select(contains('r_adl_nf')) %>% map(table, exclude = NULL)

data$s_adl_nf <- ifelse(is.na(data$s_rfcaren) & is.na(data$s_rpfcaren) &
                          is.na(data$s_rufcaren), NA,
                        rowSums(data[,c('s_rfcaren', 's_rpfcaren', 's_rufcaren')], na.rm = TRUE))
data %>% select(contains('s_adl_nf')) %>% map(table, exclude = NULL)


# GROUP: WELFARE

# lifeinv :	r,s	value life insurance policies (>= 0, continuous)
# cap at 2m
data %>% select(contains('lifeinv')) %>% map(table, exclude = NULL)

# nlfins : r,s number life insurance policies (>= 0, continuous integer)
data %>% select(contains('nlfins')) %>% map(table, exclude = NULL)

# witwill	: r,s has a witnessed will (0,1)
data %>% select(contains('witwill')) %>% map(table, exclude = NULL)

# wlifein	: r,s has whole life insurance (0,1)
data %>% select(contains('wlifein')) %>% map(table, exclude = NULL)

# wtrust : r,s has a trust (0,1)
data %>% select(contains('wtrust')) %>% map(table, exclude = NULL)

# create lifeins_fam: r,s whether a family member is beneficiary of life ins (0,1)
data <- data %>% mutate(r_lfinscg = replace(r_lfinscg, r_lfinscg != "0" & 
                                              r_lfinscg != "1", NA))

data <- data %>% mutate(r_lfinsrl = replace(r_lfinsrl, r_lfinsrl != "0" & 
                                              r_lfinsrl != "1", NA))

data <- data %>% mutate(r_lfinssp = replace(r_lfinssp, r_lfinssp != "0" & 
                                              r_lfinssp != "1", NA))

data$r_lfinscg <- as.numeric(data$r_lfinscg)
data$r_lfinsrl <- as.numeric(data$r_lfinsrl)
data$r_lfinssp <- as.numeric(data$r_lfinssp)

data <- data %>% mutate(s_lfinscg = replace(s_lfinscg, s_lfinscg != "0" & 
                                              s_lfinscg != "1", NA))

data <- data %>% mutate(s_lfinsrl = replace(s_lfinsrl, s_lfinsrl != "0" & 
                                              s_lfinsrl != "1", NA))

data <- data %>% mutate(s_lfinssp = replace(s_lfinssp, s_lfinssp != "0" & 
                                              s_lfinssp != "1", NA))

data$s_lfinscg <- as.numeric(data$s_lfinscg)
data$s_lfinsrl <- as.numeric(data$s_lfinsrl)
data$s_lfinssp <- as.numeric(data$s_lfinssp)

data <- data %>% mutate(r_lifeins_fam = ifelse(rowSums(data[, c("r_lfinscg", "r_lfinsrl", "r_lfinssp")], 
                                                       na.rm=TRUE) >=1 , 1, 0))
data %>% select(contains('r_lifeins_fam')) %>% map(table, exclude = NULL)

data <- data %>% mutate(s_lifeins_fam = ifelse(rowSums(data[, c("s_lfinscg", "s_lfinsrl", "s_lfinssp")], 
                                                       na.rm=TRUE) >=1 , 1, 0))
data %>% select(contains('s_lifeins_fam')) %>% map(table, exclude = NULL)

# create will_fam: r,s whether has provisions for a family member (0,1)
data <- data %>% mutate(r_willch = replace(r_willch, r_willch != "0" & 
                                             r_willch != "1" & 
                                             r_willch != "u", NA)) %>% 
  mutate(r_willch = replace(r_willch, r_willch == "u", 0))

data <- data %>% mutate(r_willfm = replace(r_willfm, r_willfm != "0" & 
                                             r_willfm != "1", NA))

data <- data %>% mutate(r_willgk = replace(r_willgk, r_willgk != "0" & 
                                             r_willgk != "1", NA))

data$r_willch <- as.numeric(data$r_willch)
data$r_willfm <- as.numeric(data$r_willfm)
data$r_willgk <- as.numeric(data$r_willgk)

data <- data %>% mutate(s_willch = replace(s_willch, s_willch != "0" & 
                                             s_willch != "1" & 
                                             s_willch != "u", NA)) %>% 
  mutate(s_willch = replace(s_willch, s_willch == "u", 0)) 

data <- data %>% mutate(s_willfm = replace(s_willfm, s_willfm != "0" & 
                                             s_willfm != "1" & 
                                             s_willfm != "u", NA)) %>% 
  mutate(s_willfm = replace(s_willfm, s_willfm == "u", 0))


data <- data %>% mutate(s_willgk = replace(s_willgk, s_willgk != "0" & 
                                             s_willgk != "1" & 
                                             s_willgk != "u", NA)) %>% 
    mutate(s_willgk = replace(s_willgk, s_willgk == "u", 0))

data$s_willch <- as.numeric(data$s_willch)
data$s_willfm <- as.numeric(data$s_willfm)
data$s_willgk <- as.numeric(data$s_willgk)

data <- data %>% mutate(r_will_fam = ifelse((r_willch + r_willfm + r_willgk) >= 1, 1, 0))
data %>% select(contains('r_will_fam')) %>% map(table, exclude = NULL)

data <- data %>% mutate(s_will_fam = ifelse((s_willch + s_willfm + s_willgk) >= 1, 1, 0))
data %>% select(contains('s_will_fam')) %>% map(table, exclude = NULL)




# Group: VARIABLES FROM LONGITUDINAL DATA

# byear Birth year
data %>% select(contains('byear')) %>% map(table, exclude = NULL)

data <- data %>% mutate(r_byear = replace(r_byear, r_byear == "m", NA))

data <- data %>% mutate(s_byear = replace(s_byear, s_byear == "v", NA)) %>% 
  mutate(s_byear = replace(s_byear, s_byear == "u", -1))

# create bcohort Cohort of birth 
# <1924 (oldest cohorts, entered in the sample at 70 or older), 
# 1924 - 1930 (children of the great depression), 
# 1931 - 1947 (Original HRS + War babies, childhood during WWII),
# >=1948 (Boomers)
data <- data %>% 
  mutate(r_bcohort = case_when(r_byear < 1924 ~ 1,
                            r_byear < 1931 ~ 2, 
                            r_byear < 1948 ~ 3, 
                            r_byear >= 1948 ~ 4))

data <- data %>% 
  mutate(s_bcohort = case_when(s_byear == -1 ~ -1,
                               s_byear < 1924 ~ 1,
                               s_byear < 1931 ~ 2,
                               s_byear < 1948 ~ 3, 
                               s_byear >= 1948 ~ 4))

data %>% select(contains('bcohort')) %>% map(table, exclude = NULL)

# bmonth Birth month
data %>% select(contains('bmonth')) %>% map(table, exclude = NULL)

# dyear Death year
data %>% select(contains('dyear')) %>% map(table, exclude = NULL)

# dmonth Death month
data %>% select(contains('dmonth')) %>% map(table, exclude = NULL)

# agem_b Age in month at interview
data %>% select(contains('agem_b')) %>% map(table, exclude = NULL)

# agey_b age in years at interview
data %>% select(contains('_agey_b')) %>% map(table, exclude = NULL)

# gender
data %>% select(contains('gender')) %>% map(table, exclude = NULL)

data <- data %>% mutate(r_gender = replace(r_gender, r_gender == "m", NA))

data <- data %>% mutate(s_gender = replace(s_gender, s_gender == "m" | 
                                             s_gender == "v", NA))

data <- data %>% mutate(s_gender = replace(s_gender, s_gender == "u" , 0))

data$r_gender <- as.numeric(data$r_gender) - 1
data$s_gender <- as.numeric(data$s_gender) - 1

# racem
data %>% select(contains('racem')) %>% map(table, exclude = NULL)

data <- data %>% 
  mutate(r_racem = replace(r_racem, r_racem == "m" , NA))

data <- data %>% 
  mutate(s_racem = replace(s_racem, s_racem == "m"|
                             s_racem == "v", NA))

data <- data %>% 
  mutate(s_racem = replace(s_racem, s_racem == "u", -1))

data$r_racem <- as.numeric(data$r_racem)
data$s_racem <- as.numeric(data$s_racem)

# hispan
data %>% select(contains('hispan')) %>% map(table, exclude = NULL)

data <- data %>% 
  mutate(r_hispan = replace(r_hispan, r_hispan == "m" , NA))

data <- data %>% 
  mutate(s_hispan = replace(s_hispan, s_hispan == "m"|
                                      s_hispan == "v" , NA))

data <- data %>% 
  mutate(s_hispan = replace(s_hispan, s_hispan == "u", -1))

data$r_hispan <- as.numeric(data$r_hispan)
data$s_hispan <- as.numeric(data$s_hispan)

# create r_race: non-hispanic white, non-hispanic black, hispanic, other
data <- data %>% 
  mutate(r_race = case_when((r_racem == 1 & r_hispan == 0) ~ 1,
                            (r_racem == 2 & r_hispan == 0) ~ 2, 
                            (r_hispan == 1) ~ 3))

data %>% select(contains('r_race')) %>% map(table, exclude = NULL)

data <- data %>% 
  mutate(s_race = case_when((s_racem == 1 & s_hispan == 0) ~ 1,
                            (s_racem == 2 & s_hispan == 0) ~ 2, 
                            (s_hispan == 1) ~ 3,
                            (s_racem == -1 | s_hispan == -1) ~ -1))

data %>% select(contains('s_race')) %>% map(table, exclude = NULL)

# edyrs Years of education
data %>% select(contains('edyrs')) %>% map(table, exclude = NULL)

# edegrm Education, highest degree
data %>% select(contains('edegrm')) %>% map(table, exclude = NULL)

# meduc Mother's Years of education 
data %>% select(contains('meduc')) %>% map(table, exclude = NULL)

# feduc Father's years of education 
data %>% select(contains('feduc')) %>% map(table, exclude = NULL)

# mstat Marital Status
data %>% select(contains('mstat')) %>% map(table, exclude = NULL)

# mrct Number of marriages 
data %>% select(contains('mrct')) %>% map(table, exclude = NULL)

# create r_evermrg: ever married 

data$r_mrct <- as.numeric(data$r_mrct)
data <- data %>% mutate(r_evermrg = case_when(r_mrct == 0 ~ 0,
                                              r_mrct > 0 ~ 1, 
                                              TRUE ~ r_mrct))

data %>% select(contains('r_evermrg')) %>% map(table, exclude = NULL)


data <- data %>% 
  mutate(s_mrct = replace(s_mrct, s_mrct == "u" , 0))

data$s_mrct <- as.numeric(data$s_mrct)
data <- data %>% mutate(s_evermrg = case_when(s_mrct == 0 ~ 0,
                                              s_mrct > 0 ~ 1, 
                                              T ~ s_mrct))

data %>% select(contains('s_evermrg')) %>% map(table, exclude = NULL)

# mdiv Number of divorces
data %>% select(contains('mdiv')) %>% map(table, exclude = NULL)

# create r_everdiv: ever divorced 
data$r_mdiv <- as.numeric(data$r_mdiv)
data <- data %>% mutate(r_everdiv = case_when(r_mdiv == 0 ~ 0,
                                              r_mdiv > 0 ~ 1, 
                                              TRUE ~ r_mdiv))

data %>% select(contains('r_everdiv')) %>% map(table, exclude = NULL)


data <- data %>% 
  mutate(s_mdiv = replace(s_mdiv, s_mdiv == "u" , 0))

data$s_mdiv <- as.numeric(data$s_mdiv)
data <- data %>% mutate(s_everdiv = case_when(s_mdiv == 0 ~ 0,
                                              s_mdiv > 0 ~ 1, 
                                              T ~ s_mdiv))

data %>% select(contains('s_everdiv')) %>% map(table, exclude = NULL)

# mwid Number of times widowed
data %>% select(contains('mwid')) %>% map(table, exclude = NULL)

# create r_everwid: ever widowed 
data$r_mwid <- as.numeric(data$r_mwid)
data <- data %>% mutate(r_everwid = case_when(r_mwid == 0 ~ 0,
                                              r_mwid > 0 ~ 1, 
                                              TRUE ~ r_mwid))

data %>% select(contains('r_everwid')) %>% map(table, exclude = NULL)


data <- data %>% 
  mutate(s_mwid = replace(s_mwid, s_mwid == "u" , 0))

data$s_mwid <- as.numeric(data$s_mwid)
data <- data %>% mutate(s_everwid = case_when(s_mwid == 0 ~ 0,
                                              s_mwid > 0 ~ 1, 
                                              T ~ s_mwid))

data %>% select(contains('s_everwid')) %>% map(table, exclude = NULL)

# relig Religion
data %>% select(contains('relig')) %>% map(table, exclude = NULL)

# vetrn Veteran Status
data %>% select(contains('vetrn')) %>% map(table, exclude = NULL)

# bplace Place of Birth (Cens Region)
data %>% select(contains('bplace')) %>% map(table, exclude = NULL)

# momliv Mother alive
data %>% select(contains('momliv')) %>% map(table, exclude = NULL)

# dadliv Father alive
data %>% select(contains('dadliv')) %>% map(table, exclude = NULL)

# momage Mother age current/at death
data %>% select(contains('momage')) %>% map(table, exclude = NULL)

# dadage Father age current/at death
data %>% select(contains('dadage')) %>% map(table, exclude = NULL)

# hhres Number of people in HH
data %>% select(contains('hhres')) %>% map(table, exclude = NULL)

# child Number of living children
data %>% select(contains('child')) %>% map(table, exclude = NULL)

# evbrn Number of Children Ever Born
data %>% select(contains('evbrn')) %>% map(table, exclude = NULL)

# smokev Ever smoked cigarettes
data %>% select(contains('smokev')) %>% map(table, exclude = NULL)


# GROUP: JOB

# csize:	r,s	r size of total company/org (0 - 1.000.000, continuous)
# change self-employed to 0
data %>% select(matches(c('r_csize', 's_csize'))) %>% map(table, exclude = NULL )

data <- data %>% mutate(r_csize = replace(r_csize, r_csize == "s", 0))

data <- data %>% mutate(s_csize = replace(s_csize, s_csize == "s", 0))
data <- data %>% mutate(s_csize = replace(s_csize, s_csize == "u", 0))

data <- data %>% 
  mutate(r_csize = replace(r_csize, r_csize == "d" | 
                             r_csize == "e" |
                             r_csize == "m" |
                             r_csize == "n" |
                             r_csize == "o" |
                             r_csize == "q" |
                             r_csize == "r" |
                             r_csize == "t" |
                             r_csize == "w" , NA))

data <- data %>% 
  mutate(s_csize = replace(s_csize, s_csize == "d" | 
                             s_csize == "e" |
                             s_csize == "m" |
                             s_csize == "n" |
                             s_csize == "o" |
                             s_csize == "q" |
                             s_csize == "r" |
                             s_csize == "t" |
                             s_csize == "v" |
                             s_csize == "w" , NA))

data$r_csize <- as.numeric(data$r_csize)
data$s_csize <- as.numeric(data$s_csize)

data$r_csize <- case_when(data$r_csize >= 1000000 ~ 1000000,TRUE ~ as.numeric(data$r_csize))
data$s_csize <- case_when(data$s_csize >= 1000000 ~ 1000000,TRUE ~ as.numeric(data$s_csize))


# jdealpplb:	r,s	r freq cur job dealing with people  (1-4)
# change work few hrs/yr to 4
data %>% select(contains('jdealpplb')) %>% map(table, exclude = NULL)

data <- data %>% mutate(r_jdealpplb = replace(r_jdealpplb, r_jdealpplb == "f", 4))

data <- data %>% mutate(s_jdealpplb = replace(s_jdealpplb, s_jdealpplb == "f", 4))


# jdiffa:	r,s	r cur job more difficult than used to be (1-4)
data %>% select(contains('jdiffa')) %>% map(table, exclude = NULL)


# jenjwrka:	r,s	r really enjoys going to work (1-4)
data %>% select(contains('jenjwrka')) %>% map(table, exclude = NULL)


# jobsum	r,s	"r job stress summary mean score (includes SATJOB, DEMAND, SALARY, 
# SECURE, TIMEPRE and NSKILLS)" (1-4, continuous) OK
data %>% select(c("r_jobsum","s_jobsum")) %>% map(table, exclude = NULL)


# jpdysa:	r,s	r cur job paid days off (0-440, continuous)
# change self-employed to 0
data %>% select(contains('jpdysa')) %>% map(table, exclude = NULL)

data <- data %>% mutate(r_jpdysa = replace(r_jpdysa, r_jpdysa == "s", 0))

data <- data %>% mutate(s_jpdysa = replace(s_jpdysa, s_jpdysa == "s", 0))


# jrsleft:	r,s	r reason for stopping working (if not working) (1-8)
data %>% select(contains('jrsleft')) %>% map(table, exclude = NULL)


# jsprvsn:	r,s	number people r supervises (0-1.000, continuous)
# change self-employed to 0
data %>% select(contains('jsprvsn')) %>% map(table, exclude = NULL)

data <- data %>% mutate(r_jsprvsn = replace(r_jsprvsn, r_jsprvsn == "s", 0))

data <- data %>% mutate(s_jsprvsn = replace(s_jsprvsn, s_jsprvsn == "s", 0))
data <- data %>% mutate(s_jsprvsn = replace(s_jsprvsn, s_jsprvsn == "u", 0))

data <- data %>% 
  mutate(r_jsprvsn = replace(r_jsprvsn, r_jsprvsn == "d" | 
                               r_jsprvsn == "f" |
                               r_jsprvsn == "h" |
                               r_jsprvsn == "m" |
                               r_jsprvsn == "p" |
                               r_jsprvsn == "q" |
                               r_jsprvsn == "r" |
                               r_jsprvsn == "w" |
                               r_jsprvsn == "z" , NA))

data <- data %>% 
  mutate(s_jsprvsn = replace(s_jsprvsn, s_jsprvsn == "d" | 
                               s_jsprvsn == "f" |
                               s_jsprvsn == "h" |
                               s_jsprvsn == "m" |
                               s_jsprvsn == "p" |
                               s_jsprvsn == "q" |
                               s_jsprvsn == "r" |
                               s_jsprvsn == "v" |
                               s_jsprvsn == "w" |
                               s_jsprvsn == "z" , NA))

data$r_jsprvsn <- as.numeric(data$r_jsprvsn)
data$s_jsprvsn <- as.numeric(data$s_jsprvsn)


data$r_jsprvsn <- case_when(data$r_jsprvsn >= 1000 ~ 1000,TRUE ~ as.numeric(data$r_jsprvsn))
data$s_jsprvsn <- case_when(data$s_jsprvsn >= 1000 ~ 1000,TRUE ~ as.numeric(data$s_jsprvsn))

# lookwrkpf:	r,s	r look part or full-time job (if not working) (1-4)
# new category "4. not looking for work"
data %>% select(contains('lookwrkpf')) %>% map(table, exclude = NULL )

data <- data %>% mutate(r_lookwrkpf = replace(r_lookwrkpf, r_lookwrkpf == "l", 4))
data <- data %>% mutate(r_lookwrkpf = replace(r_lookwrkpf, r_lookwrkpf == "n", 4))
data <- data %>% mutate(r_lookwrkpf = replace(r_lookwrkpf, r_lookwrkpf == "t", 4))

data <- data %>% mutate(s_lookwrkpf = replace(s_lookwrkpf, s_lookwrkpf == "l", 4))
data <- data %>% mutate(s_lookwrkpf = replace(s_lookwrkpf, s_lookwrkpf == "n", 4))
data <- data %>% mutate(s_lookwrkpf = replace(s_lookwrkpf, s_lookwrkpf == "t", 4))


# satjob:	r,s	whether r is satisfied with their job (1-4)
data %>% select(contains('satjob')) %>% map(table, exclude = NULL )


# SUMMARY VARIABLES

# work_discrim - job discrimination score

# closly:	r,s	how often r watched more closely than others  (1-6 worst)
data %>% select(contains('closly')) %>% map(table, exclude = NULL )

data <- data %>% 
  mutate(r_closly = replace(r_closly, r_closly == "c" | 
                              r_closly == "e" |
                              r_closly == "m" |
                              r_closly == "w" , NA))

data <- data %>% 
  mutate(s_closly = replace(s_closly, s_closly == "c" | 
                              s_closly == "e" |
                              s_closly == "m" |
                              s_closly == "v" |
                              s_closly == "w" , NA)) %>% 
  mutate(s_closly = replace(s_closly, s_closly == "u", 0))

data$r_closly <- as.numeric(data$r_closly)
data$s_closly <- as.numeric(data$s_closly)

# deserve:	r,s	whether r received deserved recognition from work (1 worst -4)
data %>% select(contains('deserve')) %>% map(table, exclude = NULL )

data <- data %>% 
  mutate(r_deserve = replace(r_deserve, r_deserve == "a" |
                               r_deserve == "c" |
                               r_deserve == "e" |
                               r_deserve == "m" |
                               r_deserve == "w" , NA))

data <- data %>% 
  mutate(s_deserve = replace(s_deserve, s_deserve == "a" |
                               s_deserve == "c" |
                               s_deserve == "e" |
                               s_deserve == "m" |
                               s_deserve == "v" |
                               s_deserve == "w" , NA)) %>% 
  mutate(s_deserve = replace(s_deserve, s_deserve == "u", 0))

data$r_deserve <- as.numeric(data$r_deserve)
data$s_deserve <- as.numeric(data$s_deserve)

# jkrace:	r,s	how often r bothered by supervisor/coworkers racial jokes (1-6 worst)
data %>% select(contains('jkrace')) %>% map(table, exclude = NULL )

data <- data %>% 
  mutate(r_jkrace = replace(r_jkrace, r_jkrace == "c" | 
                              r_jkrace == "e" |
                              r_jkrace == "m" |
                              r_jkrace == "w" , NA))

data <- data %>% 
  mutate(s_jkrace = replace(s_jkrace, s_jkrace == "c" | 
                              s_jkrace == "e" |
                              s_jkrace == "m" |
                              s_jkrace == "v" |
                              s_jkrace == "w" , NA)) %>% 
  mutate(s_jkrace = replace(s_jkrace, s_jkrace == "u", 0))

data$r_jkrace <- as.numeric(data$r_jkrace)
data$s_jkrace <- as.numeric(data$s_jkrace)

# jpromynga:	r,s	r cur job employer promotes young over old (1-4 worst)
data %>% select(contains('jpromynga')) %>% map(table, exclude = NULL )

data <- data %>% 
  mutate(r_jpromynga = replace(r_jpromynga, r_jpromynga == "d" | 
                                 r_jpromynga == "f" |
                                 r_jpromynga == "h" |
                                 r_jpromynga == "m" |
                                 r_jpromynga == "n" |
                                 r_jpromynga == "p" |
                                 r_jpromynga == "q" |
                                 r_jpromynga == "r" |
                                 r_jpromynga == "s" |
                                 r_jpromynga == "w" , NA)) 

data <- data %>% 
  mutate(s_jpromynga = replace(s_jpromynga, s_jpromynga == "d" | 
                                 s_jpromynga == "f" |
                                 s_jpromynga == "h" |
                                 s_jpromynga == "m" |
                                 s_jpromynga == "n" |
                                 s_jpromynga == "p" |
                                 s_jpromynga == "q" |
                                 s_jpromynga == "r" |
                                 s_jpromynga == "s" |
                                 s_jpromynga == "v" |
                                 s_jpromynga == "w" , NA)) %>% 
  mutate(s_jpromynga = replace(s_jpromynga, s_jpromynga == "u", 0))


data$r_jpromynga <- as.numeric(data$r_jpromynga)
data$s_jpromynga <- as.numeric(data$s_jpromynga)

# ntboss:	r,s	how often r ignored by boss (1-6 worst)
data %>% select(contains('ntboss')) %>% map(table, exclude = NULL )

data <- data %>% 
  mutate(r_ntboss = replace(r_ntboss, r_ntboss == "c" | 
                              r_ntboss == "e" |
                              r_ntboss == "m" |
                              r_ntboss == "w" , NA))

data <- data %>% 
  mutate(s_ntboss = replace(s_ntboss, s_ntboss == "c" | 
                              s_ntboss == "e" |
                              s_ntboss == "m" |
                              s_ntboss == "v" |
                              s_ntboss == "w" , NA)) %>% 
  mutate(s_ntboss = replace(s_ntboss, s_ntboss == "u", 0))

data$r_ntboss <- as.numeric(data$r_ntboss)
data$s_ntboss <- as.numeric(data$s_ntboss)

# uftask:	r,s	how often r unfairly given tasks at work (1-6 worst)
data %>% select(contains('uftask')) %>% map(table, exclude = NULL )

data <- data %>% 
  mutate(r_uftask = replace(r_uftask, r_uftask == "c" | 
                              r_uftask == "e" |
                              r_uftask == "m" |
                              r_uftask == "w" , NA))

data <- data %>% 
  mutate(s_uftask = replace(s_uftask, s_uftask == "c" | 
                              s_uftask == "e" |
                              s_uftask == "m" |
                              s_uftask == "v" |
                              s_uftask == "w" , NA)) %>% 
  mutate(s_uftask = replace(s_uftask, s_uftask == "u" , 0))

data$r_uftask <- as.numeric(data$r_uftask)
data$s_uftask <- as.numeric(data$s_uftask)

# upset:	r,s	whether r feels bothered or upset in their work (1-4 worst)
data %>% select(contains('upset')) %>% map(table, exclude = NULL )

data <- data %>% 
  mutate(r_upset = replace(r_upset, r_upset == "a" |
                             r_upset == "c" |
                             r_upset == "e" |
                             r_upset == "m" |
                             r_upset == "w" , NA))

data <- data %>% 
  mutate(s_upset = replace(s_upset, s_upset == "a" |
                             s_upset == "c" |
                             s_upset == "e" |
                             s_upset == "m" |
                             s_upset == "v" |
                             s_upset == "w" , NA)) %>% 
  mutate(s_upset = replace(s_upset, s_upset == "u", NA))

data$r_upset <- as.numeric(data$r_upset)
data$s_upset <- as.numeric(data$s_upset)

# whuml:	r,s	how often r unfairly humiliated in front of others at work (1-6 worst)
data %>% select(contains('whuml')) %>% map(table, exclude = NULL )

data <- data %>% 
  mutate(r_whuml = replace(r_whuml, r_whuml == "c" | 
                             r_whuml == "e" |
                             r_whuml == "m" |
                             r_whuml == "w" , NA))

data <- data %>% 
  mutate(s_whuml = replace(s_whuml, s_whuml == "c" | 
                             s_whuml == "e" |
                             s_whuml == "m" |
                             s_whuml == "v" |
                             s_whuml == "w" , NA)) %>% 
  mutate(s_whuml = replace(s_whuml, s_whuml == "u", 0))

data$r_whuml <- as.numeric(data$r_whuml)
data$s_whuml <- as.numeric(data$s_whuml)

# wtwice:	r,s	how often r feels has to work twice as hard as other (1-6 worst)
data %>% select(contains('wtwice')) %>% map(table, exclude = NULL )

data <- data %>% 
  mutate(r_wtwice = replace(r_wtwice, r_wtwice == "c" | 
                              r_wtwice == "e" |
                              r_wtwice == "m" |
                              r_wtwice == "w" , NA))

data <- data %>% 
  mutate(s_wtwice = replace(s_wtwice, s_wtwice == "c" | 
                              s_wtwice == "e" |
                              s_wtwice == "m" |
                              s_wtwice == "v" |
                              s_wtwice == "w" , NA)) %>% 
  mutate(s_wtwice = replace(s_wtwice, s_wtwice == "u" , 0))

data$r_wtwice <- as.numeric(data$r_wtwice)
data$s_wtwice <- as.numeric(data$s_wtwice)


# combination
data <- data %>% mutate(r_wdiscrim = (r_closly/6)*4 - r_deserve + (r_jkrace/6)*4 + r_jpromynga 
                        + (r_ntboss/6)*4 + (r_uftask/6)*4 + r_upset + (r_whuml/6)*4 + (r_wtwice/6)*4)
data <- data %>% mutate(s_wdiscrim = (s_closly/6)*4 - s_deserve + (s_jkrace/6)*4 + s_jpromynga 
                        + (s_ntboss/6)*4 + (s_uftask/6)*4 + s_upset + (s_whuml/6)*4 + (s_wtwice/6)*4)

data %>% select(contains('wdiscrim')) %>% map(table, exclude = NULL)



# jobsum2 - job stress summary score 


# demand:	r,s	whether r's job is physically demanding (1-4 worst)
data %>% select(matches(c('r_demand','s_demand'))) %>% map(table, exclude = NULL)

data <- data %>% 
  mutate(r_demand = replace(r_demand, r_demand == "a" |
                              r_demand == "c" |
                              r_demand == "e" |
                              r_demand == "m" |
                              r_demand == "w" , NA))

data <- data %>% 
  mutate(s_demand = replace(s_demand, s_demand == "a" |
                              s_demand == "c" |
                              s_demand == "e" |
                              s_demand == "m" |
                              s_demand == "v" |
                              s_demand == "w" , NA)) %>% 
  mutate(s_demand = replace(s_demand, s_demand == "u", 0))

data$r_demand <- as.numeric(data$r_demand)
data$s_demand <- as.numeric(data$s_demand)

# interf:	r,s	whether r's job interferes with personal life (1-4 worst)
data %>% select(contains('interf')) %>% map(table, exclude = NULL )

data <- data %>% 
  mutate(r_interf = replace(r_interf, r_interf == "a" |
                              r_interf == "c" |
                              r_interf == "e" |
                              r_interf == "m" |
                              r_interf == "w" , NA))

data <- data %>% 
  mutate(s_interf = replace(s_interf, s_interf == "a" |
                              s_interf == "c" |
                              s_interf == "e" |
                              s_interf == "m" |
                              s_interf == "v" |
                              s_interf == "w" , NA)) %>% 
  mutate(s_interf = replace(s_interf, s_interf == "u", 0))

data$r_interf <- as.numeric(data$r_interf)
data$s_interf <- as.numeric(data$s_interf)

# jdiffa:	r,s	r cur job more difficult than used to be (1 worst -4)
data %>% select(contains('jdiffa')) %>% map(table, exclude = NULL )

data <- data %>% 
  mutate(r_jdiffa = replace(r_jdiffa, r_jdiffa == "d" | 
                              r_jdiffa == "f" |
                              r_jdiffa == "h" |
                              r_jdiffa == "m" |
                              r_jdiffa == "n" |
                              r_jdiffa == "p" |
                              r_jdiffa == "q" |
                              r_jdiffa == "r" |
                              r_jdiffa == "w" , NA))

data <- data %>% 
  mutate(s_jdiffa = replace(s_jdiffa, s_jdiffa == "d" | 
                              s_jdiffa == "f" |
                              s_jdiffa == "h" |
                              s_jdiffa == "m" |
                              s_jdiffa == "n" |
                              s_jdiffa == "p" |
                              s_jdiffa == "q" |
                              s_jdiffa == "r" |
                              s_jdiffa == "v" |
                              s_jdiffa == "w" , NA)) %>% 
  mutate(s_jdiffa = replace(s_jdiffa, s_jdiffa == "u", 0))

data$r_jdiffa <- as.numeric(data$r_jdiffa)
data$s_jdiffa <- as.numeric(data$s_jdiffa)

# jmldema:	r,s	r cur job allows move to less demanding job if wanted (1-4 worst)
data %>% select(contains('jmldema')) %>% map(table, exclude = NULL)

data <- data %>% 
  mutate(r_jmldema = replace(r_jmldema, r_jmldema == "d" | 
                               r_jmldema == "f" |
                               r_jmldema == "h" |
                               r_jmldema == "m" |
                               r_jmldema == "n" |
                               r_jmldema == "p" |
                               r_jmldema == "q" |
                               r_jmldema == "r" |
                               r_jmldema == "s" |
                               r_jmldema == "w" , NA))

data <- data %>% 
  mutate(s_jmldema = replace(s_jmldema, s_jmldema == "d" | 
                               s_jmldema == "f" |
                               s_jmldema == "h" |
                               s_jmldema == "m" |
                               s_jmldema == "n" |
                               s_jmldema == "p" |
                               s_jmldema == "q" |
                               s_jmldema == "r" |
                               s_jmldema == "s" |
                               s_jmldema == "v" |
                               s_jmldema == "w" , NA)) %>% 
  mutate(s_jmldema = replace(s_jmldema, s_jmldema == "u", 0))

data$r_jmldema <- as.numeric(data$r_jmldema)
data$s_jmldema <- as.numeric(data$s_jmldema)

# jobsum	r,s	"r job stress summary mean score (continuous 1-4 worst)
data %>% select(c('r_jobsum','s_jobsum')) %>% map(table, exclude = NULL )

data <- data %>% 
  mutate(r_jobsum = replace(r_jobsum, r_jobsum == "c" |
                              r_jobsum == "m" |
                              r_jobsum == "w" , NA))

data <- data %>% 
  mutate(s_jobsum = replace(s_jobsum, s_jobsum == "c" |
                              s_jobsum == "m" |
                              s_jobsum == "v" |
                              s_jobsum == "w" , NA)) %>% 
  mutate(s_jobsum = replace(s_jobsum, s_jobsum == "u", 0))

data$r_jobsum <- as.numeric(data$r_jobsum)
data$s_jobsum <- as.numeric(data$s_jobsum)

# ltlfree:	r,s	whether r has very little freedom to do work (1-4 worst)
data %>% select(contains('ltlfree')) %>% map(table, exclude = NULL )

data <- data %>% 
  mutate(r_ltlfree = replace(r_ltlfree, r_ltlfree == "a" |
                               r_ltlfree == "c" |
                               r_ltlfree == "e" |
                               r_ltlfree == "m" |
                               r_ltlfree == "w" , NA))

data <- data %>% 
  mutate(s_ltlfree = replace(s_ltlfree, s_ltlfree == "a" |
                               s_ltlfree == "c" |
                               s_ltlfree == "e" |
                               s_ltlfree == "m" |
                               s_ltlfree == "v" |
                               s_ltlfree == "w" , NA)) %>% 
  mutate(s_ltlfree = replace(s_ltlfree, s_ltlfree == "u", 0))


data$r_ltlfree <- as.numeric(data$r_ltlfree)
data$s_ltlfree <- as.numeric(data$s_ltlfree)

# nskills:	r,s	whether r has the opportunity to develop new skills (1 worst -4)
data %>% select(contains('nskills')) %>% map(table, exclude = NULL )

data <- data %>% 
  mutate(r_nskills = replace(r_nskills, r_nskills == "a" |
                               r_nskills == "c" |
                               r_nskills == "e" |
                               r_nskills == "m" |
                               r_nskills == "w" , NA))

data <- data %>% 
  mutate(s_nskills = replace(s_nskills, s_nskills == "a" |
                               s_nskills == "c" |
                               s_nskills == "e" |
                               s_nskills == "m" |
                               s_nskills == "v" |
                               s_nskills == "w" , NA)) %>% 
  mutate(s_nskills = replace(s_nskills, s_nskills == "u", 0))

data$r_nskills <- as.numeric(data$r_nskills)
data$s_nskills <- as.numeric(data$s_nskills)

# promot:	r,s	whether r's job promotion prospects are poor (1-4 worst)
data %>% select(contains('promot')) %>% map(table, exclude = NULL )

data <- data %>% 
  mutate(r_promot = replace(r_promot, r_promot == "a" |
                              r_promot == "c" |
                              r_promot == "e" |
                              r_promot == "m" |
                              r_promot == "w" , NA))

data <- data %>% 
  mutate(s_promot = replace(s_promot, s_promot == "a" |
                              s_promot == "c" |
                              s_promot == "e" |
                              s_promot == "m" |
                              s_promot == "v" |
                              s_promot == "w" , NA)) %>% 
  mutate(s_promot = replace(s_promot, s_promot == "u", 0))

data$r_promot <- as.numeric(data$r_promot)
data$s_promot <- as.numeric(data$s_promot)

# salary:	r,s	whether r's salary is adequate (1 worst -4)
data %>% select(contains('salary')) %>% map(table, exclude = NULL )

data <- data %>% 
  mutate(r_salary = replace(r_salary, r_salary == "a" |
                              r_salary == "c" |
                              r_salary == "e" |
                              r_salary == "m" |
                              r_salary == "w" , NA))

data <- data %>% 
  mutate(s_salary = replace(s_salary, s_salary == "a" |
                              s_salary == "c" |
                              s_salary == "e" |
                              s_salary == "m" |
                              s_salary == "v" |
                              s_salary == "w" , NA)) %>% 
  mutate(s_salary = replace(s_salary, s_salary == "u", NA))


data$r_salary <- as.numeric(data$r_salary)
data$s_salary <- as.numeric(data$s_salary)

# secure:	r,s	whether r's job security is poor (1-4 worst)
data %>% select(contains('secure')) %>% map(table, exclude = NULL )

data <- data %>% 
  mutate(r_secure = replace(r_secure, r_secure == "a" |
                              r_secure == "c" |
                              r_secure == "e" |
                              r_secure == "m" |
                              r_secure == "w" , NA))

data <- data %>% 
  mutate(s_secure = replace(s_secure, s_secure == "a" |
                              s_secure == "c" |
                              s_secure == "e" |
                              s_secure == "m" |
                              s_secure == "v" |
                              s_secure == "w" , NA)) %>% 
  mutate(s_secure = replace(s_secure, s_secure == "u", 0))

data$r_secure <- as.numeric(data$r_secure)
data$s_secure <- as.numeric(data$s_secure)

# timepre:	r,s	whether r is under constant time pressure due to a heavy workload (1-4 worst)
data %>% select(contains('timepre')) %>% map(table, exclude = NULL )

data <- data %>% 
  mutate(r_timepre = replace(r_timepre, r_timepre == "a" |
                               r_timepre == "c" |
                               r_timepre == "e" |
                               r_timepre == "m" |
                               r_timepre == "w" , NA))

data <- data %>% 
  mutate(s_timepre = replace(s_timepre, s_timepre == "a" |
                               s_timepre == "c" |
                               s_timepre == "e" |
                               s_timepre == "m" |
                               s_timepre == "v" |
                               s_timepre == "w" , NA)) %>% 
  mutate(s_timepre = replace(s_timepre, s_timepre == "u", 0))

data$r_timepre <- as.numeric(data$r_timepre)
data$s_timepre <- as.numeric(data$s_timepre)

# workfast:	r,s	whether r has to work very fast (1-4 worst)
data %>% select(contains('workfast')) %>% map(table, exclude = NULL )

data <- data %>% 
  mutate(r_workfast = replace(r_workfast, r_workfast == "a" |
                                r_workfast == "c" |
                                r_workfast == "e" |
                                r_workfast == "m" |
                                r_workfast == "w" , NA))

data <- data %>% 
  mutate(s_workfast = replace(s_workfast, s_workfast == "a" |
                                s_workfast == "c" |
                                s_workfast == "e" |
                                s_workfast == "m" |
                                s_workfast == "v" |
                                s_workfast == "w" , NA)) %>% 
  mutate(s_workfast = replace(s_workfast, s_workfast == "u", 0))

data$r_workfast <- as.numeric(data$r_workfast)
data$s_workfast <- as.numeric(data$s_workfast)

# combination
data <- data %>% mutate(r_jobsum2 = r_demand + r_interf - r_jdiffa + r_jmldema + r_jobsum + r_ltlfree -  
                          - r_nskills + r_promot - r_salary + r_secure + r_timepre + r_workfast)
data <- data %>% mutate(s_jobsum2 = s_demand + s_interf - s_jdiffa + s_jmldema + s_jobsum + s_ltlfree -  
                          - s_nskills + s_promot - s_salary + s_secure + s_timepre + s_workfast)

data %>% select(contains('jobsum2')) %>% map(table, exclude = NULL)


# VARIABLES FROM LONGITUDINAL DATA

# work: currently working for pay (0,1)
data %>% select(c('r_work','s_work')) %>% map(table, exclude = NULL)

# unemp: unemployed (0,1)
data %>% select(c('r_unemp','s_unemp')) %>% map(table, exclude = NULL)
data <- data %>% mutate(r_unemp = replace(r_unemp, r_unemp == "x", -1))

data <- data %>% mutate(s_unemp = replace(s_unemp, s_unemp == "x", -1))

# jhours: hours of work per week at current job (0-98, continuous)
data %>% select(c('r_jhours','s_jhours')) %>% map(table, exclude = NULL)


data <- rename(data,c('r_work_csize'='r_csize','s_work_csize'='s_csize',
                      'r_work_jdealpplb'='r_jdealpplb','s_work_jdealpplb'='s_jdealpplb',
                      'r_work_jdiffa'='r_jdiffa','s_work_jdiffa'='s_jdiffa',
                      'r_work_jenjwrka'='r_jenjwrka','s_work_jenjwrka'='s_jenjwrka',
                      'r_work_jobsum'='r_jobsum','r_work_jobsum'='r_jobsum',
                      'r_work_jpdysa'='r_jpdysa','s_work_jpdysa'='s_jpdysa',
                      'r_work_jrsleft'='r_jrsleft','s_work_jrsleft'='s_jrsleft',
                      'r_work_jsprvsn'='r_jsprvsn','s_work_jsprvsn'='s_jsprvsn',
                      'r_work_lookwrkpf'='r_lookwrkpf','s_work_lookwrkpf'='s_lookwrkpf',
                      'r_work_satjob'='r_satjob','s_work_satjob'='s_satjob',
                      'r_work_wdiscrim'='r_wdiscrim','s_work_wdiscrim'='s_wdiscrim',
                      'r_work_jobsum2'='r_jobsum2','s_work_jobsum2'='s_jobsum2',
                      'r_work_jhours'='r_jhours','s_work_jhours'='s_jhours'))


# GROUP: SOCIAL

# dcsxori	r,s	r discrimination reason:sexual orientation (0/1)
data %>% select(contains('dcsxori')) %>% map(table, exclude = NULL)

# dscrim	r,s	"r 6 discrimination summary mean score: indicates the mean of the 
# six different day-to-day life discrimination questions (LSRSPCT,PRSRVC, NOTSMRT, 
# HARASS, PRTRMT and ACTAFD) and can be used as a summary score." (1-6, continuous)
data %>% select(c('r_dscrim','s_dscrim')) %>% map(table, exclude = NULL)

# fsupport	r,s	"r lack of friends support summary mean score: mean of the seven 
# different friends support questions FUSTDFE, FRELY,FOPENUP, FDEMAND, FCRITZE, 
# FLETDOW and FGETNEV" (1-4, continuous)
data %>% select(c('r_fsupport','s_fsupport')) %>% map(table, exclude = NULL)

# gcaany	h	provides any informal care (0/1)
data %>% select(contains('gcaany')) %>% map(table, exclude = NULL)

data <- data %>% mutate(h_gcaany = replace(h_gcaany, h_gcaany == "l", 1))

# kcnt	r,s,h	any weekly contact with children in person/phone/email (0/1)
data %>% select(c('r_kcnt','s_kcnt','h_kcnt')) %>% map(table, exclude = NULL)

data <- data %>% mutate(r_kcnt = replace(r_kcnt, r_kcnt == "k", 0))
data <- data %>% mutate(s_kcnt = replace(s_kcnt, s_kcnt == "k", 0))
data <- data %>% mutate(h_kcnt = replace(h_kcnt, h_kcnt == "k", 0))

# ksupport	r,s	"r lack of children support summary mean score: mean of the seven 
# different children support questions (KUSTDFE, KRELY,KOPENUP, KDEMAND, KCRITZE, 
# KLETDOW and KGETNEV" (1-4, continuous)
data %>% select(c('r_ksupport','s_ksupport')) %>% map(table, exclude = NULL)

# npdisum	r,s	r neighborhood physical disorder summary mean score 
# (includes VANDAL AFWALK RUBBISH VACANT) (1-7, continuous)
data %>% select(c('r_npdisum','s_npdisum')) %>% map(table, exclude = NULL)

# nsocosum	r,s	r neighborhood social cohesion summary mean score:  
# mean(BELONG, TRUST, UNFRIEND, HLPNTR) (1-7, continuous)
data %>% select(c('r_nsocosum','s_nsocosum')) %>% map(table, exclude = NULL)

# osupport	r,s	r lack of other family members support summary mean score (includes 
# OUSTDFE, ORELY, OOPENUP, ODEMAND, OCRITZE, OLETDOW and OGETNEV) (1-4, continuous)
data %>% select(c('r_osupport','s_osupport')) %>% map(table, exclude = NULL)

# pcnt	h	any weekly contact with parents in person/phone/email  (0/1)
data %>% select(contains('pcnt')) %>% map(table, exclude = NULL)

data <- data %>% mutate(h_pcnt = replace(h_pcnt, h_pcnt == "n", 0))

# relgwk	r,s	r any weekly part. in religious services  (0/1)
data %>% select(contains('relgwk')) %>% map(table, exclude = NULL)

# rfcnt	r,s	r any weekly contact with relative/friend in person/phone/mail/email  (0/1)
data %>% select(c('r_rfcnt','s_rfcnt')) %>% map(table, exclude = NULL)

# socwk	r,s	r any weekly social activities  (0/1)
data %>% select(contains('socwk')) %>% map(table, exclude = NULL)

# unfair	r,s	r lifetime unfair experiences count: sum of UFDIS, UFHIRE, 
# UFDENYP, UFPVMOV, UFDENYB, UFPOLIC  (0-7)
data %>% select(c('r_unfair','r_unfair')) %>% map(table, exclude = NULL)

# ssupport	r,s	lack of r spouse support summary mean score: RwSSUPPORT indicates 
# the mean of the seven different spouse support questions (SUSTDFE, SRELY, SOPENUP, 
# SDEMAND, SCRITZE, SLETDOW and SGETNEV)   (1-4, continuous)      
data %>% select(c('r_ssupport','s_ssupport')) %>% map(table, exclude = NULL)


# GROUP: WEALTH

# adebt	h	assets:debts [not yet asked]--cross-wave (0 - 6.666.733, continuous)
data %>% select(contains('adebt')) %>% map(table, exclude = NULL)

# itot	h	incm: total hhold / r+sp only (0-60.014.376, continuous)
data %>% select('h_itot') %>% map(table, exclude = NULL)
data$h_itot <- as.numeric(data$h_itot)
data$h_itot <- case_when(data$h_itot >= 2000000 ~ 2000000,TRUE ~ as.numeric(data$h_itot))


# SUMMARY VARIABLES

# atotb		total all assets inc. 2nd hm--cross-wave (numeric -4.383.000 - 300.000.000)
data %>% select('h_atotb') %>% map(table, exclude = NULL)
data <- data %>% mutate(h_atotb = replace(h_atotb, h_atotb == "q", 0))
data$h_amrtb <- as.numeric(data$h_atotb)
data$h_atotb <- case_when(data$h_atotb >= 10000000 ~ 10000000,TRUE ~ as.numeric(data$h_atotb))


#Keep variables included in analysis
data <- data %>% select(c('hhidpn',                                     #model
                          'r_bmonth','s_bmonth',
                          'r_byear','s_byear',
                          'r_dyear','s_dyear',
                          'r_dmonth','s_dmonth',
                          'inw1','inw2','inw3','inw4','inw5','inw6','inw7','inw8',
                          'inw9','inw10','inw11','inw12','inw13','inw14',
                          "r_cidimde3", "s_cidimde3",            # mental health
                          "r_cidisymp", "s_cidisymp", 
                          "r_lideal3", "s_lideal3", 
                          "r_lstsf3", "s_lstsf3", 
                          "r_rested", "s_rested", 
                          "r_rxpsych", "s_rxpsych", 
                          "r_trpsych", "s_trpsych",
                          "r_lifeinv", "s_lifeinv",               # Welfare 
                          "r_nlfins", "s_nlfins", 
                          "r_witwill", "s_witwill",
                          "r_wlifein", "s_wlifein",
                          "r_wtrust", "s_wtrust", 
                          "r_lifeins_fam",
                          "r_will_fam", "s_will_fam",
                          "r_ftrhlp", "s_ftrhlp",                  # support 
                          "r_mealhlp", "s_mealhlp",
                          "r_medhlp",  "s_medhlp",
                          "r_moneyhlp", "s_moneyhlp",
                          "r_phonehlp", "s_phonehlp",
                          "r_shophlp", "s_shophlp",
                          "r_rcany", "s_rcany",
                          "r_adl_hour_fam", "s_adl_hour_fam",
                          "r_adl_fam", "s_adl_fam",
                          "r_adl_hour_nf", "s_adl_hour_nf",
                          "r_adl_nf", "s_adl_nf", 
                          "r_byear",  "s_byear",       # longitudinal variable list
                          "r_bmonth", "s_bmonth", 
                          "r_bcohort", "s_bcohort", 
                          "r_dyear",  "s_dyear",
                          "r_dmonth", "s_dmonth",
                          "r_agem_b",
                          "r_agey_b", "s_agey_b",
                          "r_gender", "s_gender",
                          "r_race", "s_race",
                          "r_edyrs", "s_edyrs",
                          "r_meduc", "s_meduc",
                          "r_feduc", "s_feduc",
                          "r_mstat",
                          "r_mrct", "s_mrct",
                          "r_everwid", "s_everwid",
                          "r_everdiv", "s_everdiv",
                          "r_relig", "s_relig",
                          "r_vetrn", "s_vetrn",
                          "r_bplace", "s_bplace",
                          "r_momliv", "s_momliv",
                          "r_dadliv", "s_dadliv", 
                          "r_momage", "s_momage", 
                          "r_dadage", "s_dadage",
                          "h_hhres", 
                          "h_child", 
                          "r_evbrn", "s_evbrn",
                          "r_smokev", "s_smokev",
                          "r_mdiv", "s_mdiv",
                          'r_pwarm',                                  #childhood
                          'r_lhchild','s_lhchild',
                          'r_dadgrela','s_dadgrela',
                          'r_pabused','s_pabused', 
                          'r_financh','s_financh',
                          'r_chshlt','s_chshlt',
                          'r_dadoccup', 's_dadoccup', 
                          'r_cage','r_smokev_smokef',                #habits
                          'r_binged', 'r_smokev_strtsmok',
                          'r_smokev_quitsmok', 's_cage',
                          's_smokev_smokef', 's_binged', 
                          's_smokev_strtsmok','s_smokev_quitsmok',
                          'h_lvwith', 'r_hometyp',                    #demographic
                          'r_evermrg', 
                          'r_traumatic_events', 'h_rural',
                          'h_depndntn', 'h_kidu14',
                          'h_kidu6','r_arriage',
                          'r_educl','r_mheight',
                          'r_mbmi','r_mweight',
                          'r_mobese','r_citizen',
                          'r_mfstyr','h_grchild',
                          's_hometyp','s_traumatic_events',
                          's_arriage','s_educl',
                          's_mheight','s_mbmi',
                          's_mweight','s_mobese',
                          's_citizen','s_mfstyr',
                          'r_balance','r_sum_med',                   #physical health
                          'r_adlfive',
                          'r_gender_lstmnspd','s_gender_lstmnspd',
                          'r_urinai','r_lowermob', 
                          'r_cancre_reccancr','r_hrtatte_rechrtatt',
                          'r_stroke_recstrok','r_diasto',
                          'r_systo','r_pulse',
                          'r_cancre_cancrst',
                          'r_hipe','r_hrtrhme',
                          'r_angine','r_catrcte',
                          'r_conhrtfe','r_hrtatte',
                          'r_hrtsrge','r_hchole',
                          'r_gender_hystere','r_jointre',
                          'r_osteoe','r_shingle',
                          'r_glaucoma','r_ncatrct',
                          'r_pneushte','r_hrtrhm',
                          'r_catrct','r_conhrtf',
                          'r_hrtatt','r_sight',
                          'r_hearing','r_fatigue',
                          'r_shnglshte','r_uppermob',
                          'r_painlv','r_lunglmt',
                          'r_hrtrhme_diaghrtr','r_angine_diagangin',
                          'r_conhrtfe_diagchf','r_diabe_diagdiab',
                          'r_hrtatte_frhrtatt','r_limimpar',
                          's_balance','s_sum_med',        
                          's_adlfive','s_gender_lstmnspd',
                          's_urinai','s_lowermob', 
                          's_cancre_reccancr','s_hrtatte_rechrtatt',
                          's_stroke_recstrok','s_diasto',
                          's_systo','s_pulse',
                          's_cancre_cancrst',
                          's_hipe','s_hrtrhme',
                          's_angine','s_catrcte',
                          's_conhrtfe','s_hrtatte',
                          's_hrtsrge','s_hchole',
                          's_gender_hystere','s_jointre',
                          's_osteoe','s_shingle',
                          's_glaucoma','s_ncatrct',
                          's_pneushte','s_hrtrhm',
                          's_catrct','s_conhrtf',
                          's_hrtatt','s_sight',
                          's_hearing','s_fatigue',
                          's_shnglshte','s_uppermob',
                          's_painlv','s_lunglmt',
                          's_hrtrhme_diaghrtr','s_angine_diagangin',
                          's_conhrtfe_diagchf','s_diabe_diagdiab',
                          's_hrtatte_frhrtatt','s_limimpar',
                          'r_stroke','s_stroke',
                          'r_cancre','s_cancre',
                          'r_diabe','s_diabe',
                          'r_work_csize','s_work_csize',                  #job
                          'r_work_jdealpplb','s_work_jdealpplb',
                          'r_work_jdiffa','s_work_jdiffa',
                          'r_work_jenjwrka','s_work_jenjwrka',
                          'r_work_jobsum','r_work_jobsum',
                          'r_work_jpdysa','s_work_jpdysa',
                          'r_work_jrsleft','s_work_jrsleft',
                          'r_work_jsprvsn','s_work_jsprvsn',
                          'r_work_lookwrkpf','s_work_lookwrkpf',
                          'r_work_satjob','s_work_satjob',
                          'r_work_wdiscrim','s_work_wdiscrim',
                          'r_work_jobsum2','s_work_jobsum2',
                          'r_work_jhours','s_work_jhours',
                          'r_work','s_work',
                          'r_unemp','s_unemp',
                          'r_dcsxori','s_dcsxori',                     #social
                          'r_dscrim','s_dscrim',
                          'r_fsupport','s_fsupport',
                          'h_gcaany',
                          'r_kcnt','s_kcnt','h_kcnt',
                          'r_ksupport','s_ksupport',
                          'r_npdisum','s_npdisum',
                          'r_nsocosum','s_nsocosum',
                          'r_osupport','s_osupport',
                          'h_pcnt',
                          'r_relgwk','s_relgwk',
                          'r_rfcnt','s_rfcnt',
                          'r_socwk','s_socwk',
                          'r_unfair','s_unfair',
                          'r_ssupport','s_ssupport',
                          'h_adebt',                                  #wealth
                          'h_itot',
                          'h_atotb'
                          ))

data[data == "." | 
       data == "a" |
       data == "e" |
       data == "j" |
       data == "d" |
       data == "c" |
       data == "f" |
       data == "h" |
       data == "r" |
       data == "k" |
       data == "p" |
       data == "w" |
       data == "n" |
       data == "x" |
       data == "q" |
       data == "v" |
       data == "r" |
       data == "s" |
       data == "o" |
       data == "x" |
       data == "m" ] <- NA


cat_vars <- c('cidimde3','cidisymp','lideal3','lstsf3','rested', 'rxpsych','witwill',
              'wlifein','wtrust','will_fam','ftrhlp','mealhlp','medhlp',
              'moneyhlp','phonehlp','shophlp','rcany','gender','race','bcohort',
              'relig','vetrn','bplace','momliv','dadliv', 'evbrn','smokev',
              'lhchild','dadgrela','pabused','chshlt','dadoccup','cage',
              'mobese','traumatic_events','educl','citizen','balance','adlfive',
              'urinai','cancre_cancrst','hipe','hrtrhme','angine','cancre',
              'diabe','catrcte','conhrtfe','hrtatte','stroke','hrtsrge',
              'hchole','gender_hystere','jointre','osteoe','shingle','glaucoma',
              'lowermob','ncatrct','pneushte','hrtrhm','catrct','conhrtf',
              'hrtatt','sight','hearing','fatigue','shnglshte','uppermob',
              'painlv','lunglmt','limimpar','work_jdealpplb','work_jdiffa',
              'work_jenjwrka','work_jrsleft','work_lookwrkpf','work_satjob',
              'work','unemp','dcsxori','kcnt','relgwk','rfcnt','socwk','unfair',
              'everdiv','everwid')

cat_vars <- c(paste0("s_", cat_vars))
data[,cat_vars] <- data %>% select(all_of(cat_vars)) %>% replace(. == "u", "-1")


#For spouse continuous variables, if not married or do not have a partner put 0
data[data == "u"] <- "0"
data <- as.data.frame(sapply(data, as.numeric))


# Create interaction terms

# smokev_smokef
data %>% select('r_smokev') %>% map(table, exclude = NULL)
data %>% select('r_smokev_smokef') %>% map(table, exclude = NULL)

data <- data %>% mutate(r_smokev_smokef = case_when(r_smokev == 0 ~ 0,
                                                    r_smokev == 1 ~ r_smokev_smokef,     
                                                    TRUE ~ r_smokev))

data %>% select('r_smokev_smokef') %>% map(table, exclude = NULL)

data %>% select('s_smokev') %>% map(table, exclude = NULL)
data %>% select('s_smokev_smokef') %>% map(table, exclude = NULL)

data <- data %>% mutate(s_smokev_smokef = case_when(s_smokev == 0 ~ 0,
                                                    s_smokev == 1 ~ s_smokev_smokef,     
                                                    TRUE ~ s_smokev))

data %>% select('s_smokev_smokef') %>% map(table, exclude = NULL)

# smokev_strtsmok
data %>% select('r_smokev') %>% map(table, exclude = NULL)
data %>% select('r_smokev_strtsmok') %>% map(table, exclude = NULL)

data <- data %>% mutate(r_smokev_strtsmok = case_when(r_smokev == 0 ~ 0,
                                                      r_smokev == 1 ~ r_smokev_strtsmok,     
                                                      TRUE ~ r_smokev))

data %>% select('r_smokev_strtsmok') %>% map(table, exclude = NULL)

data %>% select('s_smokev') %>% map(table, exclude = NULL)
data %>% select('s_smokev_strtsmok') %>% map(table, exclude = NULL)

data <- data %>% mutate(s_smokev_strtsmok = case_when(s_smokev == 0 ~ 0,
                                                      s_smokev == 1 ~ s_smokev_strtsmok,     
                                                      TRUE ~ s_smokev))

data %>% select('s_smokev_strtsmok') %>% map(table, exclude = NULL)

# smokev_quitsmok
data %>% select('r_smokev') %>% map(table, exclude = NULL)
data %>% select('r_smokev_quitsmok') %>% map(table, exclude = NULL)

data <- data %>% mutate(r_smokev_quitsmok = case_when(r_smokev == 0 ~ 0,
                                                      r_smokev == 1 ~ r_smokev_quitsmok,     
                                                      TRUE ~ r_smokev))

data %>% select('r_smokev_quitsmok') %>% map(table, exclude = NULL)

data %>% select('s_smokev') %>% map(table, exclude = NULL)
data %>% select('s_smokev_quitsmok') %>% map(table, exclude = NULL)

data <- data %>% mutate(s_smokev_quitsmok = case_when(s_smokev == 0 ~ 0,
                                                      s_smokev == 1 ~ s_smokev_quitsmok,     
                                                      TRUE ~ s_smokev))

data %>% select('s_smokev_quitsmok') %>% map(table, exclude = NULL)

# gender_lstmnspd
data %>% select('r_gender') %>% map(table, exclude = NULL)
data %>% select('r_gender_lstmnspd') %>% map(table, exclude = NULL)

data <- data %>% mutate(r_gender_lstmnspd = case_when(r_gender == 0 ~ 0,
                                                      r_gender == 1 ~ r_gender_lstmnspd,     
                                                      TRUE ~ r_gender))
data %>% select('r_gender_lstmnspd') %>% map(table, exclude = NULL)

data %>% select('s_gender') %>% map(table, exclude = NULL)
data %>% select('s_gender_lstmnspd') %>% map(table, exclude = NULL)

data <- data %>% mutate(s_gender_lstmnspd = case_when(s_gender == 0 ~ 0,
                                                      s_gender == 1 ~ s_gender_lstmnspd,     
                                                      TRUE ~ s_gender))
data %>% select('s_gender_lstmnspd') %>% map(table, exclude = NULL)

# gender_hystere
data %>% select('r_gender') %>% map(table, exclude = NULL)
data %>% select('r_gender_hystere') %>% map(table, exclude = NULL)

data <- data %>% mutate(r_gender_hystere = case_when(r_gender == 0 ~ 0,
                                                     r_gender == 1 ~ r_gender_hystere,     
                                                     TRUE ~ r_gender))
data %>% select('r_gender_hystere') %>% map(table, exclude = NULL)

data %>% select('s_gender') %>% map(table, exclude = NULL)
data %>% select('s_gender_hystere') %>% map(table, exclude = NULL)

data <- data %>% mutate(s_gender_hystere = case_when(s_gender == 0 ~ 0,
                                                     s_gender == 1 ~ s_gender_hystere,     
                                                     TRUE ~ s_gender))
data %>% select('s_gender_hystere') %>% map(table, exclude = NULL)

# r_cancre_reccancr
data %>% select('r_cancre') %>% map(table, exclude = NULL)
data %>% select('r_cancre_reccancr') %>% map(table, exclude = NULL)

data <- data %>% mutate(r_cancre_reccancr = case_when(r_cancre == 0 ~ 0,
                                                      r_cancre == 1 ~ r_cancre_reccancr,     
                                                      TRUE ~ r_cancre))
data %>% select('r_cancre_reccancr') %>% map(table, exclude = NULL)

data %>% select('s_cancre') %>% map(table, exclude = NULL)
data %>% select('s_cancre_reccancr') %>% map(table, exclude = NULL)

data <- data %>% mutate(s_cancre_reccancr = case_when(s_cancre == 0 ~ 0,
                                                      s_cancre == 1 ~ s_cancre_reccancr,     
                                                      TRUE ~ s_cancre))
data %>% select('s_cancre_reccancr') %>% map(table, exclude = NULL)

# cancre_cancrst
data %>% select('r_cancre') %>% map(table, exclude = NULL)
data %>% select('r_cancre_cancrst') %>% map(table, exclude = NULL)

data <- data %>% mutate(r_cancre_cancrst = case_when(r_cancre == 0 ~ 0,
                                                     r_cancre == 1 ~ r_cancre_cancrst,     
                                                     TRUE ~ r_cancre))
data %>% select('r_cancre_cancrst') %>% map(table, exclude = NULL)

data %>% select('s_cancre') %>% map(table, exclude = NULL)
data %>% select('s_cancre_cancrst') %>% map(table, exclude = NULL)

data <- data %>% mutate(s_cancre_cancrst = case_when(s_cancre == 0 ~ 0,
                                                     s_cancre == 1 ~ s_cancre_cancrst,     
                                                     TRUE ~ s_cancre))
data %>% select('s_cancre_cancrst') %>% map(table, exclude = NULL)

# hrtatte_rechrtatt
data %>% select('r_hrtatte') %>% map(table, exclude = NULL)
data %>% select('r_hrtatte_rechrtatt') %>% map(table, exclude = NULL)

data <- data %>% mutate(r_hrtatte_rechrtatt = case_when(r_hrtatte == 0 ~ 0,
                                                        r_hrtatte == 1 ~ r_hrtatte_rechrtatt,     
                                                        TRUE ~ r_hrtatte))
data %>% select('r_hrtatte_rechrtatt') %>% map(table, exclude = NULL)

data %>% select('s_hrtatte') %>% map(table, exclude = NULL)
data %>% select('s_hrtatte_rechrtatt') %>% map(table, exclude = NULL)

data <- data %>% mutate(s_hrtatte_rechrtatt = case_when(s_hrtatte == 0 ~ 0,
                                                        s_hrtatte == 1 ~ s_hrtatte_rechrtatt,     
                                                        TRUE ~ s_hrtatte))
data %>% select('s_hrtatte_rechrtatt') %>% map(table, exclude = NULL)

# r_stroke_recstrok
data %>% select('r_stroke') %>% map(table, exclude = NULL)
data %>% select('r_stroke_recstrok') %>% map(table, exclude = NULL)

data <- data %>% mutate(r_stroke_recstrok = case_when(r_stroke == 0 ~ 0,
                                                      r_stroke == 1 ~ r_stroke_recstrok,     
                                                      TRUE ~ r_stroke))
data %>% select('r_stroke_recstrok') %>% map(table, exclude = NULL)

data %>% select('s_stroke') %>% map(table, exclude = NULL)
data %>% select('s_stroke_recstrok') %>% map(table, exclude = NULL)

data <- data %>% mutate(s_stroke_recstrok = case_when(s_stroke == 0 ~ 0,
                                                      s_stroke == 1 ~ s_stroke_recstrok,     
                                                      TRUE ~ s_stroke))
data %>% select('s_stroke_recstrok') %>% map(table, exclude = NULL)

# hrtrhme_diaghrtr
data %>% select('r_hrtrhme') %>% map(table, exclude = NULL)
data %>% select('r_hrtrhme_diaghrtr') %>% map(table, exclude = NULL)

data <- data %>% mutate(r_hrtrhme_diaghrtr = case_when(r_hrtrhme == 0 ~ 0,
                                                       r_hrtrhme == 1 ~ r_hrtrhme_diaghrtr,     
                                                       TRUE ~ r_hrtrhme))
data %>% select('r_hrtrhme_diaghrtr') %>% map(table, exclude = NULL)

data %>% select('s_hrtrhme') %>% map(table, exclude = NULL)
data %>% select('s_hrtrhme_diaghrtr') %>% map(table, exclude = NULL)

data <- data %>% mutate(s_hrtrhme_diaghrtr = case_when(s_hrtrhme == 0 ~ 0,
                                                       s_hrtrhme == 1 ~ s_hrtrhme_diaghrtr,     
                                                       TRUE ~ s_hrtrhme))
data %>% select('s_hrtrhme_diaghrtr') %>% map(table, exclude = NULL)

# angine_diagangin
data %>% select('r_angine') %>% map(table, exclude = NULL)
data %>% select('r_angine_diagangin') %>% map(table, exclude = NULL)

data <- data %>% mutate(r_angine_diagangin = case_when(r_angine == 0 ~ 0,
                                                       r_angine == 1 ~ r_angine_diagangin,     
                                                       TRUE ~ r_angine))
data %>% select('r_angine_diagangin') %>% map(table, exclude = NULL)

data %>% select('s_angine') %>% map(table, exclude = NULL)
data %>% select('s_angine_diagangin') %>% map(table, exclude = NULL)

data <- data %>% mutate(s_angine_diagangin = case_when(s_angine == 0 ~ 0,
                                                       s_angine == 1 ~ s_angine_diagangin,     
                                                       TRUE ~ s_angine))
data %>% select('s_angine_diagangin') %>% map(table, exclude = NULL)

# conhrtfe_diagchf
data %>% select('r_conhrtfe') %>% map(table, exclude = NULL)
data %>% select('r_conhrtfe_diagchf') %>% map(table, exclude = NULL)

data <- data %>% mutate(r_conhrtfe_diagchf = case_when(r_conhrtfe == 0 ~ 0,
                                                       r_conhrtfe == 1 ~ r_conhrtfe_diagchf,     
                                                       TRUE ~ r_conhrtfe))
data %>% select('r_conhrtfe_diagchf') %>% map(table, exclude = NULL)

data %>% select('s_conhrtfe') %>% map(table, exclude = NULL)
data %>% select('s_conhrtfe_diagchf') %>% map(table, exclude = NULL)

data <- data %>% mutate(s_conhrtfe_diagchf = case_when(s_conhrtfe == 0 ~ 0,
                                                       s_conhrtfe == 1 ~ s_conhrtfe_diagchf,     
                                                       TRUE ~ s_conhrtfe))
data %>% select('s_conhrtfe_diagchf') %>% map(table, exclude = NULL)

# diabe_diagdiab
data %>% select('r_diabe') %>% map(table, exclude = NULL)
data %>% select('r_diabe_diagdiab') %>% map(table, exclude = NULL)

data <- data %>% mutate(r_diabe_diagdiab = case_when(r_diabe == 0 ~ 0,
                                                     r_diabe == 1 ~ r_diabe_diagdiab,     
                                                     TRUE ~ r_diabe))
data %>% select('r_diabe_diagdiab') %>% map(table, exclude = NULL)

data %>% select('s_diabe') %>% map(table, exclude = NULL)
data %>% select('s_diabe_diagdiab') %>% map(table, exclude = NULL)

data <- data %>% mutate(s_diabe_diagdiab = case_when(s_diabe == 0 ~ 0,
                                                     s_diabe == 1 ~ s_diabe_diagdiab,     
                                                     TRUE ~ s_diabe))
data %>% select('s_diabe_diagdiab') %>% map(table, exclude = NULL)

# hrtatte_frhrtatt
data %>% select('r_hrtatte') %>% map(table, exclude = NULL)
data %>% select('r_hrtatte_frhrtatt') %>% map(table, exclude = NULL)

data <- data %>% mutate(r_hrtatte_frhrtatt = case_when(r_hrtatte == 0 ~ 0,
                                                       r_hrtatte == 1 ~ r_hrtatte_frhrtatt,     
                                                       TRUE ~ r_hrtatte))
data %>% select('r_hrtatte_frhrtatt') %>% map(table, exclude = NULL)

data %>% select('s_hrtatte') %>% map(table, exclude = NULL)
data %>% select('s_hrtatte_frhrtatt') %>% map(table, exclude = NULL)

data <- data %>% mutate(s_hrtatte_frhrtatt = case_when(s_hrtatte == 0 ~ 0,
                                                       s_hrtatte == 1 ~ s_hrtatte_frhrtatt,     
                                                       TRUE ~ s_hrtatte))
data %>% select('s_hrtatte_frhrtatt') %>% map(table, exclude = NULL)

# work_csize
data %>% select('r_work') %>% map(table, exclude = NULL)
data %>% select('r_work_csize') %>% map(table, exclude = NULL)

data <- data %>% mutate(r_work_csize = case_when(r_work == 0 ~ 0,
                                                 r_work == 1 ~ r_work_csize,     
                                                 TRUE ~ r_work))
data %>% select('r_work_csize') %>% map(table, exclude = NULL)

data %>% select('s_work') %>% map(table, exclude = NULL)
data %>% select('s_work_csize') %>% map(table, exclude = NULL)

data <- data %>% mutate(s_work_csize = case_when(s_work == 0 ~ 0,
                                                 s_work == 1 ~ s_work_csize,     
                                                 TRUE ~ s_work))
data %>% select('s_work_csize') %>% map(table, exclude = NULL)

# work_jdealpplb
data %>% select('r_work') %>% map(table, exclude = NULL)
data %>% select('r_work_jdealpplb') %>% map(table, exclude = NULL)

data <- data %>% mutate(r_work_jdealpplb = case_when(r_work == 0 ~ 0,
                                                     r_work == 1 ~ r_work_jdealpplb,     
                                                     TRUE ~ r_work))
data %>% select('r_work_jdealpplb') %>% map(table, exclude = NULL)

data %>% select('s_work') %>% map(table, exclude = NULL)
data %>% select('s_work_jdealpplb') %>% map(table, exclude = NULL)

data <- data %>% mutate(s_work_jdealpplb = case_when(s_work == 0 ~ 0,
                                                     s_work == 1 ~ s_work_jdealpplb,     
                                                     TRUE ~ s_work))
data %>% select('s_work_jdealpplb') %>% map(table, exclude = NULL)

# work_jdiffa
data %>% select('r_work') %>% map(table, exclude = NULL)
data %>% select('r_work_jdiffa') %>% map(table, exclude = NULL)

data <- data %>% mutate(r_work_jdiffa = case_when(r_work == 0 ~ 0,
                                                  r_work == 1 ~ r_work_jdiffa,     
                                                  TRUE ~ r_work))
data %>% select('r_work_jdiffa') %>% map(table, exclude = NULL)

data %>% select('s_work') %>% map(table, exclude = NULL)
data %>% select('s_work_jdiffa') %>% map(table, exclude = NULL)

data <- data %>% mutate(s_work_jdiffa = case_when(s_work == 0 ~ 0,
                                                  s_work == 1 ~ s_work_jdiffa,     
                                                  TRUE ~ s_work))
data %>% select('s_work_jdiffa') %>% map(table, exclude = NULL)

# work_jenjwrka
data %>% select('r_work') %>% map(table, exclude = NULL)
data %>% select('r_work_jenjwrka') %>% map(table, exclude = NULL)

data <- data %>% mutate(r_work_jenjwrka = case_when(r_work == 0 ~ 0,
                                                    r_work == 1 ~ r_work_jenjwrka,     
                                                    TRUE ~ r_work))
data %>% select('r_work_jenjwrka') %>% map(table, exclude = NULL)

data %>% select('s_work') %>% map(table, exclude = NULL)
data %>% select('s_work_jenjwrka') %>% map(table, exclude = NULL)

data <- data %>% mutate(s_work_jenjwrka = case_when(s_work == 0 ~ 0,
                                                    s_work == 1 ~ s_work_jenjwrka,     
                                                    TRUE ~ s_work))
data %>% select('s_work_jenjwrka') %>% map(table, exclude = NULL)

# work_jobsum
data %>% select('r_work') %>% map(table, exclude = NULL)
data %>% select('r_work_jobsum') %>% map(table, exclude = NULL)

data <- data %>% mutate(r_work_jobsum = case_when(r_work == 0 ~ 0,
                                                  r_work == 1 ~ r_work_jobsum,     
                                                  TRUE ~ r_work))
data %>% select('r_work_jobsum') %>% map(table, exclude = NULL)

# work_jpdysa
data %>% select('r_work') %>% map(table, exclude = NULL)
data %>% select('r_work_jpdysa') %>% map(table, exclude = NULL)

data <- data %>% mutate(r_work_jpdysa = case_when(r_work == 0 ~ 0,
                                                  r_work == 1 ~ r_work_jpdysa,     
                                                  TRUE ~ r_work))
data %>% select('r_work_jpdysa') %>% map(table, exclude = NULL)

data %>% select('s_work') %>% map(table, exclude = NULL)
data %>% select('s_work_jpdysa') %>% map(table, exclude = NULL)

data <- data %>% mutate(s_work_jpdysa = case_when(s_work == 0 ~ 0,
                                                  s_work == 1 ~ s_work_jpdysa,     
                                                  TRUE ~ s_work))
data %>% select('s_work_jpdysa') %>% map(table, exclude = NULL)

# work_jrsleft
data %>% select('r_work') %>% map(table, exclude = NULL)
data %>% select('r_work_jrsleft') %>% map(table, exclude = NULL)

data <- data %>% mutate(r_work_jrsleft = case_when(r_work == 0 ~ 0,
                                                   r_work == 1 ~ r_work_jrsleft,     
                                                   TRUE ~ r_work))
data %>% select('r_work_jrsleft') %>% map(table, exclude = NULL)

data %>% select('s_work') %>% map(table, exclude = NULL)
data %>% select('s_work_jrsleft') %>% map(table, exclude = NULL)

data <- data %>% mutate(s_work_jrsleft = case_when(s_work == 0 ~ 0,
                                                   s_work == 1 ~ s_work_jrsleft,     
                                                   TRUE ~ s_work))
data %>% select('s_work_jrsleft') %>% map(table, exclude = NULL)

# work_jsprvsn
data %>% select('r_work') %>% map(table, exclude = NULL)
data %>% select('r_work_jsprvsn') %>% map(table, exclude = NULL)

data <- data %>% mutate(r_work_jsprvsn = case_when(r_work == 0 ~ 0,
                                                   r_work == 1 ~ r_work_jsprvsn,     
                                                   TRUE ~ r_work))
data %>% select('r_work_jsprvsn') %>% map(table, exclude = NULL)

data %>% select('s_work') %>% map(table, exclude = NULL)
data %>% select('s_work_jsprvsn') %>% map(table, exclude = NULL)

data <- data %>% mutate(s_work_jsprvsn = case_when(s_work == 0 ~ 0,
                                                   s_work == 1 ~ s_work_jsprvsn,     
                                                   TRUE ~ s_work))
data %>% select('s_work_jsprvsn') %>% map(table, exclude = NULL)

# work_lookwrkpf
data %>% select('r_work') %>% map(table, exclude = NULL)
data %>% select('r_work_lookwrkpf') %>% map(table, exclude = NULL)

data <- data %>% mutate(r_work_lookwrkpf = case_when(r_work == 0 ~ 0,
                                                     r_work == 1 ~ r_work_lookwrkpf,     
                                                     TRUE ~ r_work))
data %>% select('r_work_lookwrkpf') %>% map(table, exclude = NULL)

data %>% select('s_work') %>% map(table, exclude = NULL)
data %>% select('s_work_lookwrkpf') %>% map(table, exclude = NULL)

data <- data %>% mutate(s_work_lookwrkpf = case_when(s_work == 0 ~ 0,
                                                     s_work == 1 ~ s_work_lookwrkpf,     
                                                     TRUE ~ s_work))
data %>% select('s_work_lookwrkpf') %>% map(table, exclude = NULL)

# work_satjob
data %>% select('r_work') %>% map(table, exclude = NULL)
data %>% select('r_work_satjob') %>% map(table, exclude = NULL)

data <- data %>% mutate(r_work_satjob = case_when(r_work == 0 ~ 0,
                                                  r_work == 1 ~ r_work_satjob,     
                                                  TRUE ~ r_work))
data %>% select('r_work_satjob') %>% map(table, exclude = NULL)

data %>% select('s_work') %>% map(table, exclude = NULL)
data %>% select('s_work_satjob') %>% map(table, exclude = NULL)

data <- data %>% mutate(s_work_satjob = case_when(s_work == 0 ~ 0,
                                                  s_work == 1 ~ s_work_satjob,     
                                                  TRUE ~ s_work))
data %>% select('s_work_satjob') %>% map(table, exclude = NULL)

# work_wdiscrim
data %>% select('r_work') %>% map(table, exclude = NULL)
data %>% select('r_work_wdiscrim') %>% map(table, exclude = NULL)

data <- data %>% mutate(r_work_wdiscrim = case_when(r_work == 0 ~ 0,
                                                    r_work == 1 ~ r_work_wdiscrim,     
                                                    TRUE ~ r_work))
data %>% select('r_work_wdiscrim') %>% map(table, exclude = NULL)

data %>% select('s_work') %>% map(table, exclude = NULL)
data %>% select('s_work_wdiscrim') %>% map(table, exclude = NULL)

data <- data %>% mutate(s_work_wdiscrim = case_when(s_work == 0 ~ 0,
                                                    s_work == 1 ~ s_work_wdiscrim,     
                                                    TRUE ~ s_work))
data %>% select('s_work_wdiscrim') %>% map(table, exclude = NULL)

# work_jobsum2
data %>% select('r_work') %>% map(table, exclude = NULL)
data %>% select('r_work_jobsum2') %>% map(table, exclude = NULL)

data <- data %>% mutate(r_work_jobsum2 = case_when(r_work == 0 ~ 0,
                                                   r_work == 1 ~ r_work_jobsum2,     
                                                   TRUE ~ r_work))
data %>% select('r_work_jobsum2') %>% map(table, exclude = NULL)

data %>% select('s_work') %>% map(table, exclude = NULL)
data %>% select('s_work_jobsum2') %>% map(table, exclude = NULL)

data <- data %>% mutate(s_work_jobsum2 = case_when(s_work == 0 ~ 0,
                                                   s_work == 1 ~ s_work_jobsum2,     
                                                   TRUE ~ s_work))
data %>% select('s_work_jobsum2') %>% map(table, exclude = NULL)

# work_jhours
data %>% select('r_work') %>% map(table, exclude = NULL)
data %>% select('r_work_jhours') %>% map(table, exclude = NULL)

data <- data %>% mutate(r_work_jhours = case_when(r_work == 0 ~ 0,
                                                  r_work == 1 ~ r_work_jhours,     
                                                  TRUE ~ r_work))
data %>% select('r_work_jhours') %>% map(table, exclude = NULL)

data %>% select('s_work') %>% map(table, exclude = NULL)
data %>% select('s_work_jhours') %>% map(table, exclude = NULL)

data <- data %>% mutate(s_work_jhours = case_when(s_work == 0 ~ 0,
                                                  s_work == 1 ~ s_work_jhours,     
                                                  TRUE ~ s_work))
data %>% select('s_work_jhours') %>% map(table, exclude = NULL)

# save data
saveRDS(data, file = "hrs_long_cleaned.rds")

# end

