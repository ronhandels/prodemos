
######################################## README ########################################

# Version 1.0.0

# For details see readme on https://www.github.com/ronhandels/prodemos



######################################## INSTALLATION GUIDE ########################################

# install packages
# install.packages("heemod") # remove '#' at beginning of the line and run once to install this package

# set working directory
# setwd("C:/Users/ron.handels/surfdrive/PhD/PAPERS/PRODEMOS cost-utility/model R") # if needed, change to the directory to the folder in which the R code and the life table folder is located
# setwd("~/GitHub/prodemos") # alternatively, when using GitHub one could use this to set the working directory




######################################## TECHNICAL PREPARATION ########################################

# clear
rm(list = ls()) # clear environment
cat("\014") # clear console

# Load libraries
library(heemod)



######################################## 1. DEFINE MODEL INPUTS ########################################

######################################## pre-model data manipulation ########################################

# initialize mortality table
df.r.mortality <- data.frame(age=c(50:100), m_CVD=NA, m_oth=NA, f_CVD=NA, f_oth=NA)

# import tables
## life table
df.r.lifetable <- as.data.frame(read.csv(file="data-raw/lifetable_UK.csv"))
df.r.lifetable$rate <- df.r.lifetable$rate / 100000
## prevalence table
df.prevalence <- as.data.frame(read.csv(file="data-raw/prevalence.csv"))
## proportion death related to CVD by age and sex
df.p.death_CVD <- as.data.frame(read.csv(file="data-raw/proportion_death_CVD.csv"))

# split risk
  # the health-economic model aims to simulate mortality rate in those without dementia and those with dementia (same for CHD and stroke)
  # the model multiplies the mortality rate in those without dementia with the rate ratio (RR) of death due to dementia (RR is the ratio of the mortality rate with dementia divided by the mortality rate without dementia, hazard ratio is 5.82)
  # the general population mortality rate includes persons with dementia
  # if the general population mortality rate is assumed to reflect those without dementia, the model would overestimate (i.e., double count) dementia mortality when the general population mortality rate is multiplied with the dementia mortality RR
  # the general population mortality rate is split to obtain the general population mortality rate for the subselection without dementia
  # a rate can be split using the formula by Gidwani and Russell in paragraph 4.2 [Gidwani, 2020: https://doi.org/10.1007/s40273-020-00937-z]
## split risk adjustment factor (acts as rate ratio for mortality rate without dementia relative to mortality rate in the general population, i.e., both with and without dementia combined)
adjustment_dem_m <- 1/(1 - df.prevalence$prevalence[df.prevalence$disease=="dem" & df.prevalence$sex=="male"] + df.prevalence$prevalence[df.prevalence$disease=="dem" & df.prevalence$sex=="male"]*5.823019)
adjustment_chd_m <- 1/(1 - df.prevalence$prevalence[df.prevalence$disease=="chd" & df.prevalence$sex=="male"] + df.prevalence$prevalence[df.prevalence$disease=="chd" & df.prevalence$sex=="male"]*1.429407)
adjustment_str_m <- 1/(1 - df.prevalence$prevalence[df.prevalence$disease=="str" & df.prevalence$sex=="male"] + df.prevalence$prevalence[df.prevalence$disease=="str" & df.prevalence$sex=="male"]*1.272421)
adjustment_dem_f <- 1/(1 - df.prevalence$prevalence[df.prevalence$disease=="dem" & df.prevalence$sex=="female"] + df.prevalence$prevalence[df.prevalence$disease=="dem" & df.prevalence$sex=="female"]*5.823019)
adjustment_chd_f <- 1/(1 - df.prevalence$prevalence[df.prevalence$disease=="chd" & df.prevalence$sex=="female"] + df.prevalence$prevalence[df.prevalence$disease=="chd" & df.prevalence$sex=="female"]*1.429407)
adjustment_str_f <- 1/(1 - df.prevalence$prevalence[df.prevalence$disease=="str" & df.prevalence$sex=="female"] + df.prevalence$prevalence[df.prevalence$disease=="str" & df.prevalence$sex=="female"]*1.272421)

# calculate mortality rate split for with and without dementia
df.r.mortality$m_CVD <- df.r.lifetable$rate[df.r.lifetable$sex=="male"] * df.p.death_CVD$proportion[df.p.death_CVD$sex=="male"] * adjustment_dem_m * adjustment_chd_m * adjustment_str_m
df.r.mortality$m_oth <- df.r.lifetable$rate[df.r.lifetable$sex=="male"] * (1-df.p.death_CVD$proportion[df.p.death_CVD$sex=="male"]) * adjustment_dem_m * adjustment_chd_m * adjustment_str_m
df.r.mortality$f_CVD <- df.r.lifetable$rate[df.r.lifetable$sex=="female"] * df.p.death_CVD$proportion[df.p.death_CVD$sex=="female"] * adjustment_dem_f * adjustment_chd_f * adjustment_str_f
df.r.mortality$f_oth <- df.r.lifetable$rate[df.r.lifetable$sex=="female"] * (1-df.p.death_CVD$proportion[df.p.death_CVD$sex=="female"]) * adjustment_dem_f * adjustment_chd_f * adjustment_str_f

# extend mortality table to allow different starting ages (for the model to run up to lifetime horizon operationalized as age 99)
df.r.mortality[52:151,"age"] <- 101:200
df.r.mortality[50:151,"m_CVD"] <- 1e-20
df.r.mortality[50:151,"m_oth"] <- 1e20
df.r.mortality[50:151,"f_CVD"] <- 1e-20
df.r.mortality[50:151,"f_oth"] <- 1e20

# treatment RR
## dementia
m.RR_Tx_dem <- matrix(
  data = c(
    RR = c(2.1,2.17,1.81,1.64,1.1,1.7), 
    prevalence_difference = c(-0.011,-0.023,-0.01,-0.067,0.031,-0.031), 
    RR_Tx = rep(NA,6)
  ), 
  nrow = 6, 
  ncol = 3, 
  dimnames = list(c("hypertension","obesity","hypercholesterolaemia","physicalinactivity","smokingpast","smokingcurrent"), c("RR","prevalence_difference","RR_Tx"))
)
## CVD male
m.RR_Tx_cvd_m <- matrix(
  data = c(
    RR = c(1.004,1.022,1.417), 
    prevalence_difference = c(-0.8,-0.21,-0.031), 
    RR_Tx = rep(NA,3)
  ), 
  nrow = 3, 
  ncol = 3, 
  dimnames = list(c("sbp","bmi","smokingcurrent"), c("RR","prevalence_difference","RR_Tx"))
)
## CVD female
m.RR_Tx_cvd_f <- matrix(
  data = c(
    RR = c(1.005,1.015,1.53), 
    prevalence_difference = c(-0.8,-0.21,-0.031), 
    RR_Tx = rep(NA,3)
  ), 
  nrow = 3, 
  ncol = 3, 
  dimnames = list(c("sbp","bmi","smokingcurrent"), c("RR","prevalence_difference","RR_Tx"))
)
## RR corresponding to change in proportion in risk factor (per risk factor)
m.RR_Tx_dem[,"RR_Tx"] <- m.RR_Tx_dem[,"RR"]^m.RR_Tx_dem[,"prevalence_difference"]
m.RR_Tx_cvd_m[,"RR_Tx"] <- m.RR_Tx_cvd_m[,"RR"]^m.RR_Tx_cvd_m[,"prevalence_difference"]
m.RR_Tx_cvd_f[,"RR_Tx"] <- m.RR_Tx_cvd_f[,"RR"]^m.RR_Tx_cvd_f[,"prevalence_difference"]
## RR combined effect on all risk factors
rr.dem_intervention <- prod(m.RR_Tx_dem[,"RR_Tx"])
rr.cvd_intervention_m <- prod(m.RR_Tx_cvd_m[,"RR_Tx"])
rr.cvd_intervention_f <- prod(m.RR_Tx_cvd_f[,"RR_Tx"])

# adherence
v.adherence <- rep(x=0, times=50)
v.adherence[1:11] <- 0.9^c(0:10)



######################################## within-model data manipulation ########################################

param <- define_parameters(
  
  # inputs
  ## age
  age_init = 57,#!# # starting age
  
  ## sex
  sex = "male",#!# # pick from "male", "female"
  
  ## initial state probabilities
  init_dem = 0,#!# 0, 
  init_chd = 
    ifelse(test = age_init==57 & sex=="male"  , yes = 0/54, no = 
    ifelse(test = age_init==57 & sex=="female", yes = 0/84, no = 
    ifelse(test = age_init==62 & sex=="male"  , yes = 7/53, no = 
    ifelse(test = age_init==62 & sex=="female", yes = 4/80, no = 
    ifelse(test = age_init==67 & sex=="male"  , yes = 7/72, no = 
    ifelse(test = age_init==67 & sex=="female", yes = 3/86, no = 
    ifelse(test = age_init==72 & sex=="male"  , yes = 9/68, no = 
    ifelse(test = age_init==72 & sex=="female", yes = 8/103, no = 
    0)))))))),#!#
  init_str = 
    ifelse(test = age_init==57 & sex=="male"  , yes = 3/54, no = 
    ifelse(test = age_init==57 & sex=="female", yes = 2/84, no = 
    ifelse(test = age_init==62 & sex=="male"  , yes = 3/53, no = 
    ifelse(test = age_init==62 & sex=="female", yes = 1/80, no = 
    ifelse(test = age_init==67 & sex=="male"  , yes = 6/72, no = 
    ifelse(test = age_init==67 & sex=="female", yes = 4/86, no = 
    ifelse(test = age_init==72 & sex=="male"  , yes = 3/67, no = 
    ifelse(test = age_init==72 & sex=="female", yes = 9/103, no = 
    0)))))))),#!#
  init_atr = 1 - init_dem - init_chd - init_str,#!#
  
  ## coefficients poisson regression for incidence
  ### dementia
  coef_dem_b0 = -12.39739,#!# # intercept
  coef_dem_age = 0.1076994,#!# # age 
  coef_dem_female = 0.1582646,#!# # female sex
  coef_dem_hisdem = 0,#!# # history of dementia
  coef_dem_hischd = 0,#!# # history of chd
  coef_dem_hisstr = 0.5373188,#!# # history of stroke
  ### chd
  coef_chd_b0 = -7.498702,#!# # etc.
  coef_chd_age = 0.0253612,#!# 
  coef_chd_female = 0.3890458,#!# 
  coef_chd_hisdem = 0,#!# 
  coef_chd_hischd = 2.027214,#!# # not use as this reflects a recurrent event, which is not reflected by this model
  coef_chd_hisstr = 0,#!# 
  ### stroke
  coef_str_b0 = -6.838972,#!# 
  coef_str_age = 0.0229375,#!# 
  coef_str_female = 0.1996352,#!# 
  coef_str_hisdem = 0,#!# 
  coef_str_hischd = 0.748028,#!# 
  coef_str_hisstr = 1.507333,#!# # not use as this reflects a recurrent event, which is not reflected by this model
  ### death related to cvd
  rr.dthcvd_hisdem = 5.823019,#!# 
  rr.dthcvd_hischd = 1.429407,#!# 
  rr.dthcvd_hisstr = 1.272421,#!# 
  ### death related to other
  rr.dthoth_hisdem = 5.823019,#!# 
  rr.dthoth_hischd = 1.429407,#!# 
  rr.dthoth_hisstr = 1.272421,#!# 
  
  ## relative risk target population compared to general population
  rr.dem_pop_male   = 1,#!# 4.285071924055,#!# 
  rr.dem_pop_female = 1,#!# 4.285071924055,#!# 
  rr.chd_pop_male   = 1,#!# 1.15562398984548,#!# 
  rr.chd_pop_female = 1,#!# 1.18808254743951,#!# 
  rr.str_pop_male   = 1,#!# 1.15562398984548,#!# 
  rr.str_pop_female = 1,#!# 1.18808254743951,#!# 
  
  ## intervention effect on incidence of disease
  rr.dem_intervention_male = rr.dem_intervention,#!#
  rr.dem_intervention_female = rr.dem_intervention,#!#
  rr.chd_intervention_male = rr.cvd_intervention_m,#!#
  rr.chd_intervention_female = rr.cvd_intervention_f,#!#
  rr.str_intervention_male = rr.cvd_intervention_m,#!#
  rr.str_intervention_female = rr.cvd_intervention_f,#!#
  
  ## utility general population
  coef_u_b0 = 0.9508566,#!#
  coef_u_sex = 0.0212126,#!#
  coef_u_age = -0.0002587,#!#
  coef_u_age2 = -0.0000332,#!#
  
  ## utility ratio
  u.ratio_demhis = 0.876184929,#!# 
  u.ratio_chdhis = 0.922977872,#!# 
  u.ratio_strhis = 0.83915869,#!# 
  
  ## costs
  c.dem_directmedical = 7973.337885467240,#!#
  c.dem_informal = 10028.479310580900,#!#
  c.chd_directmedical = 275.572481572482,#!#
  c.chd_informal = 0,#!#
  c.str_directmedical = 4116.072379522720,#!#
  c.str_informal = 6953.338500486480,#!#
  c.platform = 5,#!# 
  c.coaching = 800.012217536071,#!# 
  
  ## intervention
  intervention_duration = 11,#!#
  
  ## discount rate
  discount_effect = 0,#!# 
  discount_qaly = 0.035,#!# 
  discount_cost = 0.035,#!# 
  
  
  
  ######################################## pick input values (per cycle) ########################################
  
  ## age
  # FOR DEBUGGING: model_time = 1
  age = age_init + model_time - 1,#!# # age advances with model time (cycle length is 1 year)
  
  ## mortality rate at age and sex
  r.dthcvd = ifelse(test = sex=="male", yes = df.r.mortality[age - 49,"m_CVD"], no = df.r.mortality[age - 49,"f_CVD"]),#!#
  r.dthoth = ifelse(test = sex=="male", yes = df.r.mortality[age - 49,"m_oth"], no = df.r.mortality[age - 49,"f_oth"]),#!#
  
  ## parameter disease incidence coefficients at sex
  coef_dem_sex = ifelse(test = sex=="male", yes = 0, no = coef_dem_female),#!#
  coef_chd_sex = ifelse(test = sex=="male", yes = 0, no = coef_chd_female),#!#
  coef_str_sex = ifelse(test = sex=="male", yes = 0, no = coef_str_female),#!#
  ## parameter for relative risk disease incidence population at sex
  rr.dem_pop = ifelse(test = sex=="male", yes = rr.dem_pop_male, no = rr.dem_pop_female),#!#
  rr.chd_pop = ifelse(test = sex=="male", yes = rr.chd_pop_male, no = rr.chd_pop_female),#!#
  rr.str_pop = ifelse(test = sex=="male", yes = rr.str_pop_male, no = rr.str_pop_female),#!#
  ## parameter for relative risk intervention effect at sex
  rr.dem_intervention = ifelse(test = sex=="male", yes = rr.dem_intervention_male, no = rr.dem_intervention_female),#!#
  rr.chd_intervention = ifelse(test = sex=="male", yes = rr.chd_intervention_male, no = rr.chd_intervention_female),#!#
  rr.str_intervention = ifelse(test = sex=="male", yes = rr.str_intervention_male, no = rr.str_intervention_female),#!#
  
  ## adherence
  adherence = v.adherence[model_time],#!#
  
  ## utility by age and sex
  u.genpop = ifelse(test = sex=="male", coef_u_b0 + coef_u_sex*0 + coef_u_age*age + coef_u_age2*age^2, coef_u_b0 + coef_u_sex*1 + coef_u_age*age + coef_u_age2*age^2),#!#
  
  # difference between strategies
  ## effect intervention dementia incidence
  rr.dem_tx = dispatch_strategy(
    soc = 1,
    int = rr.dem_intervention^adherence
  ),#!#
  
  ## effect intervention chd incidence
  rr.chd_tx = dispatch_strategy(
    soc = 1,
    int = rr.chd_intervention^adherence
  ),#!#
  
  ## effect intervention stroke incidence
  rr.str_tx = dispatch_strategy(
    soc = 1,
    int = rr.str_intervention^adherence
  ),#!#
  
  ## costs intervention
  c.int_platform = dispatch_strategy(
    soc = 0,
    int = c.platform
  ),#!# 
  
  ## costs intervention
  c.int_coaching = dispatch_strategy(
    soc = 0,
    int = c.coaching
  ),#!# 
  
  # calculations
  ## disease rates multiplied with RR target population
  r.dem = exp(coef_dem_b0 + coef_dem_age * age + coef_dem_sex) * rr.dem_pop,#!# 
  r.chd = exp(coef_chd_b0 + coef_chd_age * age + coef_chd_sex) * rr.chd_pop,#!# 
  r.str = exp(coef_str_b0 + coef_str_age * age + coef_str_sex) * rr.str_pop,#!# 
  
  ## coefficient to relative risk
  ### dementia
  rr.dem_hisdem = exp(coef_dem_hisdem),#!# 
  rr.dem_hischd = exp(coef_dem_hischd),#!# 
  rr.dem_hisstr = exp(coef_dem_hisstr),#!# 
  ### chd
  rr.chd_hisdem = exp(coef_chd_hisdem),#!# 
  rr.chd_hischd = exp(coef_chd_hischd),#!# 
  rr.chd_hisstr = exp(coef_chd_hisstr),#!# 
  ### stroke
  rr.str_hisdem = exp(coef_str_hisdem),#!# 
  rr.str_hischd = exp(coef_str_hischd),#!# 
  rr.str_hisstr = exp(coef_str_hisstr),#!# 
  
  ## transition rates
  ### rates to deeath: RR disease history, convert rate to probability
  ### death-cvd
  atr_dthcvd       = rate_to_prob(r = r.dthcvd),#!# 
  dem_dthcvd       = rate_to_prob(r = r.dthcvd * rr.dthcvd_hisdem),#!# 
  chd_dthcvd       = rate_to_prob(r = r.dthcvd * rr.dthcvd_hischd),#!# 
  str_dthcvd       = rate_to_prob(r = r.dthcvd * rr.dthcvd_hisstr),#!#
  demchd_dthcvd    = rate_to_prob(r = r.dthcvd * rr.dthcvd_hisdem * rr.dthcvd_hischd),#!# 
  demstr_dthcvd    = rate_to_prob(r = r.dthcvd * rr.dthcvd_hisdem * rr.dthcvd_hisstr),#!# 
  chdstr_dthcvd    = rate_to_prob(r = r.dthcvd * rr.dthcvd_hischd * rr.dthcvd_hisstr),#!# 
  demchdstr_dthcvd = rate_to_prob(r = r.dthcvd * rr.dthcvd_hisdem * rr.dthcvd_hischd * rr.dthcvd_hisstr),#!# 
  #### death-other
  atr_dthoth       = rate_to_prob(r = r.dthoth),#!# 
  dem_dthoth       = rate_to_prob(r = r.dthoth * rr.dthoth_hisdem),#!# 
  chd_dthoth       = rate_to_prob(r = r.dthoth * rr.dthoth_hischd),#!# 
  str_dthoth       = rate_to_prob(r = r.dthoth * rr.dthoth_hisstr),#!#
  demchd_dthoth    = rate_to_prob(r = r.dthoth * rr.dthoth_hisdem * rr.dthoth_hischd),#!# 
  demstr_dthoth    = rate_to_prob(r = r.dthoth * rr.dthoth_hisdem * rr.dthoth_hisstr),#!# 
  chdstr_dthoth    = rate_to_prob(r = r.dthoth * rr.dthoth_hischd * rr.dthoth_hisstr),#!# 
  demchdstr_dthoth = rate_to_prob(r = r.dthoth * rr.dthoth_hisdem * rr.dthoth_hischd * rr.dthoth_hisstr),#!# 
  ### rates to disease states: incidence rate, RR disease history, RR intervention effect, conditional on survival, convert rate to probability
  #### from at-risk
    # # FOR DEBUGGING:
    # atr_dthcvd = 0,#!#
    # dem_dthcvd = 0,#!#
    # chd_dthcvd = 0,#!#
    # str_dthcvd = 0,#!#
    # demchd_dthcvd = 0,#!#
    # demstr_dthcvd = 0,#!#
    # chdstr_dthcvd = 0,#!#
    # atr_dthoth = 0,#!#
    # dem_dthoth = 0,#!#
    # chd_dthoth = 0,#!#
    # str_dthoth = 0,#!#
    # demchd_dthoth = 0,#!#
    # demstr_dthoth = 0,#!#
    # chdstr_dthoth = 0,#!#
    # # FOR DEBUGGING: rr.dem_tx = rr.chd_tx = rr.str_tx = 1
  atr_dem          = rate_to_prob(r = r.dem * rr.dem_tx) * (1 - atr_dthcvd - atr_dthoth),#!#
  atr_chd          = rate_to_prob(r = r.chd * rr.chd_tx) * (1 - atr_dthcvd - atr_dthoth),#!#
  atr_str          = rate_to_prob(r = r.str * rr.str_tx) * (1 - atr_dthcvd - atr_dthoth),#!#
  #### from dementia, chd, stroke
  dem_demchd       = rate_to_prob(r = r.chd * rr.chd_hisdem * rr.chd_tx) * (1 - dem_dthcvd - dem_dthoth),#!#
  dem_demstr       = rate_to_prob(r = r.str * rr.str_hisdem * rr.str_tx) * (1 - dem_dthcvd - dem_dthoth),#!#
  chd_demchd       = rate_to_prob(r = r.dem * rr.dem_hischd * rr.dem_tx) * (1 - chd_dthcvd - chd_dthoth),#!#
  chd_chdstr       = rate_to_prob(r = r.str * rr.str_hischd * rr.str_tx) * (1 - chd_dthcvd - chd_dthoth),#!#
  str_demstr       = rate_to_prob(r = r.dem * rr.dem_hisstr * rr.dem_tx) * (1 - str_dthcvd - str_dthoth),#!#
  str_chdstr       = rate_to_prob(r = r.chd * rr.chd_hisstr * rr.chd_tx) * (1 - str_dthcvd - str_dthoth),#!#
  #### from combined
  demchd_demchdstr = rate_to_prob(r = r.str * rr.str_hisdem * rr.str_hischd * rr.str_tx) * (1 - demchd_dthcvd - demchd_dthoth),#!#
  demstr_demchdstr = rate_to_prob(r = r.chd * rr.chd_hisdem * rr.chd_hisstr * rr.chd_tx) * (1 - demstr_dthcvd - demstr_dthoth),#!#
  chdstr_demchdstr = rate_to_prob(r = r.dem * rr.dem_hischd * rr.dem_hisstr * rr.dem_tx) * (1 - chdstr_dthcvd - chdstr_dthoth),#!#
  
  ## intervention costs
  c.int_platform = ifelse(test = model_time<(intervention_duration+1), yes = c.int_platform, no = 0),#!#
  c.int_coaching = ifelse(test = model_time<(intervention_duration+1), yes = c.int_coaching, no = 0),#!#
  
  ## discount rate
  r.discount_effect = 1 / (1 + discount_effect) ^ (model_time - 1),#!#
  r.discount_qaly = 1 / (1 + discount_qaly) ^ (model_time - 1),#!#
  r.discount_cost = 1 / (1 + discount_cost) ^ (model_time - 1)
)



######################################## 2. DECISION MODEL IMPLEMENTATION ########################################

######################################## transition probability matrix ########################################

# state names
statenames <- c("atr","dem","chd","str","demchd","demstr","chdstr","demchdstr","dthcvd","dthoth")

# transition probability matrix
transmat <- define_transition(
  state_names = statenames,
  C, atr_dem, atr_chd, atr_str, 0         , 0         , 0         , 0               , atr_dthcvd      , atr_dthoth, 
  0, C      , 0      , 0      , dem_demchd, dem_demstr, 0         , 0               , dem_dthcvd      , dem_dthoth, 
  0, 0      , C      , 0      , chd_demchd, 0         , chd_chdstr, 0               , chd_dthcvd      , chd_dthoth, 
  0, 0      , 0      , C      , 0         , str_demstr, str_chdstr, 0               , str_dthcvd      , str_dthoth, 
  0, 0      , 0      , 0      , C         , 0         , 0         , demchd_demchdstr, demchd_dthcvd   , demchd_dthoth, 
  0, 0      , 0      , 0      , 0         , C         , 0         , demstr_demchdstr, demstr_dthcvd   , demstr_dthoth, 
  0, 0      , 0      , 0      , 0         , 0         , C         , chdstr_demchdstr, chdstr_dthcvd   , chdstr_dthoth, 
  0, 0      , 0      , 0      , 0         , 0         , 0         , C               , demchdstr_dthcvd, demchdstr_dthoth, 
  0, 0      , 0      , 0      , 0         , 0         , 0         , 0               , 1               , 0, 
  0, 0      , 0      , 0      , 0         , 0         , 0         , 0               , 0               , 1
)



######################################## define states ########################################

atr <- define_state(
  utility = u.genpop * r.discount_qaly,
  cost_directmedical = (0) * r.discount_cost,
  cost_informal = (0) * r.discount_cost,
  cost_int_platform = (c.int_platform) * adherence * r.discount_cost,
  cost_int_coaching = (c.int_coaching) * adherence * r.discount_cost,
  cost = (cost_directmedical + cost_informal + cost_int_platform + cost_int_coaching)
)
dem <- define_state(
  utility = u.genpop * u.ratio_demhis * r.discount_qaly, 
  cost_directmedical = (c.dem_directmedical) * r.discount_cost, 
  cost_informal = (c.dem_informal) * r.discount_cost, 
  cost_int_platform = (0) * adherence * r.discount_cost, 
  cost_int_coaching = (0) * adherence * r.discount_cost, 
  cost = (cost_directmedical + cost_informal + cost_int_platform + cost_int_coaching)
)
chd <- define_state(
  utility = u.genpop * u.ratio_chdhis * r.discount_qaly, 
  cost_directmedical = (c.chd_directmedical) * r.discount_cost, 
  cost_informal = (c.chd_informal) * r.discount_cost, 
  cost_int_platform = (c.int_platform) * adherence * r.discount_cost, 
  cost_int_coaching = (c.int_coaching) * adherence * r.discount_cost, 
  cost = (cost_directmedical + cost_informal + cost_int_platform + cost_int_coaching)
)
str <- define_state(
  utility = u.genpop * u.ratio_strhis * r.discount_qaly, 
  cost_directmedical = (c.str_directmedical) * r.discount_cost, 
  cost_informal = (c.str_informal) * r.discount_cost, 
  cost_int_platform = (c.int_platform) * adherence * r.discount_cost, 
  cost_int_coaching = (c.int_coaching) * adherence * r.discount_cost, 
  cost = (cost_directmedical + cost_informal + cost_int_platform + cost_int_coaching)
)
demchd <- define_state(
  utility = u.genpop * u.ratio_demhis * u.ratio_chdhis * r.discount_qaly, 
  cost_directmedical = (c.dem_directmedical + c.chd_directmedical) * r.discount_cost, 
  cost_informal = (c.chd_informal + c.dem_informal) * r.discount_cost, 
  cost_int_platform = (0) * adherence * r.discount_cost, 
  cost_int_coaching = (0) * adherence * r.discount_cost, 
  cost = (cost_directmedical + cost_informal + cost_int_platform + cost_int_coaching)
)
demstr <- define_state(
  utility = u.genpop * u.ratio_demhis * u.ratio_strhis * r.discount_qaly, 
  cost_directmedical = (c.dem_directmedical + c.str_directmedical) * r.discount_cost, 
  cost_informal = (c.str_informal + c.dem_informal) * r.discount_cost, 
  cost_int_platform = (0) * adherence * r.discount_cost, 
  cost_int_coaching = (0) * adherence * r.discount_cost, 
  cost = (cost_directmedical + cost_informal + cost_int_platform + cost_int_coaching)
)
chdstr <- define_state(
  utility = u.genpop * u.ratio_chdhis * u.ratio_strhis * r.discount_qaly, 
  cost_directmedical = (c.chd_directmedical + c.str_directmedical) * r.discount_cost,
  cost_informal = (c.chd_informal + c.str_informal) * r.discount_cost,
  cost_int_platform = (c.int_platform) * adherence * r.discount_cost,
  cost_int_coaching = (c.int_coaching) * adherence * r.discount_cost,
  cost = (cost_directmedical + cost_informal + cost_int_platform + cost_int_coaching)
)
demchdstr <- define_state(
  utility = u.genpop * u.ratio_demhis * u.ratio_chdhis * u.ratio_strhis * r.discount_qaly, 
  cost_directmedical = (c.dem_directmedical + c.chd_directmedical + c.str_directmedical) * r.discount_cost, 
  cost_informal = (c.chd_informal + c.str_informal + c.dem_informal) * r.discount_cost, 
  cost_int_platform = (0) * adherence * r.discount_cost, 
  cost_int_coaching = (0) * adherence * r.discount_cost, 
  cost = (cost_directmedical + cost_informal + cost_int_platform + cost_int_coaching)
)
dthcvd <- define_state(
  utility = 0, 
  cost = 0, 
  cost_directmedical = 0, 
  cost_informal = 0, 
  cost_int_platform = 0,
  cost_int_coaching = 0
)
dthoth <- define_state(
  utility = 0, 
  cost = 0, 
  cost_directmedical = 0, 
  cost_informal = 0, 
  cost_int_platform = 0,
  cost_int_coaching = 0
)



######################################## initialize model ########################################

inits <- define_init(
  atr = init_atr, 
  dem = init_dem, 
  chd = init_chd, 
  str = init_str, 
  demchd = 0, 
  demstr = 0, 
  chdstr = 0, 
  demchdstr = 0, 
  dthcvd = 0, 
  dthoth = 0
)



######################################## define strategies ########################################

strat <- define_strategy(
  atr = atr, 
  dem = dem, 
  chd = chd, 
  str = str, 
  demchd = demchd, 
  demstr = demstr, 
  chdstr = chdstr, 
  demchdstr = demchdstr, 
  dthcvd = dthcvd, 
  dthoth = dthoth, 
  transition = transmat
)



######################################## 3. CALIBRATION ########################################

# n/a



######################################## 4. ANALYSIS ########################################

# run model with single starting age and sex
res_mod_hom <- run_model(
  soc = strat,
  int = strat,
  parameters = param,
  init = inits,
  cycles = 50,
  cost = cost,
  effect = utility,
  method = 'beginning'
)

# prepare heterogeneous model inputs
tab_indiv = data.frame(
  age_init = c(57,57,62,62,67,67,72,72),
  sex = c("male","female","male","female","male","female","male","female"),
  .weights = c(54/600, 84/600, 53/600, 80/600, 72/600, 86/600, 68/600, 103/600)
)

# run model with weighted set of starting ages and sexes
res_mod_het <- update(object = res_mod_hom, newdata = tab_indiv)

# results
## overall
res_mod_het

## counts in data.frame
counts <- plot(res_mod_het, type = "counts")$data
counts_soc <- counts[counts$.strategy_name=="soc",]
counts_soc <- reshape(data = counts_soc[,c("model_time","state_names","count")], idvar = "model_time", timevar = "state_names", direction = "wide")
counts_int <- counts[counts$.strategy_name=="int",]
counts_int <- reshape(data = counts_int[,c("model_time","state_names","count")], idvar = "model_time", timevar = "state_names", direction = "wide")

## person-years with and without disease and alive
sum(counts_soc[,c("count.dem","count.demchd","count.demstr","count.demchdstr")]) # with dem
sum(counts_soc[,c("count.chd","count.demchd","count.chdstr","count.demchdstr")]) # with chd
sum(counts_soc[,c("count.str","count.demstr","count.chdstr","count.demchdstr")]) # with str
sum(counts_soc[,c("count.atr","count.chd","count.str","count.chdstr")]) # without dem
sum(counts_soc[,c("count.atr","count.dem","count.str","count.demstr")]) # without chd
sum(counts_soc[,c("count.atr","count.dem","count.chd","count.demchd")]) # without str
sum(counts_soc[,c("count.atr","count.dem","count.chd","count.str","count.demchd","count.demstr","count.chdstr","count.demchdstr")]) # alive
sum(counts_int[,c("count.dem","count.demchd","count.demstr","count.demchdstr")])
sum(counts_int[,c("count.chd","count.demchd","count.chdstr","count.demchdstr")])
sum(counts_int[,c("count.str","count.demstr","count.chdstr","count.demchdstr")])
sum(counts_int[,c("count.atr","count.chd","count.str","count.chdstr")])
sum(counts_int[,c("count.atr","count.dem","count.str","count.demstr")])
sum(counts_int[,c("count.atr","count.dem","count.chd","count.demchd")])
sum(counts_int[,c("count.atr","count.dem","count.chd","count.str","count.demchd","count.demstr","count.chdstr","count.demchdstr")])

## QALYs and costs
summary(res_mod_het)$sum_comb$res_values

## net health benefit
nhb_soc <- summary(res_mod_het)$sum_comb$res_values$utility[1] - summary(res_mod_het)$sum_comb$res_values$cost[1] / 20000
nhb_int <- summary(res_mod_het)$sum_comb$res_values$utility[2] - summary(res_mod_het)$sum_comb$res_values$cost[2] / 20000
inhb <- nhb_int - nhb_soc
print(inhb, digits=10) # -0.1903244151

## ICER
print(format(summary(res_mod_het)$sum_comb$res_comp$.icer[2], digits=10)) # "249780.2897"



######################################## 5. VALIDATION ########################################

# This part is for developers only and requires orginal Excel-based results



######################################## homogeneous ########################################

# # install packages
# install.packages("readxl") # remove '#' at beginning of the line and run once to install this package
# packageVersion("readxl")
# 
# # homogeneous model type (i.e., single starting age and sex)
# 
# # in excel run scenario "compare to R"
# 
# # compare transition probability matrix for mortality (not other transitions as in R they are conditional on survival, in excel this is done integrated in the TP table)
# ## soc: R
# l.TP_soc <- res_mod_hom$eval_strategy_list$soc$transition
# for (i in 1:length(l.TP_soc)) colnames(l.TP_soc[[i]]) <- rownames(l.TP_soc[[i]]) <- statenames
# ## soc: excel
# m.TP_soc_excel <- as.matrix(read_excel(path="internal validation (2025-01-06)/TP_soc homogeneous.xlsx", col_names=FALSE))
# a.TP_soc_excel <- array(data = m.TP_soc_excel, dim = c(51,10,10))
# ## soc: difference (mortality only)
# format(l.TP_soc[[1]][,9] - a.TP_soc_excel[1,,9], digits=15) # should be 0
# format(l.TP_soc[[1]][,10] - a.TP_soc_excel[1,,10], digits=15) # should be 0
# format(l.TP_soc[[10]][,9] - a.TP_soc_excel[10,,9], digits=15) # should be 0
# ## int: R
# l.TP_int <- res_mod_hom$eval_strategy_list$int$transition
# for (i in 1:length(l.TP_int)) colnames(l.TP_int[[i]]) <- rownames(l.TP_int[[i]]) <- statenames
# print(l.TP_int[[1]])
# ## int: excel
# m.TP_int_excel <- as.matrix(read_excel(path="internal validation (2025-01-06)/TP_int homogeneous.xlsx", col_names=FALSE))
# a.TP_int_excel <- array(data = m.TP_int_excel, dim = c(51,10,10))
# print(a.TP_int_excel[1,,])
# ## soc: difference (mortality only)
# format(l.TP_int[[1]][,9] - a.TP_int_excel[1,,9], digits=15) # should be 0
# format(l.TP_int[[1]][,10] - a.TP_int_excel[1,,10], digits=15) # should be 0
# format(l.TP_int[[10]][,9] - a.TP_int_excel[10,,9], digits=15) # should be 0
# 
# # compare state trace
# ## compare state trace: soc
# m.trace_r_hom_soc <- as.matrix(res_mod_hom$eval_strategy_list$soc$counts)
# m.trace_excel_hom_soc <- as.data.frame(read_excel(path="internal validation (2025-01-06)/statetrace_excel_soc homogeneous.xlsx", col_names=TRUE))
# m.trace_hom_soc_dif <- abs(m.trace_r_hom_soc[1:43,] - m.trace_excel_hom_soc[1:43,]) # compare only first 43 rows to match health-economic outcomes truncated to time horizon up to age 99 in excel
# format(m.trace_hom_soc_dif, digits=15)
# print(range(m.trace_hom_soc_dif, na.rm=TRUE)) # should be 0 or very small number
# ## compare state trace: int
# m.trace_r_hom_int <- as.matrix(res_mod_hom$eval_strategy_list$int$counts)
# m.trace_excel_hom_int <- as.data.frame(read_excel(path="internal validation (2025-01-06)/statetrace_excel_int homogeneous.xlsx", col_names=TRUE))
# m.trace_hom_int_dif <- abs(m.trace_r_hom_int[1:43,] - m.trace_excel_hom_int[1:43,]) # compare only first 43 rows to match health-economic outcomes truncated to time horizon up to age 99 in excel
# format(m.trace_hom_int_dif, digits=15)
# print(range(m.trace_hom_int_dif, na.rm=TRUE)) # should be 0 or very small number
# 
# # compare health-economic outcomes
# res_mod_hom$eval_strategy_list$soc$values # manually compare to excel
# res_mod_hom$eval_strategy_list$int$values # manually compare to excel
# res_mod_hom # manually compare to excel (FYI: excel health-economic outcomes are truncated to time horizon age 99, same as time horizon in R)
# format(summary(res_mod_hom)$res_comp[2,".icer"], digits=10)



######################################## heterogeneous ########################################

# # in excel run scenario "compare to R"
# 
# # compare state trace
# ## soc
# m.trace_r_hom_soc <- counts_soc
# m.trace_e_hom_soc <- as.data.frame(read_excel(path="D:/surfdrive virtual/PhD/PAPERS/PRODEMOS cost-utility/model excel/internal validation (2025-03-07)/statetrace_excel_soc heterogeneous.xlsx", col_names=TRUE))
# m.trace_hom_soc_dif <- abs(m.trace_r_hom_soc[1:28,2:11] - m.trace_e_hom_soc[1:28,]) # compare only first 28 years because excel starting age 57 runs state trace up to age 101 (FYI: health-economic outcomes are run up to age 99) and R runs up to age 99 (=28 years)
# format(m.trace_hom_soc_dif, digits=15)
# print(range(m.trace_hom_soc_dif, na.rm=TRUE)) # should be 0 or very small number
# ## int
# m.trace_r_hom_int <- counts_int
# m.trace_e_hom_int <- as.data.frame(read_excel(path="D:/surfdrive virtual/PhD/PAPERS/PRODEMOS cost-utility/model excel/internal validation (2025-03-07)/statetrace_excel_int heterogeneous.xlsx", col_names=TRUE))
# m.trace_hom_int_dif <- abs(m.trace_r_hom_int[1:28,2:11] - m.trace_e_hom_int[1:28,]) # compare only first 28 years because excel starting age 57 runs state trace up to age 101 (FYI: health-economic outcomes are run up to age 99) and R runs up to age 99 (=28 years)
# format(m.trace_hom_int_dif, digits=15)
# print(range(m.trace_hom_int_dif, na.rm=TRUE)) # should be 0 or very small number
# 
# # compare health-economic outcomes
# # (see earlier "person-years with and without disease and alive")
# summary(res_mod_het)$sum_comb$res_values # manually compare to excel (FYI: excel health-economic outcomes are truncated to time horizon age 99, same as time horizon in R)



######################################## OTHER ########################################

# # possible results to be generated
# str(res_mod_hom, max.level=2)
# head(get_counts(res_mod_hom))
# head(get_values(res_mod_hom))
# res_mod_hom$eval_strategy_list$soc$parameters$r.dthcvd # mortality rate
# res_mod_hom$eval_strategy_list$soc$transition[] # transition probability matrix by cycle
# res_mod_hom$eval_strategy_list$soc$counts # counts (i.e., state trace)
# res_mod_hom$eval_strategy_list$soc$values # values (i.e., health-economic outcomes)
# plot(res_mod_hom)
