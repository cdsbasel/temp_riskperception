# DESCRIPTION -------------------------------------------------------------

# In this script we fit the MASC model (Anusic & Schimmack, 2016 JPSP). 
# We also specify the dataset used to fit the MASC model and create new variables where required.

# Author(s): Amanda Holzer(1), Arzie Bajrami(1), Rui Mata(1)
# (1)Centre for Cognitive and Decision Sciences, Faculty of Psychology, University of Basel.

# PACKAGES ---------------------------------------------------------------
#install.packages("brms")
install.packages("cmdstanr", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))
#install.packages("loo")
#install.packages("bayesplot")
#install.packages("tidybayes")
#install.packages("posterior")
#install.packages("janitor")
#install.packages("openxlsx")
#install.packages("metafor")
#install.packages("tidyverse")
#install.packages("here")
#install.packages("readxl")

library(tidyverse)
library(brms)
library(cmdstanr)
library(loo)
library(bayesplot)
library(tidybayes)
library(posterior)
library(here)
library(janitor)
library(openxlsx)
library(metafor)
cmdstanr::install_cmdstan()
library(cmdstanr)
library(readxl)

# PATH ---------------------------------------------------------------
#set working directory
setwd(here())
getwd()

# DATA ---------------------------------------------------------------
#Prepare data file to get only relevant studies, i.e. studies with correlations
df_prep <- read_csv("data/final.csv")


#cleaning columns names
df_prep <- clean_names(df_prep)


selected_columns <- select(df_prep, x1, author, paper_title, study_design, risk_1:risk_5, 
                           intervention_yesno_1:intervention_yesno_5, temporal_analysis_1:temporal_analysis_5,
                           exposure_yesno_1:exposure_yesno_5, health, nature, crime, finance, 
                           nuclear, political, social, 
                           correlation_results_1_1:correlation_results_1_8, correlation_results_1:correlation_results_5, 
                           icc_results_1_1, test_retest_interval_1:test_retest_interval_5, 
                           type_participants_1:type_participants_5, sample_category_1:sample_category_5, 
                           age_category_1:age_category_5, country_1:country_5, sample_size_1:sample_size_5, 
                           prop_female_1:prop_female_5, mean_age_1:mean_age_5, sd_age_1:sd_age_5)


# Pivot the selected columns
df_pivoted <- selected_columns %>%
  mutate_all(as.character()) %>%
  mutate_if(is.logical, as.character) %>%
  mutate_if(is.double, as.character) %>%
  pivot_longer(-c(x1, author, paper_title, study_design),  # Columns to exclude from pivot
               names_to = "var",
               values_to = "val")

# Filter out rows with NA values in the val column
df_filtered<- df_pivoted %>%
  filter(!is.na(val))

filtered_rows <- df_filtered %>%
  filter(var %in% paste0("temporal_analysis_", 1:5), val == 1)

# Group by ID variable and keep only those groups where all temporal_analysis values have val = 1
filtered_groups <- filtered_rows %>%
  group_by(x1) %>%
  filter(all(val == 1))

# Keep only x1 rows that satisfy the condition
df_filtered <- df_filtered %>%
  filter(x1 %in% filtered_groups$x1)


# Separate the var column into correlation_name and correlation_value
df_cor_results <- df_filtered %>%
  filter(str_detect(var, "correlation_results")) %>%
  separate(var, into = c("cor_name"), sep = ":")%>%
  rename(cor_val = val)

df_interval <- df_filtered %>%
  filter(str_detect(var, "test_retest_interval")) %>%
  separate(var, into = c("interval_name"), sep = ":") %>%
  rename(interval_val = val)%>%
  select(-author, -paper_title, -study_design)


# Add correlation interval
cor_num <- gsub("correlation_results_", "", df_cor_results$cor_name)

modified_interval_names <- paste0("test_retest_interval_", cor_num)

df_cor_results$interval_name <- modified_interval_names

df_cor <- merge(df_cor_results, df_interval, by = c("interval_name", "x1"), all.x = TRUE)

df_cor <- df_cor %>% select(-interval_name)

df_cor$cor_val <- as.numeric(df_cor$cor_val)

#write.xlsx(df_cor, file = "data/cor.xlsx")

####The rest of the data such as risk formulation, interval, sample size, age, female percentage, domain, subdomain, multi/single item and Intervention/exposure were manually entered from the raw data in excel. 
####

## IMPORT CORRELATION FILE
df <- read_excel("data/cor_final.xlsx")

# MEAN IMPUTATION FOR MISSING DATA
# Age
mean_age <- mean(df$age, na.rm=T)
mean_age
i_NA_age <- which(is.na(df$age) == TRUE)
df$age[i_NA_age] <- mean_age
mean_age == mean(df$age)

# Female percentage 
mean_female <- mean(df$female, na.rm=T)
mean_female
i_NA_female <- which(is.na(df$female) == TRUE)
df$female[i_NA_female] <- mean_female
mean_female == mean(df$female)


# MANIPULATE DATA
mean(df$female, na.rm=T)
mean(df$age, na.rm=T)
df$interval_val <- df$interval_val/365
plot(df$interval_val, df$cor_val)

df <- df %>% mutate(cor_val = if_else(cor_val <0,0, cor_val), 
                    age_dec_c = (age-40)/10,
                    age_dec_c2 = age_dec_c^2,
                    female_c = (female-0.50))

df <- escalc(measure = "COR", ri=cor_val, ni=n, data=df)
df$sei <- sqrt(df$vi)

# MODEL 1 FITTING WITH CORRELATION, INTERVAL AND SIZE--------------------------------------------------------
family <- brmsfamily(
  family = "student",
  link = "identity"
)

# Define the formula
formula_m1 <- bf(
  cor_val| resp_se(sei, sigma = TRUE) ~ rel * (change * ((stabch^interval_val) - 1) + 1),
  nlf(rel ~ inv_logit(logitrel)),
  nlf(change ~ inv_logit(logitchange)),
  nlf(stabch ~ inv_logit(logitstabch)),
  logitrel ~ 1 + n,
  logitchange ~ 1 + n,
  logitstabch ~ 1 +n,
  nl = TRUE
)

# Define the weakly informative priors
priors <-
  prior(normal(0,1), nlpar="logitrel", class = "b") +
  prior(normal(0,1), nlpar="logitchange", class = "b") +
  prior(normal(0,1), nlpar="logitstabch", class = "b") +
  prior(cauchy(0,1), class = "sigma")

# Fit the model
fit_masc_m1 <- brm(
  formula = formula_m1,
  prior = priors,
  family = family,
  data = df,
  cores = 2,
  chains = 2,
  iter = 6000,
  warmup = 2000,
  # backend = "cmdstanr",
  control = list(max_treedepth = 10, adapt_delta = 0.95),
  seed = 1299
)


# MODEL 1 EVAL: MCMC DIAGNOSTICS -------------------------------------------------------

# model summary 
fit_masc_m1

#plot conditional effects
plot(conditional_effects(fit_masc_m1), points=T)

# trace plots & param. estimates
plot(fit_masc_m1, N = 5, ask = TRUE)



# MODEL 1 EVAL: PP CHECKS -------------------------------------------------------
summary(fit_masc_m1)

# simulations vs. obs: Overall
pp_check(fit_masc_m1,
         type ="dens_overlay",
         ndraws = 100)


pp_check(fit_masc_m1,
         type ="stat",
         stat = "mean",
         ndraws = 1000,
         binwidth = .001)

pp_check(fit_masc_m1,
         type ="stat",
         stat = "sd",
         ndraws = 500,
         binwidth = .001)

pp_check(fit_masc_m1,
         type ="stat",
         stat = "median",
         ndraws = 500,
         binwidth = .001)

pp_check(fit_masc_m1,
         type ="stat",
         stat = "mad",
         ndraws = 500,
         binwidth = .001)

pp_check(fit_masc_m1,
         type ="stat_2d")


pp_check(fit_masc_m1,
         type ="scatter_avg")


# MODEL 1 EVAL: LOO --------------------------------------------------------

# loo & pareto K
model_loo <- loo(fit_masc_m1, save_psis = TRUE, cores = 2)
plot(model_loo, diagnostic = "k")
plot(model_loo, diagnostic = "n_eff")

# loo pit
w <- weights(model_loo$psis_object)
ppc_loo_pit_overlay(y = fit_masc_m1$data$cor_val, 
                    yrep = posterior_predict(fit_masc_m1), 
                    lw = w)
ppc_loo_pit_qq(y = fit_masc_m1$data$cor_val, 
               yrep = posterior_predict(fit_masc_m1), 
               lw = w)



# MODEL 2 FITTING WITH DEMOGRAPHICS: AGE, AGE2, GENDER---------------------------------------------------------------
family <- brmsfamily(
  family = "student",
  link = "identity"
)

# Define the formula

formula_m2 <- bf(
  cor_val| resp_se(sei, sigma = TRUE) ~ rel * (change * ((stabch^interval_val) - 1) + 1),
  nlf(rel ~ inv_logit(logitrel)),
  nlf(change ~ inv_logit(logitchange)),
  nlf(stabch ~ inv_logit(logitstabch)),
  logitrel ~ 1 + age_dec_c + age_dec_c2  + female_c,
  logitchange ~ 1 + age_dec_c + age_dec_c2 + female_c,
  logitstabch ~ 1 + age_dec_c+ age_dec_c2  + female_c,
  nl = TRUE
)


# Define the weakly informative priors
priors <-
  prior(normal(0,1), nlpar="logitrel", class = "b") +
  prior(normal(0,1), nlpar="logitchange", class = "b") +
  prior(normal(0,1), nlpar="logitstabch", class = "b") +
  prior(cauchy(0,1), class = "sigma")

# Fit the model
fit_masc_m2 <- brm(
  formula = formula_m2,
  prior = priors,
  family = family,
  data = df,
  cores = 2,
  chains = 2,
  iter = 6000,
  warmup = 2000,
  # backend = "cmdstanr",
  control = list(max_treedepth = 10, adapt_delta = 0.95),
  seed = 1299
)


# MODEL 2 EVAL: MCMC DIAGNOSTICS --------------------------------------------------------

# model summary 
fit_masc_m2

#plot conditional effects
plot(conditional_effects(fit_masc_m2), points=T)

# trace plots & param. estimates
plot(fit_masc_m2 , N = 5, ask = TRUE)


# MODEL 2 EVAL: PP CHECKS --------------------------------------------------------
summary(fit_masc_m2 )           

# simulations vs. obs: Overall
pp_check(fit_masc_m2 ,
         type ="dens_overlay",
         ndraws = 100)


pp_check(fit_masc_m2 ,
         type ="stat",
         stat = "mean",
         ndraws = 1000,
         binwidth = .001)

pp_check(fit_masc_m2 ,
         type ="stat",
         stat = "sd",
         ndraws = 500,
         binwidth = .001)

pp_check(fit_masc_m2 ,
         type ="stat",
         stat = "median",
         ndraws = 500,
         binwidth = .001)

pp_check(fit_masc_m2 ,
         type ="stat",
         stat = "mad",
         ndraws = 500,
         binwidth = .001)

pp_check(fit_masc_m2 ,
         type ="stat_2d")


pp_check(fit_masc_m2 ,
         type ="scatter_avg")


# MODEL 2 EVAL: LOO --------------------------------------------------------

# loo & pareto K
model_loo <- loo(fit_masc_m2 , save_psis = TRUE, cores = 2)
plot(model_loo, diagnostic = "k")
plot(model_loo, diagnostic = "n_eff")

# loo pit
w <- weights(model_loo$psis_object)
ppc_loo_pit_overlay(y = fit_masc_m2 $data$cor_val, 
                    yrep = posterior_predict(fit_masc_m2 ), 
                    lw = w)
ppc_loo_pit_qq(y = fit_masc_m2 $data$cor_val, 
               yrep = posterior_predict(fit_masc_m2 ), 
               lw = w)


# MODEL 3 FITTING WITH MEASUREMENT CHARACTERISTICS: DOMAIN, ITEM, EVENT---------------------------------------------------------------
family <- brmsfamily(
  family = "student",
  link = "identity"
)

# Define the formula

formula_m3 <- bf(
  cor_val| resp_se(sei, sigma = TRUE) ~ rel * (change * ((stabch^interval_val) - 1) + 1),
  nlf(rel ~ inv_logit(logitrel)),
  nlf(change ~ inv_logit(logitchange)),
  nlf(stabch ~ inv_logit(logitstabch)),
  logitrel ~ 1 + domain + item + event,
  logitchange ~ 1 + domain + item + event,
  logitstabch ~ 1 + domain + item + event,
  nl = TRUE
)



# Define the weakly informative priors
priors <-
  prior(normal(0,1), nlpar="logitrel", class = "b") +
  prior(normal(0,1), nlpar="logitchange", class = "b") +
  prior(normal(0,1), nlpar="logitstabch", class = "b") +
  prior(cauchy(0,1), class = "sigma")

# Fit the model
fit_masc_m3 <- brm(
  formula = formula_m3,
  prior = priors,
  family = family,
  data = df,
  cores = 2,
  chains = 2,
  iter = 6000,
  warmup = 2000,
  # backend = "cmdstanr",
  control = list(max_treedepth = 10, adapt_delta = 0.95),
  seed = 1299
)

# MODEL 3 EVAL: MCMC DIAGNOSTICS --------------------------------------------------------

# model summary 
fit_masc_m3

#plot conditional effects
plot(conditional_effects(fit_masc_m3), points=T)

# trace plots & param. estimates
plot(fit_masc_m3 , N = 5, ask = TRUE)


# MODEL 3 EVAL: PP CHECKS --------------------------------------------------------
summary(fit_masc_m3 )

# simulations vs. obs: Overall
pp_check(fit_masc_m3 ,
         type ="dens_overlay",
         ndraws = 100)


pp_check(fit_masc_m3 ,
         type ="stat",
         stat = "mean",
         ndraws = 1000,
         binwidth = .001)

pp_check(fit_masc_m3 ,
         type ="stat",
         stat = "sd",
         ndraws = 500,
         binwidth = .001)

pp_check(fit_masc_m3 ,
         type ="stat",
         stat = "median",
         ndraws = 500,
         binwidth = .001)

pp_check(fit_masc_m3 ,
         type ="stat",
         stat = "mad",
         ndraws = 500,
         binwidth = .001)

pp_check(fit_masc_m3 ,
         type ="stat_2d")


pp_check(fit_masc_m3 ,
         type ="scatter_avg")



# MODEL 3 EVAL: LOO --------------------------------------------------------

# loo & pareto K
model_loo <- loo(fit_masc_m3 , save_psis = TRUE, cores = 2)
plot(model_loo, diagnostic = "k")
plot(model_loo, diagnostic = "n_eff")

# loo pit
w <- weights(model_loo$psis_object)
ppc_loo_pit_overlay(y = fit_masc_m3$data$cor_val, 
                    yrep = posterior_predict(fit_masc_m3 ), 
                    lw = w)
ppc_loo_pit_qq(y = fit_masc_m3$data$cor_val, 
               yrep = posterior_predict(fit_masc_m3 ), 
               lw = w)


# MODEL EVAL: LOO- COMPARISON OF ALL MODEL ----------------------------------------------------
loo1 <- loo(fit_masc_m1)

loo2 <- loo(fit_masc_m2)

loo3 <- loo(fit_masc_m3)


