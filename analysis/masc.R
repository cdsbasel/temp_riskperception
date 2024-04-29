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


color_scheme_set("teal")

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


# Attempt to pivot the selected columns
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


#add correlation interval
cor_num <- gsub("correlation_results_", "", df_cor_results$cor_name)

modified_interval_names <- paste0("test_retest_interval_", cor_num)

df_cor_results$interval_name <- modified_interval_names

df_cor <- merge(df_cor_results, df_interval, by = c("interval_name", "x1"), all.x = TRUE)

df_cor <- df_cor %>% select(-interval_name)

df_cor$cor_val <- as.numeric(df_cor$cor_val)

#write.xlsx(df_cor, file = "data/cor.xlsx")

####
#The rest of the data such as missing interval, sample size, age, female percentage and domain were manually entered from the raw data in excel. 
####

#import data
df <- read_excel("data/cor_final.xlsx")

#manipulate data
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

# 1 MODEL FITTING WITH PREDICTORS AGE, AGE2, GENDER, AUTHOR---------------------------------------------------------------
family <- brmsfamily(
  family = "student",
  link = "identity"
)

# Define the formula

formula_age_female_author <- bf(
  cor_val| resp_se(sei, sigma = TRUE) ~ rel * (change * ((stabch^interval_val) - 1) + 1),
  nlf(rel ~ inv_logit(logitrel)),
  nlf(change ~ inv_logit(logitchange)),
  nlf(stabch ~ inv_logit(logitstabch)),
  logitrel ~ 1 + age_dec_c + age_dec_c2 + female_c + (1|author),
  logitchange ~ 1 + age_dec_c + age_dec_c2 + female_c+ (1|author),
  logitstabch ~ 1 + age_dec_c+ age_dec_c2  + female_c+ (1|author),
  nl = TRUE
)



# Define the weakly informative priors
priors <-
  prior(normal(0,1), nlpar="logitrel", class = "b") +
  prior(normal(0,1), nlpar="logitchange", class = "b") +
  prior(normal(0,1), nlpar="logitstabch", class = "b") +
  prior(cauchy(0,1), class = "sigma")

# Fit the model
fit_masc_age_female_author <- brm(
  formula = formula_age_female_author,
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



# 1 MODEL EVAL: MCMC DIAGNOSTICS  WITH PREDICTORS AGE, AGE2, GENDER, AUTHOR --------------------------------------------------------

# model summary 
fit_masc_age_female_author

#plot conditional effects
plot(conditional_effects(fit_masc_age_female_author), points=T)

# trace plots & param. estimates
plot(fit_masc_age_female_author, N = 5, ask = TRUE)



# 1 MODEL EVAL: PP CHECKS  WITH PREDICTORS AGE, AGE2, GENDER, AUTHOR--------------------------------------------------------
summary(fit_masc_age_female_author)

# simulations vs. obs: Overall
pp_check(fit_masc_age_female_author,
         type ="dens_overlay",
         ndraws = 100)


pp_check(fit_masc_age_female_author,
         type ="stat",
         stat = "mean",
         ndraws = 1000,
         binwidth = .001)

pp_check(fit_masc_age_female_author,
         type ="stat",
         stat = "sd",
         ndraws = 500,
         binwidth = .001)

pp_check(fit_masc_age_female_author,
         type ="stat",
         stat = "median",
         ndraws = 500,
         binwidth = .001)

pp_check(fit_masc_age_female_author,
         type ="stat",
         stat = "mad",
         ndraws = 500,
         binwidth = .001)

pp_check(fit_masc_age_female_author,
         type ="stat_2d")


pp_check(fit_masc_age_female_author,
         type ="scatter_avg")


# 1 MODEL EVAL: LOO --------------------------------------------------------

# loo & pareto K
model_loo <- loo(fit_masc_age_female_author, save_psis = TRUE, cores = 2)
plot(model_loo, diagnostic = "k")
plot(model_loo, diagnostic = "n_eff")

# loo pit
w <- weights(model_loo$psis_object)
ppc_loo_pit_overlay(y = fit_masc_age_female_author$data$cor_val, 
                    yrep = posterior_predict(fit_masc_age_female_author), 
                    lw = w)
ppc_loo_pit_qq(y = fit_masc_age_female_author$data$cor_val, 
               yrep = posterior_predict(fit_masc_age_female_author), 
               lw = w)

# 2 MODEL FITTING WITH PREDICTORS AGE, AGE2, AUTHOR---------------------------------------------------------------
family <- brmsfamily(
  family = "student",
  link = "identity"
)

# Define the formula

formula_age_author <- bf(
  cor_val| resp_se(sei, sigma = TRUE) ~ rel * (change * ((stabch^interval_val) - 1) + 1),
  nlf(rel ~ inv_logit(logitrel)),
  nlf(change ~ inv_logit(logitchange)),
  nlf(stabch ~ inv_logit(logitstabch)),
  logitrel ~ 1 + age_dec_c + age_dec_c2  + (1|author),
  logitchange ~ 1 + age_dec_c + age_dec_c2 + (1|author),
  logitstabch ~ 1 + age_dec_c+ age_dec_c2  + (1|author),
  nl = TRUE
)



# Define the weakly informative priors
priors <-
  prior(normal(0,1), nlpar="logitrel", class = "b") +
  prior(normal(0,1), nlpar="logitchange", class = "b") +
  prior(normal(0,1), nlpar="logitstabch", class = "b") +
  prior(cauchy(0,1), class = "sigma")

# Fit the model
fit_masc_age_author <- brm(
  formula = formula_age_author,
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



# 2 MODEL EVAL: MCMC DIAGNOSTICS  WITH PREDICTORS AGE, AGE2, AUTHOR --------------------------------------------------------

# model summary 
fit_masc_age_author

#plot conditional effects
plot(conditional_effects(fit_masc_age_author), points=T)

# trace plots & param. estimates
plot(fit_masc_age_author , N = 5, ask = TRUE)


# 2 MODEL EVAL: PP CHECKS  WITH PREDICTORS AGE, AGE2, AUTHOR--------------------------------------------------------
summary(fit_masc_age_author )           

# simulations vs. obs: Overall
pp_check(fit_masc_age_author ,
         type ="dens_overlay",
         ndraws = 100)


pp_check(fit_masc_age_author ,
         type ="stat",
         stat = "mean",
         ndraws = 1000,
         binwidth = .001)

pp_check(fit_masc_age_author ,
         type ="stat",
         stat = "sd",
         ndraws = 500,
         binwidth = .001)

pp_check(fit_masc_age_author ,
         type ="stat",
         stat = "median",
         ndraws = 500,
         binwidth = .001)

pp_check(fit_masc_age_author ,
         type ="stat",
         stat = "mad",
         ndraws = 500,
         binwidth = .001)

pp_check(fit_masc_age_author ,
         type ="stat_2d")


pp_check(fit_masc_age_author ,
         type ="scatter_avg")


# 2 MODEL EVAL: LOO --------------------------------------------------------

# loo & pareto K
model_loo <- loo(fit_masc_age_author , save_psis = TRUE, cores = 2)
plot(model_loo, diagnostic = "k")
plot(model_loo, diagnostic = "n_eff")

# loo pit
w <- weights(model_loo$psis_object)
ppc_loo_pit_overlay(y = fit_masc_age_author $data$cor_val, 
                    yrep = posterior_predict(fit_masc_age_author ), 
                    lw = w)
ppc_loo_pit_qq(y = fit_masc_age_author $data$cor_val, 
               yrep = posterior_predict(fit_masc_age_author ), 
               lw = w)


# 3 MODEL FITTING WITH PREDICTORS AUTHOR---------------------------------------------------------------
family <- brmsfamily(
  family = "student",
  link = "identity"
)

# Define the formula

formula_author <- bf(
  cor_val| resp_se(sei, sigma = TRUE) ~ rel * (change * ((stabch^interval_val) - 1) + 1),
  nlf(rel ~ inv_logit(logitrel)),
  nlf(change ~ inv_logit(logitchange)),
  nlf(stabch ~ inv_logit(logitstabch)),
  logitrel ~ 1 + (1|author),
  logitchange ~ 1 + (1|author),
  logitstabch ~ 1 + (1|author),
  nl = TRUE
)



# Define the weakly informative priors
priors <-
  prior(normal(0,1), nlpar="logitrel", class = "b") +
  prior(normal(0,1), nlpar="logitchange", class = "b") +
  prior(normal(0,1), nlpar="logitstabch", class = "b") +
  prior(cauchy(0,1), class = "sigma")

# Fit the model
fit_masc_author <- brm(
  formula = formula_author,
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

# 3 MODEL EVAL: MCMC DIAGNOSTICS  WITH PREDICTOR AUTHOR --------------------------------------------------------

# model summary 
fit_masc_author

#plot conditional effects
plot(conditional_effects(fit_masc_author), points=T)

# trace plots & param. estimates
plot(fit_masc_author , N = 5, ask = TRUE)


# 3 MODEL EVAL: PP CHECKS  WITH AUTHOR--------------------------------------------------------
summary(fit_masc_author )

# simulations vs. obs: Overall
pp_check(fit_masc_author ,
         type ="dens_overlay",
         ndraws = 100)


pp_check(fit_masc_author ,
         type ="stat",
         stat = "mean",
         ndraws = 1000,
         binwidth = .001)

pp_check(fit_masc_author ,
         type ="stat",
         stat = "sd",
         ndraws = 500,
         binwidth = .001)

pp_check(fit_masc_author ,
         type ="stat",
         stat = "median",
         ndraws = 500,
         binwidth = .001)

pp_check(fit_masc_author ,
         type ="stat",
         stat = "mad",
         ndraws = 500,
         binwidth = .001)

pp_check(fit_masc_author ,
         type ="stat_2d")


pp_check(fit_masc_author ,
         type ="scatter_avg")



# MODEL EVAL: LOO --------------------------------------------------------

# loo & pareto K
model_loo <- loo(fit_masc_author , save_psis = TRUE, cores = 2)
plot(model_loo, diagnostic = "k")
plot(model_loo, diagnostic = "n_eff")

# loo pit
w <- weights(model_loo$psis_object)
ppc_loo_pit_overlay(y = fit_masc_author $data$cor_val, 
                    yrep = posterior_predict(fit_masc_author ), 
                    lw = w)
ppc_loo_pit_qq(y = fit_masc_author $data$cor_val, 
               yrep = posterior_predict(fit_masc_author ), 
               lw = w)



###################################
###################################
###################################
# 4 MODEL FITTING WITH PREDICTORS AGE, AGE2, AUTHOR, SUBDOMAINS---------------------------------------------------------------
  family <- brmsfamily(
    family = "student",
    link = "identity"
  )

# Define the formula

formula_age_author_subdomains <- bf(
  cor_val| resp_se(sei, sigma = TRUE) ~ rel * (change * ((stabch^interval_val) - 1) + 1),
  nlf(rel ~ inv_logit(logitrel)),
  nlf(change ~ inv_logit(logitchange)),
  nlf(stabch ~ inv_logit(logitstabch)),
  logitrel ~ 1 + age_dec_c + age_dec_c2  + (1|author) + health_subdomain,
  logitchange ~ 1 + age_dec_c + age_dec_c2 + (1|author)+ health_subdomain,
  logitstabch ~ 1 + age_dec_c+ age_dec_c2  + (1|author) + health_subdomain,
  nl = TRUE
)



# Define the weakly informative priors
priors <-
  prior(normal(0,1), nlpar="logitrel", class = "b") +
  prior(normal(0,1), nlpar="logitchange", class = "b") +
  prior(normal(0,1), nlpar="logitstabch", class = "b") +
  prior(cauchy(0,1), class = "sigma")

# Fit the model
fit_masc_age_author_subdomains <- brm(
  formula = formula_age_author_subdomains,
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



# 4 MODEL EVAL: MCMC DIAGNOSTICS  WITH PREDICTORS AGE, AGE2, AUTHOR, SUBDOMAINS --------------------------------------------------------

# model summary 
fit_masc_age_author_subdomains

#plot conditional effects
plot(conditional_effects(fit_masc_age_author_subdomains), points=T)

# trace plots & param. estimates
plot(fit_masc_age_author_subdomains , N = 5, ask = TRUE)


# 4 MODEL EVAL: PP CHECKS  WITH PREDICTORS AGE, AGE2, AUTHOR, SUBDOMAINS --------------------------------------------------------
summary(fit_masc_age_author_subdomains )           

# simulations vs. obs: Overall
pp_check(fit_masc_age_author_subdomains ,
         type ="dens_overlay",
         ndraws = 100)


pp_check(fit_masc_age_author_subdomains ,
         type ="stat",
         stat = "mean",
         ndraws = 1000,
         binwidth = .001)

pp_check(fit_masc_age_author_subdomains ,
         type ="stat",
         stat = "sd",
         ndraws = 500,
         binwidth = .001)

pp_check(fit_masc_age_author_subdomains ,
         type ="stat",
         stat = "median",
         ndraws = 500,
         binwidth = .001)

pp_check(fit_masc_age_author_subdomains ,
         type ="stat",
         stat = "mad",
         ndraws = 500,
         binwidth = .001)

pp_check(fit_masc_age_author_subdomains ,
         type ="stat_2d")


pp_check(fit_masc_age_author_subdomains ,
         type ="scatter_avg")


# 4 MODEL EVAL: LOO --------------------------------------------------------

# loo & pareto K
model_loo <- loo(fit_masc_age_author_subdomains , save_psis = TRUE, cores = 2)
plot(model_loo, diagnostic = "k")
plot(model_loo, diagnostic = "n_eff")

# loo pit
w <- weights(model_loo$psis_object)
ppc_loo_pit_overlay(y = fit_masc_age_author_subdomains $data$cor_val, 
                    yrep = posterior_predict(fit_masc_age_author_subdomains ), 
                    lw = w)
ppc_loo_pit_qq(y = fit_masc_age_author_subdomains $data$cor_val, 
               yrep = posterior_predict(fit_masc_age_author_subdomains ), 
               lw = w)



###################################
###################################
###################################

# MODEL EVAL: LOO- COMPARISON OF ALL MODEL ----------------------------------------------------
loo1 <- loo(fit_masc_age_female_author)

loo2 <- loo(fit_masc_age_author)

loo3 <- loo(fit_masc_author)


