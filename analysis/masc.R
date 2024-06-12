# DESCRIPTION -------------------------------------------------------------

# In this script we fit the MASC model (Anusic & Schimmack, 2016 JPSP). 
# We also specify the dataset used to fit the MASC model and create new variables where required.

# Author(s):  Arzie Bajrami(1),  Alexandra Bagaïni (1), Amanda Holzer(1), Rui Mata(1)
# (1) Centre for Cognitive and Decision Sciences, Faculty of Psychology, University of Basel.

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
#install.packages("ggtext")

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
library(ggtext)


inv_logit <- function(x){plogis(x)}

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

## IMPORT CORRELATION FILE-----
df <- read_excel("data/cor_final.xlsx")

## MEAN IMPUTATION FOR MISSING DATA-----
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


## MANIPULATE DATA--------
mean(df$female, na.rm=T)
mean(df$age, na.rm=T)
df$interval_val <- df$interval_val/365
plot(df$interval_val, df$cor_val)


# Centering age and female percentage

df <- df %>% mutate(cor_val = if_else(cor_val <0,0, cor_val), 
                    age_dec_c = (age-40)/10,
                    age_dec_c2 = age_dec_c^2,
                    female_c = (female-0.50))

df <- escalc(measure = "COR", ri=cor_val, ni=n, data=df)
df$sei <- sqrt(df$vi)


# Creating domain groups
# health and other (social, political, crime, nature, finance)
df <- df %>% mutate(domain = 
                      case_when(domain %in% c("health") ~ "health",
                                !domain %in% c("health") ~ "other"))

# Creating event groups
# none and event (intervention or exposure)
df <- df %>% mutate(event = 
                      case_when(event %in% c("none") ~ "none",
                                !event %in% c("none") ~ "event"))



# using sum contrast coding

sum_coding <- function(x, lvls = levels(x)) {
  # codes the first category with -1
  nlvls <- length(lvls)
  stopifnot(nlvls > 1)
  cont <- diag(nlvls)[, -nlvls, drop = FALSE]
  cont[nlvls, ] <- -1
  cont <- cont[c(nlvls, 1:(nlvls - 1)), , drop = FALSE]
  colnames(cont) <- lvls[-1]
  x <- factor(x, levels = lvls)
  contrasts(x) <- cont
  x
}

df <- df %>% mutate(domain = factor(domain),
                    domain = relevel(domain, ref="health"),
                    domain = sum_coding(domain, lvls = levels(domain))) %>% 
            mutate(event = factor(event),
                     event = relevel(event, ref="none"),
                     event = sum_coding(event, lvls = levels(event))) %>% 
    mutate(item= factor(item),
           item = relevel(item, ref="multiple"),
         item = sum_coding(item, lvls = levels(item)))
                                 
# Tables and plots
table(df$health_subdomain)
table(df$event)
table(df$item_c)

df %>% ggplot(aes(x = age)) + geom_histogram()
df %>% ggplot(aes(x = female)) + geom_histogram()
df %>% ggplot(aes(x = interval_val, y = cor_val)) + geom_point()


df %>% ggplot(aes(x = interval_val, y = cor_val)) + geom_point() + facet_grid(.~ event)
df %>% ggplot(aes(x = interval_val, y = cor_val)) + geom_point() + facet_wrap(.~ domain)
df %>% ggplot(aes(x = interval_val, y = cor_val)) + geom_point() + facet_wrap(.~ item)

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
  logitrel ~ 1,
  logitchange ~ 1,
  logitstabch ~ 1,
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



# MODEL 2a FITTING WITH DEMOGRAPHICS: AGE, AGE2, GENDER---------------------------------------------------------------
family <- brmsfamily(
  family = "student",
  link = "identity"
)

# Define the formula

formula_m2a <- bf(
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
fit_masc_m2a <- brm(
  formula = formula_m2a,
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


# MODEL 2a EVAL: MCMC DIAGNOSTICS --------------------------------------------------------

# model summary 
fit_masc_m2a

#plot conditional effects
plot(conditional_effects(fit_masc_m2a), points=T)


# trace plots & param. estimates
plot(fit_masc_m2a , N = 5, ask = TRUE)


# MODEL 2a EVAL: PP CHECKS --------------------------------------------------------
summary(fit_masc_m2a )           

# simulations vs. obs: Overall
pp_check(fit_masc_m2a ,
         type ="dens_overlay",
         ndraws = 100)


pp_check(fit_masc_m2a ,
         type ="stat",
         stat = "mean",
         ndraws = 1000,
         binwidth = .001)

pp_check(fit_masc_m2a ,
         type ="stat",
         stat = "sd",
         ndraws = 500,
         binwidth = .001)

pp_check(fit_masc_m2a ,
         type ="stat",
         stat = "median",
         ndraws = 500,
         binwidth = .001)

pp_check(fit_masc_m2a ,
         type ="stat",
         stat = "mad",
         ndraws = 500,
         binwidth = .001)

pp_check(fit_masc_m2a ,
         type ="stat_2d")


pp_check(fit_masc_m2a ,
         type ="scatter_avg")


# MODEL 2a EVAL: LOO --------------------------------------------------------

# loo & pareto K
model_loo <- loo(fit_masc_m2a , save_psis = TRUE, cores = 2)
plot(model_loo, diagnostic = "k")
plot(model_loo, diagnostic = "n_eff")

# loo pit
w <- weights(model_loo$psis_object)
ppc_loo_pit_overlay(y = fit_masc_m2a $data$cor_val, 
                    yrep = posterior_predict(fit_masc_m2a ), 
                    lw = w)
ppc_loo_pit_qq(y = fit_masc_m2a $data$cor_val, 
               yrep = posterior_predict(fit_masc_m2a), 
               lw = w)


# MODEL 2b FITTING WITH MEASUREMENT CHARACTERISTICS: DOMAIN, ITEM, EVENT---------------------------------------------------------------
family <- brmsfamily(
  family = "student",
  link = "identity"
)

# Define the formula

formula_m2b <- bf(
  cor_val| resp_se(sei, sigma = TRUE) ~ rel * (change * ((stabch^interval_val) - 1) + 1),
  nlf(rel ~ inv_logit(logitrel)),
  nlf(change ~ inv_logit(logitchange)),
  nlf(stabch ~ inv_logit(logitstabch)),
  logitrel ~ 1 + domain + item+ event,
  logitchange ~ 1 + domain + event,
  logitstabch ~ 1 + domain + event,
  nl = TRUE
)



# Define the weakly informative priors
priors <-
  prior(normal(0,1), nlpar="logitrel", class = "b") +
  prior(normal(0,1), nlpar="logitchange", class = "b") +
  prior(normal(0,1), nlpar="logitstabch", class = "b") +
  prior(cauchy(0,1), class = "sigma")

# Fit the model
fit_masc_m2b <- brm(
  formula = formula_m2b,
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

# MODEL 2b EVAL: MCMC DIAGNOSTICS --------------------------------------------------------

# model summary 
fit_masc_m2b

#plot conditional effects
plot(conditional_effects(fit_masc_m2b), points=T)


# trace plots & param. estimates
plot(fit_masc_m2b , N = 5, ask = TRUE)


# MODEL 2b EVAL: PP CHECKS --------------------------------------------------------
summary(fit_masc_m2b )

# simulations vs. obs: Overall
pp_check(fit_masc_m2b ,
         type ="dens_overlay",
         ndraws = 100)


pp_check(fit_masc_m2b ,
         type ="stat",
         stat = "mean",
         ndraws = 1000,
         binwidth = .001)

pp_check(fit_masc_m2b ,
         type ="stat",
         stat = "sd",
         ndraws = 500,
         binwidth = .001)

pp_check(fit_masc_m2b ,
         type ="stat",
         stat = "median",
         ndraws = 500,
         binwidth = .001)

pp_check(fit_masc_m2b ,
         type ="stat",
         stat = "mad",
         ndraws = 500,
         binwidth = .001)

pp_check(fit_masc_m2b ,
         type ="stat_2d")


pp_check(fit_masc_m2b ,
         type ="scatter_avg")



# MODEL 2b EVAL: LOO --------------------------------------------------------

# loo & pareto K
model_loo <- loo(fit_masc_m2b , save_psis = TRUE, cores = 2)
plot(model_loo, diagnostic = "k")
plot(model_loo, diagnostic = "n_eff")

# loo pit
w <- weights(model_loo$psis_object)
ppc_loo_pit_overlay(y = fit_masc_m2b$data$cor_val, 
                    yrep = posterior_predict(fit_masc_m2b ), 
                    lw = w)
ppc_loo_pit_qq(y = fit_masc_m2b$data$cor_val, 
               yrep = posterior_predict(fit_masc_m2b ), 
               lw = w)


# MODEL 3 FITTING WITH DEMOGRAPHICS AND MEASUREMENT CHARACTERISTICS: DOMAIN, ITEM, EVENT---------------------------------------------------------------
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
  logitrel ~ 1 + age_dec_c + age_dec_c2+  female_c+  domain + item + event,
  logitchange ~ 1 + age_dec_c+  age_dec_c2+ female_c+  domain + item + event,
  logitstabch ~ 1 + age_dec_c+  age_dec_c2+  female_c+  domain +item + event,
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

loo_compare(loo1, loo2, loo3)



#PREDICTIONS TIME ---------------------------------------------------------------

## Plot correlation and interval------

nd <- crossing(domain= NA,  
               sei = 0.1,
               item= NA, 
               event=NA, 
               age_dec_c= 0,
               age_dec_c2= 0,
               female_c= 0, 
               interval_val=seq(0,5, by = .1))


epred_draws_df <- nd %>% 
  add_epred_draws(fit_masc_m3, re_formula = NA)

# epred_draws_dom <- epred_draws_df %>%
#   group_by(interval_val, health_subdomain) %>%
#   mean_hdci(.epred,.width = c(.95,.8,.5)) %>%
#   pivot_wider(names_from = .width, values_from = c(.lower,.upper))



ggplot(epred_draws_df) +
  stat_lineribbon(alpha = 1/4, point_interval = "mean_hdci", aes(x = interval_val, y = .epred)) + 
  geom_point(data= df, aes(x=interval_val, y= cor_val), size = 1.5, color = "grey20", alpha = 0.7) +
  # geom_line(data = epred_draws_agg,
  #           aes(x = time_diff_dec*10, y = .epred),
  #           color = "grey95",
  #           size = .5) +
  # geom_line(data = epred_draws_dom, 
  #           aes(x = time_diff_dec*10, y = .epred, linetype = domain_name),
  #           color = "#e07f00",
  #           linewidth = .25) +
  # geom_text_repel(data = lbl_dot_df, 
  #                 aes(x = time_diff_dec*10, y = .epred, label = label),
  #                 family = "Source Sans 3", size = 2.5,
  #                 min.segment.length = 0,
  #                 segment.color = "grey50",
  #                 segment.size = .25,
  #                 box.padding = 0.5,
  #                 nudge_x = .5,
  #                 nudge_y = c(.1, -.05)
  # ) +
  # facet_wrap(.~health_subdomain,  nrow = 4) +
  theme_minimal() +
  labs(y = "Retest Correlation", x = "Retest Interval (Years)", color = "", linetype = "", fill = "",
       title = "Behaviour") +
  theme(strip.placement = "outside",
        legend.position = "none", # c(0,0) bottom left, c(1,1) top-right.
        legend.margin = margin(-.5,0,0,0, unit="cm"),
        legend.spacing.y = unit(0.15, 'cm'),
        legend.key.width = unit(1, "cm"),
        legend.key.size = unit(.3, "cm"),
        # plot.tag.position = c(0,.8),
        legend.text = element_text(size = 8.5, color = "grey20"),
        text = element_text(size = 9, color = "grey40"),
        axis.text.y = element_text( vjust=seq(0,1, length.out = 5)),
        axis.text.x = element_text( hjust=c(0,1)),
        title = element_text(size = 9, color = "grey20"),
        plot.tag  = element_text( size = 11, face = "bold", color = "grey20"),
        panel.spacing = unit(.5, "lines"),
        plot.title = element_blank(),
        panel.grid = element_blank(),
        plot.title.position = "plot",
        plot.margin = margin(b = 5, r = 5, l = 5),
        panel.background = element_rect(color = "grey75", fill = NA, size = .4)) +
  scale_fill_manual(values = c("#502a7b","#502a7c" , "#502a7a" )) +
  guides(color = guide_legend(override.aes = list(size = .75)),
         fill =  guide_legend(override.aes = list(size = .75)),
         size = "none", linetype = guide_legend(override.aes = list(size = .75))) +
  coord_cartesian(ylim = c(0, 1), xlim = c(0,5))+
  scale_y_continuous(breaks = seq(0,1,0.25)) +
  scale_x_continuous(breaks = c(0,1,2,3,4,5))



## Plot correlation and interval by event-------

nd <- crossing(domain= NA,  
               sei = 0.1,
               item= NA, 
               event= unique(df$event), 
               age_dec_c= 0,
               age_dec_c2= 0,
               female_c= 0, 
               interval_val=seq(0,5, by = .1))


epred_draws_df <- nd %>% 
  add_epred_draws(fit_masc_m3, re_formula = NA)

# epred_draws_dom <- epred_draws_df %>%
#   group_by(interval_val, health_subdomain) %>%
#   mean_hdci(.epred,.width = c(.95,.8,.5)) %>%
#   pivot_wider(names_from = .width, values_from = c(.lower,.upper))


p_event <- ggplot(epred_draws_df) +
  stat_lineribbon(alpha = 1/4, point_interval = "mean_hdci", aes(x = interval_val, y = .epred)) + 
  geom_point(data= df, aes(x=interval_val, y= cor_val), size = 1.5, color = "grey20", alpha = 0.7) +
  # geom_line(data = epred_draws_agg,
  #           aes(x = time_diff_dec*10, y = .epred),
  #           color = "grey95",
  #           size = .5) +
  # geom_line(data = epred_draws_dom, 
  #           aes(x = time_diff_dec*10, y = .epred, linetype = domain_name),
  #           color = "#e07f00",
  #           linewidth = .25) +
  # geom_text_repel(data = lbl_dot_df, 
  #                 aes(x = time_diff_dec*10, y = .epred, label = label),
  #                 family = "Source Sans 3", size = 2.5,
  #                 min.segment.length = 0,
  #                 segment.color = "grey50",
  #                 segment.size = .25,
  #                 box.padding = 0.5,
  #                 nudge_x = .5,
  #                 nudge_y = c(.1, -.05)
  # ) +
  facet_wrap(.~str_to_title(event)) +
  theme_minimal() +
  labs(y = "Retest Correlation", x = "Retest Interval (Years)", color = "", linetype = "", fill = "",
       title = "Behaviour") +
  theme(strip.placement = "outside",
        legend.position = "none", # c(0,0) bottom left, c(1,1) top-right.
        legend.margin = margin(-.5,0,0,0, unit="cm"),
        legend.spacing.y = unit(0.15, 'cm'),
        legend.key.width = unit(1, "cm"),
        legend.key.size = unit(.3, "cm"),
        # plot.tag.position = c(0,.8),
        legend.text = element_text(size = 8.5, color = "grey20"),
        text = element_text(size = 9, color = "grey40"),
        axis.text.y = element_text( vjust=seq(0,1, length.out = 5)),
        axis.text.x = element_text( hjust=c(0,1)),
        title = element_text(size = 9, color = "grey20"),
        plot.tag  = element_text( size = 11, face = "bold", color = "grey20"),
        panel.spacing = unit(.5, "lines"),
        plot.title = element_blank(),
        panel.grid = element_blank(),
        plot.title.position = "plot",
        plot.margin = margin(b = 5, r = 5, l = 5),
        panel.background = element_rect(color = "grey75", fill = NA, size = .4),
        strip.text = element_text(face = "bold"),
        strip.text.y = element_text(size = 10),
        strip.text.x = element_text(size = 10)) +
  scale_fill_manual(values = c("#502a7b","#502a7c","#502a7a" ))+
  guides(color = guide_legend(override.aes = list(size = .75)),
         fill =  guide_legend(override.aes = list(size = .75)),
         size = "none", linetype = guide_legend(override.aes = list(size = .75))) +
  coord_cartesian(ylim = c(0, 1), xlim = c(0,5))+
  scale_y_continuous(breaks = seq(0,1,0.25)) +
  scale_x_continuous(breaks = c(0,1,2,3,4,5))


p_event

# save plot as jpeg file 
ggsave(plot = p_event, filename = "analysis/masc_event.jpeg",
       dpi = 300, width = 25, height = 37, units = "cm") 


## Plot correlation and interval by domain-------

nd <- crossing(domain= unique(df$domain),  
               sei = 0.1,
               item= NA, 
               event= NA, 
               age_dec_c= 0,
               age_dec_c2= 0,
               female_c= 0, 
               interval_val=seq(0,5, by = .1))


epred_draws_df <- nd %>% 
  add_epred_draws(fit_masc_m3, re_formula = NA)

# epred_draws_dom <- epred_draws_df %>%
#   group_by(interval_val, health_subdomain) %>%
#   mean_hdci(.epred,.width = c(.95,.8,.5)) %>%
#   pivot_wider(names_from = .width, values_from = c(.lower,.upper))


ggplot(epred_draws_df) +
  stat_lineribbon(alpha = 1/4, point_interval = "mean_hdci", aes(x = interval_val, y = .epred)) + 
  geom_point(data= df, aes(x=interval_val, y= cor_val), size = 1.5, color = "grey20", alpha = 0.7) +
  # geom_line(data = epred_draws_agg,
  #           aes(x = time_diff_dec*10, y = .epred),
  #           color = "grey95",
  #           size = .5) +
  # geom_line(data = epred_draws_dom, 
  #           aes(x = time_diff_dec*10, y = .epred, linetype = domain_name),
  #           color = "#e07f00",
  #           linewidth = .25) +
  # geom_text_repel(data = lbl_dot_df, 
  #                 aes(x = time_diff_dec*10, y = .epred, label = label),
  #                 family = "Source Sans 3", size = 2.5,
  #                 min.segment.length = 0,
  #                 segment.color = "grey50",
  #                 segment.size = .25,
  #                 box.padding = 0.5,
  #                 nudge_x = .5,
  #                 nudge_y = c(.1, -.05)
  # ) +
  facet_wrap(.~str_to_title(domain)) +
  theme_minimal() +
  labs(y = "Retest Correlation", x = "Retest Interval (Years)", color = "", linetype = "", fill = "",
       title = "Behaviour") +
  theme(strip.placement = "outside",
        legend.position = "none", # c(0,0) bottom left, c(1,1) top-right.
        legend.margin = margin(-.5,0,0,0, unit="cm"),
        legend.spacing.y = unit(0.15, 'cm'),
        legend.key.width = unit(1, "cm"),
        legend.key.size = unit(.3, "cm"),
        # plot.tag.position = c(0,.8),
        legend.text = element_text(size = 8.5, color = "grey20"),
        text = element_text(size = 9, color = "grey40"),
        axis.text.y = element_text( vjust=seq(0,1, length.out = 5)),
        axis.text.x = element_text( hjust=c(0,1)),
        title = element_text(size = 9, color = "grey20"),
        plot.tag  = element_text( size = 11, face = "bold", color = "grey20"),
        panel.spacing = unit(.5, "lines"),
        plot.title = element_blank(),
        panel.grid = element_blank(),
        plot.title.position = "plot",
        plot.margin = margin(b = 5, r = 5, l = 5),
        panel.background = element_rect(color = "grey75", fill = NA, size = .4),
        strip.text = element_text(face = "bold")) +
  scale_fill_manual(values = c("#502a7b","#502a7c" , "#502a7a" )) +
  guides(color = guide_legend(override.aes = list(size = .75)),
         fill =  guide_legend(override.aes = list(size = .75)),
         size = "none", linetype = guide_legend(override.aes = list(size = .75))) +
  coord_cartesian(ylim = c(0, 1), xlim = c(0,5))+
  scale_y_continuous(breaks = seq(0,1,0.25)) +
  scale_x_continuous(breaks = c(0,1,2,3,4,5))


## Plot correlation and interval by item-------

nd <- crossing(domain= NA,  
               sei = 0.1,
               item= unique(df$item), 
               event= NA, 
               age_dec_c= 0,
               age_dec_c2= 0,
               female_c= 0, 
               interval_val=seq(0,5, by = .1))


epred_draws_df <- nd %>% 
  add_epred_draws(fit_masc_m3, re_formula = NA)

# epred_draws_dom <- epred_draws_df %>%
#   group_by(interval_val, health_subdomain) %>%
#   mean_hdci(.epred,.width = c(.95,.8,.5)) %>%
#   pivot_wider(names_from = .width, values_from = c(.lower,.upper))

epred_draws_df  <- epred_draws_df  %>%
  mutate(item = str_to_title(as.character(item)))


ggplot(epred_draws_df) +
  stat_lineribbon(alpha = 1/4, point_interval = "mean_hdci", aes(x = interval_val, y = .epred)) + 
  geom_point(data= df, aes(x=interval_val, y= cor_val), size = 1.5, color = "grey20", alpha = 0.7) +
  # geom_line(data = epred_draws_agg,
  #           aes(x = time_diff_dec*10, y = .epred),
  #           color = "grey95",
  #           size = .5) +
  # geom_line(data = epred_draws_dom, 
  #           aes(x = time_diff_dec*10, y = .epred, linetype = domain_name),
  #           color = "#e07f00",
  #           linewidth = .25) +
  # geom_text_repel(data = lbl_dot_df, 
  #                 aes(x = time_diff_dec*10, y = .epred, label = label),
  #                 family = "Source Sans 3", size = 2.5,
  #                 min.segment.length = 0,
  #                 segment.color = "grey50",
  #                 segment.size = .25,
  #                 box.padding = 0.5,
  #                 nudge_x = .5,
  #                 nudge_y = c(.1, -.05)
  # ) +
  facet_wrap(.~str_to_title(item)) +
  theme_minimal() +
  labs(y = "Retest Correlation", x = "Retest Interval (Years)", color = "", linetype = "", fill = "",
       title = "Behaviour") +
  theme(strip.placement = "outside",
        legend.position = "none", # c(0,0) bottom left, c(1,1) top-right.
        legend.margin = margin(-.5,0,0,0, unit="cm"),
        legend.spacing.y = unit(0.15, 'cm'),
        legend.key.width = unit(1, "cm"),
        legend.key.size = unit(.3, "cm"),
        # plot.tag.position = c(0,.8),
        legend.text = element_text(size = 8.5, color = "grey20"),
        text = element_text(size = 9, color = "grey40"),
        axis.text.y = element_text( vjust=seq(0,1, length.out = 5)),
        axis.text.x = element_text( hjust=c(0,1)),
        title = element_text(size = 9, color = "grey20"),
        plot.tag  = element_text( size = 11, face = "bold", color = "grey20"),
        panel.spacing = unit(.5, "lines"),
        plot.title = element_blank(),
        panel.grid = element_blank(),
        plot.title.position = "plot",
        plot.margin = margin(b = 5, r = 5, l = 5),
        panel.background = element_rect(color = "grey75", fill = NA, size = .4) ,
        strip.text = element_text(face = "bold")) + 
  scale_fill_manual(values = c("#502a7b","#502a7c" , "#502a7a" )) +
  guides(color = guide_legend(override.aes = list(size = .75)),
         fill =  guide_legend(override.aes = list(size = .75)),
         size = "none", linetype = guide_legend(override.aes = list(size = .75))) +
  coord_cartesian(ylim = c(0, 1), xlim = c(0,5)) +
  scale_y_continuous(breaks = seq(0,1,0.25)) +
  scale_x_continuous(breaks = c(0,1,2,3,4,5))



#PREDICTIONS PARAMETERS ---------------------------------------------------------------

## BY DOMAINS------

pred_df_domain <- NULL

for (curr_nlpar in c("stabch","rel","change")) {
  
  
  nd <- crossing(domain= unique(df$domain),  
                 sei = 0.1,
                 item=NA, 
                 event=NA, 
                 age_dec_c=0,
                 age_dec_c2=0,
                 female_c= 0, 
                 interval_val=0)
  
  
  
  
  fit_nlpar_domain <- nd %>% 
    add_epred_draws(fit_masc_m3, nlpar = curr_nlpar, re_formula = NA)    
  

 
  fit_nlpar_domain <- fit_nlpar_domain %>%
    group_by(domain) %>% 
    mean_hdci(.epred,.width = c(.95,.8,.5)) %>% 
    pivot_wider(names_from = .width, values_from = c(.lower,.upper)) %>% 
    mutate(nlpar = curr_nlpar,
           estimate = .epred,
           categ = "domain",
           x = domain)%>% 
    select(categ, x, nlpar, estimate, dplyr::contains("er_"))
  
  
  pred_df <- fit_nlpar_domain
  

  pred_df_domain <- bind_rows(pred_df, pred_df_domain) 
}



pred_df_domain <- pred_df_domain %>% 
  mutate(nlpar = case_when(nlpar == "rel" ~ "Reliability",
                           nlpar == "change" ~ "Change",
                           nlpar == "stabch" ~"Stab. Change"))





## BY ITEM --------


pred_df_item <- NULL

  
  
  for (curr_nlpar in c("stabch","rel","change")) {
    
    
    nd <- crossing(domain= NA,  
                   sei = 0.1,
                   item= unique(df$item), 
                   event=NA, 
                   age_dec_c=0,
                   age_dec_c2=0,
                   female_c= 0, 
                   interval_val=0)
    
    
    fit_nlpar_item <- nd %>% 
      add_epred_draws(fit_masc_m3, nlpar = curr_nlpar, re_formula = NA) 
    

  
    
    
    fit_nlpar_item <- fit_nlpar_item %>%
      group_by(item) %>% 
      mean_hdci(.epred,.width = c(.95,.8,.5)) %>% 
      pivot_wider(names_from = .width, values_from = c(.lower,.upper)) %>% 
      mutate(nlpar = curr_nlpar,
             estimate = .epred,
             categ = "item",
             x = item)%>% 
      select(categ, x, nlpar, estimate, dplyr::contains("er_"))
    
    
    
    pred_df <- fit_nlpar_item
    
    
    pred_df_item <- bind_rows(pred_df, pred_df_item) 
  }
  

pred_df_item <- pred_df_item %>% 
  mutate(nlpar = case_when(nlpar == "rel" ~ "Reliability",
                           nlpar == "change" ~ "Change",
                           nlpar == "stabch" ~"Stab. Change"))

## BY EVENT ------------


pred_df_event <- NULL



for (curr_nlpar in c("stabch","rel","change")) {
  
  
  nd <- crossing(domain= NA,  
                 sei = 0.1,
                 item= NA, 
                 event=unique(df$event), 
                 age_dec_c=0,
                 age_dec_c2=0,
                 female_c= 0, 
                 interval_val=0)
  
  
  fit_nlpar_event <- nd %>% 
    add_epred_draws(fit_masc_m3, nlpar = curr_nlpar, re_formula = NA) 
  
  
  
  
  
  fit_nlpar_event <- fit_nlpar_event %>%
    group_by(event) %>% 
    mean_hdci(.epred,.width = c(.95,.8,.5)) %>% 
    pivot_wider(names_from = .width, values_from = c(.lower,.upper)) %>% 
    mutate(nlpar = curr_nlpar,
           estimate = .epred,
           categ = "event",
           x = event)%>% 
    select(categ, x, nlpar, estimate, dplyr::contains("er_"))
  
  
  
  pred_df <- fit_nlpar_event
  
  
  pred_df_event <- bind_rows(pred_df, pred_df_event) 
}

pred_df_event <- pred_df_event %>% 
  mutate(nlpar = case_when(nlpar == "rel" ~ "Reliability",
                           nlpar == "change" ~ "Change",
                           nlpar == "stabch" ~"Stab. Change"))


## BY AGE ------------

pred_df_age <- NULL



for (curr_nlpar in c("stabch","rel","change")) {
  
  
  nd <- crossing(domain= NA,  
                 sei = 0.1,
                 item= NA, 
                 event=NA, 
                 age_dec_c= c(-2,0,2,4),
                 age_dec_c2= c(4, 0, 4, 16),
                 female_c= 0, 
                 interval_val=0)
  
  
  fit_nlpar_age <- nd %>% 
    add_epred_draws(fit_masc_m3, nlpar = curr_nlpar, re_formula = NA) 
  
  
  
  
  
  fit_nlpar_age <- fit_nlpar_age %>%
    group_by(age_dec_c) %>% 
    mean_hdci(.epred,.width = c(.95,.8,.5)) %>% 
    pivot_wider(names_from = .width, values_from = c(.lower,.upper)) %>% 
    mutate(nlpar = curr_nlpar,
           estimate = .epred,
           categ = "age",
           x = case_when(age_dec_c== 4 ~ "80+", age_dec_c== 2 ~ "60-79", age_dec_c== 0 ~ "40-59", age_dec_c== -2 ~ "20-39"))%>% 
    select(categ, x, nlpar, estimate, dplyr::contains("er_"))
  
  
  
  pred_df <- fit_nlpar_age
  
  
  pred_df_age <- bind_rows(pred_df, pred_df_age) 
}

pred_df_age <- pred_df_age %>% 
  mutate(nlpar = case_when(nlpar == "rel" ~ "Reliability",
                           nlpar == "change" ~ "Change",
                           nlpar == "stabch" ~"Stab. Change"))


## BY GENDER --------

pred_df_female <- NULL



for (curr_nlpar in c("stabch","rel","change")) {
  
  
  nd <- crossing(domain= NA,  
                 sei = 0.1,
                 item= NA, 
                 event=NA, 
                 age_dec_c= 0,
                 age_dec_c2= 0,
                 female_c= c(-0.5, 0.5), 
                 interval_val=0)
  
  
  fit_nlpar_female <- nd %>% 
    add_epred_draws(fit_masc_m3, nlpar = curr_nlpar, re_formula = NA) 
  
  
  
  
  
  fit_nlpar_female <- fit_nlpar_female %>%
    group_by(female_c) %>% 
    mean_hdci(.epred,.width = c(.95,.8,.5)) %>% 
    pivot_wider(names_from = .width, values_from = c(.lower,.upper)) %>% 
    mutate(nlpar = curr_nlpar,
           estimate = .epred,
           categ = "gender",
           x = ifelse(female_c== -0.5, "male", "female"))%>% 
    select(categ, x, nlpar, estimate, dplyr::contains("er_"))
  
  
  
  pred_df <- fit_nlpar_female
  
  
  pred_df_female <- bind_rows(pred_df, pred_df_female) 
}

pred_df_female <- pred_df_female %>% 
  mutate(nlpar = case_when(nlpar == "rel" ~ "Reliability",
                           nlpar == "change" ~ "Change",
                           nlpar == "stabch" ~"Stab. Change"))


## OVERALL --------

pred_df_overall <- NULL



for (curr_nlpar in c("stabch","rel","change")) {
  
  
  nd <- crossing(domain= NA,  
                 sei = 0.1,
                 item= NA, 
                 event=NA, 
                 age_dec_c= 0,
                 age_dec_c2= 0,
                 female_c= 0, 
                 interval_val=0)
  
  
  fit_nlpar_overall <- nd %>% 
    add_epred_draws(fit_masc_m3, nlpar = curr_nlpar, re_formula = NA) 
  
  
  
  
  
  fit_nlpar_overall <- fit_nlpar_overall %>%
    mean_hdci(.epred,.width = c(.95,.8,.5)) %>% 
    pivot_wider(names_from = .width, values_from = c(.lower,.upper)) %>% 
    mutate(nlpar = curr_nlpar,
           estimate = .epred,
           categ = "all",
           x = "overall")%>% 
    select(categ, x, nlpar, estimate, dplyr::contains("er_"))
  
  
  
  pred_df <- fit_nlpar_overall
  
  
  pred_df_overall <- bind_rows(pred_df, pred_df_overall) 
}


pred_df_overall <- pred_df_overall %>% 
  mutate(nlpar = case_when(nlpar == "rel" ~ "Reliability",
                           nlpar == "change" ~ "Change",
                           nlpar == "stabch" ~"Stab. Change"))


pred_df <- bind_rows(pred_df_domain, pred_df_item, pred_df_event, pred_df_age, pred_df_female, pred_df_overall)


# TABLE PARAMETERS----------

# Selecting only the specified columns from pred_df
pred_df_selected <- pred_df[, c("categ", "x", "nlpar", "estimate", ".lower_0.95", ".upper_0.95")]

# Create a gt table
table_gt <- gt(data = pred_df_selected,
               rowname_col = "x",
               caption = "Prediction Results",
               rownames_to_stub = TRUE)

# Formatting
table_gt <- table_gt %>%
  tab_header(title = "Estimates and Confidence Intervals") %>%
  fmt_number(columns = c("estimate", ".lower_0.95", ".upper_0.95"), decimals = 3) %>%
  cols_align(align = "center") 

# save table as html file
htmltools::save_html(table_gt, file = "analysis/table_estimates.html") 


# PLOT PARAMETERS----------

pred_df <- pred_df %>%
  mutate(categ = str_to_title(as.character(categ)),
         x = str_to_title(as.character(x)))


pred_df$nlpar <- factor(pred_df$nlpar, levels = c("Reliability", "Change", "Stab. Change"))


pred_df$x <- factor(pred_df$x, levels = c("Overall", "None","Event", "Other", "Health",  "Single", "Multiple", "80+","60-79", "40-59", "20-39", "Male", "Female"))

pred_df$categ <- factor(pred_df$categ, levels = c("All", "Event", "Domain",  "Item", "Age", "Gender"))


p_nlpar <- pred_df %>% ggplot() +
  geom_crossbar(aes(xmin = .lower_0.95, x = estimate, 
                    xmax = .upper_0.95, y = x),
                fill = "white", color = "NA",
                linewidth = .15,width = 0.25, alpha =  1) +
  geom_crossbar(aes(xmin = .lower_0.95, x = estimate, 
                    xmax = .upper_0.95, y = x),
                fill = "#502a7a", color = "NA",
                linewidth = .15,width = 0.25, alpha =  .3) +
  geom_crossbar(aes(xmin = .lower_0.8, x = estimate,
                    xmax = .upper_0.8, y = x),
                fill = "white", color = "NA",
                linewidth = .15,width = 0.25, alpha =  1) +
  geom_crossbar(aes(xmin = .lower_0.8, x = estimate,
                    xmax = .upper_0.8, y = x),
                fill = "#502a7a",color = "NA",
                linewidth = .15,width = 0.25, alpha =  .6) +
  geom_crossbar(aes(xmin = .lower_0.5, x = estimate, 
                    xmax = .upper_0.5, y = x),
                fill = "white",color = "NA",
                linewidth = .15,width = 0.25, alpha =  1) +
  geom_crossbar(aes(xmin = .lower_0.5, x = estimate, 
                    xmax = .upper_0.5, y = x),
                fill = "#502a7a",color = "NA",
                linewidth = .15,width = 0.25, alpha =  .9) +
  geom_point(aes(x = estimate, y =x),
             fill = "white", color = "grey20",
             shape = 21, 
             stroke = .25, 
             size = 2) +
  facet_grid(categ ~ nlpar, scales = "free_y", space = "free", switch = "y") + 
  # scale_x_continuous(limits = c(0,1), expand = c(0,0)) +
  scale_x_continuous(limits = c(0, 1), expand = c(0, 0), breaks = c(0, 0.5, 1), labels = c("0", "0.5", "1")) +
  scale_y_discrete(position = "left") +
  theme_minimal() +
  geom_rect(data = subset(pred_df, x %in% c("Overall")), 
            fill = NA, colour = "black", size = .75, xmin = -Inf,xmax = Inf,
            ymin = -Inf,ymax = Inf) +
  theme(panel.grid = element_blank(),
        legend.position = "none",
        plot.title.position = "plot",
        strip.placement = "outside",
        #strip.text.y = element_blank(),
        axis.title.x = element_text(size = 10, color = "grey20"),
        plot.margin = margin(b = 10, t = 10, r = 15, l = 0),
        panel.spacing.x = unit(.3, "cm"),
        # plot.tag.position = c(0.025,.9),
        # panel.grid.major.x = element_line(linewidth = .2, color = "grey80", linetype = "solid"),
        panel.background = element_rect(linewidth = .25, color = "grey50", fill = "NA"),
        #plot.background = element_rect(linewidth = .25, color = "grey40", fill = "NA"),
        strip.text =  element_text( face = "bold"),
        strip.text.y = element_text(size = 10),
        strip.text.x = element_text(size = 10),
        plot.tag  = element_text( size = 10, face = "bold", color = "grey20"),
        plot.title = element_blank(),
        title = element_text( face = "bold", size = 10),
        axis.text.x =  element_markdown( color = "black", size = 10, hjust=c(0,.5, 1)),
        axis.text.y.left =  element_markdown( angle = 0, hjust = 1, color = "grey20", size = 10), # hjust = c(0,.5,.5,.5,1)
        text = element_text()) +
  labs(y = "", x = "Parameter Estimate", title = "Propensity") 



p_nlpar

# save plot as jpeg file 
ggsave(plot = p_nlpar, filename = "analysis/masc_pred.jpeg",
       dpi = 300, width = 25, height = 37, units = "cm") 



