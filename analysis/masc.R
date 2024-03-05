# DESCRIPTION -------------------------------------------------------------

# In this script we fit the MASC model (Anusic & Schimmack, 2016 JPSP). 
# We also specify the dataset used to fit the MASC model and create new variables where required.

# Author(s): Amanda Holzer(1), Arzie Bajrami(1), Rui Mata(1)
# (1)Centre for Cognitive and Decision Sciences, Faculty of Psychology, University of Basel.

# PACKAGES ---------------------------------------------------------------

library(tidyverse)
#install.packages("brms")
library(brms)
#install.packages("cmdstanr", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))
library(cmdstanr)
#install.packages("loo")
library(loo)
#install.packages("bayesplot")
library(bayesplot)
#install.packages("tidybayes")
library(tidybayes)
#install.packages("posterior")
library(posterior)

color_scheme_set("teal")

# FUNCTIONS ----------------------------------------------------------------

source("helper_functions.R")


# PATHS ---------------------------------------------------




#________________FILES___________________#










