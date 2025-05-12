#%% Notes ==============================================

# Simulations for post-hoc theory paper
# Created 2024 01 

# Executes simulation_simple.R (no theorists) and simulation.R (w/ theorists)

#%% Setup ==============================================

rm(list = ls())
library(tidyverse)
library(cowplot)
#library(latex2exp)

# set working directory
# setwd('C:/Dropbox/Research/Post-hoc/Code') 
# setwd("/cm/chen/Post-Hoc/code")

#%% User Selection ======================================

set.seed(555)

# Set parameters
n_ideas = 100
n_theorist = 1000

mu_sig_list = seq(0.5, 4, 0.5)
mu_sig_pick = 1

ep_sig_list = seq(0.5, 4, 0.5)
ep_sig_pick = 1

hlist = 1 

weight_list = 10
# weight_list = seq(0, 80, 5)

# Simulation set up
N_simulations = 1000
methods = c("ap", "ph")

# Plot set up
nplot = 500


#%% Execute simulations ======================================
# source("simulation_simple.R", echo = TRUE)
source("simulation.R", echo = TRUE)

