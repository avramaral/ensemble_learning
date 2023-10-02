source("header.R")
source("utils.R")
source("aux.R")

training_size <- 90
uncertain_size <- 40

quant <- TRUE
horiz <- TRUE 

group <- "STA" # c("AGG", "AGE", "STA"), which stands for "aggregated," "age," and "state"

agg_idxs <- list(state_idx = 17, age_idx = 7)
age_idxs <- list(state_idx = 17, age_idx = 1:6)
sta_idxs <- list(state_idx = 1:16, age_idx = 7)

if (group == "AGG") { idxs <- agg_idxs } else if (group == "AGE") { 
                      idxs <- age_idxs } else if (group == "STA") { 
                      idxs <- sta_idxs } else { 
                      stop("Select a valid group.") 
                    } 

##################################################
# LOAD DATA
##################################################

data <- read_csv(file = "DATA/data.csv.gz")
truth_data <- read_csv(file = "DATA/truth_40d.csv.gz")

state <- unique(data$location)
state <- c(state, "DE")
state <- state[2:length(state)][idxs$state_idx]

age <- unique(data$age_group)
age <- c(age, "00+")
age <- age[2:length(age)][idxs$age_idx]

state_age <- data.frame(state = state, age = age)

models <- c("Epiforecasts", "ILM", "KIT", "LMU", "RIVM", "RKI", "SU", "SZ") 
colors <- c("#B30000", "#E69F00", "#56B4E9", "#F0E442", "#80471C", "#3C4AAD", "#CC79A7", "#000000") 

##################################################

