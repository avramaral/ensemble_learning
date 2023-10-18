source("header.R")
source("utils.R")
source("aux.R")

training_size <- 90
uncertain_size <- 40

quant <- TRUE
horiz <- TRUE 

group <- "AGE" # c("AGG", "AGE", "STA"), which stands for "aggregated," "age," and "state"

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

models <- c("Mean", "Median", "DISW (St.1.)", "DISW (St.2.)", "ISW (St.1.)", "ISW (St.2.)")
colors <- c("#009E73", "#60D1B3", "#9400D3", "#9370DB", "#C71585", "#FF1493")

##################################################

wis <- list()
for (i in 1:nrow(state_age)) {
  tmp_file <- paste("RESULTS/FITTED_OBJECTS/WIS/wis_size_", training_size, "_state_", state_age[i, "state"], "_age_", state_age[i, "age"], "_quant_", as.character(quant), "_horiz_", as.character(horiz), ".RDS", sep = "")
  wis_truth_tmp <- readRDS(file = tmp_file)
  wis[[i]] <- wis_truth_tmp$df_wis
  if (i == 1) {
    tmp_avg <- wis[[i]]$wis
  } else {
    tmp_avg <- tmp_avg + wis[[i]]$wis
  }
}
tmp_avg <- tmp_avg / nrow(state_age)

wis_avg <- wis[[1]]
wis_avg$wis <- tmp_avg

if (FALSE) { # Temporary
  wis_avg[wis_avg$model %in% c("ISW (St.1.)", "ISW (St.2.)"), "wis"] <- 0
}

wis_summ <- wis_avg %>% group_by(model) %>% mutate(total = sum(wis))
p <- plot_wis_bar(df_wis = wis_avg, wis_summ = wis_summ[1:length(models), "total"], models = models, colors = colors, ylim_manual = (max(wis_summ$wis) * 3))
p + ggtitle(group)

##################################################
