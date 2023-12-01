source("header.R")
source("utils.R")
source("aux.R")

data <- read_csv(file = "DATA/data.csv.gz")
truth_data <- read_csv(file = "DATA/truth_40d.csv.gz")

KIT_frozen_baseline <- data |> filter(model == "KIT-frozen_baseline")

state <- "DE"
age   <- "00+"

training_size <- 90

models <- c("Epiforecasts", "ILM", "KIT", "LMU", "RIVM", "RKI", "SU", "SZ") 
colors <- c("#B30000", "#E69F00", "#56B4E9", "#F0E442", "#80471C", "#3C4AAD", "#CC79A7", "#000000") 

filtered_data <- filter_data(data = data, truth_data = truth_data, models = models, loc = state, age_gr = age, extra_delay = 7, truth_past = training_size)
data <- filtered_data$data
truth_data <- filtered_data$truth_data

r <- range(data$forecast_date)
truth_data <- truth_data |> filter((date >= r[1]) & (date <= r[2]), location == "DE", age_group == "00+")

KIT_frozen_baseline <- KIT_frozen_baseline %>% filter(forecast_date >= r[1], forecast_date <= r[2], age_group %in% age, location %in% state)
baseline <- KIT_frozen_baseline |> filter(quantile == 0.5)

selected_date <- as.Date("2021-03-31")
baseline


horizon <- -28:0
probs <- c(0.025, 0.100, 0.250, 0.500, 0.750, 0.900, 0.975)




complete_data <- truth_data$truth
