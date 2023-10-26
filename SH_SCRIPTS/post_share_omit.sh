cd ~/Desktop/TESTS/ensemble_learning/

# Shared across horizons, omit recent data
# skip_recent_days, horiz, post_select_mod, method, cluster_size 

Rscript main.R TRUE FALSE Epiforecasts Mean 4 &
Rscript main.R TRUE FALSE ILM          Mean 4 &
Rscript main.R TRUE FALSE KIT          Mean 4 &
Rscript main.R TRUE FALSE LMU          Mean 4 &
Rscript main.R TRUE FALSE RIVM         Mean 4 &
Rscript main.R TRUE FALSE RKI          Mean 4 &
Rscript main.R TRUE FALSE SU           Mean 4 &
Rscript main.R TRUE FALSE SZ           Mean 4 &
Rscript main.R TRUE FALSE Mean         Mean 4 &
Rscript main.R TRUE FALSE Median       Mean 4 
