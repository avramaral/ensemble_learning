cd ~/Desktop/TESTS/ensemble_learning/

# Shared across horizons, plug in recent data
# skip_recent_days, horiz, post_select_mod, method, cluster_size 

Rscript main.R FALSE FALSE Epiforecasts Mean 4 &
Rscript main.R FALSE FALSE ILM          Mean 4 &
Rscript main.R FALSE FALSE KIT          Mean 4 &
Rscript main.R FALSE FALSE LMU          Mean 4 &
Rscript main.R FALSE FALSE RIVM         Mean 4 &
Rscript main.R FALSE FALSE RKI          Mean 4 &
Rscript main.R FALSE FALSE SU           Mean 4 &
Rscript main.R FALSE FALSE SZ           Mean 4 &
Rscript main.R FALSE FALSE Mean         Mean 4 &
Rscript main.R FALSE FALSE Median       Mean 4 
