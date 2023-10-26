cd ~/Desktop/TESTS/ensemble_learning/

# Shared across horizons, plug in recent data
# skip_recent_days, horiz, post_select_mod, method, cluster_size 

Rscript main.R FALSE TRUE Epiforecasts Mean 4 &
Rscript main.R FALSE TRUE ILM          Mean 4 &
Rscript main.R FALSE TRUE KIT          Mean 4 &
Rscript main.R FALSE TRUE LMU          Mean 4 &
Rscript main.R FALSE TRUE RIVM         Mean 4 &
Rscript main.R FALSE TRUE RKI          Mean 4 &
Rscript main.R FALSE TRUE SU           Mean 4 &
Rscript main.R FALSE TRUE SZ           Mean 4 &
Rscript main.R FALSE TRUE Mean         Mean 4 &
Rscript main.R FALSE TRUE Median       Mean 4 
