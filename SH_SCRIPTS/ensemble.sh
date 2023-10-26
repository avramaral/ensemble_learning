cd ~/Desktop/TESTS/ensemble_learning/

# Shared across horizons, plug in recent data
# skip_recent_days, horiz, method, cluster_size 

Rscript main.R TRUE  FALSE Mean      32 &
Rscript main.R FALSE FALSE Mean      32 &
Rscript main.R FALSE FALSE all_quant 8  &
Rscript main.R FALSE TRUE  Mean      40 

