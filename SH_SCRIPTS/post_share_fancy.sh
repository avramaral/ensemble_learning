cd ~/Desktop/TESTS/ensemble_learning/

# Shared across horizons, plug in recent data
# skip_recent_days, horiz, post_select_mod, method, cluster_size 

Rscript main.R FALSE FALSE Epiforecasts all_quant 4 &
Rscript main.R FALSE FALSE ILM          all_quant 4 &
Rscript main.R FALSE FALSE KIT          all_quant 4 &
Rscript main.R FALSE FALSE LMU          all_quant 4 &
Rscript main.R FALSE FALSE RIVM         all_quant 4 &
Rscript main.R FALSE FALSE RKI          all_quant 4 &
Rscript main.R FALSE FALSE SU           all_quant 4 &
Rscript main.R FALSE FALSE SZ           all_quant 4 &
Rscript main.R FALSE FALSE Mean         all_quant 4 &
Rscript main.R FALSE FALSE Median       all_quant 4 
