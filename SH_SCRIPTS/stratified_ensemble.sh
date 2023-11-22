# cd ~/Desktop/TESTS/ensemble_learning/
cd ~/Desktop/ensemble_learning/

# Stratified analysis
# ens_method, horiz, strata, cluster_size

# Rscript main.R pinball FALSE states 36 &
# Rscript main.R pinball FALSE ages   16 &
# Rscript main.R pinball TRUE  states 36 &
# Rscript main.R pinball TRUE  ages   16 &

Rscript main.R wis     FALSE  states 1 ;
Rscript main.R wis     FALSE  ages   1 ;
Rscript main.R wis     TRUE   states 1 ;
Rscript main.R wis     TRUE   ages   1

