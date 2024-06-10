#!/bin/bash
# Job name:
#SBATCH --job-name=hazard_vs_multinomial
#
# Partition:
#SBATCH --partition=savio3
#
#SBATCH --qos=biostat_savio3_normal
#SBATCH --account=co_biostat
#
# Wall clock limit ('0' for unlimited):
#SBATCH --time=72:00:00
#
# Number of nodes for use case:
#SBATCH --nodes=1
#SBATCH --ntasks=4
#SBATCH --cpus-per-task=1
#
# Mail type:
#SBATCH --mail-type=all
#
# Mail user:
#SBATCH --mail-user=sky.qiu@berkeley.edu

module load r

R CMD BATCH --no-save 001_run_sim_n_500.R logs/001_run_sim_n_500.Rout &
R CMD BATCH --no-save 001_run_sim_n_1000.R logs/001_run_sim_n_1000.Rout &
R CMD BATCH --no-save 001_run_sim_n_1500.R logs/001_run_sim_n_1500.Rout &
R CMD BATCH --no-save 001_run_sim_n_2000.R logs/001_run_sim_n_2000.Rout &
wait
