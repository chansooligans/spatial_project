#!/bin/bash
#
#SBATCH --job-name=Sp
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=16
#SBATCH --mem=120GB
#SBATCH --time=01:00:00

module purge
module load r/intel/3.4.2

R --no-save -q -f parallel_dist_spacial_bk.R