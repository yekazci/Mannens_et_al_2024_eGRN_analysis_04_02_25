#!/bin/bash

# Submit this script inside parent folder of python_scripts.

# SLURM directives:

#SBATCH -J tf_activity
#SBATCH --export=ALL
#SBATCH --cpus-per-task=32
#SBATCH --mem=256G
#SBATCH --mail-type=BEGIN
#SBATCH --mail-type=END
#SBATCH --mail-user=yusufenes.kazci@mdc-berlin.de
#SBATCH --output=slurm_output_%j.txt
#SBATCH --error=slurm_error_%j.txt
#SBATCH --chdir=./                # Set the working directory to the current, which is default.


Rscript ../r_scripts/tf_activity_by_celltype.r

echo "completed."