# Reproducibility ARchive

Reproducibility archive for the paper Ryan, O., Haslbeck, J. M. B. \& Robinaugh ,D. J. (preprint) ""Improving Treatments for Mental Disorders using Computational Models"". [Preprint here](XXX)

# Overview

## Simulation

The folder `/Simulation` contains all files to reproduce the simulated treatments reported in the paper. The folder contains the following files and folders:

- `SimScript.R` contains the script for simulating the baseline and all five treatment arms for 16 subjects. The run time of this script is below 6h parallelized across 16 cores with 2.5GHZ.
- `submit_jobs.sh` is a bash script to call `SimScript.R` on the (now defunct) Surf LISA supercomputer system of the University of Amsterdam
- `submit_all.sh` is a bash script to call `submit_jobs.sh` 32 times, which gives us 512 subjects. We report the first 500 in the paper.
- The folder `output` contains 32 output files of `SimScript.R` which each contain the data of 16 subjects

## Creating Figures

- `sim_preprocess.R` contains functions to map the minute-to-minute data of components to weekly symptom scores
- `analysis_figures.R` contains the code to re-create all (results) figures in the apper





# STUFF OISIN


This repository includes the following

- `/simcode/`: folder containing all scripts to run the treatment simulation. Sub-folder  `/output/`contains all raw output files of the simulation
- `/files/` contains all processed simulation files, `outfiles.RDS` and `symptoms_out.RDS`, used in creating figures and in analyses
- `/figures/` stores figures created by analysis files
- `sim_preprocess.R` loads all files in `/simcode/output/`,creates `outfiles.RDS` and `symptoms_out.RDS` and saves them in `/files/`
- `symptom_functions.R` contains helper functions to compute symptom scores from raw simulated output
- `analysis_figures.R` creates all figures
- `symptom_analyze.R`; TEMP: OLD FILE FOR REFERENCE

