# ComputationalTreatment
Code archive to reproduce all analysis, simulations and figures in the paper Ryan, O., Haslbeck, J. M. B. \& Robinaugh ,D. J. (preprint) ""Improving Treatments for Mental Disorders using Computational Models"". [Preprint here](XXX)

# Overview

This repository includes the following
``
-`/simcode/`: folder containing all scripts to run the treatment simulation. Sub-folder `/output/`contains all raw output files of the simulation
- `/files/` contains all processed simulation files, `outfiles.RDS` and `symptoms_out.RDS`, used in creating figures and in analyses
- `/figures/` stores figures created by analysis files
-`sim_preprocess.R` loads all files in `/simcode/output/`,creates `outfiles.RDS` and `symptoms_out.RDS` and saves them in `/files/`
- `symptom_functions.R` contains helper functions to compute symptom scores from raw simulated output
- `analysis_figures.R` creates all figures
- `symptom_analyze.R`; TEMP: OLD FILE FOR REFERENCE

