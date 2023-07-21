#!/bin/bash

mkdir "$TMPDIR"/IntSim/

cd "$HOME"/IntSim

sbatch -a 1-32 submit_jobs.sh
