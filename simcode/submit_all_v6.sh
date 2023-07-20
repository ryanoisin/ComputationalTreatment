#!/bin/bash

mkdir "$TMPDIR"/IntSim6/

cd "$HOME"/IntSim6

sbatch -a 3-32 submit_jobs_v6.sh
