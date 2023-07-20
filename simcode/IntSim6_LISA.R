# jonashaslbeck@protonmail.com; Feb 11, 2023

# --------------------------------------------------------------
# ---------- Get Iteration Number ------------------------------
# --------------------------------------------------------------

# !/usr/bin/env Rscript
iter <- commandArgs(trailingOnly=TRUE)
print(iter)
iter <- as.numeric(iter)

# ----------------------------------------------------------------------
# ----- Load Packages --------------------------------------------------
# ----------------------------------------------------------------------

# Panic Model Sim
library(PanicModel)

# Parallel
library(foreach)
library(parallel)
library(doParallel)


# ----------------------------------------------------------------------
# ----- Specify Treatments ---------------------------------------------
# ----------------------------------------------------------------------

# Updated based on info from Don on June 15

l_tx <- list()

baseline_weeks <- 0
baseline_days <- 1

# Control group (no intervention)
l_tx[[1]] <- list("I1" = NULL, # Psychoeducation -> AS
                  "I2" = NULL, # Psychoeducation -> ES
                  "I3" = NULL, # Cognitive restructuring -> AS
                  "I4" = NULL, # Interoceptive Exposure -> A, E
                  "I5" = NULL) # In vivo exposure -> Context

# Cognitive
l_tx[[2]] <- list("I1" = baseline_days+baseline_weeks*7+c(1), # Psychoeducation -> AS
                  "I2" = baseline_days+baseline_weeks*7+c(1), # Psychoeducation -> ES
                  "I3" = (baseline_days+baseline_weeks+(1:4)*7)+c(1), # Cognitive restructuring -> AS
                  "I4" = NULL, # Interoceptive Exposure -> A, E
                  "I5" = NULL) # In vivo exposure -> Context

# Behavioral
l_tx[[3]] <- list("I1" = baseline_days+baseline_weeks*7+c(1), # Psychoeducation -> AS
                  "I2" = baseline_days+baseline_weeks*7+c(1), # Psychoeducation -> ES
                  "I3" = NULL, # Cognitive restructuring -> AS
                  "I4" = baseline_days+c((baseline_weeks+1)*7+1:28), # Interoceptive Exposure -> A, E
                  "I5" = baseline_days+(baseline_weeks+3)*7+(1:14)[rep(c(TRUE,TRUE), 7)]) # In vivo exposure -> Context

# Cognitive + Behavioral
l_tx[[4]] <- list("I1" = baseline_days+baseline_weeks*7+c(1), # Psychoeducation -> AS
                  "I2" = baseline_days+baseline_weeks*7+c(1), # Psychoeducation -> ES
                  "I3" = (baseline_days+baseline_weeks+(1:2)*7)+c(1), # Cognitive restructuring -> AS
                  "I4" = baseline_days+c((baseline_weeks+1)*7+1:28), # Interoceptive Exposure -> A, E
                  "I5" = baseline_days+(baseline_weeks+3)*7+(1:14)[rep(c(TRUE,TRUE), 7)]) # In vivo exposure -> Context

# NEW Cognitive + Behavioral [with focus on Escape]
l_tx[[5]] <- list("I1"=baseline_days+baseline_weeks*7+c(1),
                  "I2"=(baseline_days+baseline_weeks+(0:2)*7)+c(1), # Psychoeducation + Self-efficacy Training --> ES: Session 1-3
                  "I3"=NULL,  # Cog Restructure --> AS: NULL
                  "I4"=(baseline_days+baseline_weeks*7)+7+(1:(7*4)),
                  "I5"=baseline_days+(baseline_weeks+3)*7+(1:14)[rep(c(TRUE,TRUE), 7)])



# ----------------------------------------------------------------------
# ----- Iterate --------------------------------------------------------
# ----------------------------------------------------------------------

# ----- Iterating -----

# Setup Parallelization
cluster <- 16
cl <- makeCluster(cluster, outfile="")
registerDoParallel(cl)

timer_total <- proc.time()[3]

set.seed(iter)

out_iter <- foreach(batch = 1:16,
                    .packages = c("PanicModel"),
                    .export = c("l_tx"),
                    .verbose = TRUE) %dopar% {

                      # ----- Inter-individual differences / initial values ------

                      # Arousal Schema & Escape Schema initial values
                      N <- 20 # making sure at least one is valid (see below)
                      AS_init <- rnorm(n=N, .8, .1) # creates ~20% panic attacks (matched to lifetime prevalence)
                      AS_initth <- AS_init[AS_init<=1 & AS_init>=0]
                      AS_init <- AS_initth[1]
                      ES_init <- rnorm(n=N, .2, .1)
                      ES_initth <- ES_init[ES_init<=1 & ES_init>=0]
                      ES_init <- ES_initth[1]

                      initial_spec <- list("S" = AS_init,
                                           "X" = ES_init)

                      # ----- Treatment response parameters -----

                      # From Don: 28th April:
                      pars_spec <- list("Tx" = list("I123_alpha" = rbeta(n=1, 1, 9),
                                                    "I4Adh" = rbeta(n=1, 3, 2),
                                                    "I4RdEs" = rbeta(n=1, 3, 2)))

                      # ----- Simulate ------

                      time0 <- 0:(60*24*7*4) # 4 week baseline

                      # Simulate
                      out_0 <- simPanic(time = time0,
                                        stepsize = .001,
                                        parameters = pars_spec,
                                        initial = initial_spec,
                                        pbar = FALSE)

                      # Round to 3 decimals to save disk space
                      out_0$outmat <- round(out_0$outmat, 3)

                      # --- Feed last values of baseline in as initial values ---
                      last_row <- out_0$outmat[nrow(out_0$outmat), ]
                      initial_spec2 <- list("A" = last_row$A,
                                            "S" = last_row$S,
                                            "H" = last_row$H,
                                            "PT" = last_row$PT,
                                            "E" = last_row$E,
                                            "X" = last_row$X)

                      # --- make batch specific seed ----
                      seed_batch <- as.numeric(paste0(iter, 999, batch))

                      # ----- Loop through 4 Treatment arms -----

                      l_out <- list()

                      time1 <- 0:(60*24*7*(5+12)) # 5 weeks treatment + 12 weeks follow up

                      for(group in 1:5) {

                        # ----- Simulate Control vs. Treatments -----

                        set.seed(seed_batch) # random input constant across treatment arms/groups

                        # Simulate
                        out_x <- simPanic(time = time1,
                                          stepsize = .001,
                                          parameters = pars_spec,
                                          initial = initial_spec2,
                                          tx = l_tx[[group]],
                                          pbar = FALSE)

                        # Round to 3 decimals to save disk space
                        out_x$outmat <- round(out_x$outmat, 3)
                        l_out[[group]] <- out_x

                      } # end for: group

                      # ----- Return ------

                      outlist <- list("heter" = list("S" = AS_init,
                                                     "X" = ES_init,
                                                     "I123_alpha" = pars_spec$Tx$I123_alpha,
                                                     "I4Adh" = pars_spec$Tx$I4Adh,
                                                     "I4RdEs" = pars_spec$Tx$I4RdEs),
                                      "out_baseline" = out_0,
                                      "out_group1" = l_out[[1]],
                                      "out_group2" = l_out[[2]],
                                      "out_group3" = l_out[[3]],
                                      "out_group4" = l_out[[4]],
                                      "out_group5" = l_out[[5]],
                                      "batchseed" = seed_batch)

                      return(outlist)

                    } # end foreach


# print total time of nodes
print(paste0("Full Timing Iteration ", iter, ":"))
proc.time()[3] - timer_total

stopCluster(cl)

# ----------------------------------------------------------------------
# ----- Export ---------------------------------------------------------
# ----------------------------------------------------------------------

# Save
saveRDS(out_iter, file = paste0("IntSim6_Iter", iter, ".RDS"))
