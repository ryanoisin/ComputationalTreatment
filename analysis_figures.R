# o.ryan@umcutrecht.nl & jonashaslbeck@gmail.com

# --------------------------------------------------------------------------- #
# --------------------------------- What is this? --------------------------- #
# --------------------------------------------------------------------------- #

# this file loads simulated output objects and creates the figures and calculations in main text
# those are Figures 3 -8

library(PanicModel)
library(RColorBrewer)
library(scales)

source("symptom_functions.R")

# --------------------------------------------------------------------------- #
# --------------------------------- Figure 3 -------------------------------- #
# --------------------------------------------------------------------------- #

# ----- Specify Intervention -----
# Here: CBT
baseline_days <- 8
baseline_weeks <- 0 # one week baseline
# March 8th: This now matches the simulation

tx <- list("I1" = baseline_days+baseline_weeks*7+c(1), # Psychoeducation -> AS
           "I2" = baseline_days+baseline_weeks*7+c(1), # Psychoeducation -> ES
           "I3" = (baseline_days+baseline_weeks+(1:2)*7)+c(1), # Cognitive restructuring -> AS
           "I4" = baseline_days+c((baseline_weeks+1)*7+1:28), # Interoceptive Exposure -> A, E
           "I5" = baseline_days+(baseline_weeks+3)*7+(1:14)[rep(c(TRUE,TRUE), 7)]) # In vivo exposure -> Context

treatment_days <- max(unlist(lapply(tx, max)))


# ----- Simulate from Panic Model -----

# Fill in last values from above:
last_row <- out_base$outmat[nrow(out_base$outmat), ]
initial_specified <- list("S" = last_row$S,
                          "A" = last_row$A,
                          "PT" = last_row$PT,
                          "E" = last_row$E,
                          "X" = last_row$X)

# Simulate
total_days <- treatment_days + 2*7 # treatment + two-week follow up
time <- total_days*60*24 # full treatment period
n_weeks_T <- total_days/7

set.seed(2)
out_treat <- simPanic(time = 1:time,
                      stepsize = .001,
                      initial = initial_specified,
                      tx = tx)
results_treat <- out_treat$outmat

# saveRDS(out_treat, file="Files/data_treat_1w_baseline_2w_followup.RDS")
out_treat <- readRDS(file="Files/data_treat_1w_baseline_2w_followup.RDS")

SimTreat <- out_treat$outmat

# ----- Compute Symptoms per week -----
week_length_T <- nrow(out_treat$outmat)/n_weeks_T
l_symptoms_T <- list()
for(i in 1:n_weeks_T) {
  l_symptoms_T[[i]] <- getSymptoms(out_treat$outmat[(1+(i-1)*week_length_T):(week_length_T*i),])
}

m_symptoms_T <- do.call(rbind, l_symptoms_T)


# ----- Make Figure -----

display.brewer.all(n=NULL, type="all", select=NULL, exact.n=TRUE, colorblindFriendly=FALSE)

# cols <- brewer.pal(5, "Dark2")
cols <- c("grey", "black", "lightblue", "tomato", "orange", "#dd1c77")

sc <- 0.8
pdf("Figures/Fig_SimData_Treatment_July18_23.pdf", width = 7*sc, height = 7*sc)

# Make Layout
lmat <- matrix(1:3, nrow=3)
lo <- layout(lmat, heights = c(.35, 0.8, .55))
# layout.show(lo)

# --- Panel: Treatment ---
int_labels <- c("Psychoeduction",
                "Cognitive Restructuring",
                "Interoceptive Exposure",
                "In Vivo Exposure")
par(mar=c(0,4.5,0,2))
plot.new()
plot.window(xlim = c(0, total_days),  ylim=c(0, 1.2))
segments(rep(-2, 4), v_trt_y, rep(total_days, 4), v_trt_y, col="black", lty=1)
text(rep(-2, 4), v_trt_y+0.075, labels = int_labels, adj=0,  col="black")
pch_intero <- abs(out_treat$adherence-1) + 20
cex_intero <- pch_intero
cex_intero[cex_intero==20] <- 1.5
cex_intero[cex_intero==21] <- 1.1

points(tx[[1+1]], rep(v_trt_y[1], length(tx[[1+1]])), pch=20, cex=1.5)
points(tx[[2+1]], rep(v_trt_y[2], length(tx[[2+1]])), pch=20, cex=1.5)
points(tx[[3+1]], rep(v_trt_y[3], length(tx[[3+1]])), pch=pch_intero, cex=cex_intero, bg="white")
points(tx[[4+1]], rep(v_trt_y[4], length(tx[[4+1]])), pch=20, cex=1.5)

# --- Panel: Panic Model Data ---
# right data
lwd <- 1
par(mar=c(1,4.5,0,2))
plot.new()
plot.window(xlim = c(0, length(SimTreat$A)),  ylim=c(-0.6, 1.1))
axis(2, las=2, seq(-0.5, 1, length=7))
no_x_ticks <- 9

lwd <- 1.5
lines(SimTreat$A, col = cols[1], lwd = lwd)
lines(SimTreat$PT, col = cols[2],lwd = lwd)
lines(SimTreat$E, col = cols[3], lwd = lwd, lty=2)
lines(SimTreat$X, col = cols[5],lwd = lwd)
lines(SimTreat$S, col = cols[4],  lwd = lwd)

# Legend
rect(10260*6.34, 0.5, 10260*8, 1, col="white", border=FALSE)
legend("topright", legend = c("Arousal","Perceived Threat", "Escape Behavior", "Arousal Schema", "Escape Schema"),
       text.col = cols ,cex = 1.15, ncol=1
       ,bty = "n"
)

# --- Panel: Symptoms ---
# Rigt: symptom data
par(mar=c(4,4.5,0,2))
plot.new()
plot.window(xlim=c(0, total_days), ylim=c(0,20))
axis(1, 0:8, at=seq(0, total_days, length=9))
axis(2, las=2)
mtext("Weeks", side = 1, line = 2.5, cex = 0.8)
lines(0:total_days, c(NA,NA,rep(m_symptoms_T[, 6], each=7)), col=cols[6], lwd=lwd)

legend("topright", legend = c("Symptom Sum Score"),
       text.col = cols[6], bty = "n",cex = 1.15, ncol=1)


dev.off()



# --------------------------------------------------------------------------- #
# --------------------------------- PROCESSING FOR FIGURES 4,5,8 -------------------------------- #
# --------------------------------------------------------------------------- #
# jonashaslbeck@protonmail.com; July 21, 2023

# -------------------------------------------------
# -------- Loading Simulated Data -----------------
# -------------------------------------------------

simDir <- "Simulation/output"

# ----- Collect only baseline data from all 5xx people -----
l_base_pers <- list()
counter <- 1
for(i in 1:32) {

  batch_i <- readRDS(paste0(simDir, "/IntSim6_Iter", i, ".RDS"))

  for(j in 1:16) {

    l_base_pers[[counter]] <- batch_i[[j]]$out_baseline
    counter <- counter + 1
  }
  print(i)
}

length(l_base_pers)



# -------------------------------------------------
# -------- Subset Subject 1 -----------------------
# -------------------------------------------------

# ------ Pick one person -----

l_person_i <- l_base_pers[[1]]
DataP1 <- l_person_i$outmat

head(DataP1)


# -------------------------------------------------
# -------- Get Symptoms of Subject 1 (Basline) ----
# -------------------------------------------------

n_weeks <- 4
week_length <- (nrow(DataP1)-1)/n_weeks
m_symptoms <- matrix(NA, 4, 6)

for(i in 1:4) {
  symptoms_lw_i <-  getSymptoms(l_person_i$outmat[(1+(i-1)*week_length):(week_length*i),])
  m_symptoms[i, ] <- symptoms_lw_i[1, 1:6]

  print(i)
} # end for: weeks

m_symptoms_Subj1_Baseline <- m_symptoms

# -------------------------------------------------
# -------- Get Symptoms of Subject X (All arms) ---
# -------------------------------------------------

# ----- Get Data from Subject 109 -----
# Selecting this one, because it shows different behavior for BT and CBT
id <- 109
i <- floor(id/16)
batch_i <- readRDS(paste0(simDir, "/IntSim6_Iter", i+1, ".RDS"))
l_subj1 <- batch_i[[id-(i*16)]]
l_symptoms <- list()

# ----- From Baseline -----
m_symptoms <- matrix(NA, 4, 6)
for(i in 1:4) {
  outmat_ji <- l_subj1$out_baseline$outmat
  symptoms_lw_i <-  getSymptoms(outmat_ji[(1+(i-1)*week_length):(week_length*i),])
  m_symptoms[i, ] <- symptoms_lw_i[1, 1:6]
} # end for: weeks
l_symptoms$baseline <- m_symptoms


# ----- From Four Treatment arms (Control, CT, BT, CBT) -----
m_symptoms <- matrix(NA, 17, 6)
for(j in 1:4) {
  for(i in 1:17) {
    outmat_ji <- l_subj1[[j+2]]$outmat
    symptoms_lw_i <-  getSymptoms(outmat_ji[(1+(i-1)*week_length):(week_length*i),])
    m_symptoms[i, ] <- symptoms_lw_i[1, 1:6]
  }
  l_symptoms[[j+1]] <- m_symptoms
}


# -------------------------------------------------
# -------- Get Symptom SS of all Subjects ---------
# -------------------------------------------------

# ----- Collect only baseline data from all 5xx people -----
a_sum_treat <- array(NA, dim=c(500, 5+1, 21))

counter <- 1
for(i in 1:32) {

  batch_i <- readRDS(paste0(simDir, "/IntSim6_Iter", i, ".RDS"))

  for(j in 1:16) {

    batch_ij <- batch_i[[j]]

    ## Get SS for Baseline
    outmat_ji <- batch_ij$out_baseline$outmat
    v_SS <- rep(NA, 4)
    for(w in 1:4) v_SS[w] <- getSymptoms(outmat_ji[(1+(w-1)*week_length):(week_length*w),])[1,6]
    a_sum_treat[counter, 1, 1:4] <- v_SS # Save
    ## Get SS for 4 Treatment arms
    for(t in 1:5) {
      outmat_ji <- batch_ij[[t+2]]$outmat
      v_SS <- rep(NA, 17)
      for(w in 1:17) v_SS[w] <- getSymptoms(outmat_ji[(1+(w-1)*week_length):(week_length*w),])[1,6]
      a_sum_treat[counter, t+1, 5:21] <- v_SS # Save
    }

    counter <- counter + 1
  }

  print(i)
}

# saveRDS(a_sum_treat, file="a_sum_treat.RDS")
# a_sum_treat <- readRDS(file="a_sum_treat.RDS")


# ----- Get Data from Subject 109 -----
# Selecting this one, because it shows different behavior for BT and CBT
id <- 109
i <- floor(id/16)
batch_i <- readRDS(paste0(simDir, "/IntSim6_Iter", i+1, ".RDS"))
l_subj1 <- batch_i[[id-(i*16)]]
l_symptoms <- list()

# ----- From Baseline -----
m_symptoms <- matrix(NA, 4, 6)
for(i in 1:4) {
  outmat_ji <- l_subj1$out_baseline$outmat
  symptoms_lw_i <-  getSymptoms(outmat_ji[(1+(i-1)*week_length):(week_length*i),])
  m_symptoms[i, ] <- symptoms_lw_i[1, 1:6]
} # end for: weeks
l_symptoms$baseline <- m_symptoms


# ----- From Four Treatment arms (Control, CT, BT, CBT) -----
m_symptoms <- matrix(NA, 17, 6)
for(j in 1:4) {
  for(i in 1:17) {
    outmat_ji <- l_subj1[[j+2]]$outmat
    symptoms_lw_i <-  getSymptoms(outmat_ji[(1+(i-1)*week_length):(week_length*i),])
    m_symptoms[i, ] <- symptoms_lw_i[1, 1:6]
  }
  l_symptoms[[j+1]] <- m_symptoms
}



# --------------------------------------------------------------------------- #
# --------------------------------- Figure 4 -------------------------------- #
# --------------------------------------------------------------------------- #
# jonashaslbeck@protonmail.com; July 21, 2023

cols_tr <- c("black", brewer.pal(4, "Set1"))

sc <- 0.9
pdf("Figures/Fig_Treat_Subj109.pdf", width=8.5*sc, height=5*sc)

# Canvas
par(mar=c(3.5, 4, 2, 1))
plot.new()
plot.window(xlim=c(0,21), ylim=c(0,21.5))
axis(1, c(0:9, 12, 15, 18, 21))
axis(2, las=2)
title(xlab="Weeks", line=2.3)
title(ylab="Symptom Sum Score", line=2.3)

## Blocks indicating phase
cex_titles <- 1.1
v_greys <- c("#f5f5f5", "#cfcfcf", "#dedede")
# Baseline
abline(v=c(0, 4), lty=2)
text((0+4)/2, 21.5, "Baseline", col = "black", cex=cex_titles)
# Treatment
abline(v=c(4, 9), lty=2)
text((4+9)/2, 21.5, "Treatment", col = "black", cex=cex_titles)
# Follow up
abline(v=c(9, 21), lty=2)
text((9+21)/2, 21.5, "Follow-up", col = "black", cex=cex_titles)

## Data
lwd <- 3
# Baseline
lines(1:4, l_symptoms[[1]][, 6], lwd=lwd)
for(i in 1:4) lines(4:5, c(l_symptoms[[1]][4, 6], l_symptoms[[i+1]][1, 6]), lwd=lwd, col=cols_tr[i]) # connect the two
# Treatment
for(i in 1:4) lines(5:21, l_symptoms[[i+1]][, 6], col=cols_tr[i], lwd=lwd)

l_symptoms

legend(13,13, legend=c("Control", "Cognitive only",
                       "Behavioral only", "Cognitive & Behavioral"), bty="n", text.col=cols_tr)


dev.off()


# --------------------------------------------------------------------------- #
# --------------------------------- Figure 5 -------------------------------- #
# --------------------------------------------------------------------------- #
# jonashaslbeck@protonmail.com; July 21, 2023


# Some settings
v_mtexts <- c("Control Group", "Cognitive only",
              "Behavioral only", "Cognitive and Behavioral")
alpha_val <- 0.3

# Plotting
pdf("Figures/Fig_Treatment_N500.pdf", width = 9, height = 6)

par(mfrow=c(2,2))

for(j in 1:4) {

  # Canvas
  par(mar=c(3.5, 4, 2, 1))
  plot.new()
  plot.window(xlim=c(0,21), ylim=c(0,21.5))
  axis(1, c(0:9, 12, 15, 18, 21))
  axis(2, las=2)
  if(j>2) title(xlab="Weeks", line=1.9)
  title(ylab="Symptom Sum Score", line=2.3)
  mtext(v_mtexts[j], side=3, col=cols_tr[j], line=.35, cex=1.15)

  ## Blocks indicating phase
  cex_titles <- 0.9
  # Baseline
  abline(v=c(0, 4), lty=2)
  text((0+4)/2, 21.5, "Baseline", col = "black", cex=cex_titles)
  # Treatment
  abline(v=c(4, 9), lty=2)
  text((4+9)/2, 21.5, "Treatment", col = "black", cex=cex_titles)
  # Follow up
  abline(v=c(9, 21), lty=2)
  text((9+21)/2, 21.5, "Follow-up", col = "black", cex=cex_titles)

  # Loop in data of N=500
  for(i in 1:500) {
    lines(a_sum_treat[i, 1, ], col=alpha("black", alpha=alpha_val))
    lines(4:5, c(a_sum_treat[i, 1, 4], a_sum_treat[i, j+1, 5]), col=alpha(cols_tr[j], alpha=alpha_val))
    lines(a_sum_treat[i, j+1, ], col=alpha(cols_tr[j], alpha=alpha_val))
  }

} # end for: j, treatments

dev.off()

# --------------------------------------------------------------------------- #
# --------------------------------- Figure 6 -------------------------------- #
# --------------------------------------------------------------------------- #


# -> Jonas: This is your figure; I just fixed some problems here in your code & folder

# Jonas
# CBT vs BT effectiveness
# left panel; symptom trajectories
# right panel; ES and AS trajectories

# TODO OR: Add bottom panel

# --------------------------------------------------------------------------- #
# --------------------------------- Figure 7 -------------------------------- #
# --------------------------------------------------------------------------- #

# Treatment effectiveness as a function of between-person characteristics


# --------------------------------------------------------------------------- #
# --------------------------------- Figure 8 -------------------------------- #
# --------------------------------------------------------------------------- #
# jonashaslbeck@protonmail.com; July 21, 2023

# Plotting
sc <- 0.9
pdf("Figures/Fig_Treatment_onlyCBT+_N500.pdf", width=8.5*sc, height=5*sc)

j <- 5

# Canvas
par(mar=c(3.5, 4, 2, 1))
plot.new()
plot.window(xlim=c(0,21), ylim=c(0,21.5))
axis(1, c(0:9, 12, 15, 18, 21))
axis(2, las=2)
if(j>2) title(xlab="Weeks", line=1.9)
title(ylab="Symptom Sum Score", line=2.3)
mtext(v_mtexts[j], side=3, col=cols_tr[j], line=.35, cex=1.15)

## Blocks indicating phase
cex_titles <- 1.1
# Baseline
abline(v=c(0, 4), lty=2)
text((0+4)/2, 21.5, "Baseline", col = "black", cex=cex_titles)
# Treatment
abline(v=c(4, 9), lty=2)
text((4+9)/2, 21.5, "Treatment", col = "black", cex=cex_titles)
# Follow up
abline(v=c(9, 21), lty=2)
text((9+21)/2, 21.5, "Follow-up", col = "black", cex=cex_titles)

# Loop in data of N=500
for(i in 1:500) {
  lines(a_sum_treat[i, 1, ], col=alpha("black", alpha=alpha_val))
  lines(4:5, c(a_sum_treat[i, 1, 4], a_sum_treat[i, j+1, 5]), col=alpha(cols_tr[j], alpha=alpha_val)) # Baseline
  lines(a_sum_treat[i, j+1, ], col=alpha(cols_tr[j], alpha=alpha_val))
}

dev.off()
