# code to simulate example data from the PanicModel and plot it

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

# Simulate
total_days <- treatment_days + 2*7 # treatment + two-week follow up
time <- total_days*60*24 # full treatment period
n_weeks_T <- total_days/7

# # COMMENTED OUT AND LOADED FROM FILE ON LINE 43
# set.seed(2)
# out_treat <- simPanic(time = 1:time,
#                       stepsize = .001,
#                       initial = list("S" = 1, "X" = 0), # Maximum severity
#                       tx = tx)
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

