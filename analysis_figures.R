# o.ryan@umcutrecht.nl & jonashaslbeck@protonmail.com

# --------------------------------------------------------------------------- #
# --------------------------------- What is this? --------------------------- #
# --------------------------------------------------------------------------- #

# this file loads simulated output objects and creates the figures and calculations in main text
# those are Figures 3 -8

library(devtools)
# install_github("jmbh/PanicModel")

library(PanicModel)
library(RColorBrewer)
library(scales)

source("symptom_functions.R")

# For plotting: create function for color gradient
color.gradient <- function(x, colors=c("red","blue"), colsteps=100) {
  return( colorRampPalette(colors) (colsteps) [
    findInterval(x, seq(min(x),max(x), length.out=colsteps)) ] )
}

# --------------------------------------------------------------------------- #
# --------------------------------- Load Preprocessed Files ----------------- #
# --------------------------------------------------------------------------- #

# From: sim_preprocess.R
symplist <- readRDS("Files/symptoms_out.RDS")
# symplist$baseline[[1]] # weekly symptoms during baseline participant ID = 1
# symplist$g1[[1]] # weekly symptoms during treatment for id =1, CONTROL arm
# g2 = cognitive
# g3 = behavioural
# g4 = CBT
# g5 = CBT +

# --------------------------------------------------------------------------- #
# --------------------------------- Figure 4 -------------------------------- #
# --------------------------------------------------------------------------- #
# jonashaslbeck@protonmail.com; July 21, 2023

cols_tr <- c("black", brewer.pal(4, "Set1"))

sc <- 0.9
pdf("Figures/Fig_Treat_Subj109.pdf", width=8.5*sc, height=5*sc)

# Get data from subject 109
l_symptoms_Subj109 <- list()
for(s in 1:5) l_symptoms_Subj109[[s]] <- symplist[[s]][[109]]

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
lines(1:4, l_symptoms_Subj109[[1]][, 6], lwd=lwd)
for(i in 1:4) lines(4:5, c(l_symptoms_Subj109[[1]][4, 6], l_symptoms_Subj109[[i+1]][1, 6]), lwd=lwd, col=cols_tr[i]) # connect the two
# Treatment
for(i in 1:4) lines(5:21, l_symptoms_Subj109[[i+1]][, 6], col=cols_tr[i], lwd=lwd)

# l_symptoms

legend(13,13, legend=c("Control", "Cognitive Only",
                       "Behavioral only", "Cognitive & Behavioral"), bty="n", text.col=cols_tr)

dev.off()


# --------------------------------------------------------------------------- #
# --------------------------------- Figure 5 -------------------------------- #
# --------------------------------------------------------------------------- #
# jonashaslbeck@protonmail.com; July 21, 2023


# Some settings
v_mtexts <- c("Control Group", "Cognitive Only",
              "Behavioral Only", "Cognitive & Behavioral",
              "Revised Cognitive & Behavioral Protocol")
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

    # Baseline
    lines(1:4, symplist$baseline[[i]][, 6], col=alpha("black", alpha=alpha_val))
    # Treatment+Follow up
    lines(4:21, c(symplist$baseline[[i]][4, 6], symplist[[j+1]][[i]][,6]), col=alpha(cols_tr[j], alpha=alpha_val))

  }

} # end for: j, treatments

dev.off()

# --------------------------------------------------------------------------- #
# ------------------- Treatment Effectiveness Descriptives ------------------ #
# --------------------------------------------------------------------------- #

# -------- Object manipualtion before analysis -------------

# For descriptive purposes, add last measurement of baseline to top of each group
symplistcopy <- symplist
n_id <- length(symplist[[1]])

for(id in 1:500){
  for(G in 2:6){
    # last baseline measurement becomes "week 0" of treatment
    symplistcopy[[G]][[id]] <- rbind(symplistcopy$baseline[[id]][4,],
                                 symplistcopy[[G]][[id]])
  }
  # drop last week of baseline measurement from baseline period
  symplistcopy$baseline[[id]] <-  symplistcopy$baseline[[id]][-4,]
}

# Calculate effectiveness for each participant
symd <- list(change = list(),
                  pre = list(),
                  mid = list(),
                  post = list())

groups <- c("g1","g2", "g3","g4","g5")
for(i in 1:length(groups)){
  qq <- symplistcopy[[groups[i]]]
  symd$change[[i]] <- do.call("rbind",lapply(qq, function(idlist){
    idlist[1,"sumscore"] - idlist[nrow(idlist),"sumscore"]
  }))
  symd$pre[[i]] <- do.call("rbind",lapply(qq, function(idlist){
    idlist[1,"sumscore"]
  }))
  symd$mid[[i]] <- do.call("rbind",lapply(qq, function(idlist){
    idlist[9,"sumscore"]
  }))
  symd$post[[i]] <- do.call("rbind",lapply(qq, function(idlist){
    idlist[nrow(idlist),"sumscore"]
  }))
}
for(i in 1:4){
  symd[[i]] <- as.data.frame(do.call("cbind", symd[[i]]))
  colnames(symd[[i]]) <- c("control", "ct","bt", "cbt","cbtnew")

}

# --------- Mean Treatment Effectiveness ------------

print("Pre-treatment symptoms"); round(colMeans(symd$pre),2) # pre-treatment
print("Post-follow-up symptoms");round(colMeans(symd$post),2) # post-follow up
print("End-of-treatment symptoms");round(colMeans(symd$mid),2) # post-treatment (mid point assessment)
print("Change pre to post");round(colMeans(symd$change),2) # change pre to post treatment

#  ------ For whom is treatment effective ? -----
# CT
# table(symd$change$ct)
sum(symd$change$ct > 0)/n_id # how many improve?
sum(symd$change$ct> 10)/n_id # how many improve a lot?

# BT
# table(symd$change$bt)
sum(symd$change$bt > 0)/n_id
sum(symd$change$bt> 10)/n_id

# CBT
# table(symd$change$cbt)
sum(symd$change$cbt > 0)/n_id
sum(symd$change$cbt> 10)/n_id

# ---------- What is the "relapse" percentage? -----------
# here we define relapse as improved in symptom levels from pre-to post treatment
# but then got worse again
ct_relapse <- which((symd$pre$ct > symd$mid$ct) & (symd$mid$ct < symd$post$ct))
length(ct_relapse)/n_id

# ----------- Relative effectiveness -------
# 1: For what proportion of individuals does CT work best?
round(sum(
          apply(symd$change,1,function(r) all(r[2] > r[-2]))
        )/n_id,
      2)
# 2: For what proportion of individuals does CBT work better than BT?
sum(symd$change$cbt > symd$change$bt)/n_id

# 3: BT better than CBT?
sum(symd$change$cbt < symd$change$bt)/n_id

# which individuals "prefer" BT to CBT?
symd$change[symd$change$cbt < symd$change$bt,c("bt","cbt")]

btids <- c(25,55,133,207,429)

# ------------ Revised CBT protocol -----------------
# How does CBT+ compare to bt?
sum(symd$change$cbtnew > symd$change$bt)/n_id # CBT+ better?
sum(symd$change$cbtnew < symd$change$bt)/n_id # BT better?

# How does CBT+ compare to CBT?
sum(symd$change$cbtnew > symd$change$cbt)/n_id # CBT+ better?
sum(symd$change$cbtnew < symd$change$cbt)/n_id # CBT better?
sum(symd$change$cbtnew < symd$change$cbt) # CBT better raw numbers?

# For whom is CBT+ effective?
table(symd$change$cbtnew)
sum(symd$change$cbtnew > 0)/n_id # how many improve?
sum(symd$change$cbtnew> 10)/n_id # how many improve a lot?

#  How does CBT+ compare to CT?
sum(symd$change$cbtnew > symd$change$ct)/n_id # CBT+ better
sum(symd$change$cbtnew < symd$change$ct)/n_id # BT better
sum(symd$change$cbtnew < symd$change$ct) # BT better raw

ctsubgroup <- symd$change[symd$change$cbtnew < symd$change$ct,]

# here we can see the third and the last row refer to two individuals
# for whom ct leads to a full recovery, while cbtnew does not improve symptoms
ctsubgroup[,c("ct","cbtnew")]

# ids of those individuals
ctids <- c(134,168)

# --------------------------------------------------------------------------- #
# --------------------------------- Figure 6 -------------------------------- #
# --------------------------------------------------------------------------- #

# read in data of individuals of interest
# btids <- c(25,55,133,207,429)

id <- 168
sel <- readRDS(paste0("Files/data_id",id,".RDS"))

cols_tr <- c("black", brewer.pal(4, "Set1"))

sc <- 0.9
 pdf("Figures/Fig_BTvsCBT.pdf",  width=8.5*sc, height=2*5*sc)
# pdf("Figures/Fig_BTvsCBTv2.pdf",  width=8.5*sc, height=2*5*sc)

# Get symptom data from subject
l_symptoms_Subj<- list()
for(s in 1:5) l_symptoms_Subj[[s]] <- symplist[[s]][[id]]

# ---------- Plot symptom trajectories ----------
par(mar=c(4, 4, 2, 1), mfrow = c(2,1))
plot.new()
plot.window(xlim=c(0,21), ylim=c(0,21.5))
axis(1, c(0:9, 12, 15, 18, 21))
axis(2, las=2)
title(xlab="Weeks", line=2.3)
title(ylab="Symptom Sum Score", line=2.3)
mtext("Symptoms", side=3, col="black", line=.35, cex=1.15)
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
lines(1:4, l_symptoms_Subj[[1]][, 6], lwd=lwd)
for(i in 1:4) lines(4:5, c(l_symptoms_Subj[[1]][4, 6], l_symptoms_Subj[[i+1]][1, 6]), lwd=lwd, col=cols_tr[i]) # connect the two
# Treatment
for(i in 1:4) lines(5:21, l_symptoms_Subj[[i+1]][, 6], col=cols_tr[i], lwd=lwd)

# l_symptoms

# legend(13,13, legend=c("Control", "Cognitive Only",
#                        "Behavioral Only", "Cognitive & Behavioral"), bty="n", text.col=cols_tr)
legend(13,13, legend=c("Behavioral Only", "Cognitive & Behavioral"), bty="n", text.col=cols_tr[c(3,4)])

# -------- Arousal Schema and Escape Schema ---------------


# Now arousal schema
# Now add arousal schema and escape schema
ymax = 1.075
plot.new()
plot.window(xlim=c(0,21), ylim=c(0,ymax))
axis(1, c(0:9, 12, 15, 18, 21))
axis(2, las=2)
title(xlab="Weeks", line=2.3)
title(ylab="Schema Value", line=2.3)
mtext("Schemas", side=3, col="black", line=.35, cex=1.15)

## Blocks indicating phase
cex_titles <- 1.1
v_greys <- c("#f5f5f5", "#cfcfcf", "#dedede")
# Baseline
abline(v=c(0, 4), lty=2)
text((0+4)/2, ymax, "Baseline", col = "black", cex=cex_titles)
# Treatment
abline(v=c(4, 9), lty=2)
text((4+9)/2, ymax, "Treatment", col = "black", cex=cex_titles)
# Follow up
abline(v=c(9, 21), lty=2)
text((9+21)/2, ymax, "Follow-up", col = "black", cex=cex_titles)

## Data
lwd <- 2
# Baseline
blen <- nrow(sel$baseline)
tlen <- nrow(sel$ct)

mat1 <- cbind(seq(4,21,length=tlen),sel[[3]]$S)
submat <- mat1[seq(1,nrow(mat1), by = 1440),]

lines(seq(0,4,length=blen),sel$baseline[,"S"], lwd=lwd)
for(i in 1:4) lines(seq(4,21,length=tlen),sel[[i+1]]$S, lwd=lwd, col=cols_tr[i])

ltyx <- 5
mat1 <- cbind(seq(0,4,length=blen),sel$baseline$X)
submat <- mat1[seq(1,nrow(mat1), by = 1440),]

lines(submat[,1],submat[,2], lwd=lwd, lty = ltyx)
for(i in 1:4){
  mat2 <- cbind(seq(4,21,length=tlen),sel[[i+1]]$X)
  submat2 <- mat2[seq(1,nrow(mat2), by = 1440),]

  lines(submat2[,1],submat2[,2], lwd=lwd, col=cols_tr[i], lty = ltyx)
}

 polygon(x=c(5.1,5.55,5.55,5.1), y =c(1.0375,1.0375,0-.1,0-.1), col = alpha("grey",0.5), lty = 0)

# polygon(x=c(5.1,5.85,5.85,5.1), y =c(1.0375,1.0375,0-.1,0-.1), col = alpha("grey",0.5), lty = 0)

legend(12.5,0.775, legend=c("Arousal Schema","Escape Schema"), bty="n", col = "black",
lty = c(1,ltyx))

dev.off()


# --------------------------------------------------------------------------- #
# --------------------------------- Figure 7 -------------------------------- #
# --------------------------------------------------------------------------- #

# Treatment effectiveness as a function of between-person characteristics
# First, read in information about participant heterogeneity
outall <- readRDS("Files/outfiles.RDS")
heter <- outall$heter

# Make plot
sc <- 1
pdf(paste0("figures/Fig_heter_twopanel.pdf"), height=sc*4, width=sc*8.9)
par(mfrow = c(1,2))

# Make Layout
layout(matrix(1:3, ncol=3), widths = c(1, 1, 0.3))

# --- Panel 1: Data ---
AS_change_col <- (symd$change$ct / symd$pre$ct[[2]])
AS_change_col[AS_change_col < 0] <- 0
grad <- color.gradient(AS_change_col)
grad <- alpha(grad, .75)
par(mar = c(6, 5, 5, 3))
plot.new()
plot.window(xlim = c(0, .75), ylim = c(0.5, 1))
axis(1, c(0., 0.25, 0.5, 0.75))
axis(2, c(0.5, 0.75, 1), las = 2)
mtext("Belief in Psychoeducation", side = 1, line = 3,cex=1)
mtext("Prior Arousal Schema", side = 2, line = 3,srt=0,cex=1)
cex.p <- 1.4
points(heter$I123_alpha, heter$S, pch = 20, col = grad, cex = cex.p)
title(main = "Cognitive Only", font.main = 1, cex.main = 2)

# --- Panel 2: Data ---
AS_change_col <- (symd$change$cbt / symd$pre$cbt)
AS_change_col[AS_change_col < 0] <- 0
grad <- color.gradient(AS_change_col)
grad <- alpha(grad, .75)
par(mar = c(6, 5, 5, 3))
plot.new()
plot.window(xlim = c(0, 1), ylim = c(0, 1))
axis(1, at = seq(0, 1, length=5))
axis(2, at = seq(0, 1, length=5), las = 2)
mtext("Quantity of Exposure", side = 1, line = 3,cex=1)
mtext("Quality of Exposure", side = 2, line = 3,srt=0,cex=1)
points(heter$I4Adh, heter$I4RdEs, pch = 20, col = grad, cex = cex.p)
title(main = "Cognitive & Behavioural", font.main = 1, cex.main = 2)
# box()

# --- Panel 3: legend ---
par(mar = c(0, 0, 0, 0))
plot.new()
plot.window(xlim = c(0, 1), ylim = c(0, 1))
shift <- -0.1
legend_image <- as.raster(matrix(color.gradient(rev(seq(0,1,.1))), ncol=1))
rasterImage(legend_image, 1.1-1+0.2+shift, .25, 1.125-1+0.2+0.2+shift,.75)
text(1.1175-1+0.3 + shift,.8,"100%\nImprovement", cex = 1.1)
text(1.1175-1+0.3 + shift,.2,"0%\nImprovement", cex = 1.1)
dev.off()


# --------------------------------------------------------------------------- #
# --------------------------------- Figure 8 -------------------------------- #
# --------------------------------------------------------------------------- #
# jonashaslbeck@protonmail.com; July 21, 2023

# Plotting
par(mfrow=c(1,1))
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
  # Baseline
  lines(1:4, symplist$baseline[[i]][, 6], col=alpha("black", alpha=alpha_val))
  # Treatment+Follow up
  lines(4:21, c(symplist$baseline[[i]][4, 6], symplist[[j+1]][[i]][,6]), col=alpha(cols_tr[j], alpha=alpha_val))
}

dev.off()
