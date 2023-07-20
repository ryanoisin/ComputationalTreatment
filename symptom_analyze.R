setwd("~/Dropbox/Research/05_Projects/098_PG7_SimulatingInterventions/3_code/SimulatingInterventions/")

library(RColorBrewer)
library(scales)
# Make Gradient
color.gradient <- function(x, colors=c("red","blue"), colsteps=100) {
  return( colorRampPalette(colors) (colsteps) [ findInterval(x, seq(min(x),max(x), length.out=colsteps)) ] )
}

######## --- load in, analyze
# symptoms <- readRDS("./symptoms_out.RDS")
symptoms <- readRDS("symptoms_out.RDS")


n_id <- length(symptoms$baseline)

base <- do.call("rbind", symptoms$baseline)
g1 <-   do.call("rbind", symptoms$g1) # control
g2 <-   do.call("rbind", symptoms$g2) # cognitive
g3 <-   do.call("rbind", symptoms$g3) # behavioural
g4 <-   do.call("rbind", symptoms$g4) # CBT
g5 <-   do.call("rbind", symptoms$g5) # CBTnew

all <- rbind(base,g1,g2,g3,g4,g5)


# ------------------------------------------------
# total symptom change score
change <- list()
pre <- list()
post <- list()
mid <- list()
groups <- c("g1","g2", "g3","g4","g5")

symptoms$g1[[500]]
symptoms$g2[[500]]
symptoms$baseline[[500]]


for(i in 1:length(groups)){
  # for(i in c("g2")){
  qq <- symptoms[[groups[i]]]
  change[[i]] <- do.call("rbind",lapply(qq, function(idlist){
    idlist[1,"sumscore"] - idlist[nrow(idlist),"sumscore"]
  }))
  pre[[i]] <- do.call("rbind",lapply(qq, function(idlist){
    idlist[1,"sumscore"]
  }))
  mid[[i]] <- do.call("rbind",lapply(qq, function(idlist){
    idlist[6,"sumscore"]
  }))
  post[[i]] <- do.call("rbind",lapply(qq, function(idlist){
    idlist[nrow(idlist),"sumscore"]
  }))
}
effmat <-  as.data.frame(do.call("cbind",change))
premat <-  as.data.frame(do.call("cbind",pre))
postmat <- as.data.frame(do.call("cbind",post))
midmat <- as.data.frame(do.call("cbind",mid))
colnames(effmat) <- colnames(premat) <- colnames(postmat) <-
  colnames(midmat) <- c("control", "ct","bt", "cbt","cbtnew")


## mean effectiveness
colMeans(premat)
colMeans(midmat)
colMeans(postmat)
colMeans(effmat)

#


# for how many people are certain treatments effective?
table(effmat$ct)
sum(effmat$ct > 0); sum(effmat$ct > 10)

table(effmat$bt)
sum(effmat$bt > 0); sum(effmat$bt > 10)

table(effmat$ct)
sum(effmat$cbt > 0); sum(effmat$cbt > 10)

table(effmat$cbtnew)
sum(effmat$cbtnew > 0) /nrow(effmat); sum(effmat$cbtnew > 10) / nrow(effmat)

plot(jitter(effmat$bt), jitter(effmat$cbt), col = alpha("black",.25), pch= 20, cex = 2)
plot(jitter(effmat$cbtnew), jitter(effmat$cbt), col = alpha("black",.25), pch= 20, cex = 2)

# who "relapses"?
ct_relapse <- which((premat$ct > midmat$ct) & (midmat$ct < postmat$ct))
length(ct_relapse)


cbt_relapse <- which((premat$cbt > midmat$cbt) & (midmat$cbt < postmat$cbt))
cbt_relapse
length(cbt_relapse)

bt_relapse <- which((premat$bt > midmat$bt) & (midmat$bt < postmat$bt))
bt_relapse
length(bt_relapse)

cbtnew_relapse <- which((premat$cbtnew > midmat$cbtnew) & (midmat$cbtnew < postmat$cbtnew))
cbtnew_relapse
length(cbtnew_relapse)



effmat[midmat$cbt < postmat$cbt,]
which(midmat$cbt < postmat$cbt) ; sum(midmat$cbt < postmat$cbt)
which(midmat$bt < postmat$bt) ; sum(midmat$bt < postmat$bt)

# for how many people are different treatments effective
sum(effmat$cbt > effmat$bt)
sum(effmat$cbt < effmat$bt)

sum(effmat$cbt > effmat$cbtnew)
sum(effmat$cbt < effmat$cbtnew)


sum(effmat$bt > effmat$cbtnew)
sum(effmat$bt < effmat$cbtnew)


sum(effmat$ct > effmat$cbtnew)
sum(effmat$ct < effmat$cbtnew)


sum((effmat$bt < 1) & (effmat$cbt >=10))
sum((effmat$cbt < 1) & (effmat$bt >=10))



sum(effmat$bt > effmat$cbt)
table(effmat$bt, effmat$cbt)

effmat[(effmat$ct > effmat$cbtnew),]

# ------- for whom is all treatment equally effective ?

sum((abs(effmat$cbt) < 5) & (abs(effmat$bt) < 5) & (abs(effmat$ct) < 5))
sum((abs(effmat$cbt) > 10) & (abs(effmat$bt) > 10) & (abs(effmat$ct) > 10))

# alternative calculation; how many people are "healthy" at end of treatment?
sum(postmat$ct < 5)
sum(postmat$bt < 5)
sum(postmat$cbt < 5)

sum((postmat$ct < 5) & (postmat$bt < 5) & (postmat$cbt < 5))
sum((postmat$bt < 5) & (postmat$cbt < 5))

# how many people are "bad" at end of treatment?
sum((postmat$ct > 15) & (postmat$bt > 15) & (postmat$cbt > 15))
sum((postmat$bt > 15) & (postmat$cbt > 15))


sum(postmat$ct > 15)
sum(postmat$bt > 15)
sum(postmat$cbt > 15)

# how many find ct better than bt/cbt?
sum(effmat$ct > effmat$cbt)

abs(postmat$bt - postmat$cbt) < 3

# sum(abs(effmat$cbt - effmat$bt) < 12)
# sum(abs(effmat$cbt - effmat$bt) < 12)

#
#
# # --- descriptives
# # which symptoms reach 0/4 and which do not?
# apply(g4[,1:5],2,function(col) any(col ==0))
# hist(g4[,"q3_fear_cont"])
# abline(v = quantile(g4[,"q3_fear_cont"]))
#
#
# apply(g4[,1:5],2, function(col) any(col == 0))
# # fear is always there
# table(postmat$cbt)
#
# health_fear <- sapply(which(postmat$cbt == 1), function(s){
#   symptoms$g4[[s]][17,"q3_fear_cont"]
# })
# quantile(health_fear)
#
#
# hist(g4[,"q3_fear_cont"])
# abline(v = quantile(g4[,"q3_fear_cont"]))
#
#
# # and which don't reach the ceiling?
# apply(g1[,1:5],2,function(col) any(col ==4))
# sick_cont <- sapply(which(postmat$control == 19), function(s){
#   symptoms$g1[[s]][17,"q5_context_cont"]
# })
# quantile(sick_cont)
#
# health_cont <- sapply(which(postmat$cbt== 1), function(s){
#   symptoms$g4[[s]][17,"q5_context_cont"]
# })
# quantile(health_cont)
#
# hist(g4[,"q5_context_cont"])
# quantile(g4[,"q5_context_cont"])

# load in adherence rates
outall <- readRDS("outfilesOR.RDS")
# outall$out_treat[1,,1]
# outall$out_treat[1,,2]
# outall$out_treat[1,,3]
# outall$out_treat[1,,4]
# outall$out_treat[1,,5]


# --------- #### 4 TREATMENTS TRAJECTORY FIGURE make treatment trajectory figures-----
gcol <- brewer.pal(5, "Set1")
gcol[1]<-"#808080"
titlevec <- c("Control", "Cognitive Only",
              "Behavioral Only","Cognitive & Behavioral","Revised Protocol")
endbase <- (7)*0
endtreat <- endbase + 5

tpoints <- 17
sc <- 1
pdf(paste0("figures/aps_2023_outcomes.pdf"),height=8,width=12)
par(mfrow = c(2,2))

for(g in 1:4){
  # g= 5
  outg <- symptoms[[g+1]]
  start <- end <- mid <- rep(NA,length(outg))
  plot.new()
  plot.window(xlim = c(0, 17), ylim = c(0,20))
  axis(1, at = c(0,5,10,17)); axis(2)

  for(i in 1:n_id) {
    # extract person from the group
    tmp <- outg[[i]]
    # plot person trajectory
    lines(y= tmp[,"sumscore"], x=(0:(nrow(tmp)-1)), col = alpha(gcol[g],0.15), type = "b",
          pch = 20, cex = 2)
    start[i] <- tmp[1,"sumscore"]
    end[i] <- tmp[nrow(tmp),"sumscore"]
    mid[i] <- tmp[6,"sumscore"]
  }

  abline(v = endbase, lty = 1,col="dark grey")
  abline(v = endbase + 5, lty = 1,col="dark grey")
  abline(v = endbase+1, lty = 2,col="light grey")
  abline(v = endbase+2, lty = 2,col="light grey")
  abline(v = endbase+3, lty = 2,col="light grey")
  abline(v = endbase+4, lty = 2,col="light grey")

  print(c(mean(start),mean(mid), mean(end)))
  lines(x=c(0,5,17), y = c(mean(start), mean(mid), mean(end)),lwd=2.5, type = "b", col = "black", bg="white", pch = 21,
        cex = 3)
  # print(paste("start", round(mean(start),2), "end", round(mean(end),2), "group", g))
  # write this to data frame

  # what is this supposed to be?
  # abline(h=mean(outg2[,"AS",dim(out)[3]]), lty = 1,lwd=4,col="light grey")

  title(titlevec[g], xlab = "Time (weeks)", ylab = "Symptom Sum Score", cex.main =2, cex.lab = 1.5)
} # end of group loop
dev.off()

### ----------------------------------------------------------------
### What is happening when behavioral therapy works better?

for(i in which(postmat$bt < postmat$cbt)){
plot.new()
plot.window(xlim = c(0, tpoints), ylim = c(0,20))
axis(1, at = c(0,5,10,17)); axis(2)

# extract person from the group
for(g in 3:4){
  outg <- symptoms[[g+1]] # NOTE: symptoms contains pre-treatment baseline, so skip
  tmp <- outg[[i]]
  # plot person trajectory
  lines(y= tmp[,"sumscore"], x=jitter((0:(nrow(tmp)-1))), col = alpha(gcol[g],1), type = "b", pch= 20, cex = 2)
}
abline(v = endbase, lty = 1,col="light grey")
abline(v = endbase + 5, lty = 1,col="light grey")
abline(v = endbase+1, lty = 2,col="light grey")
abline(v = endbase+2, lty = 2,col="light grey")
abline(v = endbase+3, lty = 2,col="light grey")
abline(v = endbase+4, lty = 2,col="light grey")

# lines(x=c(0,tpoints), y = c(mean(start), mean(end)), type = "b", col = "black", pch = 19)
# print(paste("start", round(mean(start),2), "end", round(mean(end),2), "group", g))
# write this to data frame

# what is this supposed to be?
# abline(h=mean(outg2[,"AS",dim(out)[3]]), lty = 1,lwd=4,col="light grey")

title(main = paste0("Simulated Participant ",i), xlab = "Time (weeks)", ylab = "Symptom Sum Score", cex.main =2, cex.lab = 1.5)
# dev.off()
}

### PARTICIPANT 423: BT>CBT -------
pdf("figures/APS_2023_Behavioral Therapy Better_Symptom.pdf",height=4,width=6)
plot.new()
plot.window(xlim = c(0, 18), ylim = c(0,22))
axis(1, at = c(0,5,10,17)); axis(2)
# extract person from the group
for(g in 3:4){
  outg <- symptoms[[g+1]] # NOTE: symptoms contains pre-treatment baseline, so skip
  tmp <- outg[[423]]
  # plot person trajectory
  lines(y= tmp[,"sumscore"], x=seq(0,17,1),
        col = alpha(gcol[g],0.75), lwd=3, type = "b", pch= 20, cex = 2)
}
abline(v = endbase, lty = 1,col="light grey")
abline(v = endbase + 5, lty = 1,col="light grey")
abline(v = endbase+1, lty = 2,col="light grey")
abline(v = endbase+2, lty = 2,col="light grey")
abline(v = endbase+3, lty = 2,col="light grey")
abline(v = endbase+4, lty = 2,col="light grey")
title(main = "", xlab = "Time (weeks)", ylab = "Symptom Sum Score", cex.main =2, cex.lab = 1.5)
dev.off()


### CBT vs CBT NEW -------
gcol <- brewer.pal(5, "Set1")
gcol[1]<-"#808080"
titlevec <- c("Control", "Cognitive Only",
              "Behavioral Only","Standard CBT","Revised CBT")
endbase <- (7)*0
endtreat <- endbase + 5

tpoints <- 18
sc <- 1
pdf(paste0("figures/aps_2023_cbt vs cbtnew.pdf"),height=4,width=12)
par(mfrow = c(1,2))

for(g in 4:5){
  # g= 5
  outg <- symptoms[[g+1]]
  start <- end <- mid <- rep(NA,length(outg))
  plot.new()
  plot.window(xlim = c(0, tpoints), ylim = c(0,20))
  axis(1, at = c(0,5,10,17)); axis(2)

  for(i in 1:n_id) {
    # extract person from the group
    tmp <- outg[[i]]
    # plot person trajectory
    lines(y= tmp[,"sumscore"], x=(0:(nrow(tmp)-1)), col = alpha(gcol[g],0.15), type = "b",
          pch = 20, cex = 2)
    start[i] <- tmp[1,"sumscore"]
    end[i] <- tmp[nrow(tmp),"sumscore"]
    mid[i] <- tmp[6,"sumscore"]
  }

  abline(v = endbase, lty = 1,col="dark grey")
  abline(v = endbase + 5, lty = 1,col="dark grey")
  abline(v = endbase+1, lty = 2,col="light grey")
  abline(v = endbase+2, lty = 2,col="light grey")
  abline(v = endbase+3, lty = 2,col="light grey")
  abline(v = endbase+4, lty = 2,col="light grey")

  print(c(mean(start),mean(mid), mean(end)))
  lines(x=c(0,5,17), y = c(mean(start), mean(mid), mean(end)),lwd=2.5, type = "b", col = "black", bg="white", pch = 21,
        cex = 3)
  # print(paste("start", round(mean(start),2), "end", round(mean(end),2), "group", g))
  # write this to data frame

  # what is this supposed to be?
  # abline(h=mean(outg2[,"AS",dim(out)[3]]), lty = 1,lwd=4,col="light grey")

  title(titlevec[g], xlab = "Time (weeks)", ylab = "Symptom Sum Score", cex.main =2, cex.lab = 1.5)
} # end of group loop
dev.off()


### CBT vs CBT NEW -------
gcol <- brewer.pal(5, "Set1")
gcol[1]<-"#808080"
titlevec <- c("Control", "Cognitive Only",
              "Behavioral Only","Standard CBT","Revised CBT")
endbase <- (7)*0
endtreat <- endbase + 5

tpoints <- 18
sc <- 1

pdf(paste0("figures/aps_2023_cbt vs cbtnew_diff.pdf"),height=6,width=10)
par(mfrow = c(1,1))

  outg1 <- symptoms[[5]]
  outg2 <-symptoms[[6]]
  start <- end <- mid <- rep(NA,length(outg))
  plot.new()
  plot.window(xlim = c(0, tpoints), ylim = c(-20,20))
  axis(1, at = c(0,5,10,17)); axis(2)

  for(i in 1:n_id) {
    # extract person from the group
    tmp1 <- outg1[[i]]
    tmp2 <- outg2[[i]]
    tmp1[,"sumscore"]- tmp2[,"sumscore"]
    # plot person trajectory
    lines(y= tmp1[,"sumscore"]- tmp2[,"sumscore"], x=(0:(nrow(tmp1)-1)), col = alpha(gcol[1],0.15), type = "b",
          pch = 20, cex = 2)
    start[i] <- tmp1[1,"sumscore"]- tmp2[1,"sumscore"]
    end[i] <- tmp1[nrow(tmp1),"sumscore"]- tmp2[nrow(tmp1),"sumscore"]
    mid[i] <- tmp1[6,"sumscore"]- tmp2[6,"sumscore"]
  }

  abline(v = endbase, lty = 1,col="dark grey")
  abline(v = endbase + 5, lty = 1,col="dark grey")
  abline(v = endbase+1, lty = 2,col="light grey")
  abline(v = endbase+2, lty = 2,col="light grey")
  abline(v = endbase+3, lty = 2,col="light grey")
  abline(v = endbase+4, lty = 2,col="light grey")
  abline(h=0,lty=1,col="black")
  print(c(mean(start),mean(mid), mean(end)))
  lines(x=c(0,5,17), y = c(mean(start), mean(mid), mean(end)),lwd=2.5, type = "b", col = "black", bg="white", pch = 21,
        cex = 3)
  # print(paste("start", round(mean(start),2), "end", round(mean(end),2), "group", g))
  # write this to data frame

  # what is this supposed to be?
  # abline(h=mean(outg2[,"AS",dim(out)[3]]), lty = 1,lwd=4,col="light grey")

  title(xlab = "Time (weeks)", ylab = "Symptom Sum Score Difference", cex.main =2, cex.lab = 1.5)
# end of group loop
dev.off()

#### Adherence Plot

# load in adherence rates
outall <- readRDS("outfilesOR.RDS")
# adher <- outall$adher # probability that you will do the exercise at all
# adher <- outall$adher
heter <- outall$heter

# For CBT, what determines improvement?
# percentage improvement in symptoms.... "worsening" symptoms set to zero
AS_change_col<-(change[[4]]/pre[[4]])
AS_change_col[AS_change_col<0]<-0

grad <- color.gradient(AS_change_col)
grad <- alpha(grad, .75)
legend_image <- as.raster(matrix(color.gradient(rev(seq(0,1,.1))), ncol=1))


length(change[[1]])
pdf(paste0("figures/cbt_effectiveness.pdf"),height=4,width=4)
plot.new()
plot.window(xlim=c(0,1.175),ylim=c(0,1))
axis(1,at=seq(0,1,.25)); axis(2,at=seq(0,1,.25),las=2)
mtext("Quantity of Exposure", side = 1, line = 3,cex=1.25)
mtext("Quality of Exposure", side = 2, line = 3,srt=0,cex=1.25)
points(heter$I4Adh,heter$I4RdEs,pch=21,bg=grad,lwd=.25)
rasterImage(legend_image, 1.1, .25, 1.125,.75)
text(1.1175,.8,"100%\nImprovement",cex=.8)
text(1.1175,.2,"0%\nImprovement",cex=.8)
dev.off()



 # what about BT?
AS_change_col<-(change[[3]]/pre[[3]])
AS_change_col[AS_change_col<0]<-0

grad <- color.gradient(AS_change_col)
grad <- alpha(grad, .75)
legend_image <- as.raster(matrix(color.gradient(rev(seq(0,1,.1))), ncol=1))


length(change[[1]])
pdf(paste0("figures/bt_effectiveness.pdf"),height=4,width=4)
plot.new()
plot.window(xlim=c(0,1.175),ylim=c(0,1))
axis(1,at=seq(0,1,.25)); axis(2,at=seq(0,1,.25),las=2)
mtext("Quantity of Exposure", side = 1, line = 3,cex=1.25)
mtext("Quality of Exposure", side = 2, line = 3,srt=0,cex=1.25)
points(heter$I4Adh,heter$I4RdEs,pch=21,bg=grad,lwd=.25)
rasterImage(legend_image, 1.1, .25, 1.125,.75)
text(1.1175,.8,"100%\nImprovement",cex=.8)
text(1.1175,.2,"0%\nImprovement",cex=.8)
dev.off()

# ----- Cognitive Therapy effectiveness ------

AS_change_col<-(change[[2]]/pre[[2]])
AS_change_col[AS_change_col<0]<-0

grad <- color.gradient(AS_change_col)
grad <- alpha(grad, .75)
legend_image <- as.raster(matrix(color.gradient(rev(seq(0,1,.1))), ncol=1))


plot.new()
plot.window(xlim=c(0,1.175),ylim=c(0,1))
axis(1,at=seq(0,1,.25)); axis(2,at=seq(0,1,.25),las=2)
mtext("Belief in Psychoeducation", side = 1, line = 3,cex=1.25)
mtext("Prior Arousal Schema", side = 2, line = 3,srt=0,cex=1.25)
points(heter$I123_alpha,heter$S,pch=21,bg=grad,lwd=.25)
rasterImage(legend_image, 1.1, .25, 1.125,.75)
text(1.1175,.8,"100%\nImprovement",cex=.8)
text(1.1175,.2,"0%\nImprovement",cex=.8)


plot.new()
plot.window(xlim=c(0,1.175),ylim=c(0,1))
axis(1,at=seq(0,1,.25)); axis(2,at=seq(0,1,.25),las=2)
mtext("Belief in Psychoeducation", side = 1, line = 3,cex=1.25)
mtext("Prior Escape Schema", side = 2, line = 3,srt=0,cex=1.25)
points(heter$I123_alpha,heter$X,pch=21,bg=grad,lwd=.25)
rasterImage(legend_image, 1.1, .25, 1.125,.75)
text(1.1175,.8,"100%\nImprovement",cex=.8)
text(1.1175,.2,"0%\nImprovement",cex=.8)

# ------- Putting these together
# sc <- 0.5
# layout(matrix(c(1,2,3),1,3), widths = sc*c(1,1,.175))

# EDIT JONAS JULY 18th, 2023
sc <- 1.3
pdf(paste0("figures/heter_twopanel_editJonas.pdf"),height=sc*4*0.9,width=sc*8.5)
par(mfrow = c(1,2))

# Make Layout
layout(matrix(1:3, ncol=3), widths = c(1,1,.2))

# --- Panel 1: Data ---
AS_change_col<-(change[[2]]/pre[[2]])
AS_change_col[AS_change_col<0]<-0
grad <- color.gradient(AS_change_col)
grad <- alpha(grad, .75)
par(mar=c(6,5,5,3))
plot.new()
plot.window(xlim=c(0,.75),ylim=c(0.5,1))
axis(1,at=seq(0,.75,.25)); axis(2,at=seq(0.5,1,.25),las=2)
mtext("Belief in Psychoeducation", side = 1, line = 3,cex=1)
mtext("Prior Arousal Schema", side = 2, line = 3,srt=0,cex=1)
cex.p <- 1.4
points(heter$I123_alpha,heter$S,pch=20,col=grad,cex=cex.p)
title(main = "Cognitive Only", font.main = 1, cex.main = 2)

# --- Panel 2: Data ---
AS_change_col<-(change[[4]]/pre[[4]])
AS_change_col[AS_change_col<0]<-0
grad <- color.gradient(AS_change_col)
grad <- alpha(grad, .75)
par(mar=c(6,5,5,3))
plot.new()
plot.window(xlim=c(0,1),ylim=c(0,1))
axis(1,at=seq(0,1,.25)); axis(2,at=seq(0,1,.25),las=2)
mtext("Quantity of Exposure", side = 1, line = 3,cex=1)
mtext("Quality of Exposure", side = 2, line = 3,srt=0,cex=1)
points(heter$I4Adh,heter$I4RdEs,pch=20,col=grad,cex=cex.p)
title(main = "Cognitive & Behavioural", font.main = 1, cex.main = 2)
# box()

# --- Panel 3: legend ---
par(mar=c(0,0,0,0))
plot.new()
plot.window(xlim=c(0,1), ylim=c(0,1))
# box()
shift <- -0.1
# box()
legend_image <- as.raster(matrix(color.gradient(rev(seq(0,1,.1))), ncol=1))
rasterImage(legend_image, 1.1-1+0.2+shift, .25, 1.125-1+0.2+0.2+shift,.75)
text(1.1175-1+0.3 + shift,.8,"100%\nImprovement",cex=1)
text(1.1175-1+0.3 + shift,.2,"0%\nImprovement",cex=1)
dev.off()


# ------------------------ Why is behavioural therapy sometimes better? ------

### PARTICIPANT 423: BT>CBT -------
# pdf("figures/APS_2023_Behavioral Therapy Better_Symptom.pdf",height=4,width=6)

# EDIT JONAS JULY 18th, 2023

cols_tr <- c("black", brewer.pal(4, "Set1"))

scp <- 4.2
pdf("figures/BTvsCBT_participant_updateJonas.pdf",
    height = scp * 1, width = scp * 2.3)

par(xpd = FALSE)
laymat <- matrix(c(1,2), 1, 2)
sc1 <- 1
layout(laymat, widths = sc1*c(1,1), heights = sc1*1)


# --- LEFT PANEL ---
par(mar=c(4,4,2,1))
plot.new()
plot.window(xlim = c(0, 17), ylim = c(0,20))
axis(1, at = c(0,5,10,17))
axis(2, las=2)
# extract person from the group
for(g in 3:4){
  outg <- symptoms[[g+1]] # NOTE: symptoms contains pre-treatment baseline, so skip
  tmp <- outg[[423]]
  # plot person trajectory
  lines(y= tmp[,"sumscore"], x=seq(0,17,1),
        col = alpha(cols_tr[g],0.75), lwd=3, type = "l", pch= 20, cex = 3)
}
abline(v = endbase, lty = 1,col="light grey")
abline(v = endbase + 5, lty = 1,col="light grey")
abline(v = endbase+1, lty = 3,col="light grey")
abline(v = endbase+2, lty = 3,col="light grey")
abline(v = endbase+3, lty = 3,col="light grey")
abline(v = endbase+4, lty = 3,col="light grey")
title(main = "", xlab = "Time (weeks)", cex.lab = 1, line=2.2)
title(ylab = "Symptom Sum Score", cex.lab = 1, line = 2.5)

legend("top", c("Behavioral Only", "Cognitive & Behavioral"), text.col = cols_tr[3:4],bty = "n", cex = 1.1)

# extract lower level schema changes
btmat <- outall$out_treat[,,3]
btmat <- btmat[btmat[,"ID"]==423,][,c("S", "X")]

cbtmat <- outall$out_treat[,,4]
cbtmat <- cbtmat[cbtmat[,"ID"]==423,][,c("S", "X")]

# --- RIGHT PANEL ---
plot.new()
plot.window(xlim = c(0,17), ylim = c(0,1))
axis(1, at = c(0,5,10,17))
axis(2, las=2)

lines(y= btmat[1:119,"X"], x= seq(0,17,length.out = 119),
      col = alpha(cols_tr[3],0.75), lwd=3, lty = 1, cex = 2)
lines(y= cbtmat[1:119,"X"], x= seq(0,17,length.out = 119),
      col = alpha(cols_tr[4],0.75), lwd=3, lty = 1, cex = 2)

lines(y= btmat[1:119,"S"], x= seq(0,17,length.out = 119),
      col = alpha(cols_tr[3],0.75), lwd=3, lty = 2, cex = 2)
lines(y= cbtmat[1:119,"S"], x= seq(0,17,length.out = 119),
      col = alpha(cols_tr[4],0.75), lwd=3, lty = 2, cex = 2)

abline(v = endbase, lty = 1,col="light grey")
abline(v = endbase + 5, lty = 1,col="light grey")
abline(v = endbase+1, lty = 3,col="light grey")
abline(v = endbase+2, lty = 3,col="light grey")
abline(v = endbase+3, lty = 3,col="light grey")
abline(v = endbase+4, lty = 3,col="light grey")
title(main = "", xlab = "Time (weeks)", cex.lab = 1, line=2.2)
title(ylab = "Schema Value", cex.lab = 1, line = 2.5)

legend("top", legend=c("Escape Schema", "Arousal Schema"), lty=c(1,2), bty="n", lwd=3)

# plot(cbtmat[,"X"])
# plot.new()
# par(xpd = TRUE)
# legend(x = -1, y = 0.9,
#        c("Behav.\nTherapy",
#          "Cog.\nBehav.\nTherapy", "Escape\nSchema", "Arousal\nSchema"),
#        col = c(gcol[3:4], "gray","gray"),
#        pch = c(19,19, NA, NA),
#        lty = c(NA, NA, 1,2),
#        bty = "n", cex = 1.2,
#        lwd= 3)
dev.off()


# ------ additional panel --------

sel <- readRDS("sub_id423.RDS")
# exposure in between week 1 and 2 where BT responds and CBT doesn't
# base timescale is minutes
# get minutes in week 1 - 2
# arousal and PT or fear; look for exposures
# seperate list item in the raw data gives exposures
nrow(sel$g3)

# see; generateexamplets_jonas/exampleTS_V5, line 116 - 119
# something divisible by 60*24
par(mfrow=c(2,1))
plot(sel$g4[,"PT"][(60*24*1):(60*24*21)])
abline(v = seq(1:20)*60*24, col = "red")

plot(sel$g4[,"X"][(60*24*1):(60*24*21)], ylim = c(0,.5))
abline(v = seq(1:20)*60*24, col = "red")

# TROUBLESHOOT; looks like the opposite happens here than in the figure
# check everything
# now identify correct time periods
  # note; outall file in weeks
  # this is in .... minutes? raw data format - check

