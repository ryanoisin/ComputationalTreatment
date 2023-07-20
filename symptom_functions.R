# function to extract symptoms


# 0:(60*24*7*(5+12)) # 5 weeks treatment + 12 weeks follow up
# (60*24*7*(5+12))

# out_baseline has 4 weeks of data + 1 minute on a minute timescale
# 60*24*7*4 (mins x hrs x days x weeks) = 40320
# 40321 in total
# out_groupX has  17 weeks + 1 minute
# (60*24*7*(5+12)) + 1

# symptoms are assessed on a weekly time-scale

# Q1: how many panic attacks in the last week?
# 0 No panic or limited symptom episodes
# 1 Mild: no full panic attacks and no more than 1 limited symptom attack/day
# 2 Moderate: 1 or 2 full panic attacks and/or multiple limited symptom attacks/day
# 3 Severe: more than 2 full attacks but not more than 1/day on average
# 4 Extreme: full panic attacks occurred more than once a day, more days than not
# q2: how distressing were the panic attacks?

getSymptoms <- function(dframe, av_thresh = .5, as_thresh = .4, oldsim = FALSE){
  # in new simulation, avoidance is "V" rather than "AV" as in the original simulation
if(isTRUE(oldsim)) AV <- "AV" else AV <- "V"
  # av_thresh <- .5
  # as_thresh <- .4

  # AF <- sapply(1:nrow(dframe), function(s){
  #   ifelse(dframe$A[s] > 0,  sqrt(dframe$A[s]*dframe$PT[s]), 0)
  # })

  plist <- PanicModel::detectPanic(dframe$AF)

  n_panic <- plist$n_panic

  # q1 ; skip "limited symptom" condition, only recode n_panic to none, 1-2, more than 2 but not once a day, >1 a day
  q1_panic <- 0
  if(0 < n_panic && n_panic <= 2 ) q1_panic <- 2
  if(n_panic > 2 && n_panic <= 7) q1_panic <- 3
  if(n_panic > 7) q1_panic <- 4
  # NOTE: no concept of limited symptom panic attack, so no "1" criteria
    # possibility: shoehorn in some "fear" threshold to make it 1 (fear above .25?)

  # q2: how distressing were the panic attacks?
    # assuming severity between 0 and 1
  q2_padistress <- NA
  q2_distress_cont <- 0
  if(n_panic == 0) q2_padistress <- 0
  if(n_panic >= 1){
    msev <- mean(plist$panic_stats[,"severity"])
    if(msev <= .25) q2_padistress <- 1
    if(msev > .25 && msev <= .5 ) q2_padistress <- 2
    if(msev > .5 && msev <= .75) q2_padistress <- 3
    if(.75 < msev) q2_padistress <- 4
    q2_distress_cont <- msev
  }


  # q3: worried or felt anxious about / fear about next panic attack?
    # OR: to me, this is related to either AV or just fear, so i will split these into two questions
    # to account for the fact that you have a recovery period, i need to omit panic attacks + recovery period
  pind <- plist$ind_label
  # find out when the indicator turns on or off
  tmp <- which(pind[-1] != pind[-length(pind)]) + 1

  # find end points of the panic attacks
  ends <- tmp[seq(2,length(tmp))]



  # count two hours from each end point if there was a panic attack
  if(n_panic > 0){
    # edge case; if there is a single panic attack that starts at the end of the window and doesnt end
    if(length(tmp) == 1){
      outside_pa <- seq(1:nrow(dframe))[-tmp]
    }else{
  ends + 60*2
  tmp2 <- as.numeric(sapply(ends, function(s){
    seq(s, s + 60*2,1)
  }))

  recovery <- rep(0,length(pind))
  recovery[tmp2] <- 1

  # here are the time poitns which are outside panic attacks & recovery periods
  # note: previous operation can add extra elements to recovery (if PA happens < 2 hours before end)
  outside_pa <- (pind ==0 & recovery[1:length(pind)] == 0)
    }
  } else{
  outside_pa <- seq(1:nrow(dframe))
}
  fear_mean <- mean(dframe[outside_pa,"AF"])
  avoid_mean <- mean(dframe[outside_pa,AV]) # in new code,

  #
  q3_fear_cont <- fear_mean
  q3_fear <- NA
  # if(fear_mean <= .01) q3_fear <- 0
  #   if(fear_mean <= .25) q3_fear <- 1
  #   if(fear_mean > .25 && fear_mean <= .5 ) q3_fear <- 2
  #   if(fear_mean > .5 && fear_mean <= .75) q3_fear <- 3
  #   if(.75 < fear_mean) q3_fear <- 4
  if(fear_mean <= .003) q3_fear <- 0
  if(fear_mean > .003 && fear_mean <= .005) q3_fear <- 1
  if(fear_mean > .005 && fear_mean <= .01 ) q3_fear <- 2
  if(fear_mean > .01 && fear_mean <= .02) q3_fear <- 3
  if(.02 < fear_mean) q3_fear <- 4



  q4_context_cont <- 1- mean(dframe$p_C)
  q4_context <- NA
  if( q4_context_cont <= .908)  q4_context <- 0
  if(.908 < q4_context_cont && q4_context_cont <= .910)  q4_context <- 1
  if(.910 < q4_context_cont && q4_context_cont <= .990)  q4_context <- 2
  if(.990 < q4_context_cont && q4_context_cont <= .9915)  q4_context <- 3
  if(.9915 <  q4_context_cont)  q4_context <- 4


  # q4: any places or situations you avoided or felt afraid of because of fear of panic?
  q5_avoid_cont <- avoid_mean
  q5_avoid <- NA
  if(avoid_mean <= .01) q5_avoid <- 0
  if(avoid_mean > .01 && avoid_mean <= .25) q5_avoid <- 1
  if(avoid_mean > .25 && avoid_mean <= .5 ) q5_avoid <- 2
  if(avoid_mean > .5 && avoid_mean <= .75) q5_avoid <- 3
  if(.75 < avoid_mean) q5_avoid <- 4

# q5 - avoid situations. p_C; range approx .01 to ...?
# (range unknown - maybe relative to some baseline/ start value?)
# same problem for fear and avoid; check min values ofhealthy (no AS)
# vs max values of unhealthy (AS maxed out)

  sumscore <- q1_panic +  q2_padistress +  q3_fear + q4_context + q5_avoid
  out = matrix(c(q1_panic, q2_padistress, q3_fear, q4_context,q5_avoid, sumscore,
                 n_panic,q2_distress_cont, q3_fear_cont, q4_context_cont, q5_avoid_cont),1, 11)
  colnames(out) <- c("q1_panic", "q2_padistress", "q3_fear", "q4_avoid","q5_context", "sumscore",
                     "n_panic","q2_distress_cont", "q3_fear_cont", "q4_avoid_cont", "q5_context_cont")

  out
  # store[i,,1] <- c(id, group, pcount, rpa_s, av_s, as_s, av_m, as_m)

    # store[i,,1] <- c(id, group, pcount, rpa_s, av_s, as_s, av_m, as_m)
  }





# old function
# getSymptoms <- function(l_all, av_thresh = .5, as_thresh = .4,
#                         post_period = "lastmonth", # when should symptoms be assessed? either all or lastmonth
#                         inds = NULL){
#
#   # av_thresh <- .5
#   # as_thresh <- .4
#
#   # baseline
#   if(is.null(inds)) inds <- 1:length(l_all)
#   store <- array(NA, dim=c(length(l_all), 8, 2))
#   for(i in inds){
#     dat <- l_all[[i]]$out_baseline$outmat
#     id <- i
#     group <- l_all[[i]]$group
#
#
#     dp <- detectPanic(dat[,"A"], dat[,"E"], dat[,"AF"], dat[,"C"])
#
#     pcount <- nrow(dp$panic_stats)
#     if(is.null(pcount)) pcount <- 0
#
#     # symptoms
#     rpa_s <- pcount > 1
#     av_m <- mean(dat$AV)
#     as_m <- mean(dat$AS)
#
#     av_s <- av_m > av_thresh
#     as_s <- as_m > as_thresh
#
#     store[i,,1] <- c(id, group, pcount, rpa_s, av_s, as_s, av_m, as_m)
#   }
#
#
#
#
#   # follow up
#   for(i in 1:length(l_all)){
#     dat <- l_all[[i]]$out_treat$outmat
#
#     if(post_period== "lastmonth"){
#       start <- nrow(dat) - 40320
#     }
#     if(post_period == "all"){
#       start <- nrow(dat) - 40320*3
#     }
#
#     dat <- dat[start:nrow(dat),]
#
#     id <- i
#     group <- l_all[[i]]$group
#
#
#     dp <- detectPanic(dat[,"A"], dat[,"E"], dat[,"AF"], dat[,"C"])
#
#     pcount <- nrow(dp$panic_stats)
#     if(is.null(pcount)) pcount <- 0
#
#     # symptoms
#     rpa_s <- pcount > 1
#     av_m <- mean(dat$AV)
#     as_m <- mean(dat$AS)
#
#     av_s <- av_m > av_thresh
#     as_s <- as_m > as_thresh
#
#     store[i,,2] <- c(id, group, pcount, rpa_s, av_s, as_s, av_m, as_m)
#   }
#
#   dimnames(store) = list(NULL, c("id","group", "pcount","rpa_s","av_s","as_s","av_m","as_m"), c("pre", "post"))
#
#   store
#
# }


# ----------------------------------------------------
# -------- Function to extract AS --------------------


getAS <- function(l_all){

  # i know the length of l_all if i combine all baseline + treatment + post-treatment
  # thats 211682
  # i sub sample every 20th time point because otherwise the vector is too huge (slow moving so doesn't matter)
  outseq <- seq(1, 211682, by = 20)

  store <- matrix(NA, nrow = length(l_all), ncol = length(outseq))

  for(i in 1:length(l_all)){
    dat <- l_all[[i]]$out_baseline$outmat$AS
    id <- i
    group <- l_all[[i]]$group

    datpost <- l_all[[i]]$out_treat$outmat$AS

    store[i,] <- c(dat, datpost)[outseq] # kill some of the observations, don't need them

  }

  store
}
#
# # ----------------------------------------------------
# # -------- From Package: detectPanic()  --------------------
#
# # detectPanic: function to detect panic attacks --------
# detectPanic <- function(AF) # definition of panic attack on Arousal
# {
#   AF[is.na(AF)]<-mean(AF, na.rm=TRUE)
#   n_panic <- length(AF)
#
#   ind_present <- AF>=.5 # indicator: attack in progress at time step i?
#
#   # Label panic attacks
#   counter <- 1
#   ind_label <- rep(NA, n_panic)
#   ind_label[1] <- 0
#   for(p in 2:n_panic) {
#     if(!ind_present[p]) {
#       ind_label[p] <- 0
#       if(ind_label[p-1] == counter) counter <- counter + 1
#     } else {
#       ind_label[p] <- counter
#     }
#   }
#   # Calculate Stats
#   if(all(!ind_present)) counter <- 0
#   panic_stats <- matrix(NA, nrow=counter, ncol=3)
#   colnames(panic_stats) <- c("id", "length","severity")
#   if(!all(!ind_present)) { # are there ANY panic attacks in the interval
#     panic_stats[, 1] <- 1:counter
#     for(p in 1:counter) {
#       duration <- AF[ind_label == p]
#       panic_stats[p, 2] <- length(duration) #Number of time steps above panic threshold
#       panic_stats[p, 3] <- max(AF) #Amount above panic threshold
#     }
#   }
#   # Delete final row (artifact of method of detecting panic)
#   panic_stats<-panic_stats[panic_stats[,2]>0,,drop=FALSE]
#   panic_count<-nrow(panic_stats)
#
#   # Return list
#   outlist <- list("ind_label" = ind_label,
#                   "panic_stats" = panic_stats,
#                   "n_panic" = panic_count)
#   return(outlist)
# }

