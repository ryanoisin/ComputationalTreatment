# function to extract symptoms

# explanatory notes for hardcoded time vectors
# 0:(60*24*7*(5+12)) # 5 weeks treatment + 12 weeks follow up
# (60*24*7*(5+12))

# out_baseline has 4 weeks of data + 1 minute on a minute timescale
# 60*24*7*4 (mins x hrs x days x weeks) = 40320
# 40321 in total
# out_groupX has  17 weeks + 1 minute
# (60*24*7*(5+12)) + 1

# symptoms are assessed on a weekly time-scale

getSymptoms <- function(dframe, av_thresh = .5, as_thresh = .4, oldsim = FALSE){
  # in new simulation, avoidance is "V" rather than "AV" as in the original simulation
if(isTRUE(oldsim)) AV <- "AV" else AV <- "V"

  plist <- PanicModel::detectPanic(dframe$AF)

  n_panic <- plist$n_panic

  # q1 ; skip "limited symptom" condition, only recode n_panic to none, 1-2, more than 2 but not once a day, >1 a day
  q1_panic <- 0
  if(0 < n_panic && n_panic <= 2 ) q1_panic <- 2
  if(n_panic > 2 && n_panic <= 7) q1_panic <- 3
  if(n_panic > 7) q1_panic <- 4

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
  if(fear_mean <= .003) q3_fear <- 0
  if(fear_mean > .003 && fear_mean <= .005) q3_fear <- 1
  if(fear_mean > .005 && fear_mean <= .01 ) q3_fear <- 2
  if(fear_mean > .01 && fear_mean <= .02) q3_fear <- 3
  if(.02 < fear_mean) q3_fear <- 4



  # q4: any places or situations you avoided or felt afraid of because of fear of panic?
  q4_avoid_cont <- avoid_mean
  q4_avoid <- NA
  if(avoid_mean <= .01) q4_avoid <- 0
  if(avoid_mean > .01 && avoid_mean <= .25) q4_avoid <- 1
  if(avoid_mean > .25 && avoid_mean <= .5 ) q4_avoid <- 2
  if(avoid_mean > .5 && avoid_mean <= .75) q4_avoid <- 3
  if(.75 < avoid_mean) q4_avoid <- 4

# q5 - avoid situations. p_C
  q5_context_cont <- 1- mean(dframe$p_C)
  q5_context <- NA
  if( q5_context_cont <= .908)  q5_context <- 0
  if(.908 < q5_context_cont && q5_context_cont <= .910)  q5_context <- 1
  if(.910 < q5_context_cont && q5_context_cont <= .990)  q5_context <- 2
  if(.990 < q5_context_cont && q5_context_cont <= .9915)  q5_context <- 3
  if(.9915 <  q5_context_cont)  q5_context <- 4


  sumscore <- q1_panic +  q2_padistress +  q3_fear + q4_avoid + q5_context
  out = matrix(c(q1_panic, q2_padistress, q3_fear,q4_avoid, q5_context, sumscore,
                 n_panic,q2_distress_cont, q3_fear_cont,q4_avoid_cont, q5_context_cont ),1, 11)
  colnames(out) <- c("q1_panic", "q2_padistress", "q3_fear", "q4_avoid","q5_context", "sumscore",
                     "n_panic","q2_distress_cont", "q3_fear_cont", "q4_avoid_cont", "q5_context_cont")

  out
  }



# helper function to extract AS information
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

