# o.ryan@umcutrecht.nl; 20 July
# This file process the raw simulated output found in the /simcode/ folder
# As output it creates two files
  # outfiles.RDS - a list containing baseline and treatment group daily measures
                  # as well as information about treatment adherence and heterogeneity
  # symptoms_out.RDS - a list containing symptom measurements

# -------------------------------------------------------------------------
# ----------------- Output 1: outfiles.RDS --------------------------------
# -------------------------------------------------------------------------
library(abind)

### Data Preparation Step 1: Import and Organize Data -----


l_all <- list()
dir <- "simcode/output/"
v_files <- list.files(dir)
n_files <- length(v_files)

# each file contains ~16 "people"
# for each person we have a multivariate time series for
#     baseline + 5 treatment arms
# we also have adherence rate (for treatments 3,4 and 5 only)
# and a vector of between-person variable values (heter)
# we don't need to extract anything else

IDcount <- 1

for(i in 1:n_files){
  tmp <- readRDS(paste0(dir, v_files[i]))

  for(j in 1:length(tmp)){
    print(paste("file", i, "person",IDcount))
    theter <- as.data.frame(tmp[[1]]$heter)

    # round for size, take the sequence to reflect daily measurements
    tout_baseline <- round(tmp[[j]]$out_baseline$outmat,2)[seq(1,nrow(tmp[[1]]$out_baseline$outmat), by = 60*24),]
    tout_group1 <- round(tmp[[j]]$out_group1$outmat,2)[seq(1,nrow(tmp[[1]]$out_group1$outmat), by = 60*24),]
    tout_group2 <- round(tmp[[j]]$out_group2$outmat,2)[seq(1,nrow(tmp[[1]]$out_group2$outmat), by = 60*24),]
    tout_group3 <- round(tmp[[j]]$out_group3$outmat,2)[seq(1,nrow(tmp[[1]]$out_group3$outmat), by = 60*24),]
    tout_group4 <- round(tmp[[j]]$out_group4$outmat,2)[seq(1,nrow(tmp[[1]]$out_group4$outmat), by = 60*24),]
    tout_group5 <- round(tmp[[j]]$out_group5$outmat,2)[seq(1,nrow(tmp[[1]]$out_group5$outmat), by = 60*24),]

    # add an "ID" indicator everywhere
    tout_baseline$ID <- tout_group1$ID <- tout_group2$ID <- tout_group3$ID <- tout_group4$ID <- tout_group5$ID <- IDcount

    # tadher1 <- tmp[[j]]$out_group1$adher;
    # tadher2 <- tmp[[j]]$out_group2$adher
    tadher3 <- tmp[[j]]$out_group3$adher
    tadher4 <- tmp[[j]]$out_group4$adher
    tadher5 <- tmp[[j]]$out_group5$adher

    theter <- as.data.frame(tmp[[j]]$heter); theter$ID <- IDcount

    # if this is the first entry, just create output objects
    if(i ==1 && j == 1){
      heter <- theter
      out_baseline <- tout_baseline ; out_group1 <- tout_group1 ; out_group2 <- tout_group2
      out_group3 <- tout_group3; out_group4 <- tout_group4 ; out_group5 <- tout_group5
      adher3 <- tadher3
      adher4 <- tadher4
      adher5 <- tadher5

    }else{ # otherwise, bind output objects together
    if(IDcount < 501){
      heter <- rbind(heter,theter)
      out_baseline <- rbind(out_baseline, tout_baseline)
      out_group1 <- rbind(out_group1, tout_group1)
      out_group2 <- rbind(out_group2, tout_group2)
      out_group3 <- rbind(out_group3, tout_group3)
      out_group4 <- rbind(out_group4, tout_group4)
      out_group5 <- rbind(out_group5, tout_group5)
      adher3 <- rbind(adher3, tadher3)
      adher4 <- rbind(adher4, tadher4)
      adher5 <- rbind(adher5, tadher5)
    }
    }# end of if
    IDcount <- IDcount + 1
  } # end of loop through list
  rm(tmp) # tidy -up (probably makes no difference except a slight slow down)
}

# 465
n_id <- IDcount - 1

# now merge some of these files together into arrays

out_treat <- abind(out_group1,out_group2, out_group3, out_group4,out_group5, along = 3)
adher <- list(NULL, NULL,adher3,adher4, adher5)
heter
saveRDS(list(out_baseline = out_baseline,
             out_treat = out_treat,
             adher = adher,
             heter = heter),
        file = "files/outfiles.RDS")

rm(list=ls())

# -------------------------------------------------------------------------
# ----------------- Output 2: symptoms_out.RDS ----------------------------
# -------------------------------------------------------------------------
source("symptom_functions.R")
library(PanicModel)

l_all <- list()
dir <- "simcode/output/"
v_files <- list.files(dir)
n_files <- length(v_files)

# create storage
symptoms <- list(baseline = list(), g1 = list(), g2 = list(), g3 = list(), g4 = list(), g5 = list())

IDcount <- 1

# Loop through files. Extract only the first 500 participants

for(i in 1:n_files){
  tmp <- readRDS(paste0(dir, v_files[i]))

  for(j in 1:length(tmp)){
    print(paste("file", i, "person",IDcount))

    for(q in 1:6){
      print(q)
      dframe  <- tmp[[j]][[1+q]]$outmat
      wind <- seq(1,nrow(dframe), 10080)
      nw <- length(wind) -1 # number of weeks

      weeks <- cbind(wind[1:nw], wind[2:(nw +1)])

      outid <- cbind(t(apply(weeks,1,function(r){
        getSymptoms(dframe[r[1]:r[2],])
      })),IDcount)

      colnames(outid)[1:11] <- c("q1_panic", "q2_padistress", "q3_fear", "q4_avoid","q5_context", "sumscore",
                                 "n_panic","q2_distress_cont", "q3_fear_cont", "q4_avoid_cont", "q5_context_cont")

      if(IDcount < 501){
        symptoms[[q]][[IDcount]] <- outid
      }
    } # end treatment arm loop
    IDcount <- IDcount +1
  } # end person loop

} #end file loop


# move "last observation of baseline" to "first observation of treatment"
# this makes some later data processing easier, as we have a "week 0"
for(id in 1:500){
  for(G in 2:6){
    # last baseline measurement becomes "week 0" of treatment
    symptoms[[G]][[id]] <- rbind(symptoms$baseline[[id]][4,],
                                 symptoms[[G]][[id]])
  }
  # drop last week of baseline measurement from baseline period
  symptoms$baseline[[id]] <-  symptoms$baseline[[id]][-4,]
}

saveRDS(symptoms, file = "files/symptoms_out.RDS")


