source("symptom_functions_v2.R")

l_all <- list()
dir <- "simcode_lisa_Feb2023/output/"
# v_files <- list.files("output_Sim5/")
v_files <- list.files(dir)
n_files <- length(v_files)

symptoms <- list(baseline = list(), g1 = list(), g2 = list(), g3 = list(), g4 = list(), g5 = list())

IDcount <- 1

for(i in 1:n_files){
# i <- 1
  tmp <- readRDS(paste0(dir, v_files[i]))

  for(j in 1:length(tmp)){
    print(paste("file", i, "person",IDcount))

for(q in 1:6){
print(q)
  dframe  <- tmp[[j]][[1+q]]$outmat
  wind <- seq(1,nrow(dframe), 10080)
  nw <- length(wind) -1 # number of weeks

  weeks <- cbind(wind[1:nw], wind[2:(nw +1)])

   # zz <- 4
   # getSymptoms(dframe[weeks[zz,1]:weeks[zz,2],])

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

# length(symptoms$baseline)
# saveRDS(symptoms, file = "symptoms_out.RDS")

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

saveRDS(symptoms, file = "symptoms_out.RDS")
