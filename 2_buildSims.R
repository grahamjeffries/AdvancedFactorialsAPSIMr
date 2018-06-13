# Set scenarios ##########################################################
scenarios <- expand.grid(myPD = c("1-may","15-may","30-may"),
                         myHybrid = c("A_100","A_105","A_110"),
                         myNrate = c(0,40,80,120,160,200,240,280))

scenarios$scenarioID <- row.names(scenarios)

saveRDS(scenarios, "scenarios.rds")

# Save simulation files based on management scenarios ####################
template <- readLines("forFactorials.apsim")

for (i in 1:length(scenarios$scenarioID)){
  
  sim <- template
  sim <- gsub("myScenario",scenarios$scenarioID[i],sim)
  sim <- gsub("myPD",scenarios$myPD[i],sim)
  sim <- gsub("myHybrid",scenarios$myHybrid[i],sim)
  sim <- gsub("myNrate",scenarios$myNrate[i],sim)
  
  writeLines(sim,paste0("sims/",scenarios$scenarioID[i],".apsim"))
  
}
