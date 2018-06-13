# This script runs the simulation #######################################################
#install.packages("apsimr")
library(tidyverse)
library(apsimr)

setwd("sims") # make sure to change working directory to the sims folder before running

# A wraper function that call apsim R and binds the data from the simulatios ##############
myApsimRun <- function(toRun,
                       # Change the apsimDir argument to your Apsim.exe path
                       apsimDir = "C:/Program Files (x86)/APSIM710-r4171/Model/Apsim.exe"
                       ){
  require(apsimr)
  
  bind_rows(
    apsim(apsimDir,files = toRun), # this function calls apsimr  
    .id = "sim"
    ) -> out
  
  file.remove(list.files(pattern = ".sum")) # this removes summary files
  
  return(out)
}

# This runs the simulation and assembles the dataframe ###################################
# with output files using dplyr's "do" ###################################################
## Note: this may take several minutes depending on your computer's speed

readRDS("../scenarios.rds") %>%
  mutate(toRun = paste0(scenarioID,".apsim")) %>%
  group_by(myPD,myHybrid,myNrate,scenarioID) %>%
  do(data = try(myApsimRun(.$toRun))) -> sim

sim %>%
  group_by(myPD,myHybrid,myNrate,scenarioID) %>%
  unnest() %>%
  saveRDS("../factorials.rds")
  
data.frame(sim = list.files(pattern = ".out")) %>%
  mutate(scenarioID = sim) %>%
  separate(scenarioID, "scenarioID") %>%
  left_join(readRDS("../scenarios.rds")) %>%
  group_by(myPD,myHybrid,myNrate,scenarioID)-> x


outnames <- names(read.table(as.character(x$sim[1]), skip = 2, header = T))

x %>%
  mutate(sim = as.character(sim)) %>%
  group_by(myPD,myHybrid,myNrate,scenarioID,sim) %>%
  do(data = read.table(.$sim, 
                       skip = 4, header = F, col.names = outnames,
                       colClasses = c("character",rep("numeric",9)
                                      ))) %>%
  group_by() %>%
  unnest()-> x2

x2 %>%
  mutate(sim = gsub(".out","",sim),
         Date = as.Date(Date,format = "%d/%m/%Y")) -> x3



