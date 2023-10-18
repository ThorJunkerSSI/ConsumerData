#######################################
# Simulation of Consumer data
#######################################
library(dplyr)

#Path to the location of the real data
file_path <- "..."

#Load real consumer data
cpd <- read.csv(file.path(file_path,"ConsumerReceiptData_Enriched.csv"))


#Randomly sample a fixed number of purchases per participant.
n_participants <- 10
n_purchases <- 500

Sim_purchases <- function(cpd, i, n_purchases){
  cpd <- cpd %>% 
    dplyr::slice(sample(1:nrow(.), size = n_purchases, replace = TRUE)) %>%
    dplyr::select(itemname, itemnumber) %>%
    mutate(participant = paste0("participant_",i),
           receipt = NA,
           category = NA, 
           amount = 1, 
           itemPrice = 0, 
           rabat = 0, 
           purchdate = as.Date("2023-04-18"),
           purchtime = NA, 
           weekno = NA,
           monthno = NA,
           yearno =NA,
           MerchantName = "Store 1",
           DateOfBirth = NA, 
           Gender = NA, 
           ContactMe = NA)
  
  return(cpd)
}

Sim_cpd <- do.call("rbind", lapply(1:n_participants, Sim_purchases, cpd = cpd, n_purchases = n_purchases))

write.csv(Sim_cpd, file.path("C:/Users/THGJ/Documents/ConsumerData - Github/Pipeline/Data/2023_04_18/","ConsumerReceiptData_Simulated.csv"),
          fileEncoding = "UTF-8")
