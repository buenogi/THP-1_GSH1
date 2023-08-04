################################################################################
#################### Normalization and reescaling data #########################
################################################################################

# Packages

library(scales)
library(dplyr)
library(rstatix)

# Loading data 

DataSbIII <-  read.csv(file = "Experiments/Data/processed/DataSbIII_viability.csv")

# Normalizing basis

REF <- filter(DataSbIII, pop == "REF")
C6 <- filter(DataSbIII, pop == "C6")
C7 <- filter(DataSbIII, pop == "C7")
C44 <- filter(DataSbIII, pop == "C44")
C58 <- filter(DataSbIII, pop == "C58")
C67 <- filter(DataSbIII, pop == "C67")
C73 <- filter(DataSbIII, pop == "C73")
C85 <- filter(DataSbIII, pop == "C85")

REF$mean_normalized <- rescale(REF$mean_value, c(0,100))
C6$mean_normalized <- rescale(C6$mean_value, c(0,100))
C7$mean_normalized <- rescale(C7$mean_value, c(0,100))
C44$mean_normalized <- rescale(C44$mean_value, c(0,100))
C58$mean_normalized <- rescale(C58$mean_value, c(0,100))
C67$mean_normalized <- rescale(C67$mean_value, c(0,100))
C73$mean_normalized <- rescale(C73$mean_value, c(0,100))
C85$mean_normalized <- rescale(C85$mean_value, c(0,100))

# REF

viability_normalized <- c()
for (i in 1:nrow(REF)){
  viability_est <- REF$viability[i]*REF$mean_normalized[i]/REF$mean_value[i]
  viability_normalized[i] <- viability_est }

REF$viability_normalized <- viability_normalized

# C6

viability_normalized <- c()
for (i in 1:nrow(C6)){
  viability_est <- C6$viability[i]*C6$mean_normalized[i]/C6$mean_value[i]
  viability_normalized[i] <- viability_est }

C6$viability_normalized <- viability_normalized

#C7

viability_normalized <- c()
for (i in 1:nrow(C7)){
  viability_est <- C7$viability[i]*C7$mean_normalized[i]/C7$mean_value[i]
  viability_normalized[i] <- viability_est }

C7$viability_normalized <- viability_normalized


# C44
viability_normalized <- c()
for (i in 1:nrow(C44)){
  viability_est <- C44$viability[i]*C44$mean_normalized[i]/C44$mean_value[i]
  viability_normalized[i] <- viability_est }

C44$viability_normalized <- viability_normalized

# C58

viability_normalized <- c()
for (i in 1:nrow(C58)){
  viability_est <- C58$viability[i]*C58$mean_normalized[i]/C58$mean_value[i]
  viability_normalized[i] <- viability_est }

C58$viability_normalized <- viability_normalized

#C67

viability_normalized <- c()
for (i in 1:nrow(C67)){
  viability_est <- C67$viability[i]*C67$mean_normalized[i]/C67$mean_value[i]
  viability_normalized[i] <- viability_est }

C67$viability_normalized <- viability_normalized



# C73
viability_normalized <- c()
for (i in 1:nrow(C73)){
  viability_est <- C73$viability[i]*C73$mean_normalized[i]/C73$mean_value[i]
  viability_normalized[i] <- viability_est }

C73$viability_normalized <- viability_normalized


#C85

viability_normalized <- c()
for (i in 1:nrow(C85)){
  viability_est <- C85$viability[i]*C85$mean_normalized[i]/C85$mean_value[i]
  viability_normalized[i] <- viability_est }

C85$viability_normalized <- viability_normalized

# Binding data

DataSbIII_Full  <- rbind(REF,C6,C7,C44,C58, C67,C73,C85)


# CSV exportation

write.csv(DataSbIII_Full , file = "Experiments/Data/processed/DataSbIII_processed_normalized.csv", 
          sep = ",", row.names = F)

