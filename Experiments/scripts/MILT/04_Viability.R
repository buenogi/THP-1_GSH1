################################################################################
#################"Viability" - Percentual of infection #########################
################################################################################

# Packages

library(dplyr)
library(rstatix)

# Loading data 


DataMIL <- read.csv(file = 
                      "Experiments/Data/processed/EXP_full_MILT_corrected.csv")

# Removing irrelevant clones data

DataMIL <- DataMIL%>%
  filter(!grepl("C76", pop))


DataMIL<- DataMIL%>%
  filter(!grepl("C89", pop))

DataMIL<- DataMIL%>%
  filter(!grepl("THP-1", pop))

# Outliers identification

outliers_multiple <- DataMIL%>%
  group_by(pop,experiment) %>%
  identify_outliers(ama_per_cell)

outliers <- c(73,97,109,96)

DataMIL <- DataMIL[-outliers,]

# Spliting data

EXP1 <- filter(DataMIL, experiment == "EXP 1")
EXP2 <- filter(DataMIL, experiment == "EXP 2")
EXP3 <- filter(DataMIL, experiment == "EXP 3")
EXP4 <- filter(DataMIL, experiment == "EXP 4")

# Experiment 1

# Calculating control's mean ama_per_cell to viability estimative

CNTRL_value <- EXP1%>% 
  filter(conc == "0")%>%
  group_by(pop)%>%
  summarise(Cntrl_ama_per_cell = ama_per_cell)

EXP1 <- inner_join(CNTRL_value, EXP1, by = "pop")

# Viability estimatives
viability <- c()
for (i in 1:nrow(EXP1)){
  viability_est <- EXP1$ama_per_cell[i]*100/EXP1$Cntrl_ama_per_cell[i]
  viability[i] <- viability_est }

EXP1$viability <- viability

# Experiment 2

# Calculating control's mean ama_per_cell to viability estimative

CNTRL_value <- EXP2%>% 
  filter(conc == "0")%>%
  group_by(pop)%>%
  summarise(Cntrl_ama_per_cell = ama_per_cell)

EXP2 <- inner_join(CNTRL_value, EXP2, by = "pop")

# Viability estimatives
viability <- c()
for (i in 1:nrow(EXP2)){
  viability_est <- EXP2$ama_per_cell[i]*100/EXP2$Cntrl_ama_per_cell[i]
  viability[i] <- viability_est }

EXP2$viability <- viability


# Experiment 3

# Calculating control's mean ama_per_cell to viability estimative

CNTRL_value <- EXP3%>% 
  filter(conc == "0")%>%
  group_by(pop)%>%
  summarise(Cntrl_ama_per_cell = ama_per_cell)

EXP3 <- inner_join(CNTRL_value, EXP3, by = "pop")

# Viability estimatives
viability <- c()
for (i in 1:nrow(EXP3)){
  viability_est <- EXP3$ama_per_cell[i]*100/EXP3$Cntrl_ama_per_cell[i]
  viability[i] <- viability_est }

EXP3$viability <- viability

# Experiment 4

# Calculating control's mean ama_per_cell to viability estimative

CNTRL_value <- EXP4%>% 
  filter(conc == "0")%>%
  group_by(pop)%>%
  summarise(Cntrl_ama_per_cell = ama_per_cell)

EXP4 <- inner_join(CNTRL_value, EXP4, by = "pop")

# Viability estimatives
viability <- c()
for (i in 1:nrow(EXP4)){
  viability_est <- EXP4$ama_per_cell[i]*100/EXP4$Cntrl_ama_per_cell[i]
  viability[i] <- viability_est }

EXP4$viability <- viability

# Binding data

DataMIL_Full  <- rbind(EXP1, EXP2, EXP3, EXP4)

# Adding mean_values

mean_values <- DataMIL_Full%>%
  group_by(pop,conc)%>%
  summarise(mean_value = mean(viability))

mean_values$conc <- as.factor(mean_values$conc)

DataMIL_Full$conc <- as.factor(DataMIL_Full$conc)
DataMIL_Full <- left_join( DataMIL_Full,mean_values, by = c("pop", "conc"))


# CSV exportation

write.csv(DataMIL_Full , file = "Experiments/Data/processed/DataMIL_viability.csv", 
          sep = ",", row.names = F)

