################################################################################
#################"Viability" - Percentual of infection #########################
################################################################################

# Packages

library(dplyr)
library(rstatix)

# Loading data 


DataANF <- read.csv(file = 
                      "Data/processed/EXP_full_ANF_corrected.csv")

# Removing irrelevant clones data

# DataANF <- DataANF%>%
#   filter(!grepl("C76", pop))


DataANF<- DataANF%>%
  filter(!grepl("C89", pop))

DataANF<- DataANF%>%
  filter(!grepl("THP-1", pop))

#Outliers identification
 
 outliers_multiple <- DataANF%>%
 group_by(pop,experiment) %>%
 identify_outliers(ama_per_cell)
 
 outliers <- c(240,180,228,168,169,108,229, 216,217,120,156,204)
 
 DataANF <- DataANF[-outliers,]

# Spliting data

EXP1 <- filter(DataANF, experiment == "EXP 1")
EXP2 <- filter(DataANF, experiment == "EXP 2")
EXP3 <- filter(DataANF, experiment == "EXP 3")
EXP4 <- filter(DataANF, experiment == "EXP 4")

# Experiment 1

# Calculating control's mean ama_per_cell to viability estimative

CNTRL_value <- EXP1%>% 
  filter(conc == "0")%>%
  group_by(pop)%>%
  summarise(Cntrl_ama_per_cell = mean(ama_per_cell))

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
  summarise(Cntrl_ama_per_cell = mean(ama_per_cell))

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
  summarise(Cntrl_ama_per_cell = mean(ama_per_cell))

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
  summarise(Cntrl_ama_per_cell = mean(ama_per_cell))

EXP4 <- inner_join(CNTRL_value, EXP4, by = "pop")

# Viability estimatives
viability <- c()
for (i in 1:nrow(EXP4)){
  viability_est <- EXP4$ama_per_cell[i]*100/EXP4$Cntrl_ama_per_cell[i]
  viability[i] <- viability_est }

EXP4$viability <- viability

# Binding data

DataANF_Full  <- rbind(EXP1, EXP2, EXP3, EXP4)

# Adding mean_values

mean_values <- DataANF_Full%>%
  group_by(pop,conc)%>%
  summarise(mean_value = mean(viability))

mean_values$conc <- as.factor(mean_values$conc)

DataANF_Full$conc <- as.factor(DataANF_Full$conc)
DataANF_Full <- left_join( DataANF_Full,mean_values, by = c("pop", "conc"))


# CSV exportation

write.csv(DataANF_Full , file = "Data/processed/DataANF_viability.csv", 
          sep = ",", row.names = F)

