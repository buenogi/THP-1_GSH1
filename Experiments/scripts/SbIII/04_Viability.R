################################################################################
#################"Viability" - Percentual of infection #########################
################################################################################

# Packages

library(dplyr)
library(rstatix)

# Loading data 


DataSbIII <- read.csv(file = 
                        "Experiments/Data/processed/EXP_full_SbIII_corrected.csv")

# Removing irrelevant clones data

DataSbIII <- DataSbIII%>%
  filter(!grepl("C76", pop))


DataSbIII<- DataSbIII%>%
  filter(!grepl("C89", pop))

DataSbIII<- DataSbIII%>%
  filter(!grepl("THP-1", pop))

# Outliers identification

outliers_multiple <- DataSbIII%>%
  group_by(pop,experiment) %>%
  identify_outliers(ama_per_cell)

outliers <- c(120,146)

DataSbIII <- DataSbIII[-outliers,]

# Spliting data

EXP1 <- filter(DataSbIII, experiment == "EXP 1")
EXP2 <- filter(DataSbIII, experiment == "EXP 2")
EXP3 <- filter(DataSbIII, experiment == "EXP 3")
EXP4 <- filter(DataSbIII, experiment == "EXP 4")

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

DataSbIII_Full  <- rbind(EXP1, EXP2, EXP3, EXP4)

# Adding mean_values

mean_values <- DataSbIII_Full%>%
  group_by(pop,conc)%>%
  summarise(mean_value = mean(viability))

mean_values$conc <- as.factor(mean_values$conc)

DataSbIII_Full$conc <- as.factor(DataSbIII_Full$conc)
DataSbIII_Full <- left_join( DataSbIII_Full,mean_values, by = c("pop", "conc"))


# CSV exportation

write.csv(DataSbIII_Full , file = "Experiments/Data/processed/DataSbIII_viability.csv", 
          sep = ",", row.names = F)

