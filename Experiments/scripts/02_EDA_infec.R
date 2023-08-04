################################################################################
######################### Infection SbIII EDA ##################################
################################################################################

# Packages
#library(dplyr)
library(ggplot2)
#library(scales)

# Loading data
EXP1 <- read.csv(file = "Data/Processed/EXP1_28032023_corrected.csv")
EXP2 <- read.csv(file = "Data/Processed/EXP2_08042023_corrected.csv")

EXP1$experiment <- "EXP 1"
EXP2$experiment <- "EXP_2"
EXP_full <-rbind(EXP1, EXP2)

# Checking
sapply(EXP_full, class)
EXP_full$conc <- as.factor(EXP_full$conc)
EXP_full$pop <- as.factor(EXP_full$pop)
EXP_full$experiment <- as.factor(EXP_full$experiment)
sapply(EXP_full, class)

# Infectivity

Infec_data <- filter(EXP_full, conc == "0")

infectivity_plot <- ggplot(Infec_data, aes(pop, ama_per_cell))+
  geom_bar(stat = "identity", fill = "purple")+
  #geom_errorbar(aes(ymin = mean_value - sd_value, ymax = mean_value + sd_value),
  #width = 0.4,  # Largura das barras de erro
  #position = position_dodge(width = 0.9))+
  labs(y = "Amastigotes/Cell", x = "Population")+
  facet_wrap(Infec_data$experiment)
  theme_light()
infectivity_plot

# Dose response - amastigotes accounts for each conc

AmaPcellplot <- ggplot(EXP_full, aes(fill= conc,
                                       y = ama_per_cell,
                                       x = pop))+
  geom_bar(position = "dodge", stat = "identity")+
  ggtitle("Nº of amastigote per cell SbIII dosage in different populations") +
  labs(x = " Populations ", y = "Nº of cells")+
  theme(plot.title = element_text(size = 14,face="bold"),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))+
  facet_wrap(EXP_full$experiment)
  theme_bw()
AmaPcellplot


# Lines

lineplot <- ggplot(EXP_full, aes(y = ama_per_cell, x = conc, group = pop))+
  geom_line(aes(color = EXP_full$pop))+
  ggtitle("Dose response pattern for different SbIII concentrations") +
  labs(x = " Populations ", y = "Nº of cells")+
  theme(plot.title = element_text(size = 14,face="bold"),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))+
  facet_wrap(EXP_full$experiment)+
theme_bw()
lineplot


