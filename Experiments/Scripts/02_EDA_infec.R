################################################################################
######################### Infection SbIII EDA ##################################
################################################################################

# Packages
#library(dplyr)
library(ggplot2)
library(agricolae)

# Loading data
DataANF <- read.csv(file = 
                      "Experiments/Data/processed/EXP_full_ANF_corrected.csv")
DataMIL <- read.csv(file = 
                      "Experiments/Data/processed/EXP_full_MILT_corrected.csv")
DataSbIII <- read.csv(file = 
                        "Experiments/Data/processed/EXP_full_SbIII_corrected.csv")

EXP_full <-rbind(DataANF, DataMIL, DataSbIII)

# Removing inadequate clones

EXP_full <- EXP_full%>%
  filter(!grepl("C76", pop))


EXP_full<- EXP_full%>%
  filter(!grepl("C89", pop))

EXP_full<- EXP_full%>%
  filter(!grepl("THP-1", pop))

# Checking
sapply(EXP_full, class)
EXP_full$conc <- as.factor(EXP_full$conc)
EXP_full$pop <- as.factor(EXP_full$pop)
EXP_full$composto <- as.factor(EXP_full$composto)
EXP_full$experiment <- as.factor(EXP_full$experiment)
sapply(EXP_full, class)

sapply(Infec_data, class)

# Infectivity

Infec_data <- filter(EXP_full, conc == 0 | conc == 0.00)


infectivity_plot_01 <- ggplot(Infec_data, aes(x = reorder(pop,desc(ama_per_cell)),
                                                          ama_per_cell))+
  geom_bar(stat = "identity", fill = "purple")+
  labs(y = "Amastigotes/Cell", x = "Population")+
  facet_wrap(Infec_data$composto)
  theme_light()
infectivity_plot

ggsave("Experiments/figs/01_infectivity.jpg")

infectivity_plot_02 <- ggplot(Infec_data, aes(x = reorder(pop,desc(ama_per_cell))
                                              , ama_per_cell))+
  geom_bar(stat = "identity", fill = "purple")+
  labs(y = "Amastigotes/Cell", x = "Population")+
  facet_wrap(Infec_data$experiment)
theme_light()
infectivity_plot_02

ggsave("Experiments/figs/02_infectivity.jpg")

# Calculating mean and standard errors

sum_measures <- Infec_data%>%
  group_by(pop)%>%
  summarise(mean_value = mean(ama_per_cell), 
            sd_error = sd(ama_per_cell)/sqrt(4))


Infec_data_SUM <- full_join(Infec_data, sum_measures)

sapply(Infec_data_SUM, class)

infectivity_plot_03 <- ggplot(Infec_data_SUM, 
                              aes(x = reorder(pop,desc(mean_value)),mean_value))+
  geom_bar(stat = "identity", fill = "purple", 
           position = position_dodge(width = 0.02))+
  geom_errorbar(aes(ymin = mean_value - sd_error,
                    ymax = mean_value + sd_error,
                    width = 0.2, alpha = 0.5))+
  labs(y = "Amastigotes/Cell", x = "Population")+
  theme_light()
  
infectivity_plot_03
ggsave("Experiments/figs/03_infectivity.jpg")


# ANOVA

sum_measures_reptec <- Infec_data%>%
  group_by(pop, experiment)%>%
  summarise(mean_value_reptec = mean(ama_per_cell), 
            sd_value= sd(ama_per_cell))


modelo_anova <- aov(mean_value_reptec~ pop, data = sum_measures_reptec)

summary(modelo_anova)



