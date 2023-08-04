################################################################################
################################## DR ANF EDA ##################################
################################################################################

# Packages
library(dplyr)
library(ggplot2)


# Loading data
DataANF <-  read.csv(file = 
                       "Experiments/Data/processed/DataANF_processed_normalized.csv",
                     header = TRUE, sep =",")


# Checking variables
sapply(DataANF, class)
DataANF$conc <- as.factor(DataANF$conc)
DataANF$pop <- as.factor(DataANF$pop)
DataANF$experiment <- as.factor(DataANF$experiment)
sapply(DataANF, class)

# Dose response - amastigotes accounts for each conc

AmaPcellplot <- ggplot(DataANF, aes(fill= conc,
                                    y = viability_normalized,
                                    x = pop))+
  geom_bar(position = "dodge", stat = "identity")+
  ggtitle("Nº of amastigote per cell SbIII dosage in different populations") +
  labs(x = " Conc [   ] μM ", y = "Nº of cells")+
  theme(plot.title = element_text(size = 14,face="bold"),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))+
  facet_wrap(DataANF$experiment)
theme_bw()
AmaPcellplot

ggsave("Experiments/figs/04_AmaPcellplot_ANF.jpg")

# Lines

lineplot <- ggplot(DataANF, aes(y = viability_normalized, x = conc, group = pop))+
  geom_line(aes(color = pop))+
  ggtitle("Dose response pattern for different SbIII concentrations") +
  labs(x = " Conc [   ] μM ", y = "Nº of cells")+
  theme(plot.title = element_text(size = 14,face="bold"),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))+
  facet_wrap(~experiment)+
  theme_bw()
lineplot

ggsave("Experiments/figs/05_AmaPcellplot_ANF.jpg")

# Summarized

EXP_sum <- DataANF%>%
  group_by(conc,pop)%>%
  summarise(mean_value = mean(viability_normalized), sd_value = sd(ama_per_cell) )

lineplot_sum <- ggplot(EXP_sum, aes(y = mean_value, x = conc, group = pop))+
  geom_line(aes(color = EXP_sum$pop))+
  ggtitle("Dose response pattern for different ANF concentrations") +
  labs(x = " Conc [   ] μM ", y = "Nº of cells")+
  theme(plot.title = element_text(size = 14,face="bold"),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))+
  theme_bw()
lineplot_sum+  labs(color = "Populations")

ggsave("Experiments/figs/06_AmaPcellplot_ANF.jpg")
