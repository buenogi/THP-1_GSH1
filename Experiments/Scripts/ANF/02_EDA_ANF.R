################################################################################
################################## DR ANF EDA ##################################
################################################################################

# Packages
library(dplyr)
library(ggplot2)


# Loading data
DataANF <- read.csv(file = 
                      "Experiments/Data/processed/EXP_full_ANF_corrected.csv")

# Removing irrelevant clones data

DataANF <- DataANF%>%
  filter(!grepl("C76", pop))


DataANF<- DataANF%>%
  filter(!grepl("C89", pop))

DataANF<- DataANF%>%
  filter(!grepl("THP-1", pop))


# Checking variables
sapply(DataANF, class)
DataANF$conc <- as.factor(DataANF$conc)
DataANF$pop <- as.factor(DataANF$pop)
DataANF$experiment <- as.factor(DataANF$experiment)
sapply(DataANF, class)

# Dose response - amastigotes accounts for each conc

AmaPcellplot <- ggplot(DataANF, aes(fill= conc,
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
  facet_wrap(DataANF$experiment)
theme_bw()
AmaPcellplot

ggsave("Experiments/figs/01_AmaPcellplot_ANF.jpg")

# Lines

lineplot <- ggplot(DataANF, aes(y = ama_per_cell, x = conc, group = pop))+
  geom_line(aes(color = pop))+
  ggtitle("Dose response pattern for different SbIII concentrations") +
  labs(x = " Populations ", y = "Nº of cells")+
  theme(plot.title = element_text(size = 14,face="bold"),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))+
  facet_wrap(~experiment)+
  theme_bw()
lineplot

ggsave("Experiments/figs/02_AmaPcellplot_ANF.jpg")

# Summarized

EXP_sum <- DataANF%>%
  group_by(conc,pop)%>%
  summarise(mean_value = mean(ama_per_cell), sd_value = sd(ama_per_cell) )

lineplot_sum <- ggplot(EXP_sum, aes(y = mean_value, x = conc, group = pop))+
  geom_line(aes(color = EXP_sum$pop))+
  ggtitle("Dose response pattern for different ANF concentrations") +
  labs(x = " Populations ", y = "Nº of cells")+
  theme(plot.title = element_text(size = 14,face="bold"),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))+
  theme_bw()
lineplot_sum

ggsave("Experiments/figs/03_AmaPcellplot_ANF.jpg")
