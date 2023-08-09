################################################################################
################################## DR MIL EDA ##################################
################################################################################

# Packages
library(dplyr)
library(ggplot2)


# Loading data
DataMIL <- read.csv(file = 
                      "Data/processed/EXP_full_MIL_corrected.csv")

# Removing irrelevant clones data

# DataMIL <- DataMIL%>%
#   filter(!grepl("C76", pop))
# 
# 
DataMIL<- DataMIL%>%
  filter(!grepl("C89", pop))

DataMIL<- DataMIL%>%
  filter(!grepl("THP-1", pop))


# Checking variables
sapply(DataMIL, class)
DataMIL$conc <- as.factor(DataMIL$conc)
DataMIL$pop <- as.factor(DataMIL$pop)
DataMIL$experiment <- as.factor(DataMIL$experiment)
sapply(DataMIL, class)

# Dose response - amastigotes accounts for each conc

AmaPcellplot <- ggplot(DataMIL, aes(fill= conc,
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
  facet_wrap(DataMIL$experiment)
theme_bw()
AmaPcellplot

ggsave("Figures/01_MIL_AmaPcellplot.jpg")

# Lines

lineplot <- ggplot(DataMIL, aes(y = ama_per_cell, x = conc, group = pop))+
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

ggsave("Figures/01_MIL_AmaPcellplot.jpg")

# Summarized

EXP_sum <- DataMIL%>%
  group_by(conc,pop)%>%
  summarise(mean_value = mean(ama_per_cell), sd_value = sd(ama_per_cell) )

lineplot_sum <- ggplot(EXP_sum, aes(y = mean_value, x = conc, group = pop))+
  geom_line(aes(color = EXP_sum$pop))+
  ggtitle("Dose response pattern for different MIL concentrations") +
  labs(x = " Populations ", y = "Nº of cells")+
  theme(plot.title = element_text(size = 14,face="bold"),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))+
  theme_bw()
lineplot_sum

ggsave("Figures/03_MIL_AmaPcellplot.jpg")

