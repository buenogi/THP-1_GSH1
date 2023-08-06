################################################################################
################################## DR SbIII EDA ##################################
################################################################################

# Packages
library(dplyr)
library(ggplot2)


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


# Checking variables
sapply(DataSbIII, class)
DataSbIII$conc <- as.factor(DataSbIII$conc)
DataSbIII$pop <- as.factor(DataSbIII$pop)
DataSbIII$experiment <- as.factor(DataSbIII$experiment)
sapply(DataSbIII, class)

# Dose response - amastigotes accounts for each conc

AmaPcellplot <- ggplot(DataSbIII, aes(fill= conc,
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
  facet_wrap(DataSbIII$experiment)+
  theme_bw()
AmaPcellplot

ggsave("Experiments/Figures/01_AmaPcellplot_SbIII.jpg")

# Lines

lineplot <- ggplot(DataSbIII, aes(y = ama_per_cell, x = conc, group = pop))+
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

ggsave("Experiments/Figures/02_AmaPcellplot_SbIII.jpg")

# Summarized

EXP_sum <- DataSbIII%>%
  group_by(conc,pop)%>%
  summarise(mean_value = mean(ama_per_cell), sd_value = sd(ama_per_cell) )

lineplot_sum <- ggplot(EXP_sum, aes(y = mean_value, x = conc, group = pop))+
  geom_line(aes(color = EXP_sum$pop))+
  ggtitle("Dose response pattern for different SbIII concentrations") +
  labs(x = " Populations ", y = "Nº of cells")+
  theme(plot.title = element_text(size = 14,face="bold"),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))+
  theme_bw()
lineplot_sum

ggsave("Experiments/Figures/03_AmaPcellplot_SbIII.jpg")
