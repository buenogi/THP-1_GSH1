################################################################################
############################### EDA-BoxPlot  ###################################
################################################################################

# Packages

library(dplyr)
library(ggplot2)

# Loading data 

DataANF <-  read.csv(file = 
                       "Experiments/Data/processed/DataANF_processed_normalized.csv",
                     header = TRUE, sep =",")

# Checking data
head(DataANF)
sapply(DataANF, class)
DataANF$pop <- as.factor(DataANF$pop )
DataANF$experiment <- as.factor(DataANF$experiment)
DataANF$conc<- as.factor(DataANF$conc)
sapply(DataANF, class)


MTT_BP_pop <-  ggplot(DataANF, aes(conc, viability_normalized))+
  geom_boxplot()+
  ggtitle("Box plot promastigotes viability per SbIII dosage") +
  labs(x = " Conc [   ] μM", y = "Viability (%)")+
  theme(plot.title = element_text(size = 14,face="bold"),
        axis.text.x = element_text(size = 10), 
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))+
  #facet_grid(MTT_SbIII_48h$pop)+
  facet_wrap(~pop)+
  theme_bw()

MTT_BP_pop +  labs(color = "Populations")

ggsave("Experiments/Figures/02_MTT_BoxPlot_normalized.png")
