################################################################################
############################### EDA-BoxPlot  ###################################
################################################################################

# Packages

library(dplyr)
library(ggplot2)

# Loading data 

DataMIL <-  read.csv(file = 
                       "Data/processed/DataMIL_processed_normalized.csv",
                     header = TRUE, sep =",")

# Checking data
head(DataMIL)
sapply(DataMIL, class)
DataMIL$pop <- as.factor(DataMIL$pop )
DataMIL$experiment <- as.factor(DataMIL$experiment)
DataMIL$conc<- as.factor(DataMIL$conc)
sapply(DataMIL, class)


MTT_BP_pop <-  ggplot(DataMIL, aes(conc, viability_normalized))+
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

ggsave("Figures/06_MIL_MTT_BoxPlot_normalized.png")

