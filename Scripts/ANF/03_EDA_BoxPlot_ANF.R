################################################################################
############################### EDA-BoxPlot  ###################################
################################################################################

# Packages

library(ggplot2)

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

# Checking data
head(DataANF)
sapply(DataANF, class)
DataANF$pop <- as.factor(DataANF$pop )
DataANF$experiment <- as.factor(DataANF$experiment)
DataANF$conc<- as.factor(DataANF$conc)
sapply(DataANF, class)


MTT_BP_pop <-  ggplot(DataANF, aes(conc, ama_per_cell))+
  geom_boxplot()+
  ggtitle("Box plot mastigotes count per ANF dosage") +
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

ggsave("Figures/01_MTT_BoxPlot_normalized.png")

