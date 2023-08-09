################################################################################
############################### EDA-BoxPlot  ###################################
################################################################################

# Packages

library(ggplot2)

# Loading data 

DataSbIII <- read.csv(file = 
                        "Data/processed/EXP_full_SbIII_corrected.csv")

# Removing irrelevant clones data

# DataSbIII <- DataSbIII%>%
#   filter(!grepl("C76", pop))


DataSbIII<- DataSbIII%>%
  filter(!grepl("C89", pop))

DataSbIII<- DataSbIII%>%
  filter(!grepl("THP-1", pop))

# Checking data
head(DataSbIII)
sapply(DataSbIII, class)
DataSbIII$pop <- as.factor(DataSbIII$pop )
DataSbIII$experiment <- as.factor(DataSbIII$experiment)
DataSbIII$conc<- as.factor(DataSbIII$conc)
sapply(DataSbIII, class)


MTT_BP_pop <-  ggplot(DataSbIII, aes(conc, ama_per_cell))+
  geom_boxplot()+
  ggtitle("Box plot mastigotes count per SbIII dosage") +
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

