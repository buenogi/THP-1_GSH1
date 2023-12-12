################################################################################
######################### Infection  EDA #######################################
################################################################################

# Packages
library(dplyr)
library(ggplot2)
library(ggpubr)
library(multcompView)

# Loading data
DataANF <- read.csv(file = 
                      "Data/processed/EXP_full_ANF_corrected.csv")
DataMIL <- read.csv(file = 
                      "Data/processed/EXP_full_MIL_corrected.csv")
DataSbIII <- read.csv(file = 
                        "Data/processed/EXP_full_SbIII_corrected.csv")

EXP_full <-rbind(DataANF, DataMIL, DataSbIII)

# Removing inadequate clones

EXP_full<- EXP_full%>%
  filter(!grepl("THP-1", pop))

EXP_full<- EXP_full%>%
  filter(!grepl("C89", pop))

# clones c89 and c68 are going to be removed because were not evaluated for the 
# same number of replicates

# Checking
sapply(EXP_full, class)
EXP_full$conc <- as.factor(EXP_full$conc)
EXP_full$pop <- as.factor(EXP_full$pop)
EXP_full$composto <- as.factor(EXP_full$composto)
EXP_full$experiment <- as.factor(EXP_full$experiment)
sapply(EXP_full, class)

Infec_data <- EXP_full

#Infectivity

Infec_data <- filter(EXP_full, conc == 0 | conc == 0.00)
sapply(Infec_data, class)

infectivity_plot_01 <- ggplot(Infec_data, aes(x = reorder(pop,desc(ama_per_cell)),
                                              ama_per_cell))+
  geom_bar(stat = "identity", fill = "purple")+
  labs(y = "Amastigotes/Cell", x = "Population")+
  facet_wrap(Infec_data$composto)+
  theme_light()

infectivity_plot_01

# In this case, drugs are consideres technical replicates because for each drug 
# was made one plate at the same day. The experiments are biological replicates 
# because  it were performed in different days
ggsave("Figures/01_infectivity.jpg")

infectivity_plot_02 <- ggplot(Infec_data, aes(x = reorder(pop,desc(ama_per_cell))
                                              , ama_per_cell))+
  geom_bar(stat = "identity", fill = "purple")+
  labs(y = "Amastigotes/Cell", x = "Population")+
  facet_wrap(Infec_data$experiment)
theme_light()
infectivity_plot_02

ggsave("Figures/02_infectivity.jpg")

# Calculating mean and standard errors

sum_measures <- Infec_data%>%
  group_by(pop)%>%
  summarise(mean_value = mean(ama_per_cell), 
            sd_error = sd(ama_per_cell)/sqrt(4))


Infec_data_SUM <- full_join(Infec_data, sum_measures)

sapply(Infec_data_SUM, class)

infectivity_plot_03 <- ggplot(Infec_data_SUM, 
                              aes(x = reorder(pop,desc(mean_value)), y = mean_value))+
  geom_bar(stat = "identity", fill = "purple", 
           position = position_dodge(width = 0.02))+
  geom_errorbar(aes(ymin = mean_value - sd_error,
                    ymax = mean_value + sd_error,
                    width = 0.2, alpha = 0.5))+
  labs(y = "Amastigotes/Cell", x = "Population")+
  theme_light()+
  stat_compare_means(aes(group = "REF"), label = "p.signif", label.y = 35)

infectivity_plot_03 



ggsave("Figures/03_infectivity.jpg")


# ANOVA

sum_measures_reptec <- Infec_data%>%
  group_by(pop, experiment)%>%
  summarise(mean_value_reptec = mean(ama_per_cell), 
            sd_value= sd(ama_per_cell))

# 
# modelo_anova <- aov(mean_value_reptec~ pop, data = sum_measures_reptec)
# 
# summary(modelo_anova)

modelo_anova <- aov(ama_per_cell~ pop, data = Infec_data)

summary(modelo_anova)

# Identifying differences

posthoc_tukey <- TukeyHSD(modelo_anova)

print(posthoc_tukey)

# plot(posthoc_tukey , las=1 , col="brown")
# png("Figures/04_infectivity.png",  width = 6, height = 4, units = "in", res = 300)

#Showing differences

modelo_anova <- aov(ama_per_cell~ pop, data = Infec_data)


posthoc_tukey <- TukeyHSD(modelo_anova)

cld <- multcompLetters4(modelo_anova,posthoc_tukey)

# table with factors and 3rd quantile

dt <- group_by(Infec_data,pop)%>%
  summarise(mean_value = mean(ama_per_cell), 
            sd_value= sd(ama_per_cell))%>%
  arrange(desc(mean_value))

# extracting the compact letter display and adding to the Tk table

cld <- as.data.frame.list(cld$pop)
dt$cld <- cld$Letters

print(dt)

infectivity_plot_05 <- ggplot(dt, aes(reorder(pop,desc(mean_value)), mean_value)) + 
  geom_bar(stat = "identity", aes(fill = mean_value), show.legend = FALSE) +
  geom_errorbar(aes(ymin = mean_value-sd_value, ymax=mean_value+sd_value),
                width = 0.2) +
  labs(x = " ", y = "Média de amastigotas por célula") +
  scale_x_discrete(labels = c("REF" = "LiWT\n(+/+)",
                       "C6" = "LiGSH1\nC6\n(+/-)",
                       "C7" = "LiGSH1\nC7\n(+/-)",
                       "C44" = "LiGSH1\nC44\n(+/-)",
                       "C58" = "LiGSH1\nC58\n(+/-)",
                       "C67" = "LiGSH1\nC67\n(+/-)",
                       "C73" = "LiGSH1\nC73\n(+/-)",
                       "C85" = "LiGSH1\nC85\n(+/-)",
                       "C89" = "LiGSH1\nC89\n(+/-)",
                       "C76" = "LiPGPA\nC76\n(+/-)",
                       "C67p" = "LiPGPA\nC67\n(-/-)",
                       "C68" = "LiPGPA\nC68\n(-/-)"))+
  theme_bw()+
  geom_text(aes(label = cld, y = mean_value + sd_value), size = 10, vjust = -0.5) +
  ylim(0,30) 
  

infectivity_plot_05<- infectivity_plot_05+theme(plot.title = element_text(size = 24, face = "bold"),
                          axis.text.x = element_text(size = 24, face = "bold"),
                          axis.text.y = element_text(size = 24, face = "bold"),
                          axis.title.x = element_text(size = 24, face = "bold"),
                          axis.title.y = element_text(size = 24, face = "bold"),
                          legend.text = element_text(size = 24),
                          legend.title = element_text(size = 24,  face = "bold")
                          
)

infectivity_plot_05
ggsave("Figures/05_infectivity.jpg")
