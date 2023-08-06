################################################################################
######################### Infection MILT EDA ##################################
################################################################################

# Packages
library(dplyr)
library(ggplot2)
#library(scales)

# Loading data
EXP_full <- read.csv(file = "Data/processed/EXP_full_MILT_corrected.csv")
#EXP2 <- read.csv(file = "Data/Processed/EXP2_08042023_corrected.csv")

# Checking
sapply(EXP_full, class)
EXP_full$conc <- as.factor(EXP_full$conc)
EXP_full$pop <- as.factor(EXP_full$pop)
EXP_full$experiment <- as.factor(EXP_full$experiment)
sapply(EXP_full, class)

# Infectivity

Infec_data <- filter(EXP_full, conc == "0")

infectivity_plot <- ggplot(Infec_data, aes(pop, ama_per_cell))+
  geom_bar(stat = "identity", fill = "purple")+
  #geom_errorbar(aes(ymin = mean_value - sd_value, ymax = mean_value + sd_value),
  #width = 0.4,  # Largura das barras de erro
  #position = position_dodge(width = 0.9))+
  labs(y = "Amastigotes/Cell", x = "Population")+
  facet_wrap(Infec_data$experiment)
theme_light()
infectivity_plot

ggsave("figs/01_infectivity_MILT.jpg")

# Dose response - amastigotes accounts for each conc

AmaPcellplot <- ggplot(EXP_full, aes(fill= conc,
                                     y = ama_per_cell,
                                     x = pop))+
  geom_bar(position = "dodge", stat = "identity")+
  ggtitle("Nº of amastigote per cell MILT dosage in different populations") +
  labs(x = " Populations ", y = "Nº  amastigote per cell")+
  theme(plot.title = element_text(size = 14,face="bold"),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))+
  facet_wrap(EXP_full$experiment)
theme_bw()
AmaPcellplot

ggsave("figs/02_AmaPcellplot_MILT.jpg")

# Lines

lineplot <- ggplot(EXP_full, aes(y = ama_per_cell, x = conc, group = pop))+
  geom_line(aes(color = EXP_full$pop))+
  ggtitle("Dose response pattern for different MILT concentrations") +
  labs(x = " Populations ", y = "Nº of cells")+
  theme(plot.title = element_text(size = 14,face="bold"),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))+
  facet_wrap(EXP_full$experiment)+
  theme_bw()
lineplot

ggsave("figs/03_lineplot_MILT.jpg")

EXP_sum <- EXP_full%>%
  group_by(conc,pop)%>%
  summarise(mean_value = mean(ama_per_cell), sd_value = sd(ama_per_cell))

lineplot_sum <- ggplot(EXP_sum, aes(y = mean_value, x = conc, group = pop))+
  geom_line(aes(color = EXP_sum$pop))+
  ggtitle("Dose response pattern for different MILT concentrations") +
  labs(x = " Populations ", y = "Nº of cells")+
  theme(plot.title = element_text(size = 14,face="bold"),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))+
    theme_bw()
lineplot_sum
