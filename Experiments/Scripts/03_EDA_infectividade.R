################################################################################
############### Infectivity EDA replicates data cleaning #######################
################################################################################


library(ggplot2)

sbIII <- read.csv(file = "Data/processed/EXP1_SbIII_22042023_corrected.csv")
anf <- read.csv(file = "Data/processed/EXP1_ANF_22042023_corrected.csv")
milt <- read.csv(file = "Data/processed/EXP1_MILT_22042023_corrected.csv")


# infectivity

Infec_data1 <- filter(sbIII, conc == "0")
Infec_data2 <- filter(anf, conc == "0")
Infec_data3 <- filter(milt, conc == "0")

infec_full <- rbind(Infec_data1, Infec_data2, Infec_data3)%>%
  group_by(pop)%>%
  summarise("mean_value" = mean(ama_per_cell), "sd_value"  = sd(ama_per_cell))


# Full plot with 3 technical replicates fr infectivity

infectivity_plot <- ggplot(infec_full, aes(pop, mean_value))+
  geom_bar(stat = "identity", fill = "purple")+
  geom_errorbar(aes(ymin = mean_value - sd_value, ymax = mean_value + sd_value),
  width = 0.4,  # Largura das barras de erro
  position = position_dodge(width = 0.9))+
  labs(y = "Amastigotes/Cell", x = "Population")+
  #facet_wrap(Infec_data$experiment)
theme_light()
infectivity_plot

ggsave("figs/01_infectivity_REPTECS.jpg")
