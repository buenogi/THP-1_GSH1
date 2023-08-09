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

ggplot(dt, aes(reorder(pop,desc(mean_value)), mean_value)) + 
  geom_bar(stat = "identity", aes(fill = mean_value), show.legend = FALSE) +
  geom_errorbar(aes(ymin = mean_value-sd_value, ymax=mean_value+sd_value),
                width = 0.2) +
  labs(x = "População", y = "Média de amastigotas por célula") +
  geom_text(aes(label = cld, y = mean_value + sd_value), vjust = -0.5) +
  ylim(0,30) +
  theme_bw()

