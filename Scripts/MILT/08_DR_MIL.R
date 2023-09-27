################################################################################
################### MIL amastigotes Dose-response analysis #####################
################################################################################

library(dplyr)
library(ggplot2) 
library(drc)
library(gridExtra)
# Loading data 

DataMIL<- read.csv(file = "Data/processed/DataMIL_processed_normalized.csv", 
                   header = TRUE, sep = ,)

# Checking data
head(DataMIL)
sapply(DataMIL, class)
DataMIL$experiment <- as.factor(DataMIL$experiment)
sapply(DataMIL, class)

# Adding mean values

sum_values <- DataMIL%>%
  group_by(conc,pop)%>%
  summarise(mean_value_res = mean(viability_normalized), 
            sd_value_res = sd(viability_normalized))

DataMIL_SUM <- full_join(DataMIL, sum_values, by = c("conc", "pop"))


#Splitting populations
REF <- filter(DataMIL_SUM, pop == "REF")
C6 <- filter(DataMIL_SUM, pop == "C6")
C7 <- filter(DataMIL_SUM, pop == "C7")
C44 <- filter(DataMIL_SUM, pop == "C44")
C58 <- filter(DataMIL_SUM, pop == "C58")
C67 <- filter(DataMIL_SUM, pop == "C67")
C73 <- filter(DataMIL_SUM, pop == "C73")
C85 <- filter(DataMIL_SUM, pop == "C85")
C89 <- filter(DataMIL_SUM, pop == "C89")
C76 <- filter(DataMIL_SUM, pop == "C76")
C67p <- filter(DataMIL_SUM, pop == "C67p")


# Adjusting the models

REF_MIL.LL4 <- drm(viability_normalized ~ conc, data = REF , fct = LL.4(
  fixed = c(NA, 0,100, NA), names = c("b","c", "d", "e")))
plot(REF_MIL.LL4, xlim = c(0,1000), ylim = c(0,100), 
     broken = TRUE, gridsize = 100, xlab = "MIL [  ] μM", 
     ylab = "Viability (%)", type = "bars")
summary(REF_MIL.LL4)

C6_MIL.LL4 <- drm(viability_normalized ~ conc, data = C6 , fct = LL.4(
  fixed = c(NA,0,100, NA), names = c("b","c", "d", "e")))
plot(C6_MIL.LL4, xlim = c(0,1000), ylim = c(0,100), 
     broken = TRUE, gridsize = 100, xlab = "MIL [  ] μM", 
     ylab = "Viability (%)", type = "bars")

C7_MIL.LL4 <- drm(viability_normalized ~ conc, data = C7 , fct = LL.4(
  fixed = c(NA, 0,100, NA), names = c("b","c", "d", "e")))
plot(C7_MIL.LL4, xlim = c(0,1000), ylim = c(0,100), 
     broken = TRUE, gridsize = 100, xlab = "MIL [  ] μM", 
     ylab = "Viability (%)", type = "bars")

C44_MIL.LL4 <- drm(viability_normalized ~ conc, data = C44 , fct = LL.4(
  fixed = c(NA, 0,100, NA), names = c("b","c", "d", "e")))
plot(C44_MIL.LL4, xlim = c(0,1000), ylim = c(0,100), 
     broken = TRUE, gridsize = 100, xlab = "MIL [  ] μM", 
     ylab = "Viability (%)", type = "bars")

C58_MIL.LL4 <- drm(viability_normalized ~ conc, data = C58 , fct = LL.4(
  fixed = c(NA, 0,100, NA), names = c("b","c", "d", "e")))
plot(C58_MIL.LL4, xlim = c(0,1000), ylim = c(0,100), 
     broken = TRUE, gridsize = 100, xlab = "MIL [  ] μM", 
     ylab = "Viability (%)", type = "bars")

C67_MIL.LL4 <- drm(viability_normalized ~ conc, data = C67 , fct = LL.4(
  fixed = c(NA,0,100, NA), names = c("b","c", "d", "e")))
plot(C67_MIL.LL4, xlim = c(0,1000), ylim = c(0,100), 
     broken = TRUE, gridsize = 100, xlab = "MIL [  ] μM", 
     ylab = "Viability (%)", type = "bars")

C73_MIL.LL4 <- drm(viability_normalized ~ conc, data = C73 , fct = LL.4(
  fixed = c(NA, 0,100, NA), names = c("b","c", "d", "e")))
plot(C73_MIL.LL4, xlim = c(0,1000), ylim = c(0,100), 
     broken = TRUE, gridsize = 100, xlab = "MIL [  ] μM", 
     ylab = "Viability (%)", type = "bars")

C85_MIL.LL4 <- drm(viability_normalized ~ conc, data = C85 , fct = LL.4(
  fixed = c(NA,0,100, NA), names = c("b","c", "d", "e")))
plot(C85_MIL.LL4, xlim = c(0,1000), ylim = c(0,100), 
     broken = TRUE, gridsize = 100, xlab = "MIL [  ] μM", 
     ylab = "Viability (%)", type = "bars")

C76_MIL.LL4 <- drm(viability_normalized ~ conc, data = C76 , fct = LL.4(
  fixed = c(NA, 0, 100, NA), names = c("Slope", "Lower Limit", "Upper Limit", "ED50")))
plot(C76_MIL.LL4, xlim = c(0,1000), ylim = c(0,100), 
     broken = TRUE, gridsize = 100, xlab = "MIL [  ] μM", 
     ylab = "Viability (%)", type = "bars")

C67p_MIL.LL4 <- drm(viability_normalized ~ conc, data = C67p , fct = LL.4(
  fixed = c(NA, 0, 100, NA), names = c("Slope", "Lower Limit", "Upper Limit", "ED50")))
plot(C67p_MIL.LL4, xlim = c(0,1000), ylim = c(0,100), 
     broken = TRUE, gridsize = 100, xlab = "MIL [  ] μM", 
     ylab = "Viability (%)", type = "bars")

# ED50

ED(REF_MIL.LL4,50)
ED(C6_MIL.LL4,50)
ED(C7_MIL.LL4,50)
ED(C44_MIL.LL4,50)
ED(C58_MIL.LL4,50)
ED(C67_MIL.LL4,50)
ED(C73_MIL.LL4,50)
ED(C85_MIL.LL4,50)
ED(C76_MIL.LL4,50)
ED(C67p_MIL.LL4,50)


## Exploring predicted model
newdata_REF <- expand.grid(conc = seq(min(REF$conc), max(REF$conc), length.out = 200))
newdata_REF$viability_normalized <- predict(REF_MIL.LL4, newdata_REF, type = 'response')
newdata_REF$pop <- "REF"

newdata_C6 <- expand.grid(conc = seq(min(C6$conc), max(C6$conc),
                                     length.out = 200))
newdata_C6$viability_normalized <- predict(C6_MIL.LL4, newdata_C6, type = 'response')
newdata_C6$pop <- "C6"

newdata_C7 <- expand.grid(conc = seq(min(C7$conc), max(C7$conc),
                                     length.out = 200))
newdata_C7$viability_normalized <- predict(C7_MIL.LL4, newdata_C7, type = 'response')
newdata_C7$pop <- "C7"

newdata_C44 <- expand.grid(conc = seq(min(C44$conc), max(C44$conc),
                                      length.out = 200))
newdata_C44$viability_normalized <- predict(C44_MIL.LL4, newdata_C44, type = 'response')
newdata_C44$pop <- "C44"

newdata_C58 <- expand.grid(conc = seq(min(C58$conc), max(C58$conc),
                                      length.out = 200))
newdata_C58$viability_normalized <- predict(C58_MIL.LL4, newdata_C58, type = 'response')
newdata_C58$pop <- "C58"

newdata_C67 <- expand.grid(conc = seq(min(C67$conc), max(C67$conc),
                                      length.out = 200))
newdata_C67$viability_normalized <- predict(C67_MIL.LL4, newdata_C67, type = 'response')
newdata_C67$pop <- "C67"

newdata_C73 <- expand.grid(conc = seq(min(C73$conc), max(C73$conc),
                                      length.out = 200))
newdata_C73$viability_normalized <- predict(C73_MIL.LL4, newdata_C73, type = 'response')
newdata_C73$pop <- "C73"

newdata_C85 <- expand.grid(conc = seq(min(C85$conc), max(C85$conc),
                                      length.out = 200))
newdata_C85$viability_normalized <- predict(C85_MIL.LL4, newdata_C85, type = 'response')
newdata_C85$pop <- "C85"

newdata_C76 <- expand.grid(conc = seq(min(C76$conc), max(C76$conc),
                                      length.out = 200))
newdata_C76$viability_normalized <- predict(C76_MIL.LL4, newdata_C76, type = 'response')
newdata_C76$pop <- "C76"

newdata_C67p <- expand.grid(conc = seq(min(C67p$conc), max(C67p$conc),
                                       length.out = 200))
newdata_C67p$viability_normalized  <- predict(C67p_MIL.LL4, newdata_C67p, type = 'response')
newdata_C67p$pop <- "C67p"

MTT_SUM_full <- full_join(REF, C6)
MTT_SUM_full <- full_join(MTT_SUM_full, C7)
MTT_SUM_full <- full_join(MTT_SUM_full, C44)
MTT_SUM_full <- full_join(MTT_SUM_full, C58)
MTT_SUM_full <- full_join(MTT_SUM_full, C67)
MTT_SUM_full <- full_join(MTT_SUM_full, C73)
MTT_SUM_full <- full_join(MTT_SUM_full, C85)
MTT_SUM_full <- full_join(MTT_SUM_full, C76)
MTT_SUM_full <- full_join(MTT_SUM_full, C67p)


newdata_full <- rbind.data.frame(newdata_REF, newdata_C6)
newdata_full <- rbind(newdata_full, newdata_C7)
newdata_full <- rbind(newdata_full, newdata_C44)
newdata_full <- rbind(newdata_full, newdata_C58)
newdata_full <- rbind(newdata_full, newdata_C67)
newdata_full <- rbind(newdata_full, newdata_C73)
newdata_full <- rbind(newdata_full, newdata_C85)
newdata_full <- rbind(newdata_full, newdata_C76)
newdata_full <- rbind(newdata_full, newdata_C67p)


DoseResponseCurves_01 <- ggplot() +
  geom_point(data = REF, aes(x = log(conc, 10), y = viability_normalized)) +
  geom_line(data = newdata_REF, aes(x = log(conc, 10), y = newdata_REF$viability_normalized)) +
  geom_point(data = C6, aes(x = log(conc, 10), y = viability_normalized)) +
  geom_line(data = newdata_C6, aes(x = log(conc, 10), y = newdata_C6$viability_normalized))+
  geom_point(data = C7, aes(x = log(conc, 10), y = viability_normalized)) +
  geom_line(data = newdata_C7, aes(x = log(conc, 10), y = newdata_C7$viability_normalized)) +
  geom_point(data = C44, aes(x = log(conc, 10), y = viability_normalized)) +
  geom_line(data = newdata_C44
            , aes(x = log(conc, 10), y = newdata_C44$viability_normalized))+
  geom_point(data = C58, aes(x = log(conc, 10), y = viability_normalized)) +
  geom_line(data = newdata_C58, aes(x = log(conc, 10), y = newdata_C58$viability_normalized))+
  geom_point(data = C67, aes(x = log(conc, 10), y = viability_normalized)) +
  geom_line(data = newdata_C67, aes(x = log(conc, 10), y =  newdata_C67$viability_normalized )) +
  geom_point(data = C73, aes(x = log(conc, 10), y = viability_normalized)) +
  geom_line(data = newdata_C73, aes(x = log(conc, 10), y =newdata_C73$viability_normalized )) +
  geom_point(data = C85, aes(x = log(conc, 10), y = viability_normalized)) +
  geom_line(data = newdata_C85, aes(x = log(conc, 10), y = newdata_C85$viability_normalized))+
  geom_point(data = C76, aes(x = log(conc, 10), y = viability_normalized)) +
  geom_line(data = newdata_C76, aes(x = log(conc, 10), y = newdata_C76$viability_normalized)) +
  geom_point(data = C67p, aes(x = log(conc, 10), y = viability_normalized)) +
  geom_line(data = newdata_C67p, aes(x = log(conc, 10), y =  newdata_C67p$viability_normalized )) +
  ggtitle("Promastigotes dose response to MIL") +
  labs(x = "Log10 [ ] μM", y = "Viability (%)") +
  theme_bw() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15)) +
  facet_wrap(~pop)

DoseResponseCurves_01

ggsave("Figures/11_MIL_DoseResponseCurves_01.png")

DoseResponseCurves02_error <- ggplot() +
  geom_point(data = REF, aes(x = log(conc, 10), y = viability_normalized)) +
  geom_line(data = newdata_REF, aes(x = log(conc, 10), y = newdata_REF$viability_normalized)) +
  geom_errorbar(data = REF, aes(x = log(conc, 10),
                                ymin = mean_value_res - sd_value_res/sqrt(8),
                                ymax = mean_value_res + sd_value_res/sqrt(8)), width = 0.2) +
  geom_point(data = C6, aes(x = log(conc, 10), y = viability_normalized)) +
  geom_line(data = newdata_C6, aes(x = log(conc, 10), y = newdata_C6$viability_normalized)) +
  geom_errorbar(data = C6, aes(x = log(conc, 10),
                               ymin = mean_value_res - sd_value_res/sqrt(4),
                               ymax = mean_value_res + sd_value_res/sqrt(4)), width = 0.2) +
  geom_point(data = C7, aes(x = log(conc, 10), y = viability_normalized)) +
  geom_line(data = newdata_C7, aes(x = log(conc, 10), y = newdata_C7$viability_normalized)) +
  geom_errorbar(data = C7, aes(x = log(conc, 10),
                               ymin = mean_value_res - sd_value_res/sqrt(4),
                               ymax = mean_value_res + sd_value_res/sqrt(4)), width = 0.2) +
  geom_point(data = C44, aes(x = log(conc, 10), y = viability_normalized)) +
  geom_line(data = newdata_C44, aes(x = log(conc, 10), y = newdata_C44$viability_normalized)) +
  geom_errorbar(data = C44, aes(x = log(conc, 10),
                                ymin = mean_value_res - sd_value_res/sqrt(4),
                                ymax = mean_value_res + sd_value_res/sqrt(4)), width = 0.2) +
  geom_point(data = C58, aes(x = log(conc, 10), y = viability_normalized)) +
  geom_line(data = newdata_C58, aes(x = log(conc, 10), y = newdata_C58$viability_normalized)) +
  geom_errorbar(data = C58, aes(x = log(conc, 10),
                                ymin = mean_value_res - sd_value_res/sqrt(4),
                                ymax = mean_value_res + sd_value_res/sqrt(4)), width = 0.2) +
  geom_point(data = C73, aes(x = log(conc, 10), y = viability_normalized)) +
  geom_line(data = newdata_C73, aes(x = log(conc, 10), y = newdata_C73$viability_normalized)) +
  geom_errorbar(data = C73, aes(x = log(conc, 10),
                                ymin = mean_value_res - sd_value_res/sqrt(4),
                                ymax = mean_value_res + sd_value_res/sqrt(4)), width = 0.2) +
  geom_point(data = C85, aes(x = log(conc, 10), y = viability_normalized)) +
  geom_line(data = newdata_C85, aes(x = log(conc, 10), y = newdata_C85$viability_normalized)) +
  geom_errorbar(data = C85, aes(x = log(conc, 10),
                                ymin = mean_value_res - sd_value_res/sqrt(4),
                                ymax = mean_value_res + sd_value_res/sqrt(4)), width = 0.2) +
  geom_point(data = C67, aes(x = log(conc, 10), y = viability_normalized)) +
  geom_line(data = newdata_C67, aes(x = log(conc, 10), y = newdata_C67$viability_normalized)) +
  geom_errorbar(data = C67, aes(x = log(conc, 10),
                                ymin = mean_value_res - sd_value_res/sqrt(4),
                                ymax = mean_value_res + sd_value_res/sqrt(4)), width = 0.2) +
  geom_point(data = C76, aes(x = log(conc, 10), y = viability_normalized)) +
  geom_line(data = newdata_C76, aes(x = log(conc, 10), y = newdata_C76$viability_normalized)) +
  geom_errorbar(data = C76, aes(x = log(conc, 10),
                                ymin = mean_value_res - sd_value_res/sqrt(4),
                                ymax = mean_value_res + sd_value_res/sqrt(4)), width = 0.2) +
  geom_point(data = C67p, aes(x = log(conc, 10), y = viability_normalized)) +
  geom_line(data = newdata_C67p, aes(x = log(conc, 10), y = newdata_C67p$viability_normalized)) +
  geom_errorbar(data = C67p, aes(x = log(conc, 10),
                                 ymin = mean_value_res - sd_value_res/sqrt(4),
                                 ymax = mean_value_res + sd_value_res/sqrt(4)), width = 0.2) +
  ggtitle("Promastigotes dose response to MIL") +
  labs(x = "Log10 [ ] μM", y = "Viability (%)") +
  theme_bw() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15))+
  facet_wrap(~pop)

DoseResponseCurves02_error
ggsave("Figures/12_MIL_DoseResponseCurves.png")


DoseResponseCurves03_error <- ggplot() +
  geom_point(data = REF, aes(x = log(conc, 10), y = mean_value_res, color = pop)) +
  geom_line(data = newdata_REF, aes(x = log(conc, 10), y = viability_normalized, color = pop)) +
  geom_errorbar(data = REF, aes(x = log(conc, 10),
                                ymin = mean_value_res - sd_value_res/sqrt(8),
                                ymax = mean_value_res + sd_value_res/sqrt(8)), width = 0.02, alpha = 0.3) +
  geom_point(data = C6, aes(x = log(conc, 10), y = mean_value_res, color = pop)) +
  geom_line(data = newdata_C6, aes(x = log(conc, 10),  y = viability_normalized, color = pop)) +
  geom_errorbar(data = C6, aes(x = log(conc, 10),
                               ymin = mean_value_res - sd_value_res/sqrt(4),
                               ymax = mean_value_res + sd_value_res/sqrt(4)), width = 0.02,alpha = 0.3) +
  geom_point(data = C7, aes(x = log(conc, 10), y = mean_value_res, color = pop)) +
  geom_line(data = newdata_C7, aes(x = log(conc, 10),  y = viability_normalized, color = pop)) +
  geom_errorbar(data = C7, aes(x = log(conc, 10),
                               ymin = mean_value_res - sd_value_res/sqrt(4),
                               ymax = mean_value_res + sd_value_res/sqrt(4)), width = 0.02,alpha = 0.3) +
  geom_point(data = C44, aes(x = log(conc, 10), y = mean_value_res, color = pop)) +
  geom_line(data = newdata_C44, aes(x = log(conc, 10),  y = viability_normalized, color = pop)) +
  geom_errorbar(data = C44, aes(x = log(conc, 10),
                                ymin = mean_value_res - sd_value_res/sqrt(4),
                                ymax = mean_value_res + sd_value_res/sqrt(4)), width = 0.02,alpha = 0.3) +
  geom_point(data = C58, aes(x = log(conc, 10), y = mean_value_res, color = pop)) +
  geom_line(data = newdata_C58, aes(x = log(conc, 10),  y = viability_normalized, color = pop)) +
  geom_errorbar(data = C58, aes(x = log(conc, 10),
                                ymin = mean_value_res - sd_value_res/sqrt(4),
                                ymax = mean_value_res + sd_value_res/sqrt(4)), width = 0.02,alpha = 0.3) +
  geom_point(data = C73, aes(x = log(conc, 10), y = mean_value_res, color = pop)) +
  geom_line(data = newdata_C73, aes(x = log(conc, 10),  y = viability_normalized, color = pop)) +
  geom_errorbar(data = C73, aes(x = log(conc, 10),
                                ymin = mean_value_res - sd_value_res/sqrt(4),
                                ymax = mean_value_res + sd_value_res/sqrt(4)), width = 0.02,alpha = 0.3) +
  geom_point(data = C85, aes(x = log(conc, 10), y = mean_value_res, color = pop)) +
  geom_line(data = newdata_C85, aes(x = log(conc, 10),  y = viability_normalized, color = pop)) +
  geom_errorbar(data = C85, aes(x = log(conc, 10),
                                ymin = mean_value_res - sd_value_res/sqrt(4),
                                ymax = mean_value_res + sd_value_res/sqrt(4)), width = 0.02,alpha = 0.3) +
  geom_point(data = C67, aes(x = log(conc, 10), y = mean_value_res, color = pop)) +
  geom_line(data = newdata_C67, aes(x = log(conc, 10),  y = viability_normalized, color = pop)) +
  geom_errorbar(data = C67, aes(x = log(conc, 10),
                                ymin = mean_value_res - sd_value_res/sqrt(4),
                                ymax = mean_value_res + sd_value_res/sqrt(4)), width = 0.02,alpha = 0.3) +
  # geom_point(data = C68, aes(x = log(conc, 10), y = mean_value_res, color = pop)) +
  # geom_line(data = newdata_C68, aes(x = log(conc, 10), y = viability_normalized, color = pop)) +
  # geom_errorbar(data = C68, aes(x = log(conc, 10),
  #                               ymin = mean_value_res - sd_value_res/sqrt(4),
  #                               ymax = mean_value_res + sd_value_res/sqrt(4)), width = 0.02,alpha = 0.3)+
  geom_point(data = C67p, aes(x = log(conc, 10), y = mean_value_res, color = pop)) +
  geom_line(data = newdata_C67p, aes(x = log(conc, 10), y = viability_normalized, color = pop)) +
  geom_errorbar(data = C67p, aes(x = log(conc, 10),
                                 ymin = mean_value_res - sd_value_res/sqrt(4),
                                 ymax = mean_value_res + sd_value_res/sqrt(4)), width = 0.02,alpha = 0.3)+
  geom_point(data = C76, aes(x = log(conc, 10), y = mean_value_res, color = pop)) +
  geom_line(data = newdata_C76, aes(x = log(conc, 10), y = viability_normalized, color = pop)) +
  geom_errorbar(data = C76, aes(x = log(conc, 10),
                                ymin = mean_value_res - sd_value_res/sqrt(4),
                                ymax = mean_value_res + sd_value_res/sqrt(4)), width = 0.02,alpha = 0.3)+
  ggtitle("Promastigotes dose response to MIL") +
  labs(x = "Log10 [ ] μM", y = "Viability (%)") +
  theme_bw() +
  theme(    plot.title = element_text(size = 14, face = "bold"),
            axis.text.x = element_text(size = 10),
            axis.text.y = element_text(size = 10),
            axis.title.x = element_text(size = 15),
            axis.title.y = element_text(size = 15))
#facet_wrap(~pop)

DoseResponseCurves03_error + labs(color = "Populations")

ggsave("Figures/13_MIL_DoseResponseCurves.png")


# Extracting measures

COEFICIENTS <- c("Slope", "ED50")
modelREF <- summary(REF_MIL.LL4)
confintREF <- confint(REF_MIL.LL4)
modelREF_coef <- as.data.frame(modelREF$coefficients)
modelREF_conf <- as.data.frame(confintREF)
modelREF_SUM <- cbind(modelREF_coef, modelREF_conf)
modelREF_SUM$coef <- COEFICIENTS 
modelREF_SUM$pop <- rep("REF", 2)

modelC6 <- summary(C6_MIL.LL4)
confintC6 <- confint(C6_MIL.LL4)
modelC6_coef <- as.data.frame(modelC6$coefficients)
modelC6_conf <- as.data.frame(confintC6)
modelC6_SUM <- cbind(modelC6_coef, modelC6_conf)
modelC6_SUM$coef <- COEFICIENTS 
modelC6_SUM$pop <- rep("C6", 2)

modelC7 <- summary(C7_MIL.LL4)
confintC7 <- confint(C7_MIL.LL4)
modelC7_coef <- as.data.frame(modelC7$coefficients)
modelC7_conf <- as.data.frame(confintC7)
modelC7_SUM <- cbind(modelC7_coef, modelC7_conf)
modelC7_SUM$coef <- COEFICIENTS 
modelC7_SUM$pop <- rep("C7", 2)

modelC44 <- summary(C44_MIL.LL4)
confintC44 <- confint(C44_MIL.LL4)
modelC44_coef <- as.data.frame(modelC44$coefficients)
modelC44_conf <- as.data.frame(confintC44)
modelC44_SUM <- cbind(modelC44_coef, modelC44_conf)
modelC44_SUM$coef <- COEFICIENTS 
modelC44_SUM$pop <- rep("C44", 2)

modelC58 <- summary(C58_MIL.LL4)
confintC58 <- confint(C58_MIL.LL4)
modelC58_coef <- as.data.frame(modelC58$coefficients)
modelC58_conf <- as.data.frame(confintC58)
modelC58_SUM <- cbind(modelC58_coef, modelC58_conf)
modelC58_SUM$coef <- COEFICIENTS 
modelC58_SUM$pop <- rep("C58", 2)

modelC67 <- summary(C67_MIL.LL4)
confintC67 <- confint(C67_MIL.LL4)
modelC67_coef <- as.data.frame(modelC67$coefficients)
modelC67_conf <- as.data.frame(confintC67)
modelC67_SUM <- cbind(modelC67_coef, modelC67_conf)
modelC67_SUM$coef <- COEFICIENTS 
modelC67_SUM$pop <- rep("C67", 2)

modelC73 <- summary(C73_MIL.LL4)
confintC73 <- confint(C73_MIL.LL4)
modelC73_coef <- as.data.frame(modelC73$coefficients)
modelC73_conf <- as.data.frame(confintC73)
modelC73_SUM <- cbind(modelC73_coef, modelC73_conf)
modelC73_SUM$coef <- COEFICIENTS 
modelC73_SUM$pop <- rep("C73", 2)

modelC85 <- summary(C85_MIL.LL4)
confintC85 <- confint(C85_MIL.LL4)
modelC85_coef <- as.data.frame(modelC85$coefficients)
modelC85_conf <- as.data.frame(confintC85)
modelC85_SUM <- cbind(modelC85_coef, modelC85_conf)
modelC85_SUM$coef <- COEFICIENTS 
modelC85_SUM$pop <- rep("C85", 2)

modelC76 <- summary(C76_MIL.LL4)
confintC76 <- confint(C76_MIL.LL4)
modelC76_coef <- as.data.frame(modelC76$coefficients)
modelC76_conf <- as.data.frame(confintC76)
modelC76_SUM <- cbind(modelC76_coef, modelC76_conf)
modelC76_SUM$coef <- COEFICIENTS 
modelC76_SUM$pop <- rep("C76", 2)

modelC67p <- summary(C67p_MIL.LL4)
confintC67p <- confint(C67p_MIL.LL4)
modelC67p_coef <- as.data.frame(modelC67p$coefficients)
modelC67p_conf <- as.data.frame(confintC67p)
modelC67p_SUM <- cbind(modelC67p_coef, modelC67p_conf)
modelC67p_SUM$coef <- COEFICIENTS 
modelC67p_SUM$pop <- rep("C67p", 2)

summary_DR <- rbind(modelREF_SUM, modelC7_SUM, modelC6_SUM, 
                    modelC44_SUM, modelC58_SUM, modelC67_SUM,
                    modelC73_SUM, modelC85_SUM, modelC76_SUM, 
                    modelC67p_SUM)

write.csv(summary_DR , file = "Docs/summary_DR_MIL.csv", row.names = FALSE)

# Models comparisons
m1<-drm(mean_value_res ~ conc, pop, data = DataMIL_SUM, 
        fct =LL.4(names = c("Slope", "Lower Limit", "Upper Limit", "ED50"), 
                  fixed = c(NA, 0,100, NA)))
m2<-drm(mean_value_res ~ conc, data = DataMIL_SUM, 
        fct =LL.4(names = c("Slope", "Lower Limit", "Upper Limit", "ED50"),
                  fixed = c(NA, 0,100, NA)))

anova(m1,m2) 

# There is difference between curves accordingly with populations 
# (m1 IS SIGNIFICANT).

compParm(m1, "ED50")
compParm(m1, "Slope")

# Differences was found between REF AND C44,  C6,C67, 85, 76 e 67p
# C6 was considered different from all other clones except c67p
# c7 was different from c44, c67, c85 and C67p
# C44 was different fromc76 and c67p
# C58/C76, C58/C67p, C67/C73, C67/C76, C67/C67p,C73/C85, C73/C67p, C73/C67p
# C85/C76,C85/C67p, C76/C67p

# Ploting meaningfull differences:

DoseResponseCurves04 <- ggplot() +
  geom_point(data = REF, aes(x = log(conc, 10), y = mean_value_res, color = pop)) +
  geom_line(data = newdata_REF, size = 2 ,aes(x = log(conc, 10), y = viability_normalized, color = pop)) +
  geom_errorbar(data = REF, aes(x = log(conc, 10),
                                ymin = mean_value_res - sd_value_res/sqrt(8),
                                ymax = mean_value_res + sd_value_res/sqrt(8), color = pop), 
                width = 0.05, alpha = 0.3) +
  geom_point(data = C6, aes(x = log(conc, 10), y = mean_value_res, color = pop)) +
  geom_line(data = newdata_C6,size = 2 , aes(x = log(conc, 10),  y = viability_normalized, color = pop)) +
  geom_errorbar(data = C6, aes(x = log(conc, 10),
                               ymin = mean_value_res - sd_value_res/sqrt(4),
                               ymax = mean_value_res + sd_value_res/sqrt(4), color = pop),
                width = 0.05, alpha = 0.3) +
  geom_point(data = C67, aes(x = log(conc, 10), y = mean_value_res, color = pop)) +
  geom_line(data = newdata_C67,size = 2 , aes(x = log(conc, 10),  y = viability_normalized, color = pop)) +
  geom_errorbar(data = C67, aes(x = log(conc, 10),
                                ymin = mean_value_res - sd_value_res/sqrt(4),
                                ymax = mean_value_res + sd_value_res/sqrt(4), color = pop),
                width = 0.05, alpha = 0.3) +
  geom_point(data = C44, aes(x = log(conc, 10), y = mean_value_res, color = pop)) +
  geom_line(data = newdata_C44,size = 2 , aes(x = log(conc, 10),  y = viability_normalized, color = pop)) +
  geom_errorbar(data = C44, aes(x = log(conc, 10),
                                ymin = mean_value_res - sd_value_res/sqrt(4),
                                ymax = mean_value_res + sd_value_res/sqrt(4), color = pop),
                width = 0.05, alpha = 0.3) +
  geom_point(data = C67p, aes(x = log(conc, 10), y = mean_value_res, color = pop)) +
  geom_line(data = newdata_C67p,size = 2 , aes(x = log(conc, 10), y = viability_normalized, color = pop)) +
  geom_errorbar(data = C67p, aes(x = log(conc, 10),
                                 ymin = mean_value_res - sd_value_res/sqrt(4),
                                 ymax = mean_value_res + sd_value_res/sqrt(4), color = pop),
                width = 0.05, alpha = 0.3) +
  # geom_point(data = C76, aes(x = log(conc, 10), y = mean_value_res, color = pop)) +
  # geom_line(data = newdata_C76, size = 2 ,aes(x = log(conc, 10),  y = viability_normalized, color = pop)) +
  # geom_errorbar(data = C76, aes(x = log(conc, 10),
  #                               ymin = mean_value_res - sd_value_res/sqrt(4),
  #                               ymax = mean_value_res + sd_value_res/sqrt(4), color = pop),
  #               width = 0.05, alpha = 0.3) +
  geom_point(data = C85, aes(x = log(conc, 10), y = mean_value_res, color = pop)) +
  geom_line(data = newdata_C85,size = 2 , aes(x = log(conc, 10),  y = viability_normalized, color = pop)) +
  geom_errorbar(data = C85, aes(x = log(conc, 10),
                                ymin = mean_value_res - sd_value_res/sqrt(4),
                                ymax = mean_value_res + sd_value_res/sqrt(4), color = pop),
                width = 0.05, alpha = 0.3) +
  ggtitle("Amastigotes suscetibitily to MIL") +
  labs(x = "Log10 [ ] μM", y = "Viabilidade (%)", color = "Populations") +
  ylim(0,115)+
  theme_bw()


DoseResponseCurves04 + labs(color = "Populações", size = 20)+
  theme(plot.title = element_text(size = 20, face = "bold"),
        axis.text.x = element_text(size = 20, face = "bold"),
        axis.text.y = element_text(size = 20, face = "bold"),
        axis.title.x = element_text(size = 20, face = "bold"),
        axis.title.y = element_text(size = 20, face = "bold"),
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20,  face = "bold"),
        legend.position = c(0.95,0.7),   
        legend.justification = "right",
        legend.box.background = element_blank())

ggsave("Figures/14_MIL_DoseResponseCurves.png")


#  Fit diagnostic
plot1 <- ggplot(REF, aes(x = fitted(REF_MIL.LL4), y = viability)) +
  geom_point(pch = 16, cex = 0.7) +
  geom_abline(intercept = 0, slope = 1) +
  labs(title = " REF - LL4 - Ajustados x Observados", x = "Valores ajustados", 
       y = "Valores observados") +
  theme_minimal()

plot2 <- ggplot(REF, aes(x = fitted(REF_MIL.LL4), y = residuals(REF_MIL.LL4, type = "studentised"))) +
  geom_point(pch = 16, cex = 0.7) +
  geom_smooth(method = "loess", se = FALSE, color = "blue") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "REF - LL4 - Resíduos x Ajustados", x = "Valores ajustados", 
       y = "Resíduos") +
  theme_minimal()

plot3 <- ggplot(REF, aes(sample = residuals(REF_MIL.LL4, type = "studentised"))) +
  geom_qq() +
  geom_qq_line() +
  labs(title = "LL4", x = "Quantis teóricos", y = "Resíduos") +
  theme_minimal()

dfit_plot <- grid.arrange(plot1, plot2, plot3, ncol = 3)

ggsave("Figures/Fit_REF.png", plot)

plot1 <- ggplot(C6, aes(x = fitted(C6_MIL.LL4), y = viability)) +
  geom_point(pch = 16, cex = 0.7) +
  geom_abline(intercept = 0, slope = 1) +
  labs(title = " C6 - LL4 - Ajustados x Observados", x = "Valores ajustados", 
       y = "Valores observados") +
  theme_minimal()

plot2 <- ggplot(C6, aes(x = fitted(C6_MIL.LL4), y = residuals(C6_MIL.LL4, type = "studentised"))) +
  geom_point(pch = 16, cex = 0.7) +
  geom_smooth(method = "loess", se = FALSE, color = "blue") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "C6 - LL4 - Resíduos x Ajustados", x = "Valores ajustados", 
       y = "Resíduos") +
  theme_minimal()

plot3 <- ggplot(C6, aes(sample = residuals(C6_MIL.LL4, type = "studentised"))) +
  geom_qq() +
  geom_qq_line() +
  labs(title = "LL4", x = "Quantis teóricos", y = "Resíduos") +
  theme_minimal()

dfit_plot <- grid.arrange(plot1, plot2, plot3, ncol = 3)

ggsave("Figures/Fit_C6.png", plot)

plot1 <- ggplot(C7, aes(x = fitted(C7_MIL.LL4), y = viability)) +
  geom_point(pch = 16, cex = 0.7) +
  geom_abline(intercept = 0, slope = 1) +
  labs(title = " C7 - LL4 - Ajustados x Observados", x = "Valores ajustados", 
       y = "Valores observados") +
  theme_minimal()

plot2 <- ggplot(C7, aes(x = fitted(C7_MIL.LL4), y = residuals(C7_MIL.LL4, type = "studentised"))) +
  geom_point(pch = 16, cex = 0.7) +
  geom_smooth(method = "loess", se = FALSE, color = "blue") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "C7 - LL4 - Resíduos x Ajustados", x = "Valores ajustados", 
       y = "Resíduos") +
  theme_minimal()

plot3 <- ggplot(C7, aes(sample = residuals(C7_MIL.LL4, type = "studentised"))) +
  geom_qq() +
  geom_qq_line() +
  labs(title = "LL4", x = "Quantis teóricos", y = "Resíduos") +
  theme_minimal()

dfit_plot <- grid.arrange(plot1, plot2, plot3, ncol = 3)

ggsave("Figures/Fit_C7.png", plot)

plot1 <- ggplot(C44, aes(x = fitted(C44_MIL.LL4), y = viability)) +
  geom_point(pch = 16, cex = 0.7) +
  geom_abline(intercept = 0, slope = 1) +
  labs(title = " C44 - LL4 - Ajustados x Observados", x = "Valores ajustados", 
       y = "Valores observados") +
  theme_minimal()

plot2 <- ggplot(C44, aes(x = fitted(C44_MIL.LL4), y = residuals(C44_MIL.LL4, type = "studentised"))) +
  geom_point(pch = 16, cex = 0.7) +
  geom_smooth(method = "loess", se = FALSE, color = "blue") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "C44 - LL4 - Resíduos x Ajustados", x = "Valores ajustados", 
       y = "Resíduos") +
  theme_minimal()

plot3 <- ggplot(C44, aes(sample = residuals(C44_MIL.LL4, type = "studentised"))) +
  geom_qq() +
  geom_qq_line() +
  labs(title = "LL4", x = "Quantis teóricos", y = "Resíduos") +
  theme_minimal()

dfit_plot <- grid.arrange(plot1, plot2, plot3, ncol = 3)

ggsave("Figures/Fit_C44.png", plot)

plot1 <- ggplot(C58, aes(x = fitted(C58_MIL.LL4), y = viability)) +
  geom_point(pch = 16, cex = 0.7) +
  geom_abline(intercept = 0, slope = 1) +
  labs(title = " C58 - LL4 - Ajustados x Observados", x = "Valores ajustados", 
       y = "Valores observados") +
  theme_minimal()

plot2 <- ggplot(C58, aes(x = fitted(C58_MIL.LL4), y = residuals(C58_MIL.LL4, type = "studentised"))) +
  geom_point(pch = 16, cex = 0.7) +
  geom_smooth(method = "loess", se = FALSE, color = "blue") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "C58 - LL4 - Resíduos x Ajustados", x = "Valores ajustados", 
       y = "Resíduos") +
  theme_minimal()

plot3 <- ggplot(C58, aes(sample = residuals(C58_MIL.LL4, type = "studentised"))) +
  geom_qq() +
  geom_qq_line() +
  labs(title = "LL4", x = "Quantis teóricos", y = "Resíduos") +
  theme_minimal()

dfit_plot <- grid.arrange(plot1, plot2, plot3, ncol = 3)

ggsave("Figures/Fit_C58.png", plot)

plot1 <- ggplot(C67, aes(x = fitted(C67_MIL.LL4), y = viability)) +
  geom_point(pch = 16, cex = 0.7) +
  geom_abline(intercept = 0, slope = 1) +
  labs(title = " C67 - LL4 - Ajustados x Observados", x = "Valores ajustados", 
       y = "Valores observados") +
  theme_minimal()

plot2 <- ggplot(C67, aes(x = fitted(C67_MIL.LL4), y = residuals(C67_MIL.LL4, type = "studentised"))) +
  geom_point(pch = 16, cex = 0.7) +
  geom_smooth(method = "loess", se = FALSE, color = "blue") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "C67 - LL4 - Resíduos x Ajustados", x = "Valores ajustados", 
       y = "Resíduos") +
  theme_minimal()

plot3 <- ggplot(C67, aes(sample = residuals(C67_MIL.LL4, type = "studentised"))) +
  geom_qq() +
  geom_qq_line() +
  labs(title = "LL4", x = "Quantis teóricos", y = "Resíduos") +
  theme_minimal()

dfit_plot <- grid.arrange(plot1, plot2, plot3, ncol = 3)

ggsave("Figures/Fit_C67.png", plot)


plot1 <- ggplot(C73, aes(x = fitted(C73_MIL.LL4), y = viability)) +
  geom_point(pch = 16, cex = 0.7) +
  geom_abline(intercept = 0, slope = 1) +
  labs(title = " C73 - LL4 - Ajustados x Observados", x = "Valores ajustados", 
       y = "Valores observados") +
  theme_minimal()

plot2 <- ggplot(C73, aes(x = fitted(C73_MIL.LL4), y = residuals(C73_MIL.LL4, type = "studentised"))) +
  geom_point(pch = 16, cex = 0.7) +
  geom_smooth(method = "loess", se = FALSE, color = "blue") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "C73 - LL4 - Resíduos x Ajustados", x = "Valores ajustados", 
       y = "Resíduos") +
  theme_minimal()

plot3 <- ggplot(C73, aes(sample = residuals(C73_MIL.LL4, type = "studentised"))) +
  geom_qq() +
  geom_qq_line() +
  labs(title = "LL4", x = "Quantis teóricos", y = "Resíduos") +
  theme_minimal()

dfit_plot <- grid.arrange(plot1, plot2, plot3, ncol = 3)

ggsave("Figures/Fit_C73.png", plot)

plot1 <- ggplot(C85, aes(x = fitted(C85_MIL.LL4), y = viability)) +
  geom_point(pch = 16, cex = 0.7) +
  geom_abline(intercept = 0, slope = 1) +
  labs(title = " C85 - LL4 - Ajustados x Observados", x = "Valores ajustados", 
       y = "Valores observados") +
  theme_minimal()

plot2 <- ggplot(C85, aes(x = fitted(C85_MIL.LL4), y = residuals(C85_MIL.LL4, type = "studentised"))) +
  geom_point(pch = 16, cex = 0.7) +
  geom_smooth(method = "loess", se = FALSE, color = "blue") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "C85 - LL4 - Resíduos x Ajustados", x = "Valores ajustados", 
       y = "Resíduos") +
  theme_minimal()

plot3 <- ggplot(C85, aes(sample = residuals(C85_MIL.LL4, type = "studentised"))) +
  geom_qq() +
  geom_qq_line() +
  labs(title = "LL4", x = "Quantis teóricos", y = "Resíduos") +
  theme_minimal()

dfit_plot <- grid.arrange(plot1, plot2, plot3, ncol = 3)

ggsave("Figures/Fit_C85.png", plot)

plot1 <- ggplot(C76, aes(x = fitted(C76_MIL.LL4), y = viability)) +
  geom_point(pch = 16, cex = 0.7) +
  geom_abline(intercept = 0, slope = 1) +
  labs(title = " C76 - LL4 - Ajustados x Observados", x = "Valores ajustados", 
       y = "Valores observados") +
  theme_minimal()

plot2 <- ggplot(C76, aes(x = fitted(C76_MIL.LL4), y = residuals(C76_MIL.LL4, type = "studentised"))) +
  geom_point(pch = 16, cex = 0.7) +
  geom_smooth(method = "loess", se = FALSE, color = "blue") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "C76 - LL4 - Resíduos x Ajustados", x = "Valores ajustados", 
       y = "Resíduos") +
  theme_minimal()

plot3 <- ggplot(C76, aes(sample = residuals(C76_MIL.LL4, type = "studentised"))) +
  geom_qq() +
  geom_qq_line() +
  labs(title = "LL4", x = "Quantis teóricos", y = "Resíduos") +
  theme_minimal()

dfit_plot <- grid.arrange(plot1, plot2, plot3, ncol = 3)

ggsave("Figures/Fit_C76.png", plot)

plot1 <- ggplot(C67p, aes(x = fitted(C67p_MIL.LL4), y = viability)) +
  geom_point(pch = 16, cex = 0.7) +
  geom_abline(intercept = 0, slope = 1) +
  labs(title = " C67p - LL4 - Ajustados x Observados", x = "Valores ajustados", 
       y = "Valores observados") +
  theme_minimal()

plot2 <- ggplot(C67p, aes(x = fitted(C67p_MIL.LL4), y = residuals(C67p_MIL.LL4, type = "studentised"))) +
  geom_point(pch = 16, cex = 0.7) +
  geom_smooth(method = "loess", se = FALSE, color = "blue") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "C67p - LL4 - Resíduos x Ajustados", x = "Valores ajustados", 
       y = "Resíduos") +
  theme_minimal()

plot3 <- ggplot(C67p, aes(sample = residuals(C67p_MIL.LL4, type = "studentised"))) +
  geom_qq() +
  geom_qq_line() +
  labs(title = "LL4", x = "Quantis teóricos", y = "Resíduos") +
  theme_minimal()

dfit_plot <- grid.arrange(plot1, plot2, plot3, ncol = 3)

ggsave("Figures/Fit_C67p.png", plot)

