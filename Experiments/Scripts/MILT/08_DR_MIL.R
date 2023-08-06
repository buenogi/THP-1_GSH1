################################################################################
################## MILT amastigote Dose-response analisys #####################
################################################################################

library(dplyr)
library(ggplot2) 
library(drc)

# Loading data 

DataMIL<- read.csv(file = "Experiments/Data/processed/DataMIL_processed_normalized.csv", 
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

# Adjusting the models

REF_SbIII.LL4 <- drm(viability_normalized ~ conc, data = REF , fct = LL.4(
  fixed = c(NA, 0,100, NA), names = c("b","c", "d", "e")))
plot(REF_SbIII.LL4, xlim = c(0,1000), ylim = c(0,100), 
     broken = TRUE, gridsize = 100, xlab = "SbIII [  ] μM", 
     ylab = "Viability (%)", type = "bars")
summary(REF_SbIII.LL4)

C6_SbIII.LL4 <- drm(viability_normalized ~ conc, data = C6 , fct = LL.4(
  fixed = c(NA,0,100, NA), names = c("b","c", "d", "e")))
plot(C6_SbIII.LL4, xlim = c(0,1000), ylim = c(0,100), 
     broken = TRUE, gridsize = 100, xlab = "SbIII [  ] μM", 
     ylab = "Viability (%)", type = "bars")

C7_SbIII.LL4 <- drm(viability_normalized ~ conc, data = C7 , fct = LL.4(
  fixed = c(NA, 0,100, NA), names = c("b","c", "d", "e")))
plot(C7_SbIII.LL4, xlim = c(0,1000), ylim = c(0,100), 
     broken = TRUE, gridsize = 100, xlab = "SbIII [  ] μM", 
     ylab = "Viability (%)", type = "bars")

C44_SbIII.LL4 <- drm(viability_normalized ~ conc, data = C44 , fct = LL.4(
  fixed = c(NA, 0,100, NA), names = c("b","c", "d", "e")))
plot(C44_SbIII.LL4, xlim = c(0,1000), ylim = c(0,100), 
     broken = TRUE, gridsize = 100, xlab = "SbIII [  ] μM", 
     ylab = "Viability (%)", type = "bars")

C58_SbIII.LL4 <- drm(viability_normalized ~ conc, data = C58 , fct = LL.4(
  fixed = c(NA, 0,100, NA), names = c("b","c", "d", "e")))
plot(C58_SbIII.LL4, xlim = c(0,1000), ylim = c(0,100), 
     broken = TRUE, gridsize = 100, xlab = "SbIII [  ] μM", 
     ylab = "Viability (%)", type = "bars")

C67_SbIII.LL4 <- drm(viability_normalized ~ conc, data = C67 , fct = LL.4(
  fixed = c(NA,0,100, NA), names = c("b","c", "d", "e")))
plot(C67_SbIII.LL4, xlim = c(0,1000), ylim = c(0,100), 
     broken = TRUE, gridsize = 100, xlab = "SbIII [  ] μM", 
     ylab = "Viability (%)", type = "bars")

C73_SbIII.LL4 <- drm(viability_normalized ~ conc, data = C73 , fct = LL.4(
  fixed = c(NA, 0,100, NA), names = c("b","c", "d", "e")))
plot(C73_SbIII.LL4, xlim = c(0,1000), ylim = c(0,100), 
     broken = TRUE, gridsize = 100, xlab = "SbIII [  ] μM", 
     ylab = "Viability (%)", type = "bars")

C85_SbIII.LL4 <- drm(viability_normalized ~ conc, data = C85 , fct = LL.4(
  fixed = c(NA,0,100, NA), names = c("b","c", "d", "e")))
plot(C85_SbIII.LL4, xlim = c(0,1000), ylim = c(0,100), 
     broken = TRUE, gridsize = 100, xlab = "SbIII [  ] μM", 
     ylab = "Viability (%)", type = "bars")

# ED50

ED(REF_SbIII.LL4,50)
ED(C6_SbIII.LL4,50)
ED(C7_SbIII.LL4,50)
ED(C44_SbIII.LL4,50)
ED(C58_SbIII.LL4,50)
ED(C67_SbIII.LL4,50)
ED(C73_SbIII.LL4,50)
ED(C85_SbIII.LL4,50)

## Exploring predicted model
newdata_REF <- expand.grid(conc = seq(min(REF$conc), max(REF$conc), length.out = 200))
newdata_REF$viability_normalized <- predict(REF_SbIII.LL4, newdata_REF, type = 'response')
newdata_REF$pop <- "REF"

newdata_C6 <- expand.grid(conc = seq(min(C6$conc), max(C6$conc),
                                     length.out = 200))
newdata_C6$viability_normalized <- predict(C6_SbIII.LL4, newdata_C6, type = 'response')
newdata_C6$pop <- "C6"

newdata_C7 <- expand.grid(conc = seq(min(C7$conc), max(C7$conc),
                                     length.out = 200))
newdata_C7$viability_normalized <- predict(C7_SbIII.LL4, newdata_C7, type = 'response')
newdata_C7$pop <- "C7"

newdata_C44 <- expand.grid(conc = seq(min(C44$conc), max(C44$conc),
                                      length.out = 200))
newdata_C44$viability_normalized <- predict(C44_SbIII.LL4, newdata_C44, type = 'response')
newdata_C44$pop <- "C44"

newdata_C58 <- expand.grid(conc = seq(min(C58$conc), max(C58$conc),
                                      length.out = 200))
newdata_C58$viability_normalized <- predict(C58_SbIII.LL4, newdata_C58, type = 'response')
newdata_C58$pop <- "C58"

newdata_C67 <- expand.grid(conc = seq(min(C67$conc), max(C67$conc),
                                      length.out = 200))
newdata_C67$viability_normalized <- predict(C67_SbIII.LL4, newdata_C67, type = 'response')
newdata_C67$pop <- "C67"

newdata_C73 <- expand.grid(conc = seq(min(C73$conc), max(C73$conc),
                                      length.out = 200))
newdata_C73$viability_normalized <- predict(C73_SbIII.LL4, newdata_C73, type = 'response')
newdata_C73$pop <- "C73"

newdata_C85 <- expand.grid(conc = seq(min(C85$conc), max(C85$conc),
                                      length.out = 200))
newdata_C85$viability_normalized <- predict(C85_SbIII.LL4, newdata_C85, type = 'response')
newdata_C85$pop <- "C85"

MTT_SUM_full <- full_join(REF, C6)
MTT_SUM_full <- full_join(MTT_SUM_full, C7)
MTT_SUM_full <- full_join(MTT_SUM_full, C44)
MTT_SUM_full <- full_join(MTT_SUM_full, C58)
MTT_SUM_full <- full_join(MTT_SUM_full, C67)
MTT_SUM_full <- full_join(MTT_SUM_full, C73)
MTT_SUM_full <- full_join(MTT_SUM_full, C85)


newdata_full <- rbind.data.frame(newdata_REF, newdata_C6)
newdata_full <- rbind(newdata_full, newdata_C7)
newdata_full <- rbind(newdata_full, newdata_C44)
newdata_full <- rbind(newdata_full, newdata_C58)
newdata_full <- rbind(newdata_full, newdata_C67)
newdata_full <- rbind(newdata_full, newdata_C73)
newdata_full <- rbind(newdata_full, newdata_C85)


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
  ggtitle("Amastigotes dose response to MIL") +
  labs(x = "Log10 [ ] μM", y = "Infection (%)") +
  theme_bw() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15)
  ) +
  facet_wrap(~pop)

DoseResponseCurves_01

DoseResponseCurves02_error <- ggplot() +
  geom_point(data = REF, aes(x = log(conc, 10), y = viability_normalized)) +
  geom_line(data = newdata_REF, aes(x = log(conc, 10), y = newdata_REF$viability_normalized)) +
  geom_errorbar(data = REF, aes(x = log(conc, 10),
                                ymin = mean_value_res - sd_value_res/sqrt(4),
                                ymax = mean_value_res + sd_value_res/sqrt(4)), width = 0.2) +
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
  ggtitle("Amastigotes dose response to MIL") +
  labs(x = "Log10 [ ] μM", y = "Infection (%)") +
  theme_bw() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15))
#facet_wrap(~pop)

DoseResponseCurves02_error

DoseResponseCurves03_error <- ggplot() +
  geom_point(data = REF, aes(x = log(conc, 10), y = mean_value_res, color = pop)) +
  geom_line(data = newdata_REF, aes(x = log(conc, 10), y = viability_normalized, color = pop)) +
  geom_errorbar(data = REF, aes(x = log(conc, 10),
                                ymin = mean_value_res - sd_value_res/sqrt(4),
                                ymax = mean_value_res + sd_value_res/sqrt(4)), width = 0.02, alpha = 0.3) +
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
  ggtitle("Amastigotes dose response to MIL") +
  labs(x = "Log10 [ ] μM", y = "Infection(%)") +
  theme_bw() +
  theme(    plot.title = element_text(size = 14, face = "bold"),
            axis.text.x = element_text(size = 10),
            axis.text.y = element_text(size = 10),
            axis.title.x = element_text(size = 15),
            axis.title.y = element_text(size = 15))
#facet_wrap(~pop)

DoseResponseCurves03_error



# Extracting measures



COEFICIENTS <- c("b",
                 #"c", "d",
                 "e")
modelREF <- summary(REF_SbIII.LL4)
confintREF <- confint(REF_SbIII.LL4)
modelREF_coef <- as.data.frame(modelREF$coefficients)
modelREF_conf <- as.data.frame(confintREF)
modelREF_SUM <- cbind(modelREF_coef, modelREF_conf)
modelREF_SUM$coef <- COEFICIENTS 
modelREF_SUM$pop <- rep("REF", 4)

modelC6 <- summary(C6_SbIII.LL4)
confintC6 <- confint(C6_SbIII.LL4)
modelC6_coef <- as.data.frame(modelC6$coefficients)
modelC6_conf <- as.data.frame(confintC6)
modelC6_SUM <- cbind(modelC6_coef, modelC6_conf)
modelC6_SUM$coef <- COEFICIENTS 
modelC6_SUM$pop <- rep("C6", 2)

modelC7 <- summary(C7_SbIII.LL4)
confintC7 <- confint(C7_SbIII.LL4)
modelC7_coef <- as.data.frame(modelC7$coefficients)
modelC7_conf <- as.data.frame(confintC7)
modelC7_SUM <- cbind(modelC7_coef, modelC7_conf)
modelC7_SUM$coef <- COEFICIENTS 
modelC7_SUM$pop <- rep("C7", 2)

modelC44 <- summary(C44_SbIII.LL4)
confintC44 <- confint(C44_SbIII.LL4)
modelC44_coef <- as.data.frame(modelC44$coefficients)
modelC44_conf <- as.data.frame(confintC44)
modelC44_SUM <- cbind(modelC44_coef, modelC44_conf)
modelC44_SUM$coef <- COEFICIENTS 
modelC44_SUM$pop <- rep("C44", 2)

modelC58 <- summary(C58_SbIII.LL4)
confintC58 <- confint(C58_SbIII.LL4)
modelC58_coef <- as.data.frame(modelC58$coefficients)
modelC58_conf <- as.data.frame(confintC58)
modelC58_SUM <- cbind(modelC58_coef, modelC58_conf)
modelC58_SUM$coef <- COEFICIENTS 
modelC58_SUM$pop <- rep("C58", 2)

modelC67 <- summary(C67_SbIII.LL4)
confintC67 <- confint(C67_SbIII.LL4)
modelC67_coef <- as.data.frame(modelC67$coefficients)
modelC67_conf <- as.data.frame(confintC67)
modelC67_SUM <- cbind(modelC67_coef, modelC67_conf)
modelC67_SUM$coef <- COEFICIENTS 
modelC67_SUM$pop <- rep("C67", 2)

modelC73 <- summary(C73_SbIII.LL4)
confintC73 <- confint(C73_SbIII.LL4)
modelC73_coef <- as.data.frame(modelC73$coefficients)
modelC73_conf <- as.data.frame(confintC73)
modelC73_SUM <- cbind(modelC73_coef, modelC73_conf)
modelC73_SUM$coef <- COEFICIENTS 
modelC73_SUM$pop <- rep("C73", 2)

modelC85 <- summary(C85_SbIII.LL4)
confintC85 <- confint(C85_SbIII.LL4)
modelC85_coef <- as.data.frame(modelC85$coefficients)
modelC85_conf <- as.data.frame(confintC85)
modelC85_SUM <- cbind(modelC85_coef, modelC85_conf)
modelC85_SUM$coef <- COEFICIENTS 
modelC85_SUM$pop <- rep("C85", 2)

summary_DR <- rbind(modelREF_SUM, modelC7_SUM, modelC6_SUM, modelC44_SUM,
                    modelC58_SUM, modelC67_SUM, modelC73_SUM, modelC85_SUM)

write.csv(summary_DR , file = "Experiments/docs/summary_DR_MIL.csv", row.names = FALSE)


#  Fit diagnostic

par(mfrow=c(1,3), cex.lab=1.2)
plot(REF$viability, fitted(REF_SbIII.LL4), main="LL4", 
     xlab ="Adjusted values", ylab="Observed values", pch=16, cex=.7)
abline(a = 0,b = 1)
residuals(REF_SbIII.LL4, typeRes = c("studentised"))
plot(fitted(REF_SbIII.LL4),residuals(REF_SbIII.LL4, 
                                     typeRes = c("studentised")), 
     main="LL4", xlab="Adjusted values", ylab="Residuals", pch=16, cex=.7)
lines(loess.smooth(fitted(REF_SbIII.LL4), 
                   residuals(REF_SbIII.LL4, typeRes = c("studentised"))))
abline(h=0, lty=2)
qqnorm(residuals(REF_SbIII.LL4), xlab="Theoretical quantiles", 
       ylab="Residuals", main = "LL4")
qqline(residuals(REF_SbIII.LL4))

png("Figures/03_Fit_diagnostic_REF.png",  width = 6, height = 4, units = "in", res = 300)



par(mfrow=c(1,3), cex.lab=1.2)
plot(C6$viability, fitted(C6_SbIII.LL4), main="LL4", 
     xlab ="Adjusted values", ylab="Observed values", pch=16, cex=.7)
abline(a = 0,b = 1)
residuals(C6_SbIII.LL4, typeRes = c("studentised"))
plot(fitted(C6_SbIII.LL4),residuals(C6_SbIII.LL4, 
                                    typeRes = c("studentised")), 
     main="LL4", xlab="Adjusted values", ylab="Residuals", pch=16, cex=.7)
lines(loess.smooth(fitted(C6_SbIII.LL4), 
                   residuals(C6_SbIII.LL4, typeRes = c("studentised"))))
abline(h=0, lty=2)
qqnorm(residuals(C6_SbIII.LL4), xlab="Theoretical quantiles", 
       ylab="Residuals", main = "LL4")
qqline(residuals(C6_SbIII.LL4))

png("Figures/03_Fit_diagnostic_C6.png",  width = 6, height = 4, units = "in", res = 300)


par(mfrow=c(1,3), cex.lab=1.2)
plot(C7$viability, fitted(C7_SbIII.LL4), main="LL4", 
     xlab ="Adjusted values", ylab="Observed values", pch=16, cex=.7)
abline(a = 0,b = 1)
residuals(C7_SbIII.LL4, typeRes = c("studentised"))
plot(fitted(C7_SbIII.LL4),residuals(C7_SbIII.LL4, 
                                    typeRes = c("studentised")), 
     main="LL4", xlab="Adjusted values", ylab="Residuals", pch=16, cex=.7)
lines(loess.smooth(fitted(C7_SbIII.LL4), 
                   residuals(C7_SbIII.LL4, typeRes = c("studentised"))))
abline(h=0, lty=2)
qqnorm(residuals(C7_SbIII.LL4), xlab="Theoretical quantiles", 
       ylab="Residuals", main = "LL4")
qqline(residuals(C7_SbIII.LL4))

png("Figures/03_Fit_diagnostic_C7.png",  width = 6, height = 4, units = "in", res = 300)


par(mfrow=c(1,3), cex.lab=1.2)
plot(C44$viability, fitted(C44_SbIII.LL4), main="LL4", 
     xlab ="Adjusted values", ylab="Observed values", pch=16, cex=.7)
abline(a = 0,b = 1)
residuals(C44_SbIII.LL4, typeRes = c("studentised"))
plot(fitted(C44_SbIII.LL4),residuals(C44_SbIII.LL4, 
                                     typeRes = c("studentised")), 
     main="LL4", xlab="Adjusted values", ylab="Residuals", pch=16, cex=.7)
lines(loess.smooth(fitted(C44_SbIII.LL4), 
                   residuals(C44_SbIII.LL4, typeRes = c("studentised"))))
abline(h=0, lty=2)
qqnorm(residuals(C44_SbIII.LL4), xlab="Theoretical quantiles", 
       ylab="Residuals", main = "LL4")
qqline(residuals(C44_SbIII.LL4))

png("Figures/03_Fit_diagnostic_C44.png",  width = 6, height = 4, units = "in", res = 300)


par(mfrow=c(1,3), cex.lab=1.2)
plot(C58$viability, fitted(C58_SbIII.LL4), main="LL4", 
     xlab ="Adjusted values", ylab="Observed values", pch=16, cex=.7)
abline(a = 0,b = 1)
residuals(C58_SbIII.LL4, typeRes = c("studentised"))
plot(fitted(C58_SbIII.LL4),residuals(C58_SbIII.LL4, 
                                     typeRes = c("studentised")), 
     main="LL4", xlab="Adjusted values", ylab="Residuals", pch=16, cex=.7)
lines(loess.smooth(fitted(C58_SbIII.LL4), 
                   residuals(C58_SbIII.LL4, typeRes = c("studentised"))))
abline(h=0, lty=2)
qqnorm(residuals(C58_SbIII.LL4), xlab="Theoretical quantiles", 
       ylab="Residuals", main = "LL4")
qqline(residuals(C58_SbIII.LL4))

png("Figures/03_Fit_diagnostic_C58.png",  width = 6, height = 4, units = "in", res = 300)



par(mfrow=c(1,3), cex.lab=1.2)
plot(GSH1$viability, fitted(GSH1_SbIII.LL4), main="LL4", 
     xlab ="Adjusted values", ylab="Observed values", pch=16, cex=.7)
abline(a = 0,b = 1)
residuals(GSH1_SbIII.LL4, typeRes = c("studentised"))
plot(fitted(GSH1_SbIII.LL4),residuals(GSH1_SbIII.LL4, 
                                      typeRes = c("studentised")), 
     main="LL4", xlab="Adjusted values", ylab="Residuals", pch=16, cex=.7)
lines(loess.smooth(fitted(GSH1_SbIII.LL4), 
                   residuals(GSH1_SbIII.LL4, typeRes = c("studentised"))))
abline(h=0, lty=2)
qqnorm(residuals(GSH1_SbIII.LL4), xlab="Theoretical quantiles", 
       ylab="Residuals", main = "LL4")
qqline(residuals(GSH1_SbIII.LL4))

png("Figures/03_Fit_diagnostic_GSH1.png",  width = 6, height = 4, units = "in", res = 300)



par(mfrow=c(1,3), cex.lab=1.2)
plot(C67$viability, fitted(C67_SbIII.LL4), main="LL4", 
     xlab ="Adjusted values", ylab="Observed values", pch=16, cex=.7)
abline(a = 0,b = 1)
residuals(C67_SbIII.LL4, typeRes = c("studentised"))
plot(fitted(C67_SbIII.LL4),residuals(C67_SbIII.LL4, 
                                     typeRes = c("studentised")), 
     main="LL4", xlab="Adjusted values", ylab="Residuals", pch=16, cex=.7)
lines(loess.smooth(fitted(C67_SbIII.LL4), 
                   residuals(C67_SbIII.LL4, typeRes = c("studentised"))))
abline(h=0, lty=2)
qqnorm(residuals(C67_SbIII.LL4), xlab="Theoretical quantiles", 
       ylab="Residuals", main = "LL4")
qqline(residuals(C67_SbIII.LL4))

png("Figures/03_Fit_diagnostic_C67.png",  width = 6, height = 4, units = "in", res = 300)


par(mfrow=c(1,3), cex.lab=1.2)
plot(C73$viability, fitted(C73_SbIII.LL4), main="LL4", 
     xlab ="Adjusted values", ylab="Observed values", pch=16, cex=.7)
abline(a = 0,b = 1)
residuals(C73_SbIII.LL4, typeRes = c("studentised"))
plot(fitted(C73_SbIII.LL4),residuals(C73_SbIII.LL4, 
                                     typeRes = c("studentised")), 
     main="LL4", xlab="Adjusted values", ylab="Residuals", pch=16, cex=.7)
lines(loess.smooth(fitted(C73_SbIII.LL4), 
                   residuals(C73_SbIII.LL4, typeRes = c("studentised"))))
abline(h=0, lty=2)
qqnorm(residuals(C73_SbIII.LL4), xlab="Theoretical quantiles", 
       ylab="Residuals", main = "LL4")
qqline(residuals(C73_SbIII.LL4))

png("Figures/03_Fit_diagnostic_C73.png",  width = 6, height = 4, units = "in", res = 300)


par(mfrow=c(1,3), cex.lab=1.2)
plot(C44$viability, fitted(C44_SbIII.LL4), main="LL4", 
     xlab ="Adjusted values", ylab="Observed values", pch=16, cex=.7)
abline(a = 0,b = 1)
residuals(C44_SbIII.LL4, typeRes = c("studentised"))
plot(fitted(C44_SbIII.LL4),residuals(C44_SbIII.LL4, 
                                     typeRes = c("studentised")), 
     main="LL4", xlab="Adjusted values", ylab="Residuals", pch=16, cex=.7)
lines(loess.smooth(fitted(C44_SbIII.LL4), 
                   residuals(C44_SbIII.LL4, typeRes = c("studentised"))))
abline(h=0, lty=2)
qqnorm(residuals(C44_SbIII.LL4), xlab="Theoretical quantiles", 
       ylab="Residuals", main = "LL4")
qqline(residuals(C44_SbIII.LL4))

png("Figures/03_Fit_diagnostic_C44.png",  width = 6, height = 4, units = "in", res = 300)


par(mfrow=c(1,3), cex.lab=1.2)
plot(C85$viability, fitted(C85_SbIII.LL4), main="LL4", 
     xlab ="Adjusted values", ylab="Observed values", pch=16, cex=.7)
abline(a = 0,b = 1)
residuals(C85_SbIII.LL4, typeRes = c("studentised"))
plot(fitted(C85_SbIII.LL4),residuals(C85_SbIII.LL4, 
                                     typeRes = c("studentised")), 
     main="LL4", xlab="Adjusted values", ylab="Residuals", pch=16, cex=.7)
lines(loess.smooth(fitted(C85_SbIII.LL4), 
                   residuals(C85_SbIII.LL4, typeRes = c("studentised"))))
abline(h=0, lty=2)
qqnorm(residuals(C85_SbIII.LL4), xlab="Theoretical quantiles", 
       ylab="Residuals", main = "LL4")
qqline(residuals(C85_SbIII.LL4))

png("Figures/03_Fit_diagnostic_C85.png",  width = 6, height = 4, units = "in", res = 300)

par(mfrow=c(1,3), cex.lab=1.2)
plot(C89$viability, fitted(C89_SbIII.LL4), main="LL4", 
     xlab ="Adjusted values", ylab="Observed values", pch=16, cex=.7)
abline(a = 0,b = 1)
residuals(C89_SbIII.LL4, typeRes = c("studentised"))
plot(fitted(C89_SbIII.LL4),residuals(C89_SbIII.LL4, 
                                     typeRes = c("studentised")), 
     main="LL4", xlab="Adjusted values", ylab="Residuals", pch=16, cex=.7)
lines(loess.smooth(fitted(C89_SbIII.LL4), 
                   residuals(C89_SbIII.LL4, typeRes = c("studentised"))))
abline(h=0, lty=2)
qqnorm(residuals(C89_SbIII.LL4), xlab="Theoretical quantiles", 
       ylab="Residuals", main = "LL4")
qqline(residuals(C89_SbIII.LL4))

png("Figures/03_Fit_diagnostic_C89.png",  width = 6, height = 4, units = "in", res = 300)



