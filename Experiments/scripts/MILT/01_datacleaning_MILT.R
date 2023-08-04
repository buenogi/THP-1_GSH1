################################################################################
###################### Infection MILT data cleaning ###########################
################################################################################


# Packages
library(dplyr)
library(scales)

# Loading data
EXP1 <- read.csv(file = "Experiments/Data/raw/sheets/EXP1_MILT_22042023.csv")
EXP2 <- read.csv(file = "Experiments/Data/raw/sheets/EXP2_MILT_28042023.csv")
EXP3 <- read.csv(file = "Experiments/Data/raw/sheets/EXP3_MILT_02052023.csv")
EXP4 <- read.csv(file = "Experiments/Data/raw/sheets/EXP4_MILT_08052023.csv")

# removing background

## Here, I'm going to consider a background every single amastigote account 
## in non infected wells. So, we are going to subtract THP-1 amastigote counts 
## for each conc from other pops counts for each conc, also. 

# EXP 1

# Selection of non infected cells accounts

ama_noninfected <- filter(EXP1, pop =="THP-1")
ama_noninfected <- subset(ama_noninfected, select = c(n_amas, conc))
EXP1 <- full_join(EXP1, ama_noninfected, by = "conc")

# Correction

corrected_ama <- c()
for (i in 1:nrow(EXP1)){
  n_amas_wtht_bckgd <- EXP1$n_amas.x[i]-EXP1$n_amas.y[i] 
  corrected_ama[i] <- n_amas_wtht_bckgd 
}
corrected_ama <- data.frame(corrected_ama)

print(corrected_ama[i] < 0)

for (i in 1:nrow(corrected_ama)) {
  for(j in 1:ncol(corrected_ama)) {
    if(corrected_ama[i,j] < 0) {
      corrected_ama[i,j] <- 0
    }
  }
} 

#EXP1$n_amas_correct <- corrected_ama
EXP1 <- cbind(EXP1, corrected_ama)

# Ama per cell calculations

ama_per_cell <- c()
for (i in 1:nrow(EXP1)){
  ama_per_cell[i] <- EXP1$corrected_ama[i]/EXP1$n_cels_infec[i] 
}

ama_per_cell <- data.frame(ama_per_cell)

EXP1 <- cbind(EXP1, ama_per_cell)
EXP1$experiment <- "EXP 1"

# EXP2

# Selection of non infected cells accounts

ama_noninfected <- filter(EXP2, pop =="THP-1")
ama_noninfected <- subset(ama_noninfected, select = c(n_amas, conc))
EXP2 <- full_join(EXP2, ama_noninfected, by = "conc")

# Correction

corrected_ama <- c()
for (i in 1:nrow(EXP2)){
  n_amas_wtht_bckgd <- EXP2$n_amas.x[i]-EXP2$n_amas.y[i] 
  corrected_ama[i] <- n_amas_wtht_bckgd 
}
corrected_ama <- data.frame(corrected_ama)

print(corrected_ama[i] < 0)

for (i in 1:nrow(corrected_ama)) {
  for(j in 1:ncol(corrected_ama)) {
    if(corrected_ama[i,j] < 0) {
      corrected_ama[i,j] <- 0
    }
  }
} 

#EXP2$n_amas_correct <- corrected_ama
EXP2 <- cbind(EXP2, corrected_ama)

# Ama per cell calculations

ama_per_cell <- c()
for (i in 1:nrow(EXP2)){
  ama_per_cell[i] <- EXP2$corrected_ama[i]/EXP2$n_cels_infec[i] 
}

ama_per_cell <- data.frame(ama_per_cell)

EXP2 <- cbind(EXP2, ama_per_cell)
EXP2$experiment <- "EXP 2"

# # Selection of non infected cells accounts
# 
# ama_noninfected <- filter(EXP2, pop =="THP-1")
# ama_noninfected <- subset(ama_noninfected, select = c(n_amas, conc))
# conc <- sort(ama_noninfected$conc, decreasing = T)
# n_amas <- data.frame("n_amas" = c(4,4,7,6,3,3))
# ama_noninfected1 <- data.frame(n_amas,conc)
# ama_noninfected <- rbind(ama_noninfected, ama_noninfected1)
# 
# while(nrow(ama_noninfected) <= 54){
#   ama_noninfected <- rbind(ama_noninfected, ama_noninfected1)
# }
# 
# ama_noninfected <- subset(ama_noninfected, select = c(n_amas))
# names(ama_noninfected) <- c("ama_noninf")
# 
# EXP2 <- cbind(EXP2,ama_noninfected)
# 
# # Correction
# 
# corrected_ama <- c()
# for (i in 1:nrow(EXP2)){
#   n_amas_wtht_bckgd <- EXP2$n_amas[i]-EXP2$ama_noninf[i] 
#   corrected_ama[i] <- n_amas_wtht_bckgd 
# }
# corrected_ama <- data.frame(corrected_ama)
# 
# print(corrected_ama[i] < 0)
# 
# for (i in 1:nrow(corrected_ama)) {
#   for(j in 1:ncol(corrected_ama)) {
#     if(corrected_ama[i,j] < 0) {
#       corrected_ama[i,j] <- 0
#     }
#   }
# } 
# 
# EXP2$n_amas_correct <- corrected_ama
# EXP2 <- cbind(EXP2, corrected_ama)
# 
# # Ama per cell calculations
# 
# ama_per_cell <- c()
# for (i in 1:nrow(EXP2)){
#   ama_per_cell[i] <- EXP2$corrected_ama[i]/EXP2$n_celulas[i] 
# }
# 
# ama_per_cell <- data.frame(ama_per_cell)
# 
# EXP2 <- cbind(EXP2, ama_per_cell)
# 
# EXP2$experiment <- "EXP 2"
# 
# EXP2 <- EXP2[,-11]



# EXP3

# Selection of non infected cells accounts

ama_noninfected <- filter(EXP3, pop =="THP-1")
ama_noninfected <- subset(ama_noninfected, select = c(n_amas, conc))
EXP3 <- full_join(EXP3, ama_noninfected, by = "conc")

#conc <- sort(ama_noninfected$conc, decreasing = T)
#n_amas <- data.frame("n_amas" = c(5,8,5,8,24,13))
#ama_noninfected1 <- data.frame(n_amas,conc)
#ama_noninfected <- rbind(ama_noninfected, ama_noninfected1)

#while(nrow(ama_noninfected) <= 54){
#  ama_noninfected <- rbind(ama_noninfected, ama_noninfected1)
#}

#ama_noninfected <- subset(ama_noninfected, select = c(n_amas))
#names(ama_noninfected) <- c("ama_noninf")

#EXP3 <- cbind(EXP3,ama_noninfected)

# Correction

corrected_ama <- c()
for (i in 1:nrow(EXP3)){
  n_amas_wtht_bckgd <- EXP3$n_amas.x[i]-EXP3$n_amas.y[i] 
  corrected_ama[i] <- n_amas_wtht_bckgd 
}
corrected_ama <- data.frame(corrected_ama)

print(corrected_ama[i] < 0)

for (i in 1:nrow(corrected_ama)) {
  for(j in 1:ncol(corrected_ama)) {
    if(corrected_ama[i,j] < 0) {
      corrected_ama[i,j] <- 0
    }
  }
} 

#EXP3$n_amas_correct <- corrected_ama
EXP3 <- cbind(EXP3, corrected_ama)

# Ama per cell calculations

ama_per_cell <- c()
for (i in 1:nrow(EXP3)){
  ama_per_cell[i] <- EXP3$corrected_ama[i]/EXP3$n_cels_infec[i] 
}

ama_per_cell <- data.frame(ama_per_cell)

EXP3 <- cbind(EXP3, ama_per_cell)
EXP3$experiment <- "EXP 3"

# EXP 4

# Selection of non infected cells accounts

ama_noninfected <- filter(EXP4, pop =="THP-1")
ama_noninfected <- subset(ama_noninfected, select = c(n_amas, conc))
EXP4 <- full_join(EXP4, ama_noninfected, by = "conc")

#conc <- sort(ama_noninfected$conc, decreasing = T)
#n_amas <- data.frame("n_amas" = c(5,8,5,8,24,13))
#ama_noninfected1 <- data.frame(n_amas,conc)
#ama_noninfected <- rbind(ama_noninfected, ama_noninfected1)

#while(nrow(ama_noninfected) <= 54){
#  ama_noninfected <- rbind(ama_noninfected, ama_noninfected1)
#}

#ama_noninfected <- subset(ama_noninfected, select = c(n_amas))
#names(ama_noninfected) <- c("ama_noninf")

#EXP4 <- cbind(EXP4,ama_noninfected)

# Correction

corrected_ama <- c()
for (i in 1:nrow(EXP4)){
  n_amas_wtht_bckgd <- EXP4$n_amas.x[i]-EXP4$n_amas.y[i] 
  corrected_ama[i] <- n_amas_wtht_bckgd 
}
corrected_ama <- data.frame(corrected_ama)

print(corrected_ama[i] < 0)

for (i in 1:nrow(corrected_ama)) {
  for(j in 1:ncol(corrected_ama)) {
    if(corrected_ama[i,j] < 0) {
      corrected_ama[i,j] <- 0
    }
  }
} 

#EXP4$n_amas_correct <- corrected_ama
EXP4 <- cbind(EXP4, corrected_ama)

# Ama per cell calculations

ama_per_cell <- c()
for (i in 1:nrow(EXP4)){
  ama_per_cell[i] <- EXP4$corrected_ama[i]/EXP4$n_cels_infec[i] 
}

ama_per_cell <- data.frame(ama_per_cell)

EXP4 <- cbind(EXP4, ama_per_cell)
EXP4$experiment <- "EXP 4"

# Completed Dataframe

EXP_full <- rbind(EXP1, EXP2, EXP3, EXP4)

write.csv(EXP_full, file = "Data/processed/EXP_full_MILT_corrected.csv")

