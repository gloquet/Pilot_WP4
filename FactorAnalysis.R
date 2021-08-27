# Load Packages
library(tidyverse)
library(here)
library(xlsx)
library(caret)
library(psych)


# Data path
data.path <- "C:/Users/loquetg/Documents/Data/"

# Load data
data.right <- read.xlsx(paste0(data.path,"DATA_All_Norm_R_V6.xlsx"),1)
#data.left <- read.xlsx(paste0(data.path,"DATA_All_Norm_L_V6.xlsx"),1)

# Factor analysis
# Headers: PTA, DS, SRT, Region, Sex, Age, HINT_V2, HINT_bin, ACALOS_HTL, ACALOS_MCL, ACALOS_UCL, ACALOS_slope, STM,
#          IPD, BINP_diop, BINP_dichop, BIN_totp, BIN_falsep, HA, HINT_V3, IDENTIF_V3, NOISE_ANN_V3, DIRECTION_V3,
#          HEAD_DIR_V3, HEAD_W_V3, JFC_V3, IG55_V3, IG65_V3, IG80_V3, HINT_V4, IDENTIF_V4, NOISE_ANN_V4, DIRECTION_V4,
#          HEAD_DIR_V4, HEAD_W_V4, JFC_V4
data.right.source <- data.right %>% select(c(3,4,5,6,7,8,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37))
data.right.cor <- cor(data.right.source, use = 'complete.obs')

#now the factor analysis - make them orthogonal first

#png('Scree.png',width = 1000, height = 700)

nr.factors_right <- fa.parallel(data.right.cor, n.obs = 75, fa = 'fa')
nr.factors