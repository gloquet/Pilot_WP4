# Load Packages
library(tidyverse)
library(here)
library(xlsx)
library(caret)
library(psych)
library(RColorBrewer)


# Data path
data.path <- "C:/Users/loquetg/Documents/Data/"

# Load data
data.right <- read.xlsx(paste0(data.path,"DATA_All_Norm_R_V6.xlsx"),1)
#data.left <- read.xlsx(paste0(data.path,"DATA_All_Norm_L_V6.xlsx"),1)

# Factor analysis
# Headers: PTA, DS, SRT, Region, Sex, Age, HINT_V2, HINT_bin, ACALOS_HTL, ACALOS_MCL, ACALOS_UCL, ACALOS_slope, STM,
#          IPD, BINP_diop, BINP_dichop, BIN_totp, BIN_falsep, HA, HINT_V3, IDENTIF_V3, NOISE_ANN_V3, HEAD_DIR_V3,
#          HEAD_W_V3, JFC_V3, IG55_V3, IG65_V3, IG80_V3, HINT_V4, IDENTIF_V4, NOISE_ANN_V4, DIRECTION_V4, HEAD_DIR_V4,
#          HEAD_W_V4, JFC_V4
data.right.source <- data.right %>% select(c(3,4,5,6,7,8,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37))
data.right.cor <- cor(data.right.source, use = 'complete.obs')

#now the factor analysis - make them orthogonal first

#png('Scree.png',width = 1000, height = 700)

nr.factors_right <- fa.parallel(data.right.cor, n.obs = 75, fa = 'fa')

fa_val <- data.frame(1:34,nr.factors_right$fa.values + 0.05)
names(fa_val) <- c('Factor_number','Eigenvalue')

#png('Scree_new.png',width=800, height = 500)

scree1 <- ggplot(fa_val, aes(x=Factor_number, y=Eigenvalue))+
  geom_line() +
  geom_point(color = 'red', size = 4) + 
  geom_hline(yintercept = 0, linetype='dashed',color='black') +
  scale_x_discrete(name = 'Factor number',limits=factor(1:34)) +
  theme(text = element_text(size=19),
        axis.text.x = element_text(size=19),
        axis.text.y = element_text(size=19))

#-----------plot suggests 9 components-----------------#

fa.right <- fa(data.right.cor, nfactors = 9
             , rotate = 'varimax', fm = 'pa',SMC = FALSE, n.obs = 75)

#-----------plot diagram-------------------------------#

png('Factor_plot.png', width = 1000, height = 1000)
fa.diagram(fa.right, simple = FALSE)
dev.off()

#-----------plot barplot with factors------------------#

Right_plot <- data.frame(matrix(ncol = 3, nrow = 9 * 34))
x <- c("Item", "Loading", "Factors")
colnames(Right_plot) <- x


x_axis_lab <- c('PTA','DS','SRT','Region','Sex','Age','HINT_V2','HINT_bin','ACALOS_HTL','ACALOS_MCL','ACALOS_UCL',
                'ACALOS_slope','STM','IPD','BINP_diop','BINP_dichop','BIN_totp','BIN_falsep','HA','HINT_V3',
                'IDENTIF_V3','NOISE_ANN_V3','HEAD_DIR_V3','HEAD_W_V3','JFC_V3','IG55_V3','IG65_V3','IG80_V3',
                'HINT_V4','IDENTIF_V4','NOISE_ANN_V4','DIRECTION_V4','HEAD_DIR_V4','HEAD_W_V4','JFC_V4')

colnames(data.right.source) <- x_axis_lab 


Right_plot$Item <- rep(colnames(data.right.source),9)
Right_plot$Loading <- as.vector(fa.right$Structure)
Right_plot$Factors <- c(rep('Factor 1',34),
                      rep('Factor 2',35),
                      rep('Factor 3',35),
                      rep('Factor 4',35),
                      rep('Factor 5',35),
                      rep('Factor 6',35),
                      rep('Factor 7',35),
                      rep('Factor 8',35),
                      rep('Factor 9',35))


#tiff(filename = 'PCA_SSQ.tif',width = 1600, height = 700)

ggplot(data = Right_plot,aes(x = Item, y = Loading, fill = Factors)) +
  geom_bar(stat = 'identity', position = 'stack') + 
  scale_fill_brewer(palette = 'Dark2') + 
  labs(title='Factor loadings for SSQ-12 items',subtitle = 'Cumulative variance = 79%') + 
  theme(axis.text=element_text(size=22, angle = 45, hjust = 1),
        axis.title = element_text(size=30),
        title = element_text(size=27),
        legend.text = element_text(size=27)) 


dev.off()