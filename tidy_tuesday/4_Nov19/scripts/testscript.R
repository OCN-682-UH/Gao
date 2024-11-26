library(tidyverse)
library(here)
library(corrr)
library(ggcorrplot)
library(FactoMineR)
library(factoextra)

# lo0ad in data ----------
tuesdata <- tidytuesdayR::tt_load(2024, week = 47)

episode_metrics <- tuesdata$episode_metrics

# learning to do PCAs
#how does avg length affect dialogue density and the number of unique words? 
# i expect that length ^ = ^ of both metrics
# clean the data ----------
norm <- scale(episode_metrics) # normalize the data (z score normalization)
head(norm)

# analysis -----------
data.pca <- princomp(norm) # now do pca analysis on normalized data
summary(data.pca) # this shows that the first PC explains ~40% of dta, the next explains about ~19% (total 59%). so doesn't explain the data super well but explains the majority 
data.pca$loadings[, 1:2] # so pc1 is highly positively correlated with season, highly negatively correlated with question and eclaimation ratio, dialogue density, and average length. So as the seasons increase, the other 3 factors decrease?
# makes sense looking at data, the s13+s14 have almost half the dialogue density and length of earlier seasons
# PC2 super highly positively correlated with unique words, negatively correlated with exlcaimation ratio. interesting! 

# plotting ------------
fviz_eig(data.pca, addlabels = TRUE) # showing how much of the dat each PC explains
fviz_pca_var(data.pca, col.var = "black") # so avg length, dialobue density, questio ratio, and sentiment variance all are positively correlated with eachother
# exclaimation ratio is negatively correlated with season? interesting
fviz_cos2(data.pca, choice = "var", axes = 1:2) # contribution of each variable. so avg length is the most well represented variable in pc1 and 2 

fviz_pca_var(data.pca, col.var = "cos2",
             gradient.cols = c("black", "orange", "green"),
             repel = TRUE) # cool! combo of biplot and attributes importance. attributes with similar cos2 scores will have similar colors
