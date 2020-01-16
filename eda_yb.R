#
# EDA
#
library(ggplot2)
library(reshape2)
Longnecker <- readRDS("./Longnecker.rds")
Longnecker <- Longnecker[,colnames(Longnecker)!="albumin"] # Throw away albumin
#
# Correlation plot for covariates
corMat <- cor(Longnecker[,!(colnames(Longnecker) %in% c("race", "center"))],
              use = "na.or.complete", method = "spearman")
corMat[upper.tri(corMat)] <- NA
corDat <- melt(corMat)
ggplot(corDat, aes(x=Var1, y=Var2)) + 
  geom_tile(aes(fill = value)) + 
  scale_fill_distiller(palette = "OrRd", direction=1)
#
# Idea: Try out PCA
# Let's regress the response on the 1st PC INSTEAD OF all PCB's
# Log-transformation of response seems to improve the model fit
#
X <- model.matrix(gestational_age ~ . - center, data = Longnecker)
miss_indices <- as.integer(row.names(X)) # Throw out observations that have missing values
prX <- prcomp(X[,-1]) # PCA

## New data frame for fitting regression with primary principal component
data_lm <- cbind.data.frame(
      gestational_age = Longnecker$gestational_age[miss_indices],
      X[,!grepl("pcb", colnames(X))], # Exclude all PCB's 
      pc1 = X[,-1] %*% prX$rotation[,1], # First Principal component
      center = as.factor(Longnecker$center[miss_indices]))
data_lm[data_lm$gestational_age > 46, "gestational_age"] <- 46 # Truncate at age == 46
lm_fit <- lm(log(gestational_age) ~ . - 1, data = data_lm)

# Surprisingly R-squared must be taken with a LOT OF GRAIN OF SALT
# More interesting question will be: Why such a drastic improvement???
# PCA might actually be a good way to answer the research question here

