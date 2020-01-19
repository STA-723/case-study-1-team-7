# Fitting in different linear regression models

#
# Reading in data & processing
#
Longnecker <- readRDS("./Longnecker.rds")
Longnecker <- Longnecker[,colnames(Longnecker)!="albumin"] # Throw away albumin
Longnecker$center <- as.factor(Longnecker$center) # Factorize center
Longnecker[Longnecker$gestational_age > 46, "gestational_age"] <- 46 # Truncate at age == 46

# Complete case analysis for missing variables

x <- model.matrix(gestational_age ~ . - center, data = Longnecker)
miss_indices <- as.integer(row.names(x))
Longnecker_complete <- Longnecker[miss_indices,] # Only those non-missing obs.
# 
# 1. PCA => Regression
pc_dataset <- cbind.data.frame(y = Longnecker$gestational_age[miss_indices], 
                               pc1 = prcomp(x)$x[,1],
                               pc2 = prcomp(x)$x[,2])
lm_pconly <- lm(y ~ ., data = pc_dataset)

# First & Second PC's are primarly linear combinations of CHOLESTEROL and TRIGRLYCERIDES.
# If we choose to include them in the model, those two variables will have to go away.

# What about just taking the linear combination of PCB's?
PC1_pcb <- prcomp(x[,grepl("pcb", colnames(x))])$x[,1] # ~60% of variability in the column space of X
Longnecker_complete_pc <- cbind.data.frame(Longnecker_complete[,!grepl("pcb", colnames(Longnecker_complete))],
                                           PC1 = PC1_pcb)
lm_with_pc <- lm(gestational_age ~ ., data = Longnecker_complete_pc)

# 2. Logistic regression 
Longnecker_complete_pc$early <- (Longnecker_complete_pc$gestational_age < 37)
llm_with_pc <- glm(early ~ . - gestational_age, family = binomial, data = Longnecker_complete_pc)
est_probs <- predict(llm_with_pc, type = "response") # Estimated probabilities
boxplot(est_probs ~ Longnecker_complete_pc$early) # Classification 

# 3. Linear mixed effects model
library(lme4)
lmm_with_pc <- lmer(gestational_age ~ . - early - center + (1|center), data = Longnecker_complete_pc)
llmm_with_pc <- glmer(early ~ . - gestational_age - center + (1|center), 
                      family = binomial, data = Longnecker_complete_pc)
## logistic mixed effects model has CONVERGENCE ISSUES; estimates are probably not reliable

# Log transform of response/covariates do not seem to improve the model fit (NOT shown here)
# 1. IF the association is nonlinear, do we have any tools to model this? (HARD)
# 2. Is there a systematic (could be heuristic) way to decide which variables are "confounders"? (COULD BE DONE)
