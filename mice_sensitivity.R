# How much does the analysis change
# when we impute score variables vs. not?
# (Assuming we have included score variables in the full regression)
Longnecker <- readRDS("./Longnecker.rds")
Longnecker <- Longnecker[,colnames(Longnecker)!="albumin"] # Throw away albumin
Longnecker$center <- as.factor(Longnecker$center) # Factorize center
Longnecker[Longnecker$gestational_age > 46, "gestational_age"] <- 46 # Truncate at age == 46

# MICE imputation 
library(mice)
miced_data <- mice(Longnecker[-1861,]) # I don't want to impute missing PCB's (they're 1 anyways)
Longnecker_miced <- Longnecker[-1861,]
Longnecker_miced$score_education[is.na(Longnecker_miced$score_education)] <- rowMeans(miced_data$imp$score_education)
Longnecker_miced$score_occupation[is.na(Longnecker_miced$score_occupation)] <- rowMeans(miced_data$imp$score_occupation)
Longnecker_miced$score_income[is.na(Longnecker_miced$score_income)] <- rowMeans(miced_data$imp$score_income)

# Let's take and compare that model that includes principal component...
complete_formula <- as.formula(
  "gestational_age ~ dde + pcb_028 + pcb_052 + pcb_074 + pcb_105 + 
  pcb_118 + pcb_153 + pcb_170 + pcb_138 + pcb_180 + pcb_194 + pcb_203 + 
  triglycerides + race + maternal_age + smoking_status + cholesterol + 
  score_education + score_income + score_occupation + center"
)
x <- model.matrix(complete_formula, data = Longnecker)
miss_indices <- as.integer(row.names(x))
Longnecker_complete <- Longnecker[miss_indices,] # Only those non-missing obs.
PC1_pcb <- prcomp(x[,grepl("pcb", colnames(x))], scale.=TRUE)$x[,1]

Longnecker_complete_pc <- cbind.data.frame(
  Longnecker_complete[,!grepl("pcb", colnames(Longnecker_complete))],
  PC1 = PC1_pcb)

Longnecker_complete_pc$early <- (Longnecker_complete_pc$gestational_age < 37)
Longnecker_miced <- cbind.data.frame(Longnecker_miced[,!grepl("pcb", colnames(Longnecker_miced))],
                                     PC1 = PC1_pcb)
Longnecker_miced$early <- (Longnecker_miced$gestational_age < 37)

# 1. Complete-case analysis
llm_complete <- glm(early ~ . - gestational_age,
                    family = binomial,
                    data = Longnecker_complete_pc)
llm_miced <- glm(early ~ . - gestational_age,
                 family = binomial,
                 data = Longnecker_miced)
