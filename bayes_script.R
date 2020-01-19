#
# STAN code currently takes time AND has convergence issues (indicating the model itself might need to be fixed)
# May need an alternative/simpler model to estimate that also accounts for nonlinearity/error heterogeneity
#
Longnecker <- readRDS("./Longnecker.rds")
Longnecker <- Longnecker[,colnames(Longnecker)!="albumin"] # Throw away albumin
Longnecker$center <- as.factor(Longnecker$center) # Factorize center
Longnecker[Longnecker$gestational_age > 46, "gestational_age"] <- 46 # Truncate at age == 46

# Complete case analysis for missing variables
x <- model.matrix(gestational_age ~ .-center, data = Longnecker)
miss_indices <- as.integer(row.names(x))
y <- log(Longnecker$gestational_age[miss_indices]) # Only those non-missing obs.

x_center <- attributes(scale(x[,-1]))[[3]]
x_scale <- attributes(scale(x[,-1]))[[4]] 
x <- cbind(x[,1], scale(x[,-1]))
group <- as.integer(as.factor(Longnecker$center[miss_indices])) # Index starts from 1
group_labels <- as.character(as.factor(Longnecker$center)) # Original numeric labels for centers
###
library(rstan)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)
Sys.setenv(LOCAL_CPPFLAGS = '-march=native')
###
data = list(y = y, x = x, x_scale = x_scale, group = group,
            N = length(y), P = dim(x)[2], J = length(group))
stanfit <- stan(file = "gpr_model.stan", data = data, chains = 4, iter = 2000, seed = 42)
