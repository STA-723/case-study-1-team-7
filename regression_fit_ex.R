library(rstan)
options(mc.cores = parallel::detectCores()-1)
Sys.setenv(LOCAL_CPPFLAGS = '-march=native')
#
Longnecker <- readRDS("./Longnecker.rds")
Longnecker <- Longnecker[,colnames(Longnecker)!="albumin"] # Throw away albumin
# Complete case analysis for missing variables
x <- model.matrix(gestational_age ~ . - center, data = Longnecker)
miss_indices <- as.integer(row.names(x))
y <- Longnecker$gestational_age[miss_indices]
x <- cbind(x[,1], scale(x[,-1]))
pcb <- x[,grepl("pcb", colnames(x))]; x <- x[,!grepl("pcb", colnames(x))]
group <- as.integer(as.factor(Longnecker$center[miss_indices])) # Index starts from 1
group_labels <- as.character(as.factor(Longnecker$center)) # Original numeric labels for centers
#
data = list(y = y, x = x, pcb = pcb, group = group,
            N = length(y), P = dim(x)[2], J = length(group), K = 11)
# FOR NOW, DEBUG THE CODE
stanfit <- stan(file = "gpr_model.stan", data = data, chains = 1, iter = 500, seed = 42)
