# Helps to make calculations faster

# install.packages("doParallel")
library(doParallel)

cores <- parallel::detectCores()  # Check available cores
cl <- makeCluster(cores - 2)     # Use all but 2 cores
registerDoParallel(cl)

# allowParallel = TRUE  # Enable parallelism

# stopCluster(cl)
