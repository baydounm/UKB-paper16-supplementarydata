library(poLCA)
library(dplyr)
library(ggplot2)
library(data.table)

# Load data
dat <- fread('c:/users/weissj/desktop/may_20240722.csv')

# Create the formula
formula <- as.formula(paste("cbind(", paste(names(dat)[-1], collapse = ", "), ") ~ 1"))

# Initialize a list to store BIC values and models
bic_values <- numeric(9)
models <- list()

# Fit the models and store BIC values
for (k in 1:9) {
  model <- poLCA(formula, data = dat, nclass = k)
  models[[k]] <- model
  bic_values[k] <- model$bic
}

# Get the model with the maximum number of classes (nclass = 9)
l9 <- models[[9]]

# Get predicted probabilities
predicted_probs <- l9$posterior

# Combine predicted probabilities with original data
dat_with_probs <- cbind(dat, predicted_probs)

# Determine predicted class for each observation
dat_with_probs$predicted_class <- apply(predicted_probs, 1, which.max)

# Select relevant columns
selected_cols <- c("n_eid", names(dat_with_probs)[grep("V\\d+|class\\d+_log_odds", names(dat_with_probs))])
tmp <- dat_with_probs %>% select(all_of(selected_cols))

# Calculate log odds for each predicted probability
log_odds <- log(predicted_probs / (1 - predicted_probs))

# Add the log odds to the dataset
colnames(log_odds) <- paste0("class", 1:ncol(log_odds), "_log_odds")
dat_with_probs <- cbind(dat_with_probs, log_odds)

# Print BIC values for reference
print(bic_values)
