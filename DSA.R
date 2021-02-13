library(tidyverse)
library(skimr)
library(yardstick)

# Load data into tibble, and separate current model results for performance measurement
campaign <- read_csv("DSA Data Set.csv")
ModelPrediction <- tibble(prediction = campaign$ModelPrediction * 100, actual = campaign$y)

# Create thresholds list of 0% - 100% by 1%
thresholds <- tibble(threshold = seq(0, 100, by = 10))

# Visualize predictor bins by 10% in a stacked histogram
ggplot(ModelPrediction) + 
  geom_histogram(aes(prediction, color = actual, fill = actual), 
                 breaks = seq(0, 100, by = 10), alpha = .3, position = "identity") +
  theme_minimal()

## FINDINGS
# Subscribers are uniformly distributed between prediction ranges 50% - 100%
# Non-Subscribers are distributed geometrically, and are seen mostly in ranges 80% - 100%
# Samples with predictions of 40% - 80% are mostly subscribers

## CONCLUSION
# The current model predicts subscribers more accurately between ranges 40% - 80%
# There may be room for improvement of accuracy, especially for ranges 80% - 100%

# Create new function to convert numeric list to a binary variable
numeric_to_binary <- function (threshold, num_list) {
  tibble(outcome = if_else(num_list > threshold, "no", "yes"))
}

# Add tibble column with outcomes using each threshold (1 tibble per row)
results <- thresholds %>% 
  mutate(outcome = map(threshold, numeric_to_binary, num_list = ModelPrediction$prediction))

test_pred <- results$outcome[8][[1]]$outcome


sum(map2_lgl(test_pred, campaign$y, ~ .x == "yes" & .y == "yes"))

x <- results %>% mutate(TP = sum(map2_lgl(outcome[[1]]$outcome, campaign$y, ~ .x == "yes" & .y == "yes")))

# y <- tibble(outcome = x[8, 2]$outcome[[1]])
# y %>% group_by(outcome) %>% count()

x %>% mutate(sensitivty = )
sum(x$outcome[[99]] == campaign$y)
map(ModelPrediction, numeric_to_binary)

campaign_accuracy <- function(x, y) {
  map(if_else(y > x, "No", "Yes"))
}

numeric_to_binary(probabilities[1,], ModelPrediction)

add_prediction <- function(data, p) {
  
}

prediction_results <- tibble()
  
  #tibble(ModelPrediction = campaign$ModelPrediction, probabilities)
prediction_results2 <- prediction_results %>% mutate( = rep("Yes", nrow(campaign)))
#as.character(probabilities[1])

plot(campaign$ModelPrediction, as.factor(campaign$y))

skim(campaign)

current_model <- lm(campaign$ModelPrediction ~ campaign$y)
summary(current_model)
anova(current_model)
