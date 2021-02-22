# install.packages("tidyverse")
library(tidyverse)
# install.packages("skimr")
library(skimr)
# install.packages("scales")
library(scales)

# Load data into tibble, and separate current model results for performance measurement
campaign <- read_csv("DSA Data Set.csv", na = "unknown")
ModelPrediction <- tibble(prediction = campaign$ModelPrediction, `Subscribed?` = campaign$y)


# Check for duplicate rows
campaign %>% group_by(names(campaign)) %>% count() # 41188 rows, no dupes


# Quick summary of data set: NAs, uniques, min/max, mean, st dev
skim(campaign)

# Distributions of numeric columns
campaign %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_histogram()

# Distributions of character columns
campaign %>%
  keep(is.character) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_histogram(stat="count")


# Create thresholds list of 0% - 100% by 1%. Change by = 0.01 to by = 0.1 for quicker run speeds.
# NOTE: by = 0.01 was used for presentation
my_breaks <- seq(0, 1, by = 0.01)
thresholds <- tibble(threshold = my_breaks)


# Check distribution of y
yes <- campaign %>% filter(y == "yes") %>% nrow()
no <- campaign %>% filter(y == "no") %>% nrow()
yes/(yes + no)  # 11.3%
1 - yes/(yes + no)  # 88.7%


# Get max height for histogram
max_height <- nrow(ModelPrediction %>% filter(prediction > .9))


# Visualize predictor bins by 10% in a stacked histogram
ggplot(ModelPrediction) + 
  geom_histogram(aes(prediction, color = `Subscribed?`, fill = `Subscribed?`),
                 breaks = my_breaks,
                 alpha = .8, position = "stack") +
  scale_x_continuous(breaks = pretty_breaks(n = 10),
                     minor_breaks = my_breaks,
                     labels = percent_format(accuracy = 1)) +
  theme(axis.text.x = element_text(hjust=.3)) +
  scale_y_continuous(minor_breaks = seq(0 , max_height + 1000, 1000), 
                     breaks = seq(0, max_height, 5000))

# Data set of histogram above
no_counts <- ModelPrediction %>% 
  filter(`Subscribed?` == "no") %>% 
  with(hist(prediction, breaks = my_breaks)$counts)

yes_counts <- ModelPrediction %>% 
  filter(`Subscribed?` == "yes") %>% 
  with(hist(prediction, breaks = my_breaks)$counts)

bin_counts <- hist(ModelPrediction$prediction, breaks = my_breaks)$counts

percent_range <- tibble(left=my_breaks[1:10] * 100, right=my_breaks[2:11] * 100) %>% 
  transmute(percent_range=paste0(left, " - ", right, "%"))

# tibble(`Range` = percent_range$percent_range,
#        `% No`=no_counts / bin_counts,
#        `% Yes`=yes_counts / bin_counts) %>%
#   write_csv("histogram_table.csv")


## NOTES
# Subscribers are uniformly distributed between prediction ranges 50% - 100%
# Non-Subscribers are distributed geometrically, and are seen mostly in ranges 80% - 100%
# Samples with predictions of 40% - 80% are mostly subscribers



# Create new function to convert numeric list to a binary variable
numeric_to_binary <- function (threshold, num_list) {
  outcome = if_else(num_list > threshold, "no", "yes")
}

# Add tibble column with outcomes using each threshold (1 tibble per row)
results <- thresholds %>% 
  mutate(outcome = map(threshold, ~ numeric_to_binary(.x, ModelPrediction$prediction)))

# Create new function to build confusion matrix metrics
actual_vs_predicted <- function(actual, predicted, x = "yes", y = "yes") {
  sum(map2_lgl(actual, predicted, ~ .x == x & .y == y))
}

# Below code is very slow if by = 0.01
# Add confusion matrix performance measurements as new columns
performance <- results %>% 
  mutate(TruePositive = map_int(results$outcome, ~ actual_vs_predicted(campaign$y, .x, "yes", "yes"))) %>% 
  mutate(FalsePositive = map_int(results$outcome, ~ actual_vs_predicted(campaign$y, .x, "no", "yes"))) %>% 
  mutate(TrueNegative = map_int(results$outcome, ~ actual_vs_predicted(campaign$y, .x, "no", "no"))) %>% 
  mutate(FalseNegative = map_int(results$outcome, ~ actual_vs_predicted(campaign$y, .x, "yes", "no"))) %>% 
  mutate(Accuracy = (TruePositive + TrueNegative) / (TruePositive + FalseNegative + TrueNegative + FalsePositive)) %>% 
  mutate(TP_rate = TruePositive / (TruePositive + FalseNegative)) %>% 
  mutate(TN_rate = TrueNegative / (TrueNegative + FalsePositive))


# Top accuracy
performance %>% arrange(desc(Accuracy)) %>% head() # 89.8% accurate at 50% threshold

# Best threshold for improved sensitivity without huge loss to accuracy or specificity
performance %>% arrange(desc(TP_rate)) %>% select(-outcome) %>% View() # See 0.90 threshold




# Reformat performance data set for use in ggplot
# performance_gg <- performance %>% 
#   select(-outcome) %>% 
#   gather(measure, count, -threshold, -Accuracy) %>% 
#   group_by(measure) %>% 
#   mutate(percent_of_max_count = count/max(.$count))
# 
# ggplot(performance_gg) + 
#   geom_line(aes(threshold, percent_of_max_count, color=measure, size=2)) + 
#   guides(color = guide_legend(override.aes = list(size=2))) +
#   scale_y_continuous(labels = scales::percent)
# 
# ggplot(performance_gg) + 
#   geom_line(aes(threshold, Accuracy)) + 
#   scale_y_continuous(labels = scales::percent)

