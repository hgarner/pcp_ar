###
# calculate odds ratios for full model
###

full_model <- bin_accuracy_interaction

# generate predicted probabilities for all combinations of spacing & sample_size
predictions <- ggpredict(full_model, terms = c('sample_size', 'spacing'))

# convert predicted probabilities to odds
predictions <- as.data.frame(predictions) %>%
  mutate(odds = predicted / (1 - predicted)) %>%
  mutate(sample_size = x, spacing = group)

# compute odds ratios relative to reference
# CHANGE AS REQUIRED
reference_spacing = 1
reference_sample_size = 40

reference_odds <- predictions %>%
  filter(spacing == reference_spacing & sample_size == reference_sample_size) %>%
  pull(odds)

predictions <- predictions %>%
  mutate(odds_ratio = odds / reference_odds)

# select relevant columns for readability
odds_ratios <- predictions %>%
  select(spacing, sample_size, predicted_probability = predicted, odds, odds_ratio)

# print results
print(odds_ratios)

