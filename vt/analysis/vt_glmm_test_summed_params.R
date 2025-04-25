####
# evaluating parameters for factor level combinations
####

# where to store outputs
tbl_output_dir = paste0(getwd(), '/plots/');

# full model and predicted
full_model <- bin_accuracy_interaction

# get predicted coefficients
# note that we retrieve these on the link scale here as errors are only normally distributed on the link scale
full_model_predicted_link <- ggpredict(bin_accuracy_interaction, terms = c('spacing', 'sample_size'));
plot(full_model_predicted_link)

###
# regression testing of factor level combinations
###

#
# run t-test comparing each pair of aspect ratios, split by sample size
#

# split into list by sample_size
# for each df, get pairwise and do t test (either directional or 2-tailed if ignore direction)

# create dataframe from predicted, back transform predicted to link scale (logit)
full_model_predicted_link <- as.data.frame(full_model_predicted_link) %>% 
  mutate(spacing = x, sample_size = group) %>%
  mutate(predicted_link = qlogis(predicted))

# split data into a list of dataframes by fisher_z_factor and sample_size_factor
full_model_predicted_link_split <- full_model_predicted_link %>% split(list(.$sample_size))

pairwise_t_results <- lapply(full_model_predicted_link_split, function(sample_size_subset) {
  pairwise_t <- combn(sample_size_subset$spacing, 2, function(pair) {
    sample_size <- sample_size_subset %>% distinct(sample_size)
    pl_1 <- sample_size_subset %>% filter(spacing == pair[1]) %>% pull(predicted_link)
    pl_2 <- sample_size_subset %>% filter(spacing == pair[2]) %>% pull(predicted_link)
    se_1 <- sample_size_subset %>% filter(spacing == pair[1]) %>% pull(std.error)
    se_2 <- sample_size_subset %>% filter(spacing == pair[1]) %>% pull(std.error)
    t_val <- (pl_1 - pl_2) / sqrt(se_1^2 + se_2^2)
    p_val <- 2 * (1 - pnorm(abs(t_val)))
    
    return(data.frame(
      sample_size = sample_size,
      spacing_1 = pair[1],
      spacing_2 = pair[2],
      pl_1 = pl_1,
      pl_2 = pl_2,
      se_1 = se_1,
      se_2 = se_2,
      t_val = t_val,
      p_value = p_val
    ))
  }, simplify = FALSE) %>% bind_rows()
  return(pairwise_t)
}) %>% bind_rows()

sig_val = 0.05
assign_stars <- function(p) {
  case_when(
    p < sig_val / 100 ~ "***",  # Highly significant
    p < sig_val / 10  ~ "**",   # Significant
    p < sig_val  ~ "*",    # Marginally significant
    TRUE      ~ ""      # Not significant
  )
}

pairwise_t_results_table <- pairwise_t_results %>% 
  select(
    sample_size,
    spacing_1,
    spacing_2,
    p_value
  ) %>%
  mutate(
    stars = assign_stars(p_value)
  ) %>%
  gt() %>%
  tab_header(
    title = md("T-test of differences in estimated predictors by sample size")
  ) %>%
  cols_label(
    .list = list(
      sample_size = 'Sample size', 
      spacing_1 = 'AR 1', 
      spacing_2 = 'AR 2', 
      p_value = 'p-value',
      stars = 'Sig.'
    )
  ) %>%
  #fmt_number(
  #  columns = c(f_stat),
  #  decimals = 1
  #) %>%
  #cols_merge(
  #  columns = c(p_value, stars),
  #  pattern = "{1} {2}"
  #) %>%
  cols_label(
    p_value = "P-value"
  ) %>%
  fmt_scientific(
    columns = c(p_value),
    decimals = 1
  )

pairwise_t_results_table

pairwise_t_results_table %>%
  gtsave(filename = paste0(tbl_output_dir, 'vt_bin_model_p_test_summary.tex'))
