## SUMMARIES

# summary of basic stats
corr_summary <- corr_data %>% group_by(accuracy, fisher_z, spacing_factor) %>% summarise(count = n());

# mean and sd for diff spacings and corr
corr_stats <- corr_data %>% group_by(sample_size_factor, fisher_z, spacing_factor) %>% summarise(mean_judged = mean(response_fisher_z), var_judged = var(response_fisher_z), sd_judged = sd(response_fisher_z));

## PRINT corr_stats
ptbl_corr_stats <- corr_stats %>% select(fisher_z, spacing_factor, mean_judged, var_judged) %>% ungroup() %>% group_by(sample_size_factor) %>%
  gt(row_group_as_column = TRUE) %>%
  tab_header(
    title = md("Mean judged z by sample size and aspect ratio")
  ) %>% 
  cols_label(
    .list = list(sample_size_factor = 'Sample size', fisher_z = 'Actual z', mean_judged = 'Mean est. z', var_judged = 'Var. est. z', spacing_factor = 'Aspect ratio')
  ) %>%
  fmt_number(
    columns = c(mean_judged, var_judged),
    decimals = 1
  );
ptbl_corr_stats;
filename <- 'ptbl_table_corr_stats';
gtsave(ptbl_corr_stats, filename = paste0(tbl_output_dir, filename, '.tex'));
gtsave(ptbl_corr_stats, filename = paste0(tbl_output_dir, filename, '.html'));
# save to .png
webshot2::webshot(url = paste0(tbl_output_dir, filename, '.html'), file = paste0(tbl_output_dir, filename, '.png'));

# transposed
ptbl_wide_corr_stats <- corr_stats %>% 
  ungroup() %>% 
  pivot_wider(
    names_from = spacing_factor, 
    values_from = c(mean_judged, var_judged, sd_judged),
    names_glue = '{spacing_factor}-{.value}',
    names_sort = TRUE
  ) %>% 
  group_by(sample_size_factor) %>% 
  gt(row_group_as_column = TRUE) %>% 
  fmt_number(
    columns = c(
      '0.25-mean_judged', 
      '0.25-var_judged',
      '0.25-sd_judged',
      '0.5-mean_judged', 
      '0.5-var_judged',
      '0.5-sd_judged',
      '1-mean_judged', 
      '1-var_judged',
      '1-sd_judged',
      '2-mean_judged', 
      '2-var_judged',
      '2-sd_judged',
      '4-mean_judged', 
      '4-var_judged',
      '4-sd_judged'
    ),
    decimals = 1
  ) %>%
  tab_spanner(., label = '0.25', columns = starts_with('0.25')) %>%
  tab_spanner(., label = '0.5', columns = starts_with('0.5')) %>%
  tab_spanner(., label = '1', columns = starts_with('1')) %>%
  tab_spanner(., label = '2', columns = starts_with('2')) %>%
  tab_spanner(., label = '4', columns = starts_with('4')) %>%
  tab_spanner(., label = 'Aspect ratio', columns = matches('[0-9]+')) %>%
  cols_move(columns = starts_with('4'), after = fisher_z) %>%
  cols_move(columns = starts_with('2'), after = fisher_z) %>%
  cols_move(columns = starts_with('1'), after = fisher_z) %>%
  cols_move(columns = starts_with('0.5'), after = fisher_z) %>%
  cols_move(columns = starts_with('0.25'), after = fisher_z) %>%
  cols_label(
    .list = list(
      sample_size_factor = 'Sample size', 
      fisher_z = 'Actual z', 
      '0.25-mean_judged' = 'Mean est.<br>z', 
      '0.25-var_judged' = 'Var.', 
      '0.25-sd_judged' = 'SD',
      '0.5-mean_judged' = 'Mean est.<br>z', 
      '0.5-var_judged' = 'Var.', 
      '0.5-sd_judged' = 'SD',
      '1-mean_judged' = 'Mean est.<br>z', 
      '1-var_judged' = 'Var.', 
      '1-sd_judged' = 'SD',
      '2-mean_judged' = 'Mean est.<br>z', 
      '2-var_judged' = 'Var.', 
      '2-sd_judged' = 'SD',
      '4-mean_judged' = 'Mean est.<br>z', 
      '4-var_judged' = 'Var.', 
      '4-sd_judged' = 'SD'
    ),
    .fn = md
  );
ptbl_wide_corr_stats;
filename <- 'ptbl_wide_corr_stats';
gtsave(ptbl_wide_corr_stats, filename = paste0(tbl_output_dir, filename, '.tex'));
gtsave(ptbl_wide_corr_stats, filename = paste0(tbl_output_dir, filename, '.html'));
# save to .png
webshot2::webshot(url = paste0(tbl_output_dir, filename, '.html'), file = paste0(tbl_output_dir, filename, '.png'))

## END PRINT corr_stats

# quick plots
p <- ggplot(corr_data, aes(x = fisher_z_factor, y = Response, colour = spacing_factor)) + geom_violin();

# spacing and accuracy
p_spacing_accuracy <- ggplot(corr_data, aes(x = fisher_z_factor, y = accuracy, colour = spacing_factor)) + geom_violin() + facet_wrap(~ spacing_factor);

scale_accuracy_summary <- corr_data %>% group_by(fisher_z, spacing_factor, accuracy) %>% summarise(count = n());
p_spacing_accuracy_bubble <- ggplot(scale_accuracy_summary, aes(x = fisher_z_factor, y = accuracy, size = count, colour = spacing_factor)) + 
  geom_point() + 
  scale_size(range = c(.1, 24)) + 
  facet_wrap(~ spacing_factor);

response_counts <- corr_data %>% group_by(sample_size_factor, fisher_z, spacing_factor) %>% summarise(total_responses = n());
#scale_response_summary <- corr_data %>% group_by(sample_size_factor, fisher_z, spacing_factor, response_fisher_z) %>% summarise(count = n());
scale_response_summary <- corr_data %>% 
  group_by(sample_size_factor, fisher_z, spacing_factor, response_fisher_z) %>% 
  summarise(count = n()) %>% 
  left_join(response_counts) %>% 
  mutate(percent_responses = 100 * count / total_responses) %>% 
  mutate(fisher_z_factor = as.factor(fisher_z), response_fisher_z_factor = as.factor(response_fisher_z));
p_spacing_response_bubble <- ggplot(scale_response_summary) +
  aes(x = fisher_z_factor, y = response_fisher_z_factor, size = percent_responses, colour = spacing_factor) + 
  scale_color_brewer(palette = 'Set2') +
  geom_point(alpha = 0.5, ) + 
  scale_size(range = c(.05, 17)) + 
  #scale_y_discrete(limits = levels(as.factor(corr_data$fisher_z))) +
  labs(x = 'Fisher-z (real)', y = 'Fisher-z (estimated)', colour = 'Aspect ratio', size = 'Responses (%)') +
  facet_wrap(~ spacing_factor + sample_size_factor, ncol = 2) +
  theme(text = element_text(family = "Times New Roman", size = 18));

p_spacing_response_bubble;

ggsave(
  p_spacing_response_bubble, 
  filename = paste0(tbl_output_dir, 'corr_p_spacing_response_bubble.png'),
  width = 2800,
  height = 4700,
  units = 'px'
);

## response bubble - extremes of ar for compact print version
p_spacing_response_bubble_trimmed <- ggplot(
  scale_response_summary %>% filter(spacing_factor %in% c(0.25, 1, 4))
) +
  aes(x = fisher_z_factor, y = response_fisher_z_factor, size = percent_responses, colour = spacing_factor) + 
  scale_color_brewer(palette = 'Set2') +
  geom_point(alpha = 0.5, ) + 
  scale_size(range = c(.05, 17)) + 
  #scale_y_discrete(limits = levels(as.factor(corr_data$fisher_z))) +
  labs(x = 'Fisher-z (real)', y = 'Fisher-z (estimated)', colour = 'Aspect ratio', size = 'Responses (%)') +
  facet_wrap(~ spacing_factor + sample_size_factor, ncol = 2) +
  theme(text = element_text(family = "Times New Roman", size = 14));

p_spacing_response_bubble_trimmed;

ggsave(
  p_spacing_response_bubble_trimmed, 
  filename = paste0(tbl_output_dir, 'corr_p_spacing_response_bubble_trimmed.png'),
  width = 2800,
  height = 3200,
  units = 'px'
);
