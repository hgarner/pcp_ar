###
# NON-PARAM TESTS
###

## counts dataframe of count correct by participant id, spacing and sample_size
correct_counts_by_p <- vt_data %>% 
  group_by(Participant.Public.ID, sample_size, spacing) %>%
  summarise(
    n = n(),
    count_correct = sum(accuracy_bin_fuzzed)
  );

# friedman test (see https://rcompanion.org/handbook/F_10.html)
correct_counts_by_p <- correct_counts_by_p %>% mutate(Participant.Public.ID = factor(Participant.Public.ID), sample_size = factor(sample_size), spacing = factor(spacing));

# split by sample_size
correct_counts_by_p.sample_size <- split(correct_counts_by_p, correct_counts_by_p$sample_size);

# friedman test on each sample_size
ftest_results <- lapply(correct_counts_by_p.sample_size, function(d) {
  ft <- friedman.test(count_correct ~ spacing | Participant.Public.ID, data = d);
  return(data.frame('statistic' = ft$statistic, 'parameter' = ft$parameter, 'p.value' = ft$p.value));
});

### PRINT results
ptbl_summary_ftest_results <- ftest_results %>% bind_rows(.id = 'sample_size') %>% select(sample_size, statistic, p.value) %>% gt() %>%
  tab_header(
    title = md("Friedman test by sample size")
  ) %>% 
  cols_label(
    .list = list(sample_size = 'Sample size', statistic = 'Stat', p.value = 'p-value')
  ) %>%
  fmt_number(
    columns = c(statistic),
    decimals = 1
  ) %>%
  fmt_scientific(
    columns = c(p.value),
    decimals = 1
  );
filename <- 'vt_summary_ftest_results';
gtsave(ptbl_summary_ftest_results, filename = paste0(tbl_output_dir, filename, '.tex'));
gtsave(ptbl_summary_ftest_results, filename = paste0(tbl_output_dir, filename, '.html'));
# save to .png
webshot2::webshot(url = paste0(tbl_output_dir, filename, '.html'), file = paste0(tbl_output_dir, filename, '.png'))

### testing with xtable
options(xtable.floating = FALSE);
options(xtable.timestamp = '');
xtab_ftest_results <- ftest_results %>% bind_rows(.id = 'sample_size') %>% select(sample_size, statistic, p.value) %>% xtable(display = c("s","d","f","G"), math.style.exponents = TRUE);
xtab_ftest_results;