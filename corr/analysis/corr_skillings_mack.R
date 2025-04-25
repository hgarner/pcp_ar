## counts dataframe of count correct by participant id, spacing and sample_size
correct_counts_by_p <- corr_data %>% 
  group_by(Participant.Public.ID, sample_size_factor, spacing_factor) %>%
  summarise(
    n = n(),
    count_correct = sum(accuracy_1_bin)
  );

correct_counts_by_p <- correct_counts_by_p %>% 
  mutate(
    Participant.Public.ID = factor(Participant.Public.ID), 
    spacing_factor = factor(spacing_factor),
    prop_correct = count_correct / n
  );

# friedman test (see https://rcompanion.org/handbook/F_10.html)
#friedman.test(count_correct ~ spacing_factor | Participant.Public.ID, data = correct_counts_by_p);

# diffs between levels of spacing with wilcoxon signed rank (+ bonferrroni correction)
pairwise.wilcox.test(correct_counts_by_p$count_correct, correct_counts_by_p$spacing_factor, p.adjust.method = 'bonferroni');

# split by sample_size
correct_counts_by_p.sample_size <- split(correct_counts_by_p, correct_counts_by_p$sample_size_factor);

# ###
# # friedman test on each sample_size
# ###
#
# ftest_results <- lapply(correct_counts_by_p.sample_size, function(d) {
#   ft <- friedman.test(count_correct ~ spacing_factor | Participant.Public.ID, data = d);
#   return(data.frame('statistic' = ft$statistic, 'parameter' = ft$parameter, 'p.value' = ft$p.value));
# });
# 
# ### PRINT ftest results
# ptbl_summary_ftest_results <- ftest_results %>% bind_rows(.id = 'sample_size_factor') %>% select(sample_size_factor, statistic, p.value) %>% gt() %>%
#   tab_header(
#     title = md("Friedman test by sample size")
#   ) %>% 
#   cols_label(
#     .list = list(sample_size_factor = 'Sample size', statistic = 'Stat', p.value = 'p-value')
#   ) %>%
#   fmt_number(
#     columns = c(statistic),
#     decimals = 1
#   ) %>%
#   fmt_scientific(
#     columns = c(p.value),
#     decimals = 1
#   );
# filename <- 'corr_summary_ftest_results';
# gtsave(ptbl_summary_ftest_results, filename = paste0(tbl_output_dir, filename, '.tex'));
# gtsave(ptbl_summary_ftest_results, filename = paste0(tbl_output_dir, filename, '.html'));
# save to .png
#webshot2::webshot(url = paste0(tbl_output_dir, filename, '.html'), file = paste0(tbl_output_dir, filename, '.png'));

###
# Skillings-Mack
###

smtest_results <- lapply(correct_counts_by_p.sample_size, function(d) {
  # widen tibble and convert to dataframe
  d <- d %>% 
    ungroup() %>% 
    select(Participant.Public.ID, spacing_factor, prop_correct) %>%
    pivot_wider(names_from = spacing_factor, values_from = prop_correct) %>%
    as.data.frame();
  # sort row names and remove participant col as needs to convert to matrix
  row.names(d) <- d$Participant.Public.ID;
  d <- d %>% select(!(Participant.Public.ID));
  # run test on matrix
  st <- skillingsMackTest(as.matrix(d));
  return(data.frame('statistic' = st$statistic, 'parameter' = st$parameter, 'p.value' = st$p.value));
});

### PRINT smtest results
ptbl_summary_smtest_results <- smtest_results %>% bind_rows(.id = 'sample_size_factor') %>% select(sample_size_factor, statistic, p.value) %>% gt() %>%
  tab_header(
    title = md("Skillings-Mack test for difference between aspect ratios by sample size")
  ) %>%
  cols_label(
    .list = list(sample_size_factor = 'Sample size', statistic = 'Stat', p.value = 'p-value')
  ) %>%
  fmt_number(
    columns = c(statistic),
    decimals = 1
  ) %>%
  fmt_scientific(
    columns = c(p.value),
    decimals = 1
  );
filename <- 'corr_summary_smtest_results';
gtsave(ptbl_summary_smtest_results, filename = paste0(tbl_output_dir, filename, '.tex'));
gtsave(ptbl_summary_smtest_results, filename = paste0(tbl_output_dir, filename, '.html'));
# save to .png
webshot2::webshot(url = paste0(tbl_output_dir, filename, '.html'), file = paste0(tbl_output_dir, filename, '.png'));

## PRINT SUMMARY
# png/tex summary table

print_summary_correct_counts <- corr_data %>% group_by(sample_size_factor, fisher_z, spacing_factor) %>% 
  summarise(
    n = n(),
    count_correct = sum(accuracy_1_bin),
    percent_correct = (100 * sum(accuracy_1_bin) / n())
  );
ptbl_summary_correct_counts <- print_summary_correct_counts %>% select(sample_size_factor, fisher_z, spacing_factor, percent_correct) %>% gt() %>% tab_options(row_group.as_column = TRUE) %>%
  tab_header(
    title = md("Summary - Correlation estimation")
  ) %>% 
  cols_label(
    .list = list(spacing_factor = 'Aspect ratio', fisher_z = 'Fisher-z', percent_correct = 'Correct (%)')
  ) %>% 
  tab_stubhead(label = 'Sample size') %>%
  fmt_number(
    columns = percent_correct,
    decimals = 1
  );
gtsave(ptbl_summary_correct_counts, filename = paste0(tbl_output_dir, 'corr_summary_correct_counts.tex'));
gtsave(ptbl_summary_correct_counts, filename = paste0(tbl_output_dir, 'corr_summary_correct_counts.html'));
# save to .png
webshot2::webshot(url = paste0(tbl_output_dir, 'corr_summary_correct_counts.html'), file = paste0(tbl_output_dir, 'corr_summary_correct_counts.png'));
