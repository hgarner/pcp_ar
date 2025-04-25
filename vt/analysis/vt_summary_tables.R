# requires vt_data_setup.R

###
# SUMMARIES 
###
summary <- vt_data %>% group_by(sample_size, spacing) %>% 
  summarise(
    n = n(), 
    mean_accuracy = mean(accuracy, na.rm = TRUE), 
    sd_accuracy = sd(accuracy, na.rm = TRUE), 
    mean_rtime = mean(Absolute.Reaction.Time, na.rm = TRUE), 
    sd_rtime = sd(Absolute.Reaction.Time, na.rm = TRUE),
    count_accuracy_bin = sum(accuracy_bin),
    count_accuracy_bin_fuzzed = sum(accuracy_bin_fuzzed),
    prop_accurate = sum(accuracy_bin) / n(),
    prop_accurate_fuzzed = sum(accuracy_bin_fuzzed) / n()
  );
print(summary, n = 100);

###
# summary table
###
vt_cont_tab <- table(vt_data$spacing, vt_data$accuracy_bin_fuzzed);
print(vt_cont_tab);

## png/tex summary table

print_summary_correct_counts <- vt_data %>% group_by(sample_size, spacing) %>% 
  summarise(
    n = n(),
    count_correct = sum(accuracy_bin_fuzzed),
    percent_correct = (100 * sum(accuracy_bin_fuzzed) / n())
  );
ptbl_summary_correct_counts <- print_summary_correct_counts %>% select(sample_size, spacing, percent_correct) %>% gt() %>% tab_options(row_group.as_column = TRUE) %>%
  tab_header(
    title = md("Summary - Value tracing")
  ) %>% 
  cols_label(
    .list = list(spacing = 'Aspect ratio', percent_correct = 'Correct (%)')
  ) %>% 
  tab_stubhead(label = 'Sample size') %>%
  fmt_number(
    columns = percent_correct,
    decimals = 1
  );
ptbl_summary_correct_counts;
gtsave(ptbl_summary_correct_counts, filename = paste0(tbl_output_dir, 'vt_summary_correct_counts.tex'));
gtsave(ptbl_summary_correct_counts, filename = paste0(tbl_output_dir, 'vt_summary_correct_counts.html'));
# save to .png
webshot2::webshot(url = paste0(tbl_output_dir, 'vt_summary_correct_counts.html'), file = paste0(tbl_output_dir, 'vt_summary_correct_counts.png'));
###
# END SUMMARY TABLE
###