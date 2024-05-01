library(dplyr);
library(glmmTMB);
library(ggplot2);
library(ordinal);
library(ggeffects);
library(gridExtra);
library(car);
library(lmtest);
library(Skillings.Mack);
library(PMCMRplus);
# plots and tables
library(tidyr);
library(broom.mixed);
library(gtsummary);
library(gt);
library(webshot2);

# create a gt table from model, with betas, CI95 and p vals
# save to save_as + .tex/.png in tbl_output_dir
# return gt table
model_table <- function (model, plot_title = '', save_as = '', tbl_output_dir = '', exponentiate = FALSE) {
  # get the family to set the estimate header
  model_family <- family(model)$family
  estimate_label = switch(model_family, binomial = 'log(OR)', nbinom1 = 'log(IRR)', poisson = 'log(IRR)')
  
  # tidy it using broom
  tidy_model <- broom::tidy(model)
  tidy_model$estimate.exp <- exp(tidy_model$estimate)
  # draw a gt plot
  model_tbl <- tidy_model %>% 
    replace_na(list(group = '')) %>%
    mutate(
      conf.low = estimate - (std.error * 1.96), 
      conf.high = estimate + (std.error * 1.96)
    ) %>% 
    mutate(p.val = ifelse(p.value < 0.001, '<0.001', round(p.value, 2))) %>%
    select(effect, group, term, estimate, conf.low, conf.high, p.val) %>%
    gt(groupname_col = 'effect') %>% 
    fmt_number(columns = c(estimate, conf.high, conf.low), decimals = 2) %>% 
    cols_merge(columns = c(group, term)) %>% 
    cols_merge(columns = c(conf.low, conf.high), pattern = '({1}, {2})') %>%
    cols_align(columns = c(conf.low), align = 'center') %>%
    cols_label(conf.low = '95%CI', estimate = estimate_label, group = 'Term') %>%
    tab_header(title = plot_title)
  
  # if save_as set, save it
  if (save_as != '') {
    gtsave(model_tbl, filename = paste0(tbl_output_dir, save_as, '.tex'))
    gtsave(model_tbl, filename = paste0(tbl_output_dir, save_as, '.png'))
  }
  
  # return the gt object
  return(model_tbl)
}

# input dir
eval_results_dir <- '../results/';

# where to store outputs
tbl_output_dir = paste0(getwd(), '/plots/');

# all participants from Gorilla (inc rejected)
gorilla_participants <- read.csv(paste0(eval_results_dir, 'participants.csv'));

# task results files
eval_results_filenames <- c(
  'live/archive/data_exp_135554-v12_task-3dzl.csv',
  'live/archive/part_2/data_exp_135554-v15_task-3dzl.csv',
  'pilot/data_exp_135554-v9_task-3dzl.csv'
);

datasets <- lapply(eval_results_filenames, function(filename) {
  filepath = paste0(eval_results_dir, filename);
  return(read.csv(filepath, sep = '\t'));
});

names(datasets) <- eval_results_filenames;

# compile the data into a single dataframe
data <- bind_rows(datasets, .id = 'src_filename');

# clean 'END OF FILE' rows
data <- data %>% filter(Event.Index != 'END OF FILE');

# array of participants
participants <- data$Participant.Public.ID %>% unique();

# extract the events we're interested in
corr_data <- data %>% filter(Screen == 'corr_est_stimulus', Response.Type == 'tooEarly');

# keep just the required cols
corr_est_cols = c(
  'Participant.Public.ID',
  'Absolute.Reaction.Time',
  'Response',
  'Spreadsheet..corr_scale',
  'Spreadsheet..corr',
  'Spreadsheet..sample_size',
  'Spreadsheet..spacing',
  'Trial.Number'
);

corr_data <- corr_data %>% select(all_of(corr_est_cols));

# add a column for accuracy (abs distance to answer)
corr_data$accuracy <- abs(as.numeric(corr_data$Spreadsheet..corr_scale) - as.numeric(corr_data$Response));

# add a rounded rtime column
corr_data <- corr_data %>% mutate(rtime = round(Absolute.Reaction.Time));

# fix non numeric cols to numeric
corr_data$Response <- as.numeric(corr_data$Response);
corr_data$Spreadsheet..corr_scale <- as.numeric(corr_data$Spreadsheet..corr_scale);

# munging to factors
corr_data <- corr_data %>% mutate(
  spacing_factor = as.factor(Spreadsheet..spacing), 
  sample_size_factor = as.factor(Spreadsheet..sample_size), 
  Participant.Public.ID = as.factor(Participant.Public.ID),
  response_factor = as.factor(Response),
  corr_scale_factor = as.factor(Spreadsheet..corr_scale)
);

# recode spacing
corr_data <- corr_data %>% mutate(
  spacing_factor = dplyr::recode(
    spacing_factor,
    '30' = '0.25',
    '60' = '0.5',
    '120' = '1',
    '240' = '2',
    '480' = '4'
  )
);

# add a column for fisher z transform and transform Response to fisher z
corr_data <- corr_data %>%
  mutate(fisher_z = Spreadsheet..corr_scale / 2, response_fisher_z = Response / 2) %>%
  mutate(fisher_z_factor = as.factor(fisher_z));

# summary of participant stats
corr_data %>% group_by(Participant.Public.ID) %>% summarise(mean_rtime = mean(Absolute.Reaction.Time));

# screen by attention question
incorrect_attention <- data %>% 
  select(Participant.Public.ID, Screen, Response, Object.Name, Spreadsheet..correct_attention_choice) %>% 
  filter(Screen == 'attention_parallel_select' | Screen == 'attention_button_select') %>% 
  mutate(Response = case_when(Object.Name == 'Response' ~ Response, .default = substr(Object.Name, nchar(Object.Name), nchar(Object.Name)))) %>%
  filter(Response != Spreadsheet..correct_attention_choice);

incorrect_attention_participants <- incorrect_attention %>% select(Participant.Public.ID) %>% unique();

# get data to qc from incorrect_attention_participants
data_to_qc <- corr_data[corr_data$Participant.Public.ID %in% incorrect_attention_participants$Participant.Public.ID,];

# REMOVE inattentive participants from corr data
corr_data <- corr_data[!corr_data$Participant.Public.ID %in% incorrect_attention_participants$Participant.Public.ID,];

# drop rows < 1000ms
corr_data <- corr_data %>% filter(Absolute.Reaction.Time > 1000);

# create an accuracy column that has levels from 0 to 3+
# this deals with the problem of accuracy for z = 0 range being (0, 3) and z = 3 range being (0, 6)
corr_data$accuracy_norm = sapply(
  corr_data$accuracy, 
  FUN = function(accuracy) { 
    if (accuracy > 2) { return(3) } else { return(accuracy) }
  }
);
corr_data$accuracy_norm_factor = as.factor(corr_data$accuracy_norm);

# add a column for a signed version of accuracy
corr_data <- corr_data %>% mutate(accuracy_signed = if_else(Response < Spreadsheet..corr_scale, -1 * accuracy, accuracy));

# add a normalised interval 0-1 accuracy (i.e. -6 becomes 0, 6 becomes 1)
corr_data <- corr_data %>% mutate(accuracy_signed_normalised = (accuracy_signed + 6) / 12);
# add a normalised interval 0-1 for response (i.e. -3 becomes 0 and 3 becomes 1)
corr_data <- corr_data %>% mutate(response_normalised = (Response + 3) / 6);

# inverted accuracy_norm column
corr_data <- corr_data %>% mutate(accuracy_norm_inv = 3 - accuracy_norm);

# binary accuracy columns - correct true/false and correct within 1
corr_data$accuracy_bin = corr_data$Response == corr_data$Spreadsheet..corr_scale;
corr_data$accuracy_1_bin = corr_data$accuracy <= 1;
corr_data <- corr_data %>% mutate(accuracy_factor = as.factor(accuracy_bin), accuracy_1_factor = as.factor(accuracy_1_bin));

# quick histogram
ggplot(corr_data, aes(x = accuracy)) + geom_histogram();
ggplot(corr_data, aes(x = accuracy_norm)) + geom_histogram();

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
scale_response_summary <- corr_data %>% group_by(sample_size_factor, fisher_z, spacing_factor, response_fisher_z) %>% summarise(count = n()) %>% 
  left_join(response_counts) %>% mutate(percent_responses = 100 * count / total_responses);
p_spacing_response_bubble <- ggplot(scale_response_summary) +
  aes(x = as.factor(fisher_z), y = as.factor(response_fisher_z), size = percent_responses, colour = spacing_factor) + 
  scale_color_brewer(palette = 'Set2') +
  geom_point(alpha = 0.5, ) + 
  scale_size(range = c(.05, 17)) + 
  #scale_y_discrete(limits = levels(as.factor(corr_data$fisher_z))) +
  labs(x = 'Fisher-z (real)', y = 'Fisher-z (estimated)', colour = 'Aspect ratio', size = 'Responses (%)') +
  facet_wrap(~ spacing_factor + sample_size_factor, ncol = 2) +
  theme(text = element_text(family = "Times New Roman"));

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
  aes(x = fisher_z_factor, y = as.factor(response_fisher_z), size = percent_responses, colour = spacing_factor) + 
  scale_color_brewer(palette = 'Set2') +
  geom_point(alpha = 0.5, ) + 
  scale_size(range = c(.05, 17)) + 
  #scale_y_discrete(limits = levels(as.factor(corr_data$fisher_z))) +
  labs(x = 'Fisher-z (real)', y = 'Fisher-z (estimated)', colour = 'Aspect ratio', size = 'Responses (%)') +
  facet_wrap(~ spacing_factor + sample_size_factor, ncol = 2) +
  theme(text = element_text(family = "Times New Roman"));

p_spacing_response_bubble_trimmed;

ggsave(
  p_spacing_response_bubble_trimmed, 
  filename = paste0(tbl_output_dir, 'corr_p_spacing_response_bubble_trimmed.png'),
  width = 2800,
  height = 3200,
  units = 'px'
);

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

# split by sample_size
correct_counts_by_p.sample_size <- split(correct_counts_by_p, correct_counts_by_p$sample_size_factor);

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

####
# modelling response relationships
####

## add a trial_num_by_size_corr
corr_data <- corr_data %>% group_by(Participant.Public.ID, sample_size_factor, fisher_z) %>% arrange(Trial.Number, .by_group = TRUE) %>% mutate(trial_num_by_size_corr = 1:n());

###
# ordered beta regression
###
# adjust iterations
control <- glmmTMBControl(optCtrl=list(iter.max=1e3,eval.max=1e3));

# ordered beta regression for bounded continuous dist (accuracy_signed_normalised)
ob_accuracy_int_factor <- glmmTMB(
  accuracy_signed_normalised ~ fisher_z_factor*spacing_factor*sample_size_factor + (1|Participant.Public.ID) + (1|trial_num_by_size_corr), 
  data = corr_data,
  family = 'ordbeta'
);

ob_accuracy_int_factor <- update(ob_accuracy_int_factor, control = glmmTMBControl(optCtrl=list(iter.max=1e3,eval.max=1e3)));

summary(ob_accuracy_int_factor);

ob_accuracy_int_factor_predicted <- ggpredict(ob_accuracy_int_factor, terms = c('fisher_z_factor', 'spacing_factor', 'sample_size_factor'));
plot(ob_accuracy_int_factor_predicted);

# simpler model, dropping the interaction with sample size
ob_accuracy_factor <- glmmTMB(
  accuracy_signed_normalised ~ fisher_z_factor*spacing_factor + sample_size_factor + (1|Participant.Public.ID) + (1|trial_num_by_size_corr), 
  data = corr_data,
  family = 'ordbeta'
);

summary(ob_accuracy_factor);

ob_accuracy_factor_predicted <- ggpredict(ob_accuracy_factor, terms = c('fisher_z_factor', 'spacing_factor', 'sample_size_factor'));
plot(ob_accuracy_factor_predicted);

## random effects log likelihood
## drop random effects, assess with likelihood ratio test
ob_accuracy_int_factor_norandomp <- glmmTMB(
  accuracy_signed_normalised ~ fisher_z_factor*spacing_factor*sample_size_factor + (1|trial_num_by_size_corr), 
  data = corr_data,
  family = 'ordbeta'
);

ob_accuracy_int_factor_norandomp <- update(ob_accuracy_int_factor_norandomp, control = glmmTMBControl(optCtrl=list(iter.max=1e3,eval.max=1e3)));

summary(ob_accuracy_int_factor_norandomp);

anova(ob_accuracy_int_factor, ob_accuracy_int_factor_norandomp);

## drop ALL random effects (inc trial), assess with likelihood ratio test
ob_accuracy_int_factor_norandomp_notrial <- glmmTMB(
  accuracy_signed_normalised ~ fisher_z_factor*spacing_factor*sample_size_factor, 
  data = corr_data,
  family = 'ordbeta'
);

ob_accuracy_int_factor_norandomp_notrial <- update(ob_accuracy_int_factor_norandomp_notrial, control = glmmTMBControl(optCtrl=list(iter.max=1e3,eval.max=1e3)));

summary(ob_accuracy_int_factor_norandom_notrial);

anova(ob_accuracy_int_factor, ob_accuracy_int_factor_norandomp, ob_accuracy_int_factor_norandom_notrial);

####
# model summary table for supplementary
###
p_ob_accuracy_int_factor_summary <- model_table(ob_accuracy_int_factor, plot_title = 'Task A - Response accuracy model summary (GLMM, ordered beta)', save_as = 'ptbl_ob_accuracy_int_factor', tbl_output_dir = tbl_output_dir)

### end ob model summary table

## plots of ob
##

## accuracy
pd <- position_dodge(width = 0.25);
pd2 <- position_dodge(width = 0.5);
# create a summary from corr_data to get the proportion correct as a column
corr_data_mean_response <- corr_data %>% group_by(fisher_z, spacing_factor, sample_size_factor) %>% summarise(mean_response = sum(Response) / n(), mean_accuracy_signed_normalised = sum(accuracy_signed_normalised) / n());
# predicted and actual
# THIS WORKS
shapes  <- c('s1' = 23, 's2' = 5);
p_ob_accuracy_int_real <- as.data.frame(ob_accuracy_int_factor_predicted) %>% mutate(predicted = (predicted * 6) - 3, conf.low = (conf.low * 6) - 3, conf.high = (conf.high * 6) - 3) %>%
  ggplot(data = ., aes(x = x, colour = group)) + 
  scale_color_brewer(palette = 'Set2') +
  labs(x = 'Fisher-z (real)', y = 'Predicted distance response to actual (Fisher-z)', colour = 'Aspect ratio') +
  geom_point(aes(y = predicted, shape = 'pred'), position = pd2) + geom_linerange(aes(ymin = conf.low, ymax = conf.high), position=pd2) + 
  geom_point(aes(y = (corr_data_mean_response$mean_accuracy_signed_normalised * 6) - 3, shape = 'real'), position = pd2) +
  scale_shape_manual(name = 'Means', breaks = c('pred', 'real'), values = shapes_for_means, labels = c('Predicted (inc CI95)', 'Real')) +
  facet_grid(rows = c(40, 160)) +
  #ggtitle('Predicted mean accuracy (with CI95) by set size') +
  theme(text = element_text(family = "Times New Roman"));

ggsave(
  p_ob_accuracy_int_real, 
  filename = paste0(tbl_output_dir, 'corr_p_ob_accuracy_int_real.png'),
  width = 2400,
  height = 2400,
  units = 'px'
);

## response
pd <- position_dodge(width = 0.25);
pd2 <- position_dodge(width = 0.5);

###
# END ordered beta
###

##################
###
# residuals checks
###
# using DHARMa - scaled residuals
library(DHARMa);
res_sim_ob <- simulateResiduals(fittedModel = ob_accuracy_int_factor, plot = F);
plot(res_sim_ob);

###
# corr - response models - influence
###
library(influence.ME);

###
# inf for ob_accuracy_int_factor
###
ob_accuracy_int_factor_influence_time <- system.time(
  ob_accuracy_int_factor_influence <- influence_mixed(ob_accuracy_int_factor, groups="Participant.Public.ID")
);
car::infIndexPlot(ob_accuracy_int_factor_influence);
inf <- as.data.frame(ob_accuracy_int_factor_influence[["fixed.effects[-Participant.Public.ID]"]]);
inf <- transform(
  inf,
  participant=rownames(inf),
  cooks=cooks.distance(ob_accuracy_int_factor_influence)
);
inf$ord <- rank(inf$cooks);
inf <- inf %>% arrange(ord);
high_cooks <- inf %>% filter (cooks > 0.026) %>% select(participant);
corr_data_rem_cooks <- corr_data %>% filter(!Participant.Public.ID %in% high_cooks$participant);
ob_accuracy_int_factor_rem_cooks <- glmmTMB(
  as.factor(accuracy_1_bin) ~ fisher_z*spacing_factor*sample_size_factor + (1|Participant.Public.ID) + (1|trial_num_by_size_corr), 
  data = corr_data_rem_cooks,
  family = 'binomial'
);
ob_accuracy_int_factor_rem_cooks <- glmmTMB(
  as.factor(accuracy_1_bin) ~ fisher_z_factor*spacing_factor*sample_size_factor + (1|Participant.Public.ID) + (1|trial_num_by_size_corr), 
  data = corr_data_rem_cooks,
  family = 'binomial'
);
ob_accuracy_int_factor_rem_cooks <- glmmTMB(
  accuracy_signed_normalised ~ fisher_z_factor*spacing_factor*sample_size_factor + (1|Participant.Public.ID) + (1|trial_num_by_size_corr), 
  data = corr_data_rem_cooks,
  family = 'ordbeta'
);
ob_accuracy_int_factor_rem_cooks_predicted <- ggpredict(ob_accuracy_int_factor_rem_cooks, terms = c('fisher_z', 'spacing_factor', 'sample_size_factor'));
p_ob_accuracy_int_factor_rem_cooks <- plot(ob_accuracy_int_factor_rem_cooks_predicted);
grid.arrange(p_ob_accuracy_int_factor, p_ob_accuracy_int_factor_rem_cooks);



