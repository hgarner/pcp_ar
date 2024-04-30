library(dplyr);
library(glmmTMB);
library(ggplot2);
library(ordinal);
library(ggeffects);
library(gridExtra);
library(summarytools);
library(magrittr);
library(AICcmodavg);
# plots and tables
library(tidyr);
library(broom.mixed);
library(gtsummary);
library(gt);
library(webshot2);
library(xtable);

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

# task results
eval_results_filenames <- c(
  'full/data_exp_142982-v10/data_exp_142982-v10_task-i1zc.csv',
  'pilot_4/data_exp_142982-v9/data_exp_142982-v9_task-i1zc.csv'
);

is_transformed_results_scale = TRUE;

datasets <- lapply(eval_results_filenames, function(filename) {
  filepath = paste0(eval_results_dir, filename);
  return(read.csv(filepath, sep = ','));
});

names(datasets) <- eval_results_filenames;

# compile the data into a single dataframe
data <- bind_rows(datasets, .id = 'src_filename');

# clean 'END OF FILE' rows
data <- data %>% filter(Event.Index != 'END OF FILE');

raw_data <- data;

# screen by attention question
incorrect_attention <- data %>% 
  select(Participant.Public.ID, Screen, Response, Object.Name, Spreadsheet..correct_attention_choice) %>% 
  filter(Screen == 'attention_parallel_select' | Screen == 'attention_button_select') %>% 
  mutate(Response = case_when(Object.Name == 'Response' ~ Response, .default = substr(Object.Name, nchar(Object.Name), nchar(Object.Name)))) %>%
  filter(Response != Spreadsheet..correct_attention_choice);

incorrect_attention_participants <- incorrect_attention %>% select(Participant.Public.ID) %>% unique();

# get data to qc from incorrect_attention_participants
data_to_qc <- data[data$Participant.Public.ID %in% incorrect_attention_participants$Participant.Public.ID,];

# REMOVE inattentive participants from corr data
data <- data[!data$Participant.Public.ID %in% incorrect_attention_participants$Participant.Public.ID,];

# drop rows < 1000ms
data <- data %>% filter(Absolute.Reaction.Time > 1000);

# array of participants
participants <- data$Participant.Public.ID %>% unique();

# extract the events we're interested in
vt_data <- data %>% filter(Screen == 'pcp_vt_stimulus', Response.Type == 'response', Response != '');

# keep just the required cols
vt_est_cols = c(
  'Participant.Public.ID',
  'Absolute.Reaction.Time',
  'Response',
  'Spreadsheet..correct_answer',
  'Spreadsheet..group_no',
  'Spreadsheet..sample_size',
  'Spreadsheet..spacing',
  'Spreadsheet..set_no',
  'Trial.Number'
);

if (is_transformed_results_scale) {
  vt_est_cols <- append(vt_est_cols, c('Spreadsheet..refactored_correct_answer'))
}

vt_data <- vt_data %>% select(all_of(vt_est_cols));

# clean column names
colnames(vt_data) <- gsub('Spreadsheet\\.{2}', '', colnames(vt_data))

# add a column for accuracy (abs distance to answer)
vt_data$accuracy <- abs(as.numeric(vt_data$correct_answer) - as.numeric(vt_data$Response));

# add a column for binary accuracy
vt_data$accuracy_bin <- sapply(vt_data$accuracy, FUN = function(i) { if (i <= 0) { 1 } else { 0 }});

# add col for order of trial in size/group
vt_data <- vt_data %>% group_by(Participant.Public.ID, sample_size, group_no) %>% arrange(Trial.Number, .by_group = TRUE) %>% mutate(trial_num_by_size_group = 1:n());

if (is_transformed_results_scale) {
  vt_data$accuracy <- abs(as.numeric(vt_data$refactored_correct_answer) - as.numeric(vt_data$Response));
}

# accuracy fuzz
# allow for slight errors due to visually estimating correct value
accuracy_fuzz <- 2;
vt_data$accuracy_bin_fuzzed <- sapply(vt_data$accuracy, FUN = function(i) { if (i <= accuracy_fuzz) { 1 } else { 0 }});

# munging to factors
vt_data <- vt_data %>% mutate(
  spacing = as.factor(spacing), 
  sample_size = as.factor(sample_size), 
  Participant.Public.ID = factor(Participant.Public.ID), # note the use of factor() rather than as.factor here - odd errors later otherwise
  participant_id = Participant.Public.ID
);

# recode spacing
vt_data <- vt_data %>% mutate(
  spacing = recode(
    spacing,
    '30' = '0.25',
    '60' = '0.5',
    '120' = '1',
    '240' = '2',
    '480' = '4'
  )
);

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

###
# MISC CHECKING
###

### checks for extremities in response
getSizeSpacingData <- function(data, size_level, spacing_level) {
  d <- data %>% filter(sample_size == size_level, spacing == spacing_level) %>% arrange(accuracy);
  return(d);
};

####
# modelling response relationships
####

# create a summary from vt_data to get the proportion correct as a column
vt_data_props_correct <- vt_data %>% group_by(spacing, sample_size) %>% summarise(prop_accuracy_bin = sum(accuracy_bin_fuzzed) / n());
# setup position_dodge for use in plots later
pd <- position_dodge(width = 0.25);
pd2 <- position_dodge(width = 0.5);

## binomial
# simple model with all params but no interactions
vt_data_binomial <- vt_data %>% mutate(accuracy_bin_fuzzed = accuracy_bin_fuzzed, spacing = factor(spacing), sample_size = sample_size);

bin_accuracy <- glmmTMB(
  accuracy_bin_fuzzed ~ spacing + sample_size + (1|Participant.Public.ID) + (1|trial_num_by_size_group), 
  data = vt_data_binomial,
  family = 'binomial'
);
summary(bin_accuracy);
bin_accuracy_predicted <- ggpredict(bin_accuracy, terms = c('spacing', 'sample_size'));
p_accuracy_simple <- plot(bin_accuracy_predicted, dodge = 0.25);
p_accuracy_simple + geom_point(aes(y = vt_data_props_correct$prop_accuracy_bin), position = pd, shape = 'diamond open');

# ggplot for better error bars and log scale on x
p_accuracy <- ggplot(bin_accuracy_predicted, aes(x = x, y = predicted * 100, colour = group)) +
  geom_point(position = pd) +
  geom_errorbar(
    aes(ymin = conf.low * 100, ymax = conf.high * 100),
    position = pd
  ) +
  scale_x_discrete(
    labels = c(-2, -1, 0, 1, 2),
    name = 'log2 aspect ratio'
  );

ggsave(p_accuracy, filename = 'vt_p_accuracy.png')

# all params and interaction between spacing and sample_size
bin_accuracy_interaction <- glmmTMB(
  accuracy_bin_fuzzed ~ spacing*sample_size + (1|Participant.Public.ID) + (1|trial_num_by_size_group), 
  data = vt_data_binomial,
  family = 'binomial',
  na.action = na.fail
);
summary(bin_accuracy_interaction);

# marginal effects predictions
# see https://strengejacke.github.io/ggeffects/reference/ggpredict.html for examples
bin_accuracy_interaction_predicted <- ggpredict(bin_accuracy_interaction, terms = c('spacing', 'sample_size'));
plot(bin_accuracy_interaction_predicted, dodge = 0.25) + geom_point(aes(y = vt_data_props_correct$prop_accuracy_bin), position = pd, shape = 'diamond open');

shapes_for_means <- c('pred' = 19, 'real' = 9);
p_accuracy_interaction <- ggplot(bin_accuracy_interaction_predicted, aes(x = x, y = predicted, colour = group)) +
  scale_color_brewer(palette = 'Set2') +
  geom_point(aes(shape = 'pred'), position = pd) +
  geom_linerange(
    aes(ymin = conf.low, ymax = conf.high),
    position = pd
  ) +
  geom_point(
    aes(y = vt_data_props_correct$prop_accuracy_bin, shape = 'real'), 
    position = pd
  ) +
  scale_y_continuous(
    labels = scales::percent
  ) +
  scale_x_discrete(
    labels = c(-2, -1, 0, 1, 2),
  ) +
  scale_shape_manual(name = 'Means', breaks = c('pred', 'real'), values = shapes_for_means, labels = c('Predicted (inc CI95)', 'Real')) +
  labs(x = 'Aspect ratio (log scale)', y = 'Proportion of accurate responses (%)', colour = 'Sample size') +
  theme(text = element_text(family = "Times New Roman"));

p_accuracy_interaction;

ggsave(
  p_accuracy_interaction, 
  filename = paste0(tbl_output_dir, 'vt_p_accuracy_interaction.png'),
  width = 2400,
  height = 1400,
  units = 'px'
);

####
# model summary table for supplementary
###
p_bin_accuracy_interaction_summary <- model_table(bin_accuracy_interaction, plot_title = 'Task B - Binomial model', save_as = 'ptbl_vt_bin_accuracy_interaction', tbl_output_dir = tbl_output_dir)

### end ob model summary table

# aicc comparison
bin_models <- list(bin_accuracy_interaction, bin_accuracy);
bin_models.names <- c('bin_accuracy_interaction', 'bin_accuracy');

aictab(cand.set = bin_models, modnames = bin_models.names);
# anova comparison
anova(bin_accuracy, bin_accuracy_interaction);

## model validation
# residuals
plot(density(residuals(bin_accuracy_interaction)));
plot(density(residuals(bin_accuracy)));

# using DHARMa - scaled residuals
library(DHARMa);
res_sim <- simulateResiduals(fittedModel = bin_accuracy_interaction, plot = F);
plot(res_sim);

# run MuMIn dredge to fit all possible submodels
library(MuMIn);
# note need to set na.action = na.fail in model above
MuMIn::dredge(bin_accuracy_interaction);
