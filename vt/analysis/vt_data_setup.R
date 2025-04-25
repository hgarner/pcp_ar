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

# run the analysis of the participant demographic data
source('participants_analysis.R');

# histogram of lower part of response time distribution
vt_data %>% 
  filter(Absolute.Reaction.Time < 10000) %>% 
  ggplot(data = .) + aes(x = Absolute.Reaction.Time) + 
  geom_histogram(binwidth = 25) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 102));

# save rows with response < 1000ms to new df for later reporting
vt_data_toofast <- vt_data %>% filter(Absolute.Reaction.Time <= 1000);
vt_data_toofast %>% summarise(count = n(), participants = n_distinct(Participant.Public.ID));
# drop rows < 1000ms
# (no rows to drop)
vt_data <- vt_data %>% filter(Absolute.Reaction.Time > 1000);

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