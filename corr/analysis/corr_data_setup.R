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

# participant demographics
source('./participants_analysis.R');

# histogram of lower part of response time distribution
corr_data %>% 
  filter(Absolute.Reaction.Time < 5000) %>% 
  ggplot(data = .) + aes(x = Absolute.Reaction.Time) + 
  geom_histogram(binwidth = 25) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 102));

# save rows with response < 850ms to new df for later reporting
corr_data_toofast <- corr_data %>% filter(Absolute.Reaction.Time <= 850);
corr_data_toofast %>% summarise(count = n(), participants = n_distinct(Participant.Public.ID));
# drop rows < 850ms
corr_data <- corr_data %>% filter(Absolute.Reaction.Time > 850);

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

# experiment number columns - 0 for first set with 30/120/480 spacing, 1 for second set with 60/120/240 spacing
expt_2_participants <- corr_data %>% filter(Spreadsheet..spacing == 60) %>% select(Participant.Public.ID) %>% unique()
corr_data <- corr_data %>% mutate(expt_no = ifelse(Participant.Public.ID %in% expt_2_participants$Participant.Public.ID, 1, 0))

## add a trial_num_by_size_corr
corr_data <- corr_data %>% group_by(Participant.Public.ID, sample_size_factor, fisher_z) %>% arrange(Trial.Number, .by_group = TRUE) %>% mutate(trial_num_by_size_corr = 1:n());

# mean response
corr_data_mean_response <- corr_data %>% group_by(fisher_z, spacing_factor, sample_size_factor) %>% summarise(mean_response = sum(Response) / n(), mean_accuracy_signed_normalised = sum(accuracy_signed_normalised) / n());

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

