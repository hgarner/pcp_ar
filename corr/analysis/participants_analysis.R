library(dplyr);
library(ggplot2);
library(gridExtra);
library(summarytools);
library(magrittr);
# plots and tables
library(tidyr);
library(broom.mixed);
library(gtsummary);
library(gt);
library(webshot2);
library(xtable);

##
# PARTICIPANT DATA
##

##
# NOTE that this is used as part of the analysis, and is not standalone
# as it requires the filtered list of qc'ed participants (see attn q's etc)
##

# input dir
eval_results_dir <- '../results/';

# where to store outputs
tbl_output_dir = paste0(getwd(), '/plots/');

# all participants from Gorilla (inc rejected)
gorilla_participants <- read.csv(paste0(eval_results_dir, 'participants.csv'));

# task results files
eval_results_filenames <- c(
  'live/archive/data_exp_135554-v12_questionnaire-9qs4.csv',
  'live/archive/part_2/data_exp_135554-v15_questionnaire-9qs4.csv',
  'pilot/data_exp_135554-v9_questionnaire-9qs4.csv'
);

is_transformed_results_scale = TRUE;

datasets <- lapply(eval_results_filenames, function(filename) {
  filepath = paste0(eval_results_dir, filename);
  return(read.csv(filepath, sep = '\t'));
});

names(datasets) <- eval_results_filenames;

# compile the data into a single dataframe
participant_data <- bind_rows(datasets, .id = 'src_filename');

# keep just the required cols
pdata_cols = c(
  'Participant.Public.ID',
  'Response',
  'Response.Type',
  'Question',
  'Key'
);

# use the filtered participants from analysis of main task
filtered_participants <- corr_data %>% select(Participant.Public.ID) %>% unique();

# filter out non-included participants
participant_data <- participant_data %>% filter(Participant.Public.ID %in% filtered_participants$Participant.Public.ID);

# drop non-req'd cols
participant_data <- participant_data %>% select(all_of(pdata_cols));

# just keep the descriptive response rows
participant_data <- participant_data %>% filter(Key == 'value' & Response.Type == 'response');

# get the questions
questions = participant_data %>% select(Question) %>% unique() %>% filter(!Question == '');

# experience levels
exp_levels <- c(
  'None',
  '0-2 years',
  '2-5 years',
  '5-10 years',
  '10-20 years',
  '20+ years'
);

# age ranges
age_levels <- c(
  '18-25',
  '25-29',
  '30-39',
  '40-49',
  '50-59',
  '60-69',
  '70-79',
  '80+'
);



# summarise responses to likert scales
participant_question_summary <- participant_data %>% 
  filter(!(Question == '' | Question == 'Age range')) %>% 
  group_by(Question) %>%
  mutate(q_count = n()) %>%
  group_by(Question, Response, .drop = FALSE) %>% 
  summarise(n = n(), p = 100 * (n / max(q_count)));

participant_question_summary;

## PRINT question summary
ptbl_participant_question_summary <- participant_question_summary %>% select(Response, p) %>%
  gt(row_group_as_column = TRUE) %>%
  tab_header(
    title = md("Task A - Participant experience")
  ) %>% 
  cols_label(
    .list = list(p = '% participants')
  ) %>%
  fmt_number(
    columns = c(p),
    decimals = 1
  );
ptbl_participant_question_summary;
filename <- 'ptbl_a_participant_question_summary';
gtsave(ptbl_participant_question_summary, filename = paste0(tbl_output_dir, filename, '.tex'));
gtsave(ptbl_participant_question_summary, filename = paste0(tbl_output_dir, filename, '.html'));
# save to .png
webshot2::webshot(url = paste0(tbl_output_dir, filename, '.html'), file = paste0(tbl_output_dir, filename, '.png'));

# summarise age
participant_age_summary <- participant_data %>% 
  filter(Question == 'Age range') %>% 
  mutate(Response = replace(Response, Response == 'Under 25', '18-25')) %>%
  mutate(Response = factor(Response, levels = age_levels)) %>%
  group_by(Question, Response, .drop = FALSE) %>% 
  summarise(n = n(), p = 100 * (n / nrow(.)));

participant_age_summary;

## PRINT age summary
ptbl_participant_age_summary <- participant_age_summary %>% select(Response, p) %>%
  gt(row_group_as_column = TRUE) %>%
  tab_header(
    title = md("Task A - Participants by age range")
  ) %>% 
  cols_label(
    .list = list(p = '% participants')
  ) %>%
  fmt_number(
    columns = c(p),
    decimals = 1
  );
ptbl_participant_age_summary;
filename <- 'ptbl_a_participant_age_summary';
gtsave(ptbl_participant_age_summary, filename = paste0(tbl_output_dir, filename, '.tex'));
gtsave(ptbl_participant_age_summary, filename = paste0(tbl_output_dir, filename, '.html'));
# save to .png
webshot2::webshot(url = paste0(tbl_output_dir, filename, '.html'), file = paste0(tbl_output_dir, filename, '.png'));

