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
    labels = c(0.25, 0.5, 1, 2, 4),
  ) +
  scale_shape_manual(name = 'Means', breaks = c('pred', 'real'), values = shapes_for_means, labels = c('Predicted (inc CI95)', 'Real')) +
  labs(x = 'Aspect ratio', y = 'Probability of accurate responses (%)', colour = 'Sample size') +
  theme(text = element_text(family = "Times New Roman"));

p_accuracy_interaction;

ggsave(
  p_accuracy_interaction, 
  filename = paste0(tbl_output_dir, 'vt_p_accuracy_interaction.png'),
  width = 2400,
  height = 1400,
  units = 'px'
);

# table of predicted values and conf ints for supplementary
bin_accuracy_interaction_predicted_tab <- as.data.frame(bin_accuracy_interaction_predicted) %>%
  #mutate(predicted = (predicted * 6) - 3, conf.low = (conf.low * 6) - 3, conf.high = (conf.high * 6) - 3) %>%
  select(-std.error) %>%
  arrange(x, group) %>%
  gt() %>%
  tab_header(
    title = md("Task B - predicted marginal effects - response scale")
  ) %>%
  cols_move(
    c(group, predicted, conf.low, conf.high),
    after = x
  ) %>%
  cols_label(
    .list = list(
      group = 'Sample size', 
      x = 'Aspect ratio',
      predicted = html('Predicted<br> probability'), 
      conf.low = 'Conf. (low)',
      conf.high = 'Conf. (high)'
    )
  ) %>%
  fmt_number(
    columns = c(predicted, conf.high, conf.low),
    decimals = 2
  )

gtsave(bin_accuracy_interaction_predicted_tab, , filename = paste0(tbl_output_dir, 'ptbl_bin_accuracy_interaction_predicted_tab', '.tex'));

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

# model with no random fx
bin_accuracy_interaction_norandom <- update(bin_accuracy_interaction, . ~ . - (1|Participant.Public.ID) - (1|trial_num_by_size_group));
bin_accuracy_interaction_norandomt <- update(bin_accuracy_interaction, . ~ . - (1|trial_num_by_size_group));
# anova comparison
anova(bin_accuracy_interaction_norandom, bin_accuracy_interaction_norandomt, bin_accuracy_interaction);
