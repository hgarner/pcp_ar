####
# modelling response relationships
####

###
# ordered beta regression
###
# adjust iterations
control <- glmmTMBControl(optCtrl=list(iter.max=1e3,eval.max=1e3));

# ordered beta regression for bounded continuous dist (accuracy_signed_normalised)
###
# full model with all expected interactions
###
ob_accuracy_int_factor <- glmmTMB(
  accuracy_signed_normalised ~ fisher_z_factor*spacing_factor*sample_size_factor + (1|Participant.Public.ID) + (1|trial_num_by_size_corr), 
  data = corr_data,
  family = 'ordbeta'
);

ob_accuracy_int_factor <- update(ob_accuracy_int_factor, control = glmmTMBControl(optCtrl=list(iter.max=1e3,eval.max=1e3)));

summary(ob_accuracy_int_factor);

corr_data <- corr_data %>% mutate(fisher_z_factor_r = relevel(fisher_z_factor, '0'), spacing_factor_r = relevel(spacing_factor, '1'));

ob_accuracy_int_factor_reref <- glmmTMB(
  accuracy_signed_normalised ~ fisher_z_factor_r*spacing_factor_r*sample_size_factor + (1|Participant.Public.ID) + (1|trial_num_by_size_corr), 
  data = corr_data,
  family = 'ordbeta'
);

ob_accuracy_int_factor_reref <- update(ob_accuracy_int_factor_reref, control = glmmTMBControl(optCtrl=list(iter.max=1e3,eval.max=1e3)));
summary(ob_accuracy_int_factor_reref);

ob_accuracy_int_factor_predicted <- ggpredict(ob_accuracy_int_factor, terms = c('fisher_z_factor', 'spacing_factor', 'sample_size_factor'));
plot(ob_accuracy_int_factor_predicted);

ob_accuracy_int_factor_predicted_reref <- ggpredict(ob_accuracy_int_factor_reref, terms = c('fisher_z_factor_r', 'spacing_factor_r', 'sample_size_factor'));
plot(ob_accuracy_int_factor_predicted_reref);

###
# full model - test inclusion of expt no
###
# add in expt_no as random effect to account for variance between groups
# note that there could be within-group effects here that may be better accounted for with a different nested structure
ob_accuracy_int_factor_expt_no <- glmmTMB(
  accuracy_signed_normalised ~ fisher_z_factor*spacing_factor*sample_size_factor + (1|Participant.Public.ID) + (1|trial_num_by_size_corr) + (1|expt_no), 
  data = corr_data,
  family = 'ordbeta'
);

ob_accuracy_int_factor_expt_no <- update(ob_accuracy_int_factor_expt_no, control = glmmTMBControl(optCtrl=list(iter.max=1e3,eval.max=1e3)));

summary(ob_accuracy_int_factor_expt_no);

# no meaningful difference:
anova(ob_accuracy_int_factor, ob_accuracy_int_factor_expt_no);

#####
# reduced models - fewer interactions
#####

###
# drop one element at a time
###
# drop 3 way interaction
ob_accuracy_no_3way_int_factor <- update(ob_accuracy_int_factor, . ~ . - fisher_z_factor:spacing_factor:sample_size_factor);

# drop fisher_z:spacing
ob_accuracy_no_z_spacing_int_factor <- update(ob_accuracy_int_factor, . ~ . - fisher_z_factor:spacing_factor);

# drop spacing:sample_size
ob_accuracy_no_spacing_sample_int_factor <- update(ob_accuracy_int_factor, . ~ . - spacing_factor:sample_size_factor);

# drop fisher_z:sample_size
ob_accuracy_no_z_sample_int_factor <- update(ob_accuracy_int_factor, . ~ . - fisher_z_factor:sample_size_factor);

# drop fisher_z
ob_accuracy_no_z_factor <- update(ob_accuracy_int_factor, . ~ . - fisher_z_factor);

# drop spacing
ob_accuracy_no_spacing_factor <- update(ob_accuracy_int_factor, . ~ . - spacing_factor);

# drop sample_size
ob_accuracy_no_sample_factor <- update(ob_accuracy_int_factor, . ~ . - sample_size_factor);

###
# drop full set of interactions (2-way & 3-way)
###
# drop the interaction with sample size
ob_accuracy_z_spacing_int_factor <- glmmTMB(
  accuracy_signed_normalised ~ fisher_z_factor*spacing_factor + sample_size_factor + (1|Participant.Public.ID) + (1|trial_num_by_size_corr), 
  data = corr_data,
  family = 'ordbeta',
  control = control
);

summary(ob_accuracy_z_spacing_int_factor);

ob_accuracy_z_spacing_int_factor_predicted <- ggpredict(ob_accuracy_z_spacing_int_factor, terms = c('fisher_z_factor', 'spacing_factor', 'sample_size_factor'));
plot(ob_accuracy_z_spacing_int_factor_predicted);

# drop fisher_z and spacing
ob_accuracy_spacing_sample_int_factor <- glmmTMB(
  accuracy_signed_normalised ~ fisher_z_factor + spacing_factor * sample_size_factor + (1|Participant.Public.ID) + (1|trial_num_by_size_corr), 
  data = corr_data,
  family = 'ordbeta',
  control = control
);

###
# no interactions at all
###
# drop all interactions
ob_accuracy_noint <- glmmTMB(
  accuracy_signed_normalised ~ fisher_z_factor + spacing_factor + sample_size_factor + (1|Participant.Public.ID) + (1|trial_num_by_size_corr), 
  data = corr_data,
  family = 'ordbeta',
  control = control
);

###
# simple model
###
# drop all interactions
ob_accuracy_simple_z <- glmmTMB(
  accuracy_signed_normalised ~ relevel(fisher_z_factor, ref = '0'), 
  data = corr_data,
  family = 'ordbeta',
  control = control
);

# store in models list
ob_models <- list(
  ob_accuracy_int_factor = ob_accuracy_int_factor,
  ob_accuracy_no_3way_int_factor = ob_accuracy_no_3way_int_factor,
  ob_accuracy_no_z_sample_int_factor = ob_accuracy_no_z_sample_int_factor,
  ob_accuracy_no_z_spacing_int_factor = ob_accuracy_no_z_spacing_int_factor,
  ob_accuracy_no_spacing_sample_int_factor = ob_accuracy_no_spacing_sample_int_factor,
  ob_accuracy_no_z_factor = ob_accuracy_no_z_factor,
  ob_accuracy_no_spacing_factor = ob_accuracy_no_spacing_factor,
  ob_accuracy_no_sample_factor = ob_accuracy_no_sample_factor,
  ob_accuracy_reduced_z_spacing_int_factor = ob_accuracy_z_spacing_int_factor,
  ob_accuracy_reduced_spacing_sample_int_factor = ob_accuracy_spacing_sample_int_factor,
  ob_accuracy_noint = ob_accuracy_noint
);

###
# model comparison
###
anova(
  ob_models$ob_accuracy_int_factor, 
  ob_models$ob_accuracy_no_3way_int_factor, 
  ob_models$ob_accuracy_reduced_z_spacing_int_factor, 
  ob_models$ob_accuracy_reduced_spacing_sample_int_factor, 
  ob_models$ob_accuracy_noint
);

###
# random effects testing
###

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

summary(ob_accuracy_int_factor_norandomp_notrial);

anova(ob_accuracy_int_factor, ob_accuracy_int_factor_norandomp, ob_accuracy_int_factor_norandomp_notrial);

####
# model summary table for supplementary
###
p_ob_accuracy_int_factor_summary <- model_table(ob_accuracy_int_factor, plot_title = 'Task A - Response accuracy model summary (GLMM, ordered beta)', save_as = 'ptbl_ob_accuracy_int_factor', tbl_output_dir = tbl_output_dir)

### end ob model summary table

# ordered beta but using the response_normalised - this has the advantage of having the same maxima and minima for
# every real correlation factor
# BUT this results in the output being less readable - the over/under point will be different for every real corr value
ob_response_int <- glmmTMB(
  response_normalised ~ fisher_z_factor*spacing_factor*sample_size_factor + (1|Participant.Public.ID) + (1|trial_num_by_size_corr), 
  data = corr_data,
  family = 'ordbeta'
);

summary(ob_response_int);

ob_response_int_predicted <- ggpredict(ob_response_int, terms = c('fisher_z_factor', 'spacing_factor', 'sample_size_factor'));
plot(ob_response_int_predicted);

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
shapes_for_means <- c('pred' = 19, 'real' = 9);

p_ob_accuracy_int_real <- as.data.frame(ob_accuracy_int_factor_predicted) %>% mutate(predicted = (predicted * 6) - 3, conf.low = (conf.low * 6) - 3, conf.high = (conf.high * 6) - 3) %>%
  ggplot(data = ., aes(x = x, colour = group)) + 
  scale_color_brewer(palette = 'Set2') +
  labs(x = 'Fisher-z (real)', y = 'Predicted distance response to actual (Fisher-z)', colour = 'Aspect ratio') +
  geom_point(aes(y = predicted, shape = 'pred'), position = pd2) + geom_linerange(aes(ymin = conf.low, ymax = conf.high), position=pd2) + 
  geom_point(aes(y = (corr_data_mean_response$mean_accuracy_signed_normalised * 6) - 3, shape = 'real'), position = pd2) +
  scale_shape_manual(name = 'Means', breaks = c('pred', 'real'), values = shapes_for_means, labels = c('Predicted (inc CI95)', 'Real')) +
  facet_grid(rows = c(40, 160)) +
  #ggtitle('Predicted mean accuracy (with CI95) by set size') +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
  theme(text = element_text(family = "Times New Roman"));

ggsave(
  p_ob_accuracy_int_real, 
  filename = paste0(tbl_output_dir, 'corr_p_ob_accuracy_int_real.png'),
  width = 2400,
  height = 2400,
  units = 'px'
);

# table of ob predicted values and conf ints for supplementary
ob_accuracy_int_predicted_tab <- as.data.frame(ob_accuracy_int_factor_predicted) %>%
  mutate(predicted = (predicted * 6) - 3, conf.low = (conf.low * 6) - 3, conf.high = (conf.high * 6) - 3) %>%
  select(-std.error) %>%
  arrange(facet, x, group) %>%
  gt() %>%
  tab_header(
    title = md("Task A - predicted marginal effects - response scale")
  ) %>%
  cols_move(
    c(x, group, predicted, conf.low, conf.high),
    after = facet
  ) %>%
  cols_label(
    .list = list(
      facet = 'Sample size', 
      x = 'Fisher-z', 
      group = 'Aspect ratio',
      predicted = html('Predicted<br> response'), 
      conf.low = 'Conf. (low)',
      conf.high = 'Conf. (high)'
    )
  ) %>%
  fmt_number(
    columns = c(predicted, conf.high, conf.low),
    decimals = 2
  )

gtsave(ob_accuracy_int_predicted_tab, , filename = paste0(tbl_output_dir, 'ptbl_corr_ob_accuracy_int_predicted_tab', '.tex'));

## response
# plot of response_normalised (i.e. the actual value given NOT the distance (accuracy))
p_ob_response_int_real <- as.data.frame(ob_response_int_predicted) %>% mutate(predicted = (predicted * 3) - 1.5, conf.low = (conf.low * 3) - 1.5, conf.high = (conf.high * 3) - 1.5) %>%
  ggplot(data = ., aes(x = x, colour = group)) + 
  scale_color_brewer(palette = 'Set2') +
  labs(x = 'Fisher-z (real)', y = 'Predicted response (Fisher-z)', colour = 'Aspect ratio') +
  geom_point(aes(y = predicted, shape = 'pred'), position = pd2) + geom_linerange(aes(ymin = conf.low, ymax = conf.high), position=pd2) + 
  geom_point(aes(y = corr_data_mean_response$mean_response / 2, shape = 'real'), position = pd2) +
  scale_shape_manual(name = 'Means', breaks = c('pred', 'real'), values = shapes_for_means, labels = c('Predicted (inc CI95)', 'Real')) +
  facet_grid(rows = c(40, 160)) +
  #ggtitle('Predicted mean accuracy (with CI95) by set size') +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 20)) +
  theme(text = element_text(family = "Times New Roman"));

## response
pd <- position_dodge(width = 0.25);
pd2 <- position_dodge(width = 0.5);

###
# END ordered beta
###