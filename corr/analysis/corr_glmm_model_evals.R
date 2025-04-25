#####
# Model evaluations - residuals testing etc
#####

###
# residuals checks
###
# using DHARMa - scaled residuals
library(DHARMa);
res_sim_ob <- simulateResiduals(fittedModel = ob_accuracy_int_factor, plot = F);
plot(res_sim_ob);

res_sim_ob_no_3way <- simulateResiduals(fittedModel = ob_accuracy_no_3way_int_factor, plot = F);
plot(res_sim_ob_no_3way);

###
# corr - response models - influence
###
library(influence.ME);

###
# influence for ob_accuracy_int_factor
###

# influence in glmmTMB - requires other methods
# see https://cran.r-project.org/web/packages/glmmTMB/vignettes/model_evaluation.pdf
source(system.file("other_methods","influence_mixed.R", package="glmmTMB"));

ob_accuracy_int_factor_influence_time <- system.time(
  ob_accuracy_int_factor_influence <- influence_mixed(ob_accuracy_int_factor, groups="Participant.Public.ID")
);
#car::infIndexPlot(ob_accuracy_int_factor_influence);
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