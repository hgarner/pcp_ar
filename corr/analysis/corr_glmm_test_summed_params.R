####
# evaluating parameters for factor level combinations
####

# where to store outputs
tbl_output_dir = paste0(getwd(), '/plots/');

# full model and predicted
full_model <- ob_accuracy_int_factor

full_model_predicted <- ggpredict(full_model, terms = c('fisher_z_factor', 'spacing_factor', 'sample_size_factor'))
plot(full_model_predicted)

###
# regression testing of factor level combinations
###

# use the predicted values rather than just the summed coefficients as these include variability from random effects

## run a regression for each level of fisher_z_factor (intercept only)
full_model_predicted <- as.data.frame(full_model_predicted) %>% 
  rename(fisher_z_factor = x, spacing_factor = group, sample_size_factor = facet) %>%
  # transform predicted back to the fisher_z scale
  mutate(predicted_trf = (predicted * 6) - 3, conf.low_trf = (conf.low * 6) - 3, conf.high_trf = (conf.high * 6) - 3) %>%
  # transform predicted back to the link scale
  mutate(predicted_link = qlogis(predicted))

# split data into a list of dataframes by fisher_z_factor and sample_size_factor
full_model_predicted_split <- full_model_predicted %>% split(list(.$fisher_z_factor, .$sample_size_factor))

# fit intercept-only regression models to each dataframe in the list
results_list <- lapply(full_model_predicted_split, function(df) {
  model <- lm(predicted ~ 1, data = df)
  print(paste(df$fisher_z_factor, df$sample_size_factor))
  print(summary(model)$sigma)
  conf_int <- confint(model)
  return(data.frame(
    fisher_z_factor = unique(df$fisher_z_factor),
    sample_size_factor = unique(df$sample_size_factor),
    intercept = coef(model)[1],
    #variance = vcov(model)[1, 1], # this is variance of estimate of intercept
    variance = summary(model)$sigma^2, # variance of the residuals
    conf.low = conf_int[1, 1],
    conf.high = conf_int[1, 2]
  ))
})

# fit weighted intercept-only regression models to each dataframe in the list
# note that this must be on the link scale as std.error is not transformed to the response scale by ggpredict
results_list_weighted <- lapply(full_model_predicted_split, function(df) {
  weights <- 1 / (df$std.error^2) # use inverse variance as weights
  model <- lm(predicted_link ~ 1, data = df, weights = weights)
  conf_int <- confint(model)
  return(data.frame(
    fisher_z_factor = unique(df$fisher_z_factor),
    sample_size_factor = unique(df$sample_size_factor),
    intercept = coef(model)[1],
    #variance = vcov(model)[1, 1], # this is variance of estimate of intercept
    variance = summary(model)$sigma^2, # variance of the residuals
    conf.low = conf_int[1, 1],
    conf.high = conf_int[1, 2]
  ))
})

# recombine results into a single dataframe
results <- bind_rows(results_list)

View(results)

results_weighted <- bind_rows(results_list_weighted)

View(results_weighted)

# f-test of variances
# compare variance at each fisher_z_factor level
# note that we use the weighted model here
pairwise_f_results <- lapply(unique(results_weighted$sample_size_factor), function(size_factor) {
  results_subset <- results_weighted %>% filter(sample_size_factor == size_factor)
  pairwise_chisq <- combn(results_subset$fisher_z_factor, 2, function(pair) {
    var_1 <- results_subset %>% filter(fisher_z_factor == pair[1]) %>% pull(variance)
    var_2 <- results_subset %>% filter(fisher_z_factor == pair[2]) %>% pull(variance)
    #df1 <- length(full_model_predicted %>% filter(fisher_z_factor == pair[1], sample_size_factor == size_factor)) - 1
    #df2 <- length(full_model_predicted %>% filter(fisher_z_factor == pair[2], sample_size_factor == size_factor)) - 1
    df1 <- 4 # spacing_factor has 5 levels
    df2 <- 4
    var_direction <- 0
    if (var_1 > var_2) {
      f_stat <- (var_1 / var_2)
    } else {
      f_stat <- (var_2 / var_1)
      var_direction <- 1
    }
    p_value <- pf(f_stat, df1, df2, lower.tail = FALSE)
    return(data.frame(
      sample_size_factor = size_factor, 
      fisher_z_factor_1 = pair[1], 
      fisher_z_factor_2 = pair[2], 
      f_stat = f_stat, 
      p_value = p_value,
      var_direction = var_direction
    ))
  }, simplify = FALSE) %>% bind_rows()
  return(pairwise_chisq)
}) %>% bind_rows()

# print pairwise f results
print(pairwise_f_results)


# f-test results summary table
sig_val = 0.05 #/ pairwise_f_results %>% distinct(fisher_z_factor_1, fisher_z_factor_2) %>% dim(.) %>% .[1]
assign_stars <- function(p) {
  case_when(
    p < sig_val / 100 ~ "***",  # Highly significant
    p < sig_val / 10  ~ " **",   # Significant
    p < sig_val  ~ "  *",    # Marginally significant
    TRUE      ~ "    "      # Not significant
  )
}

# take a var direction (0/1) and return text to describe in context
var_direction_text <- function(var_direction) {
  case_when(
    var_direction == 0 ~ 'V1 > V2',
    TRUE ~ 'V2 > V1'
  )
}

pairwise_f_results_table <- pairwise_f_results %>% 
  mutate(
    stars = assign_stars(p_value), 
    var_direction_txt = var_direction_text(var_direction)
  ) %>% 
  select(-var_direction) %>% 
  group_by(sample_size_factor) %>%
  mutate(sample_size_factor = paste0('Sample size: ', sample_size_factor)) %>%
  gt() %>%
  tab_header(
    title = md("F-test of predicted variance by sample size and real Fisher-z values")
  ) %>%
  tab_options(row_group.as_column = FALSE) %>%
  cols_label(
    .list = list(
      sample_size_factor = 'Sample size', 
      fisher_z_factor_1 = 'Fisher-z 1', 
      fisher_z_factor_2 = 'Fisher-z 2', 
      f_stat = 'F stat', 
      var_direction_txt = 'Direction',
      p_value = 'p-value',
      stars = ''
    )
  ) %>%
  fmt_number(
    columns = c(f_stat),
    decimals = 1
  ) %>%
  fmt_passthrough(
    columns = c(var_direction_txt)
  ) %>%
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_body(
      columns = stars,
    )
  ) %>%
  cols_label(
    p_value = "P-value"
  ) %>%
  fmt_scientific(
    columns = c(p_value),
    decimals = 1
  #) %>% cols_merge(
  #  columns = c(p_value, stars),
  #  pattern = "{1} {2}"
  )


pairwise_f_results_table

pairwise_f_results_table %>%
  gtsave(filename = paste0(tbl_output_dir, 'corr_ob_model_var_test_summary.tex'))

###
# regression model - fit to slope by spacing_factor
# i.e. testing if rate of change of accuracy by fisher_z varies by spacing_factor
###
# split data into a list of dataframes by sample_size_factor
full_model_predicted_split_size <- full_model_predicted %>% split(list(.$sample_size_factor))

# fit regression models to each dataframe in the list
slope_results_list <- lapply(full_model_predicted_split_size, function(df) {
  cont_df <- df %>% 
    mutate(fisher_z = as.numeric(levels(fisher_z_factor))[fisher_z_factor]) %>% 
    mutate(fisher_z_group = ifelse(fisher_z < 0 & sample_size_factor == '160', 0, 1))
  model <- lm(predicted ~ spacing_factor + fisher_z + fisher_z_group, data = cont_df)
  model_int <- lm(predicted ~ spacing_factor + fisher_z + fisher_z_group + spacing_factor:fisher_z, data = cont_df)
  conf_int <- confint(model)
  print(paste(unique(cont_df$sample_size_factor), ':'))
  print(summary(model))
  print(summary(model_int))
  lm_chisq <- as.numeric(2 * (logLik(model_int) - logLik(model)))
  print(coef(model_int))
  interaction_coefs <- coef(model_int)[grep('^spacing_factor[0-9.]+:fisher_z$', names(coef(model_int)))]
  return(data.frame(
    sample_size_factor = unique(df$sample_size_factor),
    coefs = interaction_coefs %>% t() %>% as.data.frame(),
    lm_chisq = lm_chisq,
    p_value = 1 - pchisq(lm_chisq, length(unique(df$spacing_factor)) - 1)
    #variance = vcov(model)['fisher_z', 'fisher_z'],
  ))
})

# recombine results into a single dataframe
slope_results <- bind_rows(slope_results_list)

View(slope_results)

## fit model on sp 40 and all spacing factors
size_40_by_spacing_data <- full_model_predicted %>% filter(sample_size_factor == '40') %>% 
  mutate(fisher_z = as.numeric(levels(fisher_z_factor))[fisher_z_factor]) %>%
  mutate(spacing = as.numeric(levels(spacing_factor))[spacing_factor])

size_40_by_spacing_lm <- lm(predicted ~ spacing_factor + fisher_z, data = size_40_by_spacing_data)

size_40_by_spacing_lm_int <- lm(predicted ~ spacing_factor + fisher_z + spacing_factor:fisher_z, data = size_40_by_spacing_data)

size_40_by_spacing_lm_chisq <- 2 * (as.numeric(logLik(size_40_by_spacing_lm_int)) - as.numeric(logLik(size_40_by_spacing_lm)))
size_40_by_spacing_chisq_p <- 1 - pchisq(as.numeric(size_40_by_spacing_lm_chisq), 4)
# sig evidence that varies by aspect ratio

# test for interaction with spacing factor as continuous
size_40_by_spacing_lm_int_cont <- lm(predicted ~ spacing_factor + fisher_z + spacing:fisher_z, data = size_40_by_spacing_data)

size_40_by_spacing_lm_chisq <- 2 * (as.numeric(logLik(size_40_by_spacing_lm_int)) - as.numeric(logLik(size_40_by_spacing_lm)))
size_40_by_spacing_chisq_p <- 1 - pchisq(as.numeric(size_40_by_spacing_lm_chisq), 4)

size_40_by_spacing_lm_cont_chisq <- 2 * (as.numeric(logLik(size_40_by_spacing_lm_int_cont)) - as.numeric(logLik(size_40_by_spacing_lm)))
size_40_by_spacing_cont_chisq_p <- 1 - pchisq(as.numeric(size_40_by_spacing_lm_cont_chisq), 1)
# sig evidence that varies in a consistent way by aspect ratio

summary(size_40_by_spacing_lm)
summary(size_40_by_spacing_lm_int)
size_40_by_spacing_lm_chisq
size_40_by_spacing_chisq_p

## fit model on sp 160 and all spacing factors
# include term for different intercept for all spacing_factor >= 0
size_160_by_spacing_data <- full_model_predicted %>% filter(sample_size_factor == '160') %>% 
  mutate(fisher_z = as.numeric(levels(fisher_z_factor))[fisher_z_factor]) %>%
  mutate(fisher_z_group = ifelse(fisher_z < 0 & sample_size_factor == '160', 0, 1))

size_160_by_spacing_lm <- lm(predicted ~ spacing_factor + fisher_z_group + fisher_z, data = size_160_by_spacing_data)

# interaction with spacing factor and group (fz >= 0)
size_160_by_spacing_lm_gp_int <- lm(predicted ~ spacing_factor*fisher_z_group + fisher_z , data = size_160_by_spacing_data)

size_160_by_spacing_lm_gp_chisq <- 2 * (logLik(size_160_by_spacing_lm_gp_int) - logLik(size_160_by_spacing_lm))
size_160_by_spacing_gp_chisq_p <- 1 - pchisq(as.numeric(size_160_by_spacing_lm_gp_chisq), 4)
# as spacing factor increases, estimated intercept is going up systematically

# lm for values fz >= 0
size_160_by_spacing_data_subset <- size_160_by_spacing_data %>% filter(fisher_z_group == 1)
# what matters here is diff in intercepts between spacing factors (coef accounts for variability in fisher_z)

# create set of dummy variables (continuous 0/1) with spacing factor
size_160_by_spacing_data_subset_dummy <- size_160_by_spacing_data_subset %>% mutate(
    spacing_factor_0.25 = ifelse(spacing_factor == '0.25', 1, 0),
    spacing_factor_0.5 = ifelse(spacing_factor == '0.5', 1, 0),
    spacing_factor_1 = ifelse(spacing_factor == '1', 1, 0),
    spacing_factor_2 = ifelse(spacing_factor == '2', 1, 0),
    spacing_factor_4 = ifelse(spacing_factor == '4', 1, 0)
  ) %>% 
  mutate(
    spacing_factor_0.25_fz = spacing_factor_0.25 * fisher_z,
    spacing_factor_0.5_fz = spacing_factor_0.5 * fisher_z,
    spacing_factor_1_fz = spacing_factor_1 * fisher_z,
    spacing_factor_2_fz = spacing_factor_2 * fisher_z,
    spacing_factor_4_fz = spacing_factor_4 * fisher_z
  ) %>%
  mutate(
    spacing = as.numeric(levels(spacing_factor))[spacing_factor]
  )

size_160_by_spacing_lm_gp_subset_int <- lm(predicted ~ spacing_factor + fisher_z + spacing_factor:fisher_z, data = size_160_by_spacing_data_subset_dummy)
# check identical to above with dummy vars
size_160_by_spacing_lm_gp_subset_dummy_int <- lm(predicted ~ 
                                               spacing_factor_0.5 +
                                               spacing_factor_1 +
                                               spacing_factor_2 +
                                               spacing_factor_4 +
                                               fisher_z + 
                                               spacing_factor_0.5_fz +
                                               spacing_factor_1_fz +
                                               spacing_factor_2_fz +
                                               spacing_factor_4_fz
                                               , data = size_160_by_spacing_data_subset_dummy)
# as int is cont var, won't dealias
size_160_by_spacing_lm_gp_subset_dummy <- lm(predicted ~ 
                                               fisher_z + 
                                               spacing_factor_0.5_fz +
                                               spacing_factor_1_fz +
                                               spacing_factor_2_fz +
                                               spacing_factor_4_fz
                                             , data = size_160_by_spacing_data_subset_dummy)

size_160_by_spacing_lm_gp_subset_dummy_chisq <- 2 * (logLik(size_160_by_spacing_lm_gp_subset_dummy_int) - logLik(size_160_by_spacing_lm_gp_subset_dummy))
size_160_by_spacing_gp_subset_dummy_chisq_p <- 1 - pchisq(as.numeric(size_160_by_spacing_lm_gp_subset_dummy_chisq), 4)
# shows that intercept diffs between ar with fz >=0 that is highly sig

size_160_by_spacing_lm_gp_subset_dummy_sp_cont <- lm(predicted ~
                                               spacing +
                                               fisher_z + 
                                               spacing_factor_0.5_fz +
                                               spacing_factor_1_fz +
                                               spacing_factor_2_fz +
                                               spacing_factor_4_fz
                                             , data = size_160_by_spacing_data_subset_dummy)
# highly sig spacing terms +ve as ar inc
size_160_by_spacing_lm_gp_subset_dummy_sp_cont_chisq <- 2 * (logLik(size_160_by_spacing_lm_gp_subset_dummy_sp_cont) - logLik(size_160_by_spacing_lm_gp_subset_dummy))
size_160_by_spacing_gp_subset_dummy_sp_cont_chisq_p <- 1 - pchisq(as.numeric(size_160_by_spacing_lm_gp_subset_dummy_sp_cont_chisq), 1)

# int btwn spacing:fisherz
size_160_by_spacing_lm_int <- lm(predicted ~ spacing_factor + fisher_z_group + fisher_z + spacing_factor:fisher_z, data = size_160_by_spacing_data)

size_160_by_spacing_lm_chisq <- 2 * (logLik(size_160_by_spacing_lm_int) - logLik(size_160_by_spacing_lm))
size_160_by_spacing_chisq_p <- 1 - pchisq(as.numeric(size_160_by_spacing_lm_chisq), 4)

summary(size_160_by_spacing_lm)
summary(size_160_by_spacing_lm_int)
size_160_by_spacing_lm_chisq
size_160_by_spacing_chisq_p

#####################
# Extract model summaries
summary_40 <- tidy(size_40_by_spacing_lm_int) %>% mutate(sample_size_factor = '40')
summary_160 <- tidy(size_160_by_spacing_lm_int) %>% mutate(sample_size_factor = '160')

# Combine data and group by sample size factor
table_data <- bind_rows(summary_40, summary_160) %>%
  select(sample_size_factor, term, estimate, std.error, p.value)

# Add Chi-Square and P-Value rows
table_data <- table_data %>%
  bind_rows(
    tibble(
      sample_size_factor = "40",
      term = paste0("Aspect ratio : Fisher-z interaction chisq ", round(size_40_by_spacing_lm_chisq, 3), ", p-value ", size_40_by_spacing_chisq_p),
      estimate = NA, std.error = NA, p.value = NA
    ),
    tibble(
      sample_size_factor = "160",
      term = paste0("Aspect ratio : Fisher-z interaction chisq ", round(size_160_by_spacing_lm_chisq, 3), ", p-value ", size_160_by_spacing_chisq_p),
      estimate = NA, std.error = NA, p.value = NA
    )
  )

# Generate grouped gt table
table_data %>%
  gt() %>%
  tab_header(title = "Chi-Square and Model Summary Table") %>%
  cols_label(
    sample_size_factor = "Aspect ratio",
    term = "Term",
    estimate = "Estimate",
    std.error = "Standard Error",
    p.value = "P-Value (Term)"
  ) %>%
  tab_row_group(
    group = "Sample size 40",
    rows = sample_size_factor == "40"
  ) %>%
  tab_source_note(
    source_note = paste0("Aspect ratio : Fisher-z interaction chisq ", round(size_40_by_spacing_lm_chisq, 3), ", p-value ", size_40_by_spacing_chisq_p)
  ) %>%
  tab_row_group(
    group = "Sample size 160",
    rows = sample_size_factor == "160"
  ) %>% 
  tab_source_note(
    source_note = paste0("Aspect ratio : Fisher-z interaction chisq ", round(size_160_by_spacing_lm_chisq, 3), ", p-value ", size_160_by_spacing_chisq_p)
  )

############## 
# quick plots
#############
full_model_predicted <- full_model_predicted %>%
  mutate(fisher_z = as.numeric(levels(fisher_z_factor))[fisher_z_factor])

ggplot(full_model_predicted %>% filter(sample_size_factor == 40), aes(x = fisher_z, y = predicted, color = spacing_factor)) +
  
  # Confidence intervals for predicted values as vertical lines
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.1, alpha = 0.7) +
  
  # Points for ggpredict predicted values
  geom_point(size = 2, alpha = 0.8) +
  
  # Linear regression fit with confidence interval ribbons
  geom_smooth(method = "lm", formula = y ~ x, aes(fill = spacing_factor, linetype = spacing_factor), se = TRUE, size = 1, alpha = 0.1) +
  
  # Formatting
  labs(title = "Size 40 - Model Predictions with Linear Fit Confidence Intervals",
       x = "Fisher Z (Numeric)",
       y = "Predicted Accuracy",
       color = "Spacing Factor",
       fill = "Spacing Factor",
       linetype = "Fitted LM") +
  
  theme_minimal()

ggplot(full_model_predicted %>% filter(sample_size_factor == 160), aes(x = fisher_z, y = predicted, color = spacing_factor)) +
  
  # Confidence intervals for predicted values as vertical lines
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.1, alpha = 0.7) +
  
  # Points for ggpredict predicted values
  geom_point(size = 2, alpha = 0.8) +
  
  # Linear regression fit with confidence interval ribbons
  geom_smooth(method = "lm", formula = y ~ x, aes(fill = spacing_factor, linetype = spacing_factor), se = TRUE, size = 1, alpha = 0.1) +
  
  # Formatting
  labs(title = "Size 160 - Model Predictions with Linear Fit Confidence Intervals",
       x = "Fisher Z (Numeric)",
       y = "Predicted Accuracy",
       color = "Spacing Factor",
       fill = "Spacing Factor",
       linetype = "Fitted LM") +
  
  theme_minimal()


################### 
#### TO REMOVE - TESTING ONLY ###

test_lm <- full_model_predicted %>% filter(sample_size_factor == 40, fisher_z_factor == 1.5) %>% lm(predicted ~ 1, data = .)
summary(test_lm)

tetest_lm_slope <- full_model_predicted %>% filter(sample_size_factor == 160, spacing_factor == 4) %>% 
  mutate(fisher_z = as.numeric(levels(fisher_z_factor))[fisher_z_factor]) %>% 
  mutate(fisher_z_group = ifelse(fisher_z < 0 & sample_size_factor == '160', 0, 1)) %>%
  lm(predicted ~ fisher_z_group + fisher_z, data = .)
summary(test_lm_slope)
coef(test_lm_slope)

#### END TO REMOVE ####
