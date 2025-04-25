model_list <- ob_models

## get all combinations of the factor levels
# factor levels extraction
levels_fisher_z_factor <- levels(corr_data$fisher_z_factor)
levels_spacing_factor <- levels(corr_data$spacing_factor)
levels_sample_size_factor <- levels(corr_data$sample_size_factor)

# create a data frame with all level combinations
level_combinations <- expand.grid(
  fisher_z_factor = levels_fisher_z_factor, 
  spacing_factor = levels_spacing_factor,
  sample_size_factor = levels_sample_size_factor
)

### 
# full model
###
model_full <- ob_models$ob_accuracy_int_factor
## extract the model fixed effects
# pull the model formula from the model itself
full_model_formula_fixef <- as.formula(paste0('~ ', paste(deparse(formula(model_full, fixed.only = TRUE)[[3]]), collapse = '')))
print('full model formula (fixed effects):')
print(full_model_formula_fixef)

# create the design matrix for the combinations
contrast_matrix_full <- model.matrix(full_model_formula_fixef, data = level_combinations)
# show colnames
print('contrast_matrix_full colnames:')
print(colnames(contrast_matrix_full))

# extract model coefficients
summed_coefficients <- fixef(model_full)$cond

# calculate standard errors
se_full <- sqrt(diag(contrast_matrix_full %*% vcov(model_full)$cond %*% t(contrast_matrix_full)))

# compile results
full_model_coefs <- data.frame(
  level_combinations,
  summed_coefficients_full = as.vector(summed_coefficients),
  std_error = se_full
) %>% arrange(sample_size_factor, fisher_z_factor, spacing_factor) %>%
  mutate(
    predicted_response = exp(summed_coefficients_full) / (1 + exp(summed_coefficients_full)),
    predicted_fisher_z_diff = 6 * predicted_response - 3,
  )

print(full_model_coefs)

full_model_coefs_tab <- full_model_coefs %>%
  select(sample_size_factor, fisher_z_factor, spacing_factor, summed_coefficients_full, std_error) %>%
  gt() %>%
  tab_header(
    title = md("Model coefficients - link scale (logit)")
  ) %>%
  cols_label(
    .list = list(
      sample_size_factor = 'Sample size', 
      fisher_z_factor = 'Fisher-z', 
      spacing_factor = 'Aspect ratio',
      summed_coefficients_full = 'Summed coef', 
      std_error = 'Std. error'
    )
  ) %>%
  fmt_number(
    columns = c(summed_coefficients_full),
    decimals = 2
  ) %>%
  fmt_number(
    columns = c(std_error),
    decimals = 4
  ) 

full_model_coefs_tab


