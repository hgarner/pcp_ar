# data setup including qc
source('./corr_data_setup.R')
# skillings mack tests
source('./corr_skillings_mack.R')
# data summary plots
source('./corr_data_summaries_plots.R')
# ordered beta glmm fitting
source('./corr_glmm_models.R')
# model coefficients extraction
source('./corr_glmm_full_model_coefs.R')
# ordered beta glmm - inference
source('./corr_glmm_test_summed_params.R')
# glmm evaluation (residuals etc)
# warning - this can take some time to run so is commented out by default
#source('./corr_glmm_model_evals.R')
