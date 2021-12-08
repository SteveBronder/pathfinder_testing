library(cmdstanr)
library(posteriordb)
library(ggplot2)
library(ggExtra)
library(data.table)

pp = function(extra = NULL) {paste0("./cmdstan/", extra)}

cmdstanr::set_cmdstan_path(pp())

# NOTE: This is going to fail eventually if we ping it too many times
# To increase the github rate limit follow the instruction at [1] to get
#  a github PAT (Personal Authentication Token) token.
#  Then add that token to your R environment variables with something like
# echo "Sys.setenv(GITHUB_PAT='PASTE_YOUR_TOKEN_HERE')" >> ~/.Rprofile
# You do not need to give the token any auths
# and make sure you do not accidentally share it that would be very very bad!!!
# [1] https://www.edgoad.com/2021/02/using-personal-access-tokens-with-git-and-github.html
pdb_conn <- pdb_github()

pdb_names = posterior_names(pdb_conn)

# We will do HMM-example, though the below should work with all the others

ex_posterior = posteriordb::posterior("hmm_example-hmm_example", pdb_conn)
ex_code_path = model_code_file_path(ex_posterior, framework = "stan")
ex_mod = cmdstanr::cmdstan_model(ex_code_path)
ex_data_file_path = data_file_path(ex_posterior)

# Run path and sampler
ex_fit = ex_mod$pathfinder(algorithm = "multi", data = ex_data_file_path,
  refresh = 5, num_threads = floor(parallel::detectCores() / 2), num_paths = 5,
  psis_draws = 4000, iter = 100)

ex_fit_sample = ex_mod$sample(data = ex_data_file_path,
  parallel_chains = 4)


# Have a lookie

ex_fit$summary()
ex_fit_sample$summary()

ex_draws = as.data.frame(posterior::as_draws_df(ex_fit$draws()))
ex_sample_draws = as.data.frame(posterior::as_draws_df(ex_fit_sample$draws()))

orig_names = colnames(ex_draws)
colnames(ex_draws) = paste0("path_", colnames(ex_draws))
colnames(ex_sample_draws) = paste0("samp_", colnames(ex_sample_draws))

ex_all_df = as.data.table(cbind(ex_draws, ex_sample_draws))

for (base_param_name in orig_names) {
  param_names = c(paste0(c("path_", "samp_"), base_param_name), "samp_lp__")
  param_dt = ex_all_df[, ..param_names]
  axis_limits = c(param_dt[, min(get(param_names[1]), get(param_names[2]))],
    param_dt[, max(get(param_names[1]), get(param_names[2]))])
  base_plot = ggplot(param_dt,
    aes_string(x = paste0("`", param_names[1], "`"),
      y = paste0("`", param_names[2], "`"), color = "samp_lp__")) +
    geom_point() +
    theme_bw() +
    xlim(axis_limits) + ylim(axis_limits) +
    ggtitle(paste0("Comparison of ", base_param_name))
  marg_plot = ggMarginal(base_plot, type = "histogram", xparams = list(bins=50), fill = "red")
  if (interactive()) {
    print(marg_plot)
    user_inp = readline(prompt="Press [enter] to continue or enter q to quit")
    if (user_inp == "q") {
      break
    }
  } else {
    print("Idk how to show graphs with Rscript :-(")
    break
  }
}

