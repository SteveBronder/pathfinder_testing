library(cmdstanr)

pp = function(extra = NULL) {paste0("./cmdstan/", extra)}

cmdstanr::set_cmdstan_path(pp())

mod1 = cmdstanr::cmdstan_model(pp("examples/bernoulli/bernoulli.stan"))

fit1 = mod1$pathfinder(algorithm = "multi", data = pp("examples/bernoulli/bernoulli.data.R"),
  refresh = 1, num_threads = 12, num_paths = 12, psis_draws = 2000)

fit2 = mod1$sample(data = pp("examples/bernoulli/bernoulli.data.R"), chains = 2)

fit1$summary()
fit2$summary()

fit1_theta = as.data.frame(posterior::as_draws_df(fit1$draws()))[, 1:2, drop=FALSE]
fit2_theta = as.data.frame(posterior::as_draws_df(fit2$draws()))[, 1:2, drop=FALSE]
nrow(fit1_theta)
nrow(fit2_theta)
colnames(fit1_theta) = paste0("path_", colnames(fit1_theta))
colnames(fit2_theta) = paste0("samp_", colnames(fit2_theta))
plot(cbind(fit1_theta, fit2_theta))
