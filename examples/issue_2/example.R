# From
# https://github.com/SteveBronder/pathfinder_testing/issues/2

library(dplyr)
library(brms)
set_cmdstan_path('./cmdstan')
library(cmdstanr)

# generate some data
N <- 10000

data0 <-
  tibble(
    x1 = rnorm(N, 0, 1),
    x2 = rnorm(N, 0, 1),
    x3 = rnorm(N, 0, 1),
    x4 = rnorm(N, 0, 1),
    x5 = rnorm(N, 0, 1),
    y_star = rlogis(N, 1*x1 + 0.7*x2 + 0.5*x3 + 0.3*x4 + 0.1*x5, 1),
    y = ifelse(y_star>0,1,0)
  )

glm_sum = summary(glm(y ~ x1 + x2 + x3 + x4 + x5, data = data0, family = binomial(link = "logit")))

# define model using brm
bf0 <- bf(y ~ x1 + x2 + x3 + x4 + x5) + bernoulli(link = "logit")

# make native stan code from brm objects and save the code as stan object
nthreads = 12
my_stan_code <- make_stancode(
  formula = bf0,
  data = data0,
  #prior = prior0,
  autocor = NULL,
  data2 = NULL,
  cov_ranef = NULL,
  sparse = NULL,
  sample_prior = "no",
  stanvars = NULL,
  stan_funs = NULL,
  knots = NULL,
  #threads = threading(threads = nthreads),
  normalize = getOption("brms.normalize", TRUE),
  save_model = './examples/issue_2/issue_2.stan'
)

# make a native stan data structure from brm objects
my_stan_data <- make_standata(
  formula = bf0,
  data = data0,
  family = bernoulli(link = "logit"),
  #prior = prior0,
  autocor = NULL,
  data2 = NULL,
  cov_ranef = NULL,
  sample_prior = "no",
  stanvars = NULL,
  #threads = threading(threads = nthreads),
  knots = NULL
)

# create cmdstan model object
ex_mod = cmdstan_model(stan_file = './examples/issue_2/issue_2.stan',
  compile = FALSE)

# compile
ex_mod$compile(
  quiet = FALSE,
  dir = NULL,
  pedantic = TRUE,
  cpp_options = list(stan_threads = TRUE),
  force_recompile = TRUE
)

# NUTS (HMC)
ex_fit_sample = ex_mod$sample(data = my_stan_data,
  parallel_chains = 3,
  iter_sampling = 1000,
  chains = 3,
  threads_per_chain=4,
  output_dir = './examples/issue_2/sample_csvs/')


# meanfield variational inference
ex_fit_vb <- ex_mod$variational(
  data = my_stan_data,
  seed = NULL,
  refresh = 5,
  init = NULL,
  save_latent_dynamics = FALSE,
  output_dir = './examples/issue_2/vi_csvs/',
  output_basename = 'psc_variational',
  sig_figs = NULL,
  threads = nthreads,
  opencl_ids = NULL,
  algorithm = NULL,
  iter = 1000,
  grad_samples = NULL,
  elbo_samples = NULL,
  eta = NULL,
  adapt_engaged = NULL,
  adapt_iter = NULL,
  tol_rel_obj = 0.0000001,
  eval_elbo = NULL,
  output_samples = 10000
)

# lbfgs optimization
ex_optimize = ex_mod$optimize(data = my_stan_data,
  refresh = 5,
  algorithm = "lbfgs",
  threads = nthreads,
  iter = 1000,
  history_size = 12,
  init_alpha = 0.0000001,
  init = 3,
  output_dir = './examples/issue_2/optimize_csvs/',
  output_basename = 'psc_lbfgs_opt'
)#, tol_obj = 0, tol_grad = 0, tol_param = 0, tol_rel_grad = 0, tol_rel_obj = 0)


# Run path and sampler
# FIXME: idk why but this needs +1 num_draws for the number of draws you actually want
ex_fit_single = ex_mod$pathfinder(algorithm = "single",
  data = my_stan_data,
  refresh = 1,
  threads = nthreads,
  iter = 1000,
  num_elbo_draws = 100,
  history_size = 2,
#  init_alpha = 0.0000001,
  num_draws = 10001,
  init = 0,
  output_dir = './examples/issue_2/single_path_csvs/',
  output_basename = 'psc_single'
)#, tol_obj = 0, tol_grad = 0, tol_param = 0, tol_rel_grad = 0, tol_rel_obj = 0)

# Run path and sampler
ex_fit_multi = ex_mod$pathfinder(algorithm = "multi",
  data = my_stan_data,
  refresh = 1,
  threads = 16,
  num_paths = 12,
  psis_draws = 10000,
  iter = 1000,
  num_elbo_draws = 1000,
  history_size = 12,
  init_alpha = 0.0000001,
  num_draws = 10000,
  init = 3,
  output_dir = './examples/issue_2/multi_path_csvs/',
  output_basename = 'psc_multi'
)#, tol_obj = 0, tol_grad = 0, tol_param = 0, tol_rel_grad = 0, tol_rel_obj = 0)

write_stan_json(my_stan_data, file = "./examples/issue_2/ex.json")
ex_fit_sample$summary()
ex_fit_vb$summary()
ex_optimize$summary()

# problems with optimization or convergence
ex_fit_single$summary()
ex_fit_multi$summary()
