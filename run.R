library(batchtools)
library(mlr3oml)
library(mlr3)
library(mlr3learners)
library(mlr3pipelines)
library(bbotk)
library(mlr3mbo)  # @coco
library(mlr3tuning)
library(data.table)
library(mlr3misc)
library(paradox)
library(checkmate)
library(lgr)

# NOTE: each task has a fixed seed
# NOTE: resamplings are instantiated for tasks
# NOTE: tuners have their seeds fixed via batchtools job seed
RhpcBLASctl::blas_set_num_threads(1L)
RhpcBLASctl::omp_set_num_threads(1L)

# generic eval wrapper for using a tuner
eval_ = function(job, data, instance, ...) {
  logger = lgr::get_logger("mlr3")
  logger$set_threshold("warn")
  logger = lgr::get_logger("bbotk")
  logger$set_threshold("warn")

  future::plan("sequential")

  task = instance$task$clone(deep = TRUE)
  resampling = instance$resampling$clone(deep = TRUE)
  learner_id = instance$learner_id
  dim = instance$dim

  if (learner_id == "svm") {
    stopifnot(dim == 2L)
    learner = lrn("classif.svm")
    learner$predict_type = "prob"
    learner$param_set$values$type = "C-classification"
    learner$param_set$values$kernel = "radial"
    search_space = ps(
      cost = p_dbl(lower = -10, upper = 10, tags = "log", trafo = function(x) exp(x)),
      gamma = p_dbl(lower = -10, upper = 10, tags = "log", trafo = function(x) exp(x))
    )
  } else if (learner_id == "xgboost") {
    stopifnot(dim %in% c(2L, 3L, 5L))
    learner = lrn("classif.xgboost")
    learner$predict_type = "prob"
    learner$param_set$values$booster = "gbtree"
    search_space = if (dim == 2L) {
      ps(
        nrounds = p_dbl(lower = log(3), upper = log(2000), tags = c("int", "log"), trafo = function(x) as.integer(round(exp(x)))),
        eta = p_dbl(lower = -7, upper = 0, tags = "log", trafo = function(x) exp(x))
      )
    } else if (dim == 3L) {
      ps(
        nrounds = p_dbl(lower = log(3), upper = log(2000), tags = c("int", "log"), trafo = function(x) as.integer(round(exp(x)))),
        eta = p_dbl(lower = -7, upper = 0, tags = "log", trafo = function(x) exp(x)),
        lambda = p_dbl(lower = -7, upper = 7, tags = "log", trafo = function(x) exp(x))
      )
    } else if (dim == 5L) {
      ps(
        nrounds = p_dbl(lower = log(3), upper = log(2000), tags = c("int", "log"), trafo = function(x) as.integer(round(exp(x)))),
        eta = p_dbl(lower = -7, upper = 0, tags = "log", trafo = function(x) exp(x)),
        lambda = p_dbl(lower = -7, upper = 7, tags = "log", trafo = function(x) exp(x)),
        gamma = p_dbl(lower = -10, upper = 2, tags = "log", trafo = function(x) exp(x)),
        alpha = p_dbl(lower = -7, upper = 7, tags = "log", trafo = function(x) exp(x))
      )
    }
  }

  xs = list(...)
  tuner = xs$tuner$clone(deep = TRUE)
  factor = if (xs$tuner_id == "random_search") {
    400L
  } else {
    50L
  }
  terminator = trm("evals", n_evals = factor * dim)

  # FIXME: report for OptimizerDesignPoints "Error: Please set design datatable!"
  if (xs$tuner_id == "design") {
    design = generate_design_lhs(search_space, n = factor * dim)$data
    instance = TuningInstanceSingleCrit$new(
      task = task,
      learner = learner,
      resampling = resampling,
      measure = msr("classif.logloss"),
      search_space = search_space,
      terminator = terminator
    )
    instance$eval_batch(design)
    return(instance$archive)
  }

  instance = TuningInstanceSingleCrit$new(
    task = task,
    learner = learner,
    resampling = resampling,
    measure = msr("classif.logloss"),
    search_space = search_space,
    terminator = terminator
  )

  if (xs$tuner_id == "grid_search") {
    tuner$param_set$values$resolution = ceiling((50L * dim) ^ (1 / dim))
  }

  if (xs$tuner_id == "mbo") {
    upper = (instance$search_space$upper - instance$search_space$lower) / sqrt(instance$search_space$length)
    lower = upper / 100
    learner = lrn("regr.km", covtype = "matern5_2", upper = upper, lower = lower, multistart = 3L, optim.method = "BFGS")
    learner$fallback = lrn("regr.km", covtype = "matern5_2", upper = upper, lower = lower, multistart = 3L, optim.method = "BFGS", nugget.stability = 10^-8)
    surrogate = default_surrogate(instance, learner = learner)
    tuner[[".__enclos_env__"]][["private"]][[".optimizer"]][["surrogate"]] = surrogate  # FIXME: report this and fix this
  }

  tuner$optimize(instance)
  instance$archive
}

reg = makeExperimentRegistry(file.dir = "/gscratch/lschnei8/registry_ela_newdata_large", packages = c("mlr3", "mlr3learners", "mlr3pipelines", "bbotk", "mlr3mbo", "mlr3tuning", "data.table", "mlr3misc", "paradox", "checkmate"))
#reg = makeExperimentRegistry(file.dir = NA, packages = c("mlr3", "mlr3learners", "mlr3pipelines", "bbotk", "mlr3mbo", "mlr3tuning", "data.table", "mlr3misc", "paradox", "checkmate"))
saveRegistry(reg)

# tasks and instances
openml_ids = c(40983, 469, 41156, 6332, 23381, 1590, 1461, 40975, 41146, 40685)
tasks = setNames(sapply(openml_ids, function(id) {
  task = tsk("oml", data_id = id)
  pl = ppl("robustify", task = task, impute_missings = TRUE, factors_to_numeric = TRUE)
  task = pl$train(task)[[1L]]
  task
}), nm = as.character(openml_ids))
names(openml_ids) = map_chr(tasks, "id")
#instances = setDT(rbind(expand.grid(openml_id = openml_ids, learner_id = "svm", dim = 2L), expand.grid(openml_id = openml_ids, learner_id = "xgboost", dim = c(2L, 3L, 5L))))
instances = setDT(expand.grid(openml_id = openml_ids, learner_id = "xgboost", dim = c(2L, 3L, 5L)))


# add problems
prob_designs = map(seq_len(nrow(instances)), function(i) {
  seed = instances[i, ]$openml_id
  set.seed(seed)
  resampling = rsmp("cv", folds = 10L)  # NOTE: this used to be 10 times 10 fold
  task = tasks[[as.character(instances[i, ]$openml_id)]]$clone(deep = TRUE)
  resampling$instantiate(task)
  learner_id = as.character(instances[i, ]$learner_id)
  dim = instances[i, ]$dim
  prob_id = paste0(as.character(instances[i, ]$openml_id), "_", learner_id, "_", dim)
  addProblem(prob_id, data = list(task = task, resampling = resampling, learner_id = learner_id, dim = dim, seed = seed), seed = seed)
  setNames(list(instances[i, ]), nm = prob_id)
})
nn = sapply(prob_designs, names)
prob_designs = unlist(prob_designs, recursive = FALSE, use.names = FALSE)
names(prob_designs) = nn

# add algorithm
addAlgorithm("eval_", fun = eval_)

mbo = TunerMbo$new(loop_function = bayesopt_ego, acq_function = AcqFunctionEI$new(), acq_optimizer = AcqOptimizer$new(opt("global_local"), terminator = trm("none")))

tuners = list(
  random_search = tnr("random_search"),
  grid_search = tnr("grid_search"),
  cmaes = tnr("cmaes"),
  gensa = tnr("gensa"),
  #irace = tnr("irace"),  # NOTE: fix log file if we want to include this optimizer
  mbo = mbo,
  design = tnr("design_points")
)

for (tuner_id in names(tuners)) {
  tuner = tuners[[tuner_id]]
  ids = addExperiments(
    prob.designs = prob_designs,
    algo.designs = list(eval_ = data.table(tuner_id = tuner_id, tuner = list(tuner))),
    repls = 10L  # NOTE: we also replicate grid search (and lhs), we can use this to check the variance of the point estimate
  )
  addJobTags(ids, tuner_id)
}

tab = getJobTable()
jobs = data.table(job.id = tab$job.id, dim = map_dbl(tab$prob.pars, function(x) x$dim), tags = tab$tags)
jobs[, walltime := 3600L * 24L * 7L]
jobs[tags != "random_search", walltime := walltime * (dim / 5L)]
resources.default = list(memory = 16384, ntasks = 1L, ncpus = 1L, nodes = 1L, clusters = "teton", max.concurrent.jobs = 1000L)
submitJobs(jobs, resources = resources.default)

expired = jobs[job.id %in% findExpired()$job.id]
resources.large = list(memory = 8048L, ntasks = 1L, ncpus = 1L, nodes = 1L, clusters = "teton", max.concurrent.jobs = 1000L)
submitJobs(expired, resources = resources.large)

# FIXME:
tab = getJobTable()
tab = tab[job.id %in% findDone()$job.id]
results = reduceResultsDataTable(tab$job.id, fun = function(x, job) {
  data = x$data
  data[, method := job$algo.pars$tuner_id]
  data[, best_logloss := cummin(classif.logloss)]
  data[, task := job$instance$task$id]
  data[, dim := job$instance$dim]
  data[, repl := job$repl]
  data
})
results = rbindlist(results$result, fill = TRUE)
saveRDS(results, "/gscratch/lschnei8/ela_newdata_large_results.rds")

# runtime
openml_ids = as.character(c(40983, 469, 41156, 6332, 23381, 1590, 1461, 40975, 41146, 40685))
names(openml_ids) = c("wilt", "analcatdata_dmft", "ada", "cylinder-bands", "dresses-sales", "adult", "bank-marketing", "car", "sylvine", "shuttle"     )
tab = getJobTable()  # 3.172658 CPU years
tab = tab[job.id %in% findDone()$job.id]
tab[, method := map_chr(algo.pars, function(x) x$tuner_id)]
tab[, task_id := map_chr(problem, function(x) strsplit(x, "_")[[1L]][1L])]
tab[, dim := map_chr(problem, function(x) strsplit(x, "_")[[1L]][3L])]
tab[, task := map_chr(task_id, function(x) names(openml_ids)[which(openml_ids == x)])]
tab[, problem := paste0(task, "_", dim)]

saveRDS(tab[, c("method", "dim", "task", "problem", "repl", "time.running")], "/gscratch/lschnei8/ela_newdata_large_jobs.rds")

