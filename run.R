library(batchtools)
library(mlr3oml)
library(mlr3)
library(mlr3learners)
library(mlr3pipelines)
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

  future::plan("multicore", workers = 10L)  # we parallelize the resampling manually

  task = instance$task$clone(deep = TRUE)
  resampling = instance$resampling$clone(deep = TRUE)

  learner = lrn("classif.glmnet")
  learner$predict_type = "prob"
  gl = GraphLearner$new(ppl("robustify", task = task, learner = learner, impute_missings = TRUE, factors_to_numeric = TRUE) %>>% learner)
  search_space = ps(
    classif.glmnet.alpha = p_dbl(lower = 0, upper = 1),
    classif.glmnet.s = p_dbl(lower = -7, upper = 7, tags = "log", trafo = function(x) exp(x))
  )
  terminator = trm("evals", n_evals = 100L)

  xs = list(...)

  tuner = xs$tuner$clone(deep = TRUE)

  # FIXME: report for OptimizerDesignPoints "Error: Please set design datatable!"
  if (xs$tuner_id == "design") {
    design = generate_design_lhs(search_space, 100L)$data
    instance = TuningInstanceSingleCrit$new(
      task = task,
      learner = gl,
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
    learner = gl,
    resampling = resampling,
    measure = msr("classif.logloss"),
    search_space = search_space,
    terminator = terminator
  )

  tuner$optimize(instance)
  instance$archive
}

reg = makeExperimentRegistry(file.dir = "/gscratch/lschnei8/registry_ela_newdata", packages = c("mlr3", "mlr3learners", "mlr3pipelines", "mlr3tuning", "data.table", "mlr3misc", "paradox", "checkmate"))
#reg = makeExperimentRegistry(file.dir = NA, packages = c("mlr3", "mlr3learners", "mlr3pipelines", "mlr3tuning", "data.table", "mlr3misc", "paradox", "checkmate"))
saveRegistry(reg)

# tasks and instances
openml_ids = c(40983, 469, 41156, 6332, 23381)
tasks = setNames(sapply(openml_ids, function(id) {
  task = tsk("oml", data_id = id)
}), nm = as.character(openml_ids))

instances = data.table(openml_id = openml_ids)

# add problems
prob_designs = map(seq_len(nrow(instances)), function(i) {
  seed = instances[i, ]$openml_id
  set.seed(seed)
  resampling = rsmp("repeated_cv", folds = 10L, repeats = 10L)
  task = tasks[[as.character(instances[i, ]$openml_id)]]$clone(deep = TRUE)
  resampling$instantiate(task)
  prob_id = as.character(instances[i, ]$openml_id)
  addProblem(prob_id, data = list(task = task, resampling = resampling, seed = seed), seed = seed)
  setNames(list(instances[i, ]), nm = prob_id)
})
nn = sapply(prob_designs, names)
prob_designs = unlist(prob_designs, recursive = FALSE, use.names = FALSE)
names(prob_designs) = nn

# add algorithm
addAlgorithm("eval_", fun = eval_)

tuners = list(
  random_search = tnr("random_search"),
  grid_search = tnr("grid_search"),
  cmaes = tnr("cmaes"),
  gensa = tnr("gensa"),
  #irace = tnr("irace"),  # NOTE: fix log file if we want to include this optimizer
  mbo = tnr("mbo"),
    # all default, e.g.
    # surrogate uses lrn("regr.km", covtype = "matern3_2", optim.method = "gen") with fallback and encapsulate
    # acq_function is AcqFunctionEI
    # acq_optimizer is random search with 1000 evals
    # see default_surrogate, default_acqfun, default_acqopt
  design = tnr("design_points")
)

for (tuner_id in names(tuners)) {
  tuner = tuners[[tuner_id]]
  ids = addExperiments(
    prob.designs = prob_designs,
    algo.designs = list(eval_ = data.table(tuner_id = tuner_id, tuner = list(tuner))),
    repls = 10L  # NOTE: we also replicate grid search and lhs, we can use this to check the variance of the point estimate
  )
  addJobTags(ids, tuner_id)
}

jobs = findJobs()
# jobs need 10 cpus each so we can parallelize the resampling manually
resources.default = list(walltime = 3600L * 24L, memory = 2 * 1024L, ntasks = 1L, ncpus = 10L, nodes = 1L, clusters = "teton", max.concurrent.jobs = 1000L)
submitJobs(jobs, resources = resources.default)

tab = getJobTable()
results = map_dtr(names(tuners), function(tuner_id) {
  if (tuner_id != "irace") {
    ids = tab[grepl(tuner_id, x = tags)]$job.id
    results = reduceResultsDataTable(ids, fun = function(x, job) {
      data = x$data[, c("classif.glmnet.alpha", "classif.glmnet.s", "classif.logloss", "batch_nr")]
      data[, method := tuner_id]
      data[, best_logloss := cummin(classif.logloss)]
      data[, task := job$instance$task$id]
      data[, repl := job$repl]
      data
    })
    do.call(rbind, results$result)
  }
})
saveRDS(results, "/gscratch/lschnei8/ela_newdata_results.rds")

