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
library(R6)

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

  xs = list(...)
  tuner = xs$tuner$clone(deep = TRUE)
  factor = 50L
  terminator = trm("evals", n_evals = factor * dim)

  instance = TuningInstanceSingleCrit$new(
    task = task,
    learner = learner,
    resampling = resampling,
    measure = msr("classif.logloss"),
    search_space = search_space,
    terminator = terminator
  )

  tuner$optimize(instance)
  instance$archive
}

reg = makeExperimentRegistry(file.dir = "/gscratch/lschnei8/registry_ela_newdata_gensa", packages = c("mlr3", "mlr3learners", "mlr3pipelines", "bbotk", "mlr3mbo", "mlr3tuning", "data.table", "mlr3misc", "paradox", "checkmate"))
#reg = makeExperimentRegistry(file.dir = NA, packages = c("mlr3", "mlr3learners", "mlr3pipelines", "bbotk", "mlr3mbo", "mlr3tuning", "data.table", "mlr3misc", "paradox", "checkmate"))
saveRegistry(reg)

# tasks and instances
openml_ids = c(40983, 41156, 40975)
tasks = setNames(sapply(openml_ids, function(id) {
  task = tsk("oml", data_id = id)
  pl = ppl("robustify", task = task, impute_missings = TRUE, factors_to_numeric = TRUE)
  task = pl$train(task)[[1L]]
  task
}), nm = as.character(openml_ids))
names(openml_ids) = map_chr(tasks, "id")
instances = setDT(expand.grid(openml_id = openml_ids, learner_id = "xgboost", dim = 2L))

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

OptimizerGenSAx = R6Class("OptimizerGenSAx", inherit = bbotk::Optimizer,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      param_set = ps(
        smooth = p_lgl(default = TRUE),  # FIXME: probably switch this to FALSE
        temperature = p_dbl(default = 5230),
        visiting.param = p_dbl(default = 2.62),
        acceptance.param = p_dbl(default = -5),
        simple.function = p_lgl(default = FALSE),
        verbose = p_lgl(default = FALSE),
        trace.mat = p_lgl(default = FALSE)
      )
      super$initialize(
        param_set = param_set,
        param_classes = "ParamDbl",
        properties = "single-crit",
        packages = "GenSA"
      )
    }
  ),

  private = list(
    .optimize = function(inst) {
      v = self$param_set$values
      v$maxit = .Machine$integer.max  # make sure GenSA does not stop
      v$nb.stop.improvement = .Machine$integer.max   # make sure GenSA does not stop
      GenSA::GenSA(par = NULL, fn = inst$objective_function,
        lower = inst$search_space$lower, upper = inst$search_space$upper,
        control = v)
    }
  )
)

TunerGenSAx = R6Class("TunerGenSAx",
  inherit = mlr3tuning::TunerFromOptimizer,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      super$initialize(
        optimizer = OptimizerGenSAx$new()
      )
    }
  )
)

gensa_1 = TunerGenSAx$new()
gensa_1$param_set$values$smooth = TRUE
gensa_1$param_set$values$simple.function = TRUE
gensa_2 = TunerGenSAx$new()
gensa_2$param_set$values$smooth = FALSE
gensa_2$param_set$values$simple.function = TRUE
gensa_3 = TunerGenSAx$new()  # default
gensa_3$param_set$values$smooth = TRUE
gensa_3$param_set$values$simple.function = FALSE
gensa_4 = TunerGenSAx$new()
gensa_4$param_set$values$smooth = FALSE
gensa_4$param_set$values$simple.function = FALSE

tuners = list(
  gensa_1 = gensa_1,
  gensa_2 = gensa_2,
  gensa_3 = gensa_3,
  gensa_4 = gensa_4
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
jobs[, walltime := 3600L * 6L]
resources.default = list(memory = 4096, ntasks = 1L, ncpus = 1L, nodes = 1L, clusters = "teton", max.concurrent.jobs = 1000L)
submitJobs(jobs, resources = resources.default)



# collect results
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
saveRDS(results, "/gscratch/lschnei8/ela_newdata_gensa.rds")

