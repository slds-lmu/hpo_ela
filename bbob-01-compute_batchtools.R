library(bbotk)
library(mlr3mbo)  # @coco branch
library(mlr3)
library(mlr3learners)
library(mlr3misc)
library(data.table)
library(batchtools)

# lgr, smoof, paradox needed but not loaded

packages = c("bbotk", "mlr3mbo", "mlr3", "mlr3learners")

reg = makeExperimentRegistry(file.dir = "FIXME:", packages = packages)
# reg = makeExperimentRegistry(file.dir = NA, packages = packages)
saveRegistry(reg)

# generic wrapper for any optimizer
eval_wrapper = function(job, data, instance, ...) {
  dim = instance$dim
  fid = instance$fid
  iid = instance$iid

  xs = list(...)
  optimizer_id = xs$optimizer_id
  
  lgr::get_logger("bbotk")$set_threshold("warn")

  fn = smoof::makeBBOBFunction(dimensions = dim, fid = fid, iid = iid)

  objective = ObjectiveRFun$new(
    fun = function(xs) return(c(y = fn(xs))),
    domain = paradox::ParamSet$new(
      params = lapply(1:dim, function(d) paradox::ParamDbl$new(paste0("x", d), lower = -5, upper = 5))
    ),
    codomain = ps(
      y = p_dbl(tags = "minimize")
    ),
    properties = "deterministic"
  )
  
  terminator = trm("evals", n_evals = 50L * dim)
  
  optim_instance = OptimInstanceSingleCrit$new(
    objective = objective,
    terminator = terminator
  )

  optimizer = if (optimizer_id == "mbo") {
    mbo = opt("mbo",
      loop_function = bayesopt_ego,
      acq_function = AcqFunctionEI$new(),
      acq_optimizer = AcqOptimizer$new(opt("global_local"), terminator = trm("none"))
    )
    upper = (optim_instance$search_space$upper - optim_instance$search_space$lower) / sqrt(optim_instance$search_space$length)
    lower = upper / 100
    learner = lrn("regr.km", covtype = "matern5_2", upper = upper, lower = lower, multistart = 3L, optim.method = "BFGS")
    learner$fallback = lrn("regr.km", covtype = "matern5_2", upper = upper, lower = lower, multistart = 3L, optim.method = "BFGS", nugget.stability = 10^-8)
    surrogate = default_surrogate(optim_instance, learner = learner)
    mbo$surrogate = surrogate
    mbo
  } else {
    opt(optimizer_id)
  }

  optimizer$optimize(optim_instance)
  
  archive_data = optim_instance$archive$data
  cbind(archive_data[, -c("timestamp")], dim = dim, fid = fid, iid = iid, optimizer = optimizer_id)
}

addAlgorithm("eval_wrapper", fun = eval_wrapper)

# setup
setup = setDT(expand.grid(dim = c(2L, 5L, 20L, 40L), fid = 1:5, iid = 1L))  # FIXME: check this again, not sure anymore ...

# add problems
prob_designs = map(seq_len(nrow(setup)), function(i) {
  prob_id = paste0(setup[i, ]$dim, "_", setup[i, ]$fid, "_", paste0(setup[i, ]$iid[[1L]], collapse = "_"))
  addProblem(prob_id, data = list(dim = setup[i, ]$dim, fid = setup[i, ]$fid, iid = setup[i, ]$iid))
  setNames(list(setup[i, ]), nm = prob_id)
})
nn = sapply(prob_designs, names)
prob_designs = unlist(prob_designs, recursive = FALSE, use.names = FALSE)
names(prob_designs) = nn

optimizers = c("random_search", "grid_search", "cmaes", "gensa", "mbo")

for (optimizer_id in optimizers) {
  ids = addExperiments(
    prob.designs = prob_designs,
    algo.designs = list(eval_wrapper = data.table(optimizer_id = optimizer_id)),
    repls = 10L  # FIXME: 10 repls for now?
  )
  addJobTags(ids, optimizer_id)
}

# standard resources used to submit jobs to cluster
# FIXME: adapt walltime and memory usage if needed
resources.serial.default = list(
  walltime = 3600L * 24L, memory = 1024L * 2L, max.concurrent.jobs = 9999L
)

# submit jobs
jobs = findJobs()
submitJobs(jobs, resources = resources.serial.default)

