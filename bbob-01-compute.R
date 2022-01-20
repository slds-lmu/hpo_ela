# ==== libraries ====

# mlr3verse
library(bbotk)
library(mlr3verse)
library(mlr3mbo)  # @coco branch

# library(smoof) #

# tidyverse
library(dplyr)
library(tidyr)

# future
library(future)
library(future.apply)

# ==== main experiment function ====

run_bbob_experiments <- function(fids, iids, dims, repls, optimizers) {
  
  problems <- expand_grid(
    fid = fids,
    iid = iids,
    repl = repls,
    dim = dims,
    optimizer_id = names(optimizers)
  )
  
  p <- progressr::progressor(steps = nrow(problems))
  
  future_lapply(1L:nrow(problems), function(p_id) {
    
    # ==== Setup ====
    
    library(mlr3verse)
    
    problem <- problems[p_id,]
    fn <- smoof::makeBBOBFunction(dimensions = problem$dim, fid = problem$fid, iid = problem$iid)
    
    seed <- 1e8 * problem$dim + 1e6 * problem$fid + 1e4 * problem$iid + 1e2 * problem$repl + which(names(optimizers) == problem$optimizer_id)
    set.seed(seed)
    
    lgr::get_logger("bbotk")$set_threshold("warn")
    
    p(message = sprintf("FID=%02d, IID=%02d, DIM=%02d, REPL=%02d, OPT=%.3s",
                        problem$fid, problem$iid, problem$dim, problem$repl, problem$optimizer_id))

    # ==== Define Optimization Instance ====
    
    objective <- ObjectiveRFun$new(
      fun = function(xs) return(c(y = fn(xs))),
      domain = ParamSet$new(
        params = lapply(1:problem$dim, function(d) ParamDbl$new(paste0("x", d), lower = -5, upper = 5))
      ),
      codomain = ps(
        y = p_dbl(tags = "minimize")
      ),
      properties = "deterministic"
    )
    
    terminator <- trm("evals", n_evals = 50L * problem$dim)
    
    optim_instance <- OptimInstanceSingleCrit$new(
      objective = objective,
      terminator = terminator
    )
  
    # ==== Run Optimization ====
    
    optimizer <- optimizers[[problem$optimizer_id]]$clone(deep = TRUE)

    if (inherits(optimizer, "OptimizerMbo")) {
      upper = (optim_instance$search_space$upper - optim_instance$search_space$lower) / sqrt(optim_instance$search_space$length)
      lower = upper / 100
      
      learner = lrn("regr.km", covtype = "matern5_2", upper = upper, lower = lower, multistart = 3L, optim.method = "BFGS")
      learner$fallback = lrn("regr.km", covtype = "matern5_2", upper = upper, lower = lower, multistart = 3L, optim.method = "BFGS", nugget.stability = 10^-8)
      
      surrogate = default_surrogate(optim_instance, learner = learner)
      optimizer$surrogate = surrogate
    }

    optimizer$optimize(optim_instance)
    
    archive_data <- optim_instance$archive$data
    cbind(archive_data[,-c("timestamp")], problem)
  }, future.seed = TRUE)
}

# ==== Setup and Run Experiments ====

gc()
plan(multisession, workers = availableCores() - 1)
progressr::handlers("progress")

mbo = opt("mbo",
  loop_function = bayesopt_ego,
  acq_function = AcqFunctionEI$new(),
  acq_optimizer = AcqOptimizer$new(opt("global_local"), terminator = trm("none"))
)

progressr::with_progress({
  run_archives <- run_bbob_experiments(
    fids = 1L:5L,
    iids = 1L,
    dims = 2L,
    repls = 1L:5L,
    optimizers = c(
      random_search = opt("random_search"),
      grid_search = opt("grid_search"),  # FIXME: resolution of params needs to be adjusted
      cmaes = opt("cmaes"),
      gensa = opt("gensa"),
      mbo = mbo
      # design = opt("design_points")
    )
  )
})

# ==== Postprocess and Save Results ====

full_data <- lapply(run_archives, function(arch) {
  arch$best_y <- cummin(arch$y)

  arch
}) %>% Reduce(rbind, .)

readr::write_rds(full_data, "data/bbob_full.Rds")
