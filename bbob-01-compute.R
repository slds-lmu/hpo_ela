library(bbotk)
library(mlr3verse)
library(mlr3mbo)

library(smoof)

library(dplyr)
library(tidyr)

lgr::get_logger("bbotk")$set_threshold("warn")

optimizers <- c(
  random_search = opt("random_search"),
  grid_search = opt("grid_search"),
  cmaes = opt("cmaes"),
  gensa = opt("gensa"),
  mbo = opt("mbo")
  # design = opt("design_points")
)

IIDS <- 1L:1L
FIDS <- 1L:24L
N_REPL <- 10L

problems <- expand_grid(fid = FIDS, iid = IIDS, repl = 1L:N_REPL, optimizer_id = names(optimizers))

problems$optim_instance <- lapply(1L:nrow(problems), function(p_id) {
  problem <- problems[p_id,]
  
  fn <- makeBBOBFunction(dimensions = 2L, fid = problem$fid, iid = problem$iid)
  
  domain <- ps(
    x1 = p_dbl(-5, 5),
    x2 = p_dbl(-5, 5)
  )
  codomain <- ps(
    y = p_dbl(tags = "minimize")
  )
  
  objective <- ObjectiveRFun$new(
    fun = function(xs) return(c(y = fn(xs))),
    domain = domain,
    codomain = codomain,
    properties = "deterministic"
  )
  
  terminator <- trm("evals", n_evals = 100L)
  
  OptimInstanceSingleCrit$new(
    objective = objective,
    terminator = terminator
  )
})

pb <- progress::progress_bar$new(total = nrow(problems))

run_archives <- lapply(1L:nrow(problems), function(p_id) {
  pb$tick()
  
  problem <- problems[p_id,]

  seed <- 1e6 * problem$fid + 1e4 * problem$iid + 1e2 * problem$repl + which(names(optimizers) == problem$optimizer_id)
  set.seed(seed)
  
  optimizer <- opt(problem$optimizer_id)

  optimizer$optimize(problem$optim_instance[[1L]])
  problem$optim_instance[[1L]]$archive
})

# lapply(run_archives, function(archive) min(archive$data$y))

full_data <- lapply(1L:nrow(problems), function(i) {
  arch <- run_archives[[i]]$data %>% select(x1, x2, y, batch_nr)
  arch$best_y <- cummin(arch$y)
  arch$method <- problems[i,"optimizer_id"]
  arch$fid <- problems[i,"fid"]
  arch$iid <- problems[i,"iid"]
  arch$repl <- problems[i,"repl"]
  
  arch
}) %>% Reduce(rbind, .)

readr::write_rds(full_data, "data/bbob_full.Rds")
