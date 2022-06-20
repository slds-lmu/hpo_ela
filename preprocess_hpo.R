library(data.table)
library(mlr3misc)
library(ggplot2)
library(pammtools)
library(DiceKriging)

dat = readRDS("data/ela_newdata_large_results.rds")
dat = rbindlist(dat$result, fill = TRUE)
dat[, problem := paste0(task, "_", dim)]
dat[method == "random_search", max_batch := max(batch_nr), by = .(method, problem, dim, repl)]
dat[method == "random_search", batch_scaling := max_batch / (50 * dim), by = .(method, problem, dim, repl)]

rs_scaling = dat[method == "random_search" & batch_nr == max_batch, c("batch_scaling", "problem", "repl")]

runtime_learners = dat[batch_nr <= 50 * dim, .(runtime = sum(runtime_learners)), by = .(method, problem, dim, repl)]
runtime_learners_agg = runtime_learners[, .(learners = sum(runtime)), by = .(method, dim)]
runtime_learners_agg[, dim := as.character(dim)]

runtime_jobs = readRDS("data/ela_newdata_large_jobs.rds")
runtime_jobs[, time.running := unclass(time.running)]
runtime_jobs = merge(runtime_jobs, rs_scaling, by = c("problem", "repl"))
runtime_jobs[method == "random_search", time.running := time.running / batch_scaling]  # random search had more budget
runtime_jobs_agg = runtime_jobs[, .(sum_overall = sum(time.running)), by = .(method, dim)]
runtime_jobs_agg = merge(runtime_jobs_agg, runtime_learners_agg, by = c("method", "dim"))
sum(runtime_jobs$time.running) / 3600 / 24 / 365  # 2.2 CPU years for HPO
sum(runtime_jobs_agg[, sum_overall - learners])  # 27 CPU days for BBOB

design = dat[repl == 1 & method == "design", c("nrounds", "eta", "lambda", "gamma", "alpha", "task", "dim", "classif.logloss")]
#saveRDS(design, "data/results_design_hpo.rds")
optimizers = dat[method != "design"]
optimizers[method == "mbo", batch_nr := seq_len(.N), by = .(task, dim, repl)]
#saveRDS(optimizers, "data/results_all_hpo.rds")

design_2d = design[dim == 2, c("nrounds", "eta", "task", "classif.logloss"), with = FALSE]

surf.colors = function(x, breaks, col) {
  cols = terrain.colors(21)
  x.avg = (x[-1, -1] + x[-1, -(ncol(x) - 1)] + x[-(nrow(x) -1), -1] + x[-(nrow(x) -1), -(ncol(x) - 1)]) / 4
  colors = col[cut(x.avg, breaks = breaks, include.lowest = TRUE)]
  colors
}

pdf(paste0("plots/tasks_2D_small.pdf"), width = 16, height = 8, pointsize = 10)
par(mfrow = c(2, 5), tcl = -0.6)
for (task_ in unique(design_2d$task)) {
  orig = design_2d[task == task_]
  gp = km(design = orig[, c("nrounds", "eta")], response = orig[["classif.logloss"]], covtype = "matern5_2", nugget = 1e-6, multistart = 3L, scaling = TRUE)
  grid = map(c("nrounds", "eta"), function(param_) {
    seq(from = min(orig[[param_]]), to = max(orig[[param_]]), length.out = 30L)
  })
  f = function(x, y) {
    tmp = data.table(nrounds = x, eta = y)
    predict(gp, newdata = tmp, type = "UK", se.compute = FALSE)$mean
  }
  res = outer(grid[[1L]], grid[[2L]], FUN = f)
  best = which(res == min(res), arr.ind = TRUE)[1L, ]
  col = terrain.colors(21)
  breaks = quantile(as.vector(res), c(0, 0.001, 0.005, 1:10 / 100, seq(20, 100, by = 10) / 100))
  colors = surf.colors(res, breaks = breaks, col = col)
  perspmat = persp(x = grid[[1L]], y = grid[[2L]], z = res, xlab = "nrounds", ylab = "eta", zlab = "logloss", col = colors, theta = 60, phi = 30, main = task_, border = NA, cex.lab = 0.9, cex.axis = 0.7, ticktype = "detailed")
  best_point = trans3d(x = grid[[1L]][best[1L]], y = grid[[2L]][best[2L]], z = min(res), pmat = perspmat)
  points(x = best_point$x, y = best_point$y, col = "red", pch = 20)
  NULL
}
dev.off()

design_3d = design[dim == 3, c("nrounds", "eta", "lambda", "task", "classif.logloss"), with = FALSE]

plots = map(unique(design_3d$task), function(task_) {
  orig = design_3d[task == task_]
  gp = km(design = orig[, c("nrounds", "eta", "lambda")], response = orig[["classif.logloss"]], covtype = "matern5_2", nugget = 1e-6, multistart = 3L, scaling = TRUE)
  grid = map(c("nrounds", "eta", "lambda"), function(param_) {
    seq(from = min(orig[[param_]]), to = max(orig[[param_]]), length.out = ifelse(param_ == "lambda", 10, 30))
  })
  f = function(x, y, z) {
    tmp = data.table(nrounds = x, eta = y, lambda = z)
    predict(gp, newdata = tmp, type = "UK", se.compute = FALSE)$mean
  }
  res = map(seq_len(10), function(i) {
    outer(grid[[1L]], grid[[2L]], FUN = f, z = grid[[3L]][i])
  })
  col = terrain.colors(21)
  breaks = quantile(unlist(res), c(0, 0.001, 0.005, 1:10 / 100, seq(20, 100, by = 10) / 100))
  pdf(paste0("plots/", task_, "_3D.pdf"), width = 10, height = 5, pointsize = 10)
  par(mfrow = c(2, 5))
  for (i in seq_len(10)) {
    colors = surf.colors(res[[i]], breaks = breaks, col = col)
    persp(x = grid[[1L]], y = grid[[2L]], z = res[[i]], xlab = "nrounds", ylab = "eta", zlab = "logloss", col = colors, theta = 60, phi = 30, main = paste0("lambda = ", round(grid[[3L]][i], 2)), border = NA, cex.lab = 0.9)
  }
  dev.off()
  NULL
})

