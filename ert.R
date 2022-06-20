library(data.table)
library(mlr3misc)
library(ggplot2)
library(gridExtra)

# HPO, reference is median best performance of random search with equal 50d budget
hpo = readRDS("data/results_all_hpo.rds")
hpo[, budget := 50L * dim]

best_rs = hpo[batch_nr <= budget & method == "random_search", .(target = min(best_logloss)), by = .(task, dim, repl)]
median_rs = best_rs[, .(target = median(target)), by = .(task, dim)]
hpo = merge(median_rs, hpo)

reached = hpo[, .(target_reached = (best_logloss <= target)), by = .(task, dim, method, batch_nr, repl)]
reached[target_reached == TRUE, rt := min(batch_nr), by = .(task, dim, method, repl)]
ert = map_dtr(unique(reached$task), function(taskx) {
  map_dtr(unique(reached$dim), function(dimx) {
    tmp = reached[task == taskx & dim == dimx]
    map_dtr(unique(tmp$method), function(methodx) {
      tmpx = tmp[method == methodx]
      tmpxx = tmpx[, .(rt = min(rt, na.rm = TRUE)), by = .(repl)]
      tmpxx[, successful := is.finite(rt)]
      tmpxx[successful == FALSE, rt := 50L * dimx]  # max budget if not successful
      ert = sum(tmpxx$rt) / sum(tmpxx$successful)
      data.table(ert = ert, method = methodx, task = taskx, dim = dimx)
    })
  })
})

ert[, problem := paste0(task, "_", dim)]
ert[!is.finite(ert), ert := NA_real_]
ert[, ert := ifelse(is.na(ert), max(ert, na.rm = TRUE) * 10L, ert), by = .(problem)]  # for infinite ERT we assign part10 worst ERT
ert_rs = ert[method == "random_search", .(ert_rs = ert, problem = problem)]
ert = merge(ert, ert_rs, by = "problem")
ert[, ert_ratio := ert / ert_rs]
ert[, method := factor(method, levels = c("cmaes", "gensa", "grid_search", "mbo", "random_search"), labels = c("CMAES", "GENSA", "Grid", "MBO", "Random"))]

plots = map(unique(ert$dim), function(dim_) {
  tmp = ert[dim == dim_]
  ggplot(tmp, aes(x = method, y = task)) +
    geom_tile(aes(fill = ert_ratio)) +
    geom_text(aes(label = round(ert_ratio, 2))) +
    scale_fill_gradient2(low = "blue", mid = "white", high = "red") +
    labs(x = "Optimizer", y = "Task", fill = "ERT Ratio", title = dim_) +
    theme_minimal(base_size = 14)
})

g = grid.arrange(grobs = plots, nrow = 1L, ncol = length(plots))

ggsave("plots/hpo_erts_rs_50.pdf", plot = g, width = 20, height = 5, device = "pdf")

ert_agg = ert[, .(mean_ert = mean(ert_ratio), se_ert = sd(ert_ratio) / sqrt(.N)), by = .(method, dim)]

g = ert_agg_hpo = ggplot(ert_agg, aes(x = method, y = as.factor(dim))) +
  geom_tile(aes(fill = mean_ert)) +
  geom_text(aes(label = round(mean_ert, 2))) +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red") +
  labs(x = "Optimizer", y = "Dimensionality", fill = "Mean ERT Ratio") +
  theme_minimal(base_size = 14)

ggsave("plots/hpo_erts_rs_50_agg.pdf", plot = g, width = 6, height = 4, device = "pdf")

# BBOB, reference is median best performance of random search with equal 50d budget
random = readRDS("data/result_random_search_clean.rds")
cgg = readRDS("data/result_cma-gensa-grid_clean.rds")
mbo2 = readRDS("data/result_mbo_d2_clean.rds")
mbo3 = readRDS("data/result_mbo_d3_clean.rds")
mbo5 = readRDS("data/result_mbo_d5_clean.rds")

bbob = rbind(random, cgg, mbo2, mbo3, mbo5)
bbob[, budget := 50L * dim]
bbob[, problem := paste(fid, iid, dim, sep = "_")]

best_rs = bbob[iter <= budget & optimizer == "random_search", .(target = min(best_y)), by = .(fid, iid, dim, repl)]
median_rs = best_rs[, .(target = median(target)), by = .(fid, iid, dim)]
bbob = merge(median_rs, bbob)

reached = bbob[, .(target_reached = (best_y <= target)), by = .(fid, iid, dim, optimizer, iter, repl)]
reached[target_reached == TRUE, rt := min(iter), by = .(fid, iid, dim, optimizer, repl)]
ert = map_dtr(unique(reached$fid), function(fidx) {
  map_dtr(unique(reached$iid), function(iidx) {
    map_dtr(unique(reached$dim), function(dimx) {
      tmp = reached[fid == fidx & iid == iidx & dim == dimx]
      map_dtr(unique(tmp$optimizer), function(optimizerx) {
        tmpx = tmp[optimizer == optimizerx]
        tmpxx = tmpx[, .(rt = min(rt, na.rm = TRUE)), by = .(repl)]
        tmpxx[, successful := is.finite(rt)]
        tmpxx[successful == FALSE, rt := 50L * dimx]  # max budget if not successful
        ert = sum(tmpxx$rt) / sum(tmpxx$successful)
        data.table(ert = ert, optimizer = optimizerx, fid = fidx, iid = iidx, dim = dimx)
      })
    })
  })
})

ert[, problem := paste0(fid, "_", iid, "_", dim)]
ert[!is.finite(ert), ert := NA_real_]
ert[, ert := ifelse(is.na(ert), max(ert, na.rm = TRUE) * 10L, ert), by = .(problem)]  # for infinite ERT we assign part10 worst ERT
ert_rs = ert[optimizer == "random_search", .(ert_rs = ert, problem = problem)]
ert = merge(ert, ert_rs, by = "problem")
ert[, ert_ratio := ert / ert_rs]
ert[, optimizer := factor(optimizer, levels = c("cmaes", "gensa", "grid_search", "mbo", "random_search"), labels = c("CMAES", "GENSA", "Grid", "MBO", "Random"))]

# iid = i
for (iidx in 1:5) {
  plots = map(unique(ert$dim), function(dim_) {
    tmp = ert[dim == dim_ & iid == iidx]
    ggplot(tmp, aes(x = optimizer, y = as.factor(fid))) +
      geom_tile(aes(fill = ert_ratio)) +
      geom_text(aes(label = round(ert_ratio, 2))) +
      scale_fill_gradient2(low = "blue", mid = "white", high = "red") +
      labs(x = "Optimizer", y = "FID", fill = "ERT Ratio", title = dim_) +
      theme_minimal(base_size = 14)
  })
  
  g = grid.arrange(grobs = plots, nrow = 1L, ncol = length(plots))
  
  ggsave(paste0("plots/bbob_erts_rs_50_", iidx, ".pdf"), plot = g, width = 20, height = 5, device = "pdf")
}

ert_agg = ert[, .(mean_ert = mean(ert_ratio), se_ert = sd(ert_ratio) / sqrt(.N)), by = .(optimizer, dim)]

g = ert_agg_bbob = ggplot(ert_agg, aes(x = optimizer, y = as.factor(dim))) +
  geom_tile(aes(fill = mean_ert)) +
  geom_text(aes(label = round(mean_ert, 2))) +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red") +
  labs(x = "Optimizer", y = "Dimensionality", fill = "Mean ERT Ratio") +
  theme_minimal(base_size = 14)

ggsave("plots/bbob_erts_rs_50_agg.pdf", plot = g, width = 6, height = 4, device = "pdf")

