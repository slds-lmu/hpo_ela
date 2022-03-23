library(data.table)
library(mlr3misc)
library(ggplot2)
library(gridExtra)
performances = readRDS("optimizers.rds")
performances[, max_budget := dim * 50L]
performances = performances[batch_nr <= max_budget]

best_rs = performances[method == "random_search", .(target = min(best_logloss)), by = .(task, dim, repl)]
median_rs = best_rs[, .(target = median(target)), by = .(task, dim)]
performances = merge(median_rs, performances)

reached = performances[, .(target_reached = (best_logloss <= target)), by = .(task, dim, method, batch_nr, repl)]
reached[target_reached == TRUE, rt := min(batch_nr), by = .(task, dim, method, repl)]
ert = map_dtr(unique(reached$task), function(taskx) {
  map_dtr(unique(reached$dim), function(dimx) {
    tmp = reached[task == taskx & dim == dimx]
    map_dtr(unique(tmp$method), function(methodx) {
      tmpx = tmp[method == methodx]
      tmpxx = tmpx[, .(rt = min(rt, na.rm = TRUE)), by = .(repl)]
      tmpxx[, successful := is.finite(rt)]
      tmpxx[successful == FALSE, rt := 100]  # max budget if not successful
      ert = sum(tmpxx$rt) / sum(tmpxx$successful)
      data.table(ert = ert, method = methodx, task = taskx, dim = dimx)
    })
  })
})

plots = map(unique(ert$dim), function(dim_) {
  tmp = ert[dim == dim_]
  ggplot(tmp, aes(x = method, y = task)) +
    geom_tile(aes(fill = ert)) +
    geom_text(aes(label = round(ert, 2))) +
    scale_fill_gradient2(low = "blue", mid = "white", high = "red") +
    labs(x = "Algorithm", y = "Task", fill = "ERT", title = dim_)
})

g = grid.arrange(grobs = plots, nrow = 1L, ncol = length(plots))

ggsave("erts.png", plot = g, width = 20, height = 5)

