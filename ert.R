library(data.table)
library(mlr3misc)
performances = readRDS("optimizers.rds")

best_rs = performances[method == "random_search", .(target = min(best_logloss)), by = .(task, repl)]
median_rs = best_rs[, .(target = median(target)), by = .(task)]
performances = merge(median_rs, performances)

reached = performances[, .(target_reached = (best_logloss <= target)), by = .(task, method, batch_nr, repl)]
reached[target_reached == TRUE, rt := min(batch_nr), by = .(task, method, repl)]
ert = map_dtr(unique(reached$task), function(taskx) {
  tmp = reached[task == taskx]
  map_dtr(unique(tmp$method), function(methodx) {
    tmpx = tmp[method == methodx]
    tmpxx = tmpx[, .(rt = min(rt, na.rm = TRUE)), by = .(repl)]
    tmpxx[, successful := is.finite(rt)]
    tmpxx[successful == FALSE, rt := 100]  # max budget if not successful
    ert = sum(tmpxx$rt) / sum(tmpxx$successful)
    data.table(ert = ert, method = methodx, task = taskx)
  })
})

ggplot(ert, aes(x = method, y = task)) +
  geom_tile(aes(fill = ert)) +
  geom_text(aes(label = round(ert, 2))) +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red") +
  labs(x = "Algorithm", y = "Task", fill = "ERT")

