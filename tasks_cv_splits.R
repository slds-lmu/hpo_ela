library(mlr3)
library(mlr3oml)
library(mlr3misc)
library(mlr3pipelines)
library(data.table)
# tasks and instances
openml_ids = c(40983, 469, 41156, 6332, 23381, 1590, 1461, 40975, 41146, 40685)
tasks = setNames(sapply(openml_ids, function(id) {
  task = tsk("oml", data_id = id)
  pl = ppl("robustify", task = task, impute_missings = TRUE, factors_to_numeric = TRUE)
  task = pl$train(task)[[1L]]
  task
}), nm = as.character(openml_ids))
names(openml_ids) = map_chr(tasks, "id")
instances = setDT(expand.grid(openml_id = openml_ids, learner_id = "xgboost", dim = c(2L, 3L, 5L)))

for (task in tasks) {
  tmp = task$data()
  write.table(tmp, paste0("tasks/", task$id, ".csv"), row.names = FALSE)
}

splits = map_dtr(seq_len(nrow(instances)), function(i) {
  seed = instances[i, ]$openml_id
  set.seed(seed)
  resampling = rsmp("cv", folds = 10L)  # NOTE: this used to be 10 times 10 fold
  task = tasks[[as.character(instances[i, ]$openml_id)]]$clone(deep = TRUE)
  resampling$instantiate(task)
  cbind(openml_id = instances[i, ]$openml_id, resampling$instance)
}, .fill = TRUE)

write.table(splits, file = "ela_splits.csv", row.names = FALSE)

