library(flacco)
library(data.table)
library(mlr3misc)

data = readRDS("data/results_design_hpo.rds")
bounds = list(nrounds = c(log(3), log(2000)), eta = c(-7, 0), lambda = c(-7, 7), gamma = c(-10, 2), alpha = c(-7, 7))  # see run.R

sets_to_compute = c("ela_meta", "ic", "ela_distr", "nbc", "disp")

number_of_peaks = function(x, smoothing.bandwidth = "SJ", 
  modemass.threshold = 0.01, ...) {
  intdens = function(a, b) {
    mean(y[a:b]) * diff(d$x[c(a, b)])
  }
  d = density(x, bw = smoothing.bandwidth, ...)
  y = d$y
  n = length(y)
  index = 2L : (n - 1L)
  min.index = c(1L, which((y[index] < y[index - 1L]) & y[index] < y[index + 1L]), n + 1L)
  min.index = min.index[min.index != 1]
  min.index = c(1, min.index)
  modemass = vapply(1L : (length(min.index) - 1L), function(i) 
    intdens(min.index[i], min.index[i + 1L] - 1L), double(1))
  sum(modemass > modemass.threshold)
}

assignInNamespace("number_of_peaks", number_of_peaks, ns = "flacco")

params = c("nrounds", "eta", "lambda", "gamma", "alpha")
data[, problem := paste0(task, dim)]
data[, classif.logloss := (classif.logloss -  mean(classif.logloss)) / sd(classif.logloss), by = problem]  # standardized
for (feature in params) {
  data[, (feature) := (get(feature) - bounds[[feature]][1L]) / (bounds[[feature]][2L] - bounds[[feature]][1L])]
}

ela_features = map_dtr(unique(data$task), function(task_) {
  map_dtr(unique(data$dim), function(dim_) {
    tmp = data[task == task_ & dim == dim_]
    if (NROW(tmp) == 0L) return(data.table())
    params_ = params[seq_len(dim_)]

    feat_object = createFeatureObject(X = tmp[, params_, with = FALSE], y = tmp[["classif.logloss"]])
    feat_set = calculateFeatures(feat_object, control = list(subset = sets_to_compute))
    cbind(as.data.table(feat_set), data.table(task = task_, dim = dim_))
  })
})

saveRDS(ela_features, "data/ela_features_hpo_norm.rds")

