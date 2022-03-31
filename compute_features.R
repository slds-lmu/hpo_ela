library(flacco)
library(data.table)
library(mlr3misc)

data = read_rds("data/design.rds")

sets_to_compute = c("ela_meta", "ic", "limo", "ela_distr", "pca", "nbc", "disp")
sets_to_compute = c("ela_meta", "ela_distr", "ic")

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

data[, problem := paste0(task, dim)]
params = c("nrounds", "eta", "lambda", "gamma", "alpha")

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

saveRDS(ela_features, "ela_features_design.rds")

data = ela_features[, - c("task", "dim", grep("costs", colnames(ela_features), value = TRUE)), with = FALSE]

data = scale(data)

kk = kmeans(data, 4, nstart = 25)

library(factoextra)

fviz_cluster(kk, data = data)

