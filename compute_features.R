library(flacco)
library(tidyverse)

glmnet_ela <- read_rds("design.rds")

sets_to_compute <- c("ela_meta", "ic", "limo", "ela_distr", "pca", "nbc", "disp")
ela_features <- data.frame()

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

for (ds in glmnet_ela %>% select(task) %>% unique() %>% unlist()) {
  cat(paste0(ds, "\n"))
  
  feat_object <- createFeatureObject(X = glmnet_ela %>% filter(task == ds) %>% select(classif.glmnet.s, classif.glmnet.alpha) %>% as.matrix,
                                     y = glmnet_ela %>% filter(task == ds) %>% select(classif.logloss) %>% unlist)
  feat_set <- calculateFeatures(feat_object, control = list(subset = sets_to_compute))
  
  ela_features <- rbind(ela_features, cbind(ds, feat_set %>% as.data.frame()))
}

saveRDS(ela_features, "ela_features_design.rds")

