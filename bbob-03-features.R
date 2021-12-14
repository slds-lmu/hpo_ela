library(flacco)
library(smoof)

library(dplyr)
library(tidyr)

FIDS <- 1L:24L
IIDS <- 1L:1L
N_REPL <- 10L

bbob_features <- expand_grid(fid = FIDS, iid = IIDS, repl = 1L:N_REPL)

ela_features <- lapply(1:nrow(bbob_features), function(p_id) {
  problem <- bbob_features[p_id,]
  
  seed <- 1e4 * problem$fid + 1e2 * problem$iid + problem$repl
  set.seed(seed)
  
  fn <- makeBBOBFunction(dimensions = 2L, fid = problem$fid, iid = problem$iid)
  
  X <- createInitialSample(
    n.obs = 100L,
    dim = 2L,
    control = list(
      init_sample.type = "lhs",
      lower = -5,
      upper = 5
    )
  )
  
  sets_to_compute <- c("ela_meta", "ic", "limo", "ela_distr", "pca", "nbc", "disp")
  
  feat_object <- createFeatureObject(X = X,
                                     y = apply(X, 1, fn))
  feat_set <- calculateFeatures(feat_object, control = list(subset = sets_to_compute))
  
  cbind(problem, as.data.frame(feat_set))
}) %>% Reduce(rbind, .)

readr::write_rds(ela_features, "data/bbob_features.Rds")
