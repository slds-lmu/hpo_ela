#install.packages("RANN")
#install.packages("smoof")
#install.packages("flacco")
wd = dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(wd)

library(foreach)

set.seed(100)
cl = makeCluster(detectCores())
registerDoParallel(cl)
data = foreach(fid = 1:24, .combine = rbind) %:%
  foreach(dim = c(2, 3, 5), .combine = rbind) %:%
  foreach(iid = 1:5, .combine = rbind) %:%
  foreach(rep = 1:10, .combine = rbind) %dopar% {
    library(smoof)
    library(flacco)
    
    f = makeBBOBFunction(dim, fid, iid)
    
    X = createInitialSample(50 * dim, dim, control = list(init_sample.type = "lhs", init_sample.lower = -5, init_sample.upper = 5))
    y = apply(X, 1, f)
    feat_object = createFeatureObject(X = X, y = y, blocks = 1)
    
    
    ela_distr = data.frame(calculateFeatureSet(feat_object, set = "ela_distr"))
    ela_meta = data.frame(calculateFeatureSet(feat_object, set = "ela_meta"))
    basic = data.frame(calculateFeatureSet(feat_object, set = "basic"))
    disp = data.frame(calculateFeatureSet(feat_object, set = "disp"))
    limo = data.frame(calculateFeatureSet(feat_object, set = "limo"))
    nbc = data.frame(calculateFeatureSet(feat_object, set = "nbc"))
    pca = data.frame(calculateFeatureSet(feat_object, set = "pca"))
    ic = data.frame(calculateFeatureSet(feat_object, set = "ic"))
    
    
    data = data.frame(fid = fid, dim = dim, iid = iid, rep = rep)
    cbind(data, basic, ela_distr, ela_meta, disp, limo, nbc, pca, ic)
    
  }
stopCluster(cl)


write.csv(data, "data/ela_features_bbob.csv", row.names = F)