library(data.table)
library(mlr3misc)
library(paran)
library(factoextra)
library(corrplot)
library(mlr3)
library(mlr3learners)
library(mlr3pipelines)
library(elasticnet)

# load ela hpo
ela_hpo = readRDS("data/ela_features_design.rds")
ela_hpo = ela_hpo[, colnames(ela_hpo) %nin% grep("costs", colnames(ela_hpo), value = TRUE), with = FALSE]
ela_hpo[, problem := paste0(task, "_", dim)]
ela_hpo = ela_hpo[, - "task"]
ela_hpo[, type := "hpo"]

# load ela bbob
ela_bbob = setDT(read.csv("data/ela_features_bbob_norm.csv"))
ela_bbob[, problem := paste0(fid, "_", iid, "_", dim)]
ela_bbob = ela_bbob[rep == 1]
ela_bbob = ela_bbob[, colnames(ela_bbob)[colnames(ela_bbob) %in% colnames(ela_hpo)], with = FALSE]  # subset to hpo ela features
ela_bbob[, type := "bbob"]

# rbind ela hpo and bbob together
ela = rbind(ela_hpo, ela_bbob)
ela[, type := as.factor(type)]

ela_bbob_x = scale(ela_bbob[, - c("dim", "problem", "type")])

paran_res = paran(ela_bbob_x, graph = TRUE)  # 5
pca = prcomp(ela_bbob_x)
pca_dat = scale(pca$x[, 1:5])
loadings = pca$rotation[, 1:5]
loadings[abs(loadings) < 0.15] = 0

fviz_nbclust(pca_dat, kmeans, method = "wss", k.max = 40)
fviz_nbclust(pca_dat, kmeans, method = "silhouette")
km = kmeans(pca_dat, centers = 8, nstart = 25)

g = fviz_cluster(km, data = pca_dat)

png("kmres_7_hpo.png", width = 10, height = 10, units = "in", res = 150)
g = fviz_cluster(km, data = pca_dat, geom = "point", alpha = 0, show.clust.cent = FALSE)
g$labels$title = NULL
hpo_dat = setDT(g$data[1:30, ])
hpo_dat[, problem := ela_hpo$problem]
hpo_dat[, text_y := y - 0.05]
g = g +
  geom_point(data = hpo_dat, aes(x = x, y = y, colour = cluster)) +
  geom_text(data = hpo_dat, aes(x = x, y = text_y, colour = cluster, label = problem, alpha = 0.5)) +
  theme_minimal()
dev.off()

png("kmres_7_bbob_dim.png", width = 10, height = 10, units = "in", res = 150)
g = fviz_cluster(km, data = pca_dat, geom = NULL, show.clust.cent = FALSE)
bbob_dat = setDT(g$data[31:390, ])
bbob_dat[, problem := ela_bbob$problem]
bbob_dat[, dim := map_chr(strsplit(bbob_dat$problem, "_"), 3)]
bbob_dat[, text_y := y - 0.05]
g +
  geom_point(data = bbob_dat, aes(x = x, y = y, colour = cluster)) +
  geom_text(data = bbob_dat, aes(x = x, y = text_y, colour = cluster, label = dim, alpha = 0.5)) +
  theme_minimal()
dev.off()

png("kmres_7_bbob_group.png", width = 10, height = 10, units = "in", res = 150)
g = fviz_cluster(km, data = pca_dat, geom = NULL, show.clust.cent = FALSE)
bbob_dat = setDT(g$data[31:390, ])
bbob_dat[, problem := ela_bbob$problem]
bbob_dat[, fid := map_chr(strsplit(bbob_dat$problem, "_"), 1)]
bbob_dat[fid %in% as.character(1:5), group := "separable"]
bbob_dat[fid %in% as.character(6:9), group := "low_cond"]
bbob_dat[fid %in% as.character(10:14), group := "high_cond"]
bbob_dat[fid %in% as.character(15:19), group := "multi_strong"]
bbob_dat[fid %in% as.character(20:24), group := "multi_weak"]
g +
  geom_point(data = bbob_dat, aes(x = x, y = y, shape = group), size = 3) +
  scale_shape_manual(values = c(0, 1, 2, 3, 4), breaks = c("separable", "low_cond", "high_cond", "multi_strong", "multi_weak"))
dev.off()

png("kmres_7_bbob_global.png", width = 10, height = 10, units = "in", res = 150)
g = fviz_cluster(km, data = pca_dat, geom = NULL, show.clust.cent = FALSE)
bbob_dat = setDT(g$data[31:390, ])
bbob_dat[, problem := ela_bbob$problem]
bbob_dat[, fid := map_chr(strsplit(bbob_dat$problem, "_"), 1)]
bbob_dat[fid %in% as.character(c(1, 2, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 21, 22, 23)), structure := "none"]
bbob_dat[fid %in% as.character(c(3, 4, 15, 19)), structure := "strong"]
bbob_dat[fid %in% as.character(c(16, 17, 18)), structure := "med"]
bbob_dat[fid %in% as.character(20), structure := "deceptive"]
bbob_dat[fid %in% as.character(24), structure := "weak"]
g +
  geom_point(data = bbob_dat, aes(x = x, y = y, shape = structure), size = 3) +
  scale_shape_manual(values = c(0, 1, 2, 3, 4), breaks = c("none", "strong", "med", "deceptive", "weak"))
dev.off()

fviz_nbclust(ela_x, kmeans, method = "wss", k.max = 40)
png("kmk_raw.png", width = 10, height = 10, units = "in", res = 150)
fviz_nbclust(ela_x, kmeans, method = "silhouette")
dev.off()
km = kmeans(ela_x, centers = 5, nstart = 25)

png("kmres_raw.png", width = 10, height = 10, units = "in", res = 150)
fviz_cluster(km, data = ela_x)
dev.off()

ela[, class := as.factor(rep(c("hpo", "bbob"), c(22, 24 * 5 * 3)))]
ela[, ela_distr.number_of_peaks := as.numeric(ela_distr.number_of_peaks)]

task = TaskClassif$new("discr", backend = ela[, - "problem"], target = "class")
task$col_roles$startum = "class"

rf = as_learner(po("smote", K = 3L) %>>% lrn("classif.ranger", importance = "permutation"))
rp = as_learner(po("smote", K = 3L) %>>% lrn("classif.rpart"))

rr_rf = resample(task, rf, rsmp("repeated_cv", folds = 3L))
rr_rp = resample(task, rp, rsmp("repeated_cv", folds = 3L))

rf$train(task)
rp$train(task)

importance = sort(ranger::importance(rf$model$classif.ranger$model), decreasing = TRUE)
importance = data.table(importance = importance, variable = factor(names(importance), levels = names(importance)))
g = ggplot(importance) +
  geom_bar(aes(y = variable, x = importance), stat = "identity")
ggsave("rf_per_imp.png", g)

library(rpart.plot)
png("rp.png", width = 10, height = 10, units = "in", res = 150)
rpart.plot(rp$model$classif.rpart$model)
dev.off()
