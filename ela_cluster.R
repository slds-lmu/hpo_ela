library(data.table)
library(mlr3misc)
library(paran)
library(factoextra)
library(corrplot)
library(mlr3)
library(mlr3learners)
library(mlr3pipelines)
library(rpart.plot)
library(xtable)
bold = function(x) {paste("\\textbf{", x, "}", sep = "")}
source("predict_kmeans.R")

# load ela hpo
ela_hpo = readRDS("data/ela_features_hpo_norm.rds")
ela_hpo = ela_hpo[, colnames(ela_hpo) %nin% grep("costs", colnames(ela_hpo), value = TRUE), with = FALSE]
ela_hpo[, problem := paste0(task, "_", dim)]
ela_hpo = ela_hpo[, - "task"]
ela_hpo[, type := "hpo"]

# load ela bbob
ela_bbob = setDT(readRDS("data/ela_features_bbob_norm.rds"))
ela_bbob[, problem := paste0(fid, "_", iid, "_", dim)]
ela_bbob = ela_bbob[rep == 1]
ela_bbob = ela_bbob[, colnames(ela_bbob)[colnames(ela_bbob) %in% colnames(ela_hpo)], with = FALSE]  # subset to hpo ela features
ela_bbob[, type := "bbob"]

# rbind ela hpo and bbob together
ela = rbind(ela_hpo, ela_bbob)
ela[, type := as.factor(type)]

# pca on ela_x and cluster analysis
ela_x = scale(ela[, - c("dim", "problem", "type")])

pca = prcomp(ela_x)
summary(pca)  # roughly 60%
pca_dat = scale(pca$x[, 1:2])
rownames(pca_dat) = c(ela_hpo$problem, ela_bbob$problem)
loadings = pca$rotation[, 1:2]
loadings = cbind(data.table(loadings), data.table(feature = factor(rownames(loadings), levels = rownames(loadings))))
loadings_ = melt(loadings, measure = c("PC1", "PC2"), variable.name = "PC", value.name = "loading")
##print(xtable(t(loadings)), sanitize.colnames.function = bold, booktabs = TRUE)
g = ggplot(loadings_, aes(feature, abs(loading), fill = loading)) + 
  facet_wrap(~ PC, nrow = 1) +
  geom_bar(stat="identity") +
  coord_flip() +
  scale_fill_gradient2(name = "Loading", 
                       high = "blue", mid = "white", low = "red", 
                       midpoint = 0, guide = "none") +
  xlab("ELA Feature") +
  ylab("Loading Strength") +
  theme_minimal(base_size = 14)
ggsave("plots/ela_pca_loadings.pdf", plot = g, width = 12, height = 6, device = "pdf")

fviz_nbclust(pca_dat, kmeans, method = "wss", k.max = 40)
fviz_nbclust(pca_dat, kmeans, method = "silhouette")
km = kmeans(pca_dat, centers = 3, nstart = 25)
g = fviz_cluster(km, data = pca_dat, geom = "point", alpha = 0)
g = g +
  geom_text(aes(x = x, y = y, label = name, colour = cluster), size = 3, alpha = 0.3, data = g$data[31:390, ], show.legend = FALSE) +
  geom_text(aes(x = x, y = y, label = name, colour = cluster), size = 4, data = g$data[1:30, ], show.legend = FALSE) +
  labs(title = NULL, colour = "Cluster", fill = "Cluster", shape = "Cluster") +
  coord_fixed() +
  theme_minimal(base_size = 14)
ggsave("plots/ela_cluster.pdf", plot = g, width = 10, height = 6, device = "pdf")

gg_color_hue = function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

ela[, cluster := factor(km$cluster, levels = seq_len(max(km$cluster)))]
task_cluster = TaskClassif$new("cluster", backend = ela[, - c("problem", "dim", "type")], target = "cluster")
task_cluster$col_roles$stratum = "cluster"
learner = lrn("classif.rpart")
set.seed(1)
rr = resample(task_cluster, learner, rsmp("repeated_cv", repeats = 10L, folds = 10L))
rr$aggregate()  # classif.ce 0.1380757
learner$train(task_cluster)
pdf("plots/classify_cluster_rp.pdf", width = 8, height = 4)
rpart.plot(learner$model, roundint = FALSE, box.palette = 0, tweak = 1.2)
dev.off()

xtabs( ~ cluster + dim, data = ela)
ela[, fid := map_chr(strsplit(problem, "_"), 1L)]
ela[fid %in% as.character(1:5), group := "separable"]
ela[fid %in% as.character(6:9), group := "low_cond"]
ela[fid %in% as.character(10:14), group := "high_cond"]
ela[fid %in% as.character(15:19), group := "multi_strong"]
ela[fid %in% as.character(20:24), group := "multi_weak"]
xtabs( ~ cluster + group, data = ela)

nearest_bbob = map_chr(1:30, function(i) {
  x = pca_dat[i, ]
  distances = map_dbl(31:390, function(j) {
    sqrt(sum((x - pca_dat[j, ]) ^ 2))
  })
  rownames(pca_dat[31:390, ])[which.min(distances)]
})

hpo_nearest_bbob = data.table(problem = ela[1:30, ]$problem, nearest_bbob = nearest_bbob)

saveRDS(hpo_nearest_bbob, "data/hpo_nearest_bbob.rds")

nearest_bbob_all = map_chr(1:30, function(i) {
  x = ela_x[i, ]
  distances = map_dbl(31:390, function(j) {
    sqrt(sum((x - ela_x[j, ]) ^ 2))
  })
  ela[31:390, ]$problem[which.min(distances)]
})

hpo_nearest_bbob_all = data.table(problem = ela[1:30, ]$problem, nearest_bbob = nearest_bbob_all)

saveRDS(hpo_nearest_bbob_all, "data/hpo_nearest_bbob_all.rds")

