library(data.table)
library(mlr3misc)
library(mlr3)
library(mlr3learners)
library(mlr3pipelines)
library(rpart.plot)
set.seed(1)

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

# classify hpo vs. bbob based on ela
task = TaskClassif$new("hpo_bbob", backend = ela[, - c("problem", "dim")], target = "type")
task$col_roles$stratum = "type"
learner = lrn("classif.rpart")
set.seed(1)
rr = resample(task, learner, rsmp("repeated_cv", repeats = 10L, folds = 10L))
rr$aggregate()  # classif.ce 0.03538462
learner$train(task)
png("plots/classify_type_rp.png", width = 8, height = 4, units = "in", res = 150)
rpart.plot(learner$model, box.palette = 0, tweak = 1.2)
dev.off()

# is "dimensionality" reflecetd in bbob somewhat relateable to hpo "dimensionality"?
# predict dimensionality of bbob and holdout performance on hpo
ela_hpo[, dim := as.factor(dim)]
task_hpo_dim = TaskClassif$new("hpo_dim", backend = ela_hpo[, - c("problem", "type")], target = "dim")
task_hpo_dim$col_roles$stratum = "dim"
ela_bbob[, dim := as.factor(dim)]
task_bbob_dim = TaskClassif$new("bbob_dim", backend = ela_bbob[, - c("problem", "type")], target = "dim")
task_bbob_dim$col_roles$stratum = "dim"
set.seed(1)
rr = resample(task_bbob_dim, learner, rsmp("repeated_cv", repeats = 10L, folds = 10L))
rr$aggregate()  # 0.07388889
learner$train(task_bbob_dim)
png("plots/classify_dim_bbob_rp.png", width = 8, height = 4, units = "in", res = 150)
rpart.plot(learner$model, roundint = FALSE, box.palette = 0, tweak = 1.2)
dev.off()
learner$predict(task_hpo_dim)$score()  # 0.1

## can we predict the best algorithm on bbob and generalize?
#hpo_best = readRDS("data/hpo_best.rds")
#bbob_best = readRDS("data/bbob_best.rds")
#ela_hpo = merge(ela_hpo, hpo_best, by = "problem")
#ela_hpo[, best := factor(best, levels = c("CMAES", "MBO", "GENSA", "Grid", "Random"))]
#ela_bbob = merge(ela_bbob, bbob_best, by = "problem")
#ela_bbob[, best := factor(best, levels = c("CMAES", "MBO", "GENSA", "Grid", "Random"))]
#task_hpo_best = TaskClassif$new("hpo_best", backend = ela_hpo[, - c("problem", "type", "dim")], target = "best")
#task_bbob_best = TaskClassif$new("bbob_best", backend = ela_bbob[, - c("problem", "type", "dim")], target = "best")
#set.seed(1)
#rr = resample(task_bbob_best, learner, rsmp("repeated_cv", repeats = 10L, folds = 10L))
#rr$aggregate()  # 0.4402778
#learner$train(task_bbob_best)
#png("plots/classify_best_bbob_rp.png", width = 8, height = 4, units = "in", res = 150)
#rpart.plot(learner$model, roundint = FALSE, box.palette = 0, tweak = 1.2)
#dev.off()
#learner$predict(task_hpo_best)$score()  # 0.5
#
## can we predict gensa vs. rest
#ela_hpo[, best := factor(ifelse(best == "GENSA", "GENSA", "other"), levels = c("GENSA", "other"))]
#ela_bbob[, best := factor(ifelse(best == "GENSA", "GENSA", "other"), levels = c("GENSA", "other"))]
#task_hpo_best = TaskClassif$new("hpo_best", backend = ela_hpo[, - c("problem", "type", "dim")], target = "best")
#task_bbob_best = TaskClassif$new("bbob_best", backend = ela_bbob[, - c("problem", "type", "dim")], target = "best")
#set.seed(1)
#rr = resample(task_bbob_best, learner, rsmp("repeated_cv", repeats = 10L, folds = 10L))
#rr$aggregate()  # 0.2061111
#learner$train(task_bbob_best)
#png("plots/classify_gensa_other_bbob_rp.png", width = 8, height = 4, units = "in", res = 150)
#rpart.plot(learner$model, roundint = FALSE, box.palette = 0, tweak = 1/2)
#dev.off()
#learner$predict(task_hpo_best)$score()  # 0.1666667

