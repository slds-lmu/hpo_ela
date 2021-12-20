library(data.table)
library(mlr3)
library(mlr3learners)
library(ggplot2)

ela = as.data.table(readRDS("ela_features_design.rds"))
ert = as.data.table(readRDS("ert_optimizers.rds"))

features = grep("ela", x = colnames(ela), value = TRUE)
features = setdiff(features, grep("costs", x = features, value = TRUE))
tmp = cbind(ert, ela[, features, with = FALSE])
tmp[is.na(ert), ert := 10000]
tmp[, ert := (ert - 0) / 10000]
model0 = lm(ert ~ method, data = tmp)
upper = as.formula(paste("ert ~ method +", paste(features, collapse = " + ")))
stepsel = step(model0, scope = list(upper = upper, lower = ert ~ method), direction = "forward", k = 0)

task = TaskRegr$new("ela", target = "ert", backend = tmp)
task$col_roles$feature = setdiff(task$col_roles$feature, c("task", "n_succ_runs"))
learner = lrn("regr.ranger")
learner$param_set$values$importance = "impurity"
learner$param_set$values$min.node.size = 2L
learner$param_set$values$num.trees = 2000L
learner$train(task)
learner$predict(task)$score(msr("regr.rsq"))

imp = learner$importance()
