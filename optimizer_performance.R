library(data.table)
library(mlr3misc)
library(scmamp)
library(ggplot2)
library(pammtools)
library(xtable)

#random = setDT(readRDS("data/result_random_search.rds"))
#random = random[dim %in% c(2, 3, 5), c("fid", "iid", "dim", "optimizer", "best_y", "iter", "repl")]
#saveRDS(random, "data/result_random_search_clean.rds")

#cgg = setDT(readRDS("data/result_cma-gensa-grid.rds"))
#cgg = cgg[dim %in% c(2, 3, 5), c("fid", "iid", "dim", "optimizer", "best_y", "iter", "repl")]
#saveRDS(cgg, "data/result_cma-gensa-grid_clean.rds")

#mbo2 = setDT(readRDS("data/result_mbo_d2.rds"))
#mbo2 = mbo2[dim %in% c(2, 3, 5), c("fid", "iid", "dim", "optimizer", "best_y", "iter", "repl")]
#saveRDS(mbo2, "data/result_mbo_d2_clean.rds")

#mbo3 = setDT(readRDS("data/result_mbo_d3.rds"))
#mbo3 = mbo3[dim %in% c(2, 3, 5), c("fid", "iid", "dim", "optimizer", "best_y", "iter", "repl")]
#saveRDS(mbo3, "data/result_mbo_d3_clean.rds")

#mbo5 = setDT(readRDS("data/result_mbo_d5.rds"))
#mbo5 = mbo5[dim %in% c(2, 3, 5), c("fid", "iid", "dim", "optimizer", "best_y", "iter", "repl")]
#saveRDS(mbo5, "data/result_mbo_d5_clean.rds")

### Friedman tests and CD plots for BBOB and HPO
### BBOB
random = readRDS("data/result_random_search_clean.rds")
cgg = readRDS("data/result_cma-gensa-grid_clean.rds")
mbo2 = readRDS("data/result_mbo_d2_clean.rds")
mbo3 = readRDS("data/result_mbo_d3_clean.rds")
mbo5 = readRDS("data/result_mbo_d5_clean.rds")

bbob = rbind(random, cgg, mbo2, mbo3, mbo5)
bbob[, budget := 50L * dim]
bbob[, problem := paste(fid, iid, dim, sep = "_")]
bbob = bbob[iter <= budget]

bbob_agg = bbob[iter == budget, .(mean_best_y = mean(best_y), se_best_y = sd(best_y) / sqrt(.N)), by = .(problem, dim, optimizer)]
bbob_agg[, optimizer := factor(optimizer, levels = c("cmaes", "gensa", "grid_search", "mbo", "random_search"), labels = c("CMAES", "GENSA", "Grid", "MBO", "Random"))]
# All
tmp_ = dcast(bbob_agg, problem ~ optimizer, value.var = "mean_best_y")
tmp = - as.matrix(dcast(bbob_agg, problem ~ optimizer, value.var = "mean_best_y")[, -1])
tmp_[, best := colnames(tmp)[apply(tmp, 1, which.max)]]
saveRDS(tmp_[, c("problem", "best")], "data/bbob_best.rds")
tmp = na.omit(tmp)
friedmanTest(tmp)  # Friedman's chi-squared = 581.51, df = 4, p-value < 2.2e-16
plotCD(tmp, cex = 1.5)
# 2D
pdf("plots/bbob_cd_2.pdf", width = 6, height = 4)
tmp = - as.matrix(dcast(bbob_agg[dim == 2L], problem ~ optimizer, value.var = "mean_best_y")[, -1])
tmp = na.omit(tmp)
friedmanTest(tmp)  # Friedman's chi-squared = 154.55, df = 4, p-value < 2.2e-16
plotCD(tmp, cex = 1.5)
dev.off()
# 3D
pdf("plots/bbob_cd_3.pdf", width = 6, height = 4)
tmp = - as.matrix(dcast(bbob_agg[dim == 3L], problem ~ optimizer, value.var = "mean_best_y")[, -1])
tmp = na.omit(tmp)
friedmanTest(tmp)  # Friedman's chi-squared = 219.16, df = 4, p-value < 2.2e-16
plotCD(tmp, cex = 1.5)
dev.off()
# 5D
pdf("plots/bbob_cd_5.pdf", width = 6, height = 4)
tmp = - as.matrix(dcast(bbob_agg[dim == 5L], problem ~ optimizer, value.var = "mean_best_y")[, -1])
tmp = na.omit(tmp)
friedmanTest(tmp)  # Friedman's chi-squared = 258.69, df = 4, p-value < 2.2e-16
plotCD(tmp, cex = 1.5)
dev.off()

### HPO
hpo = readRDS("data/results_all_hpo.rds")
hpo[, budget := 50L * dim]
hpo[, problem := paste0(task, "_", dim)]
hpo = hpo[batch_nr <= budget]

hpo_agg = hpo[batch_nr == budget, .(mean_best_logloss = mean(best_logloss), se_best_logloss = sd(best_logloss) / sqrt(.N)), by = .(problem, dim, method)]
hpo_agg[, method := factor(method, levels = c("cmaes", "gensa", "grid_search", "mbo", "random_search"), labels = c("CMAES", "GENSA", "Grid", "MBO", "Random"))]
# All
tmp_ = dcast(hpo_agg, problem ~ method, value.var = "mean_best_logloss")
tmp = - as.matrix(dcast(hpo_agg, problem ~ method, value.var = "mean_best_logloss")[, -1])
tmp_[, best := colnames(tmp)[apply(tmp, 1, which.max)]]
saveRDS(tmp_[, c("problem", "best")], "data/hpo_best.rds")
friedmanTest(tmp)  # Friedman's chi-squared = 104.99, df = 4, p-value < 2.2e-16
plotCD(tmp, cex = 1.5)
# 2D
pdf("plots/hpo_cd_2.pdf", width = 6, height = 4)
tmp = - as.matrix(dcast(hpo_agg[dim == 2L], problem ~ method, value.var = "mean_best_logloss")[, -1])
tmp = na.omit(tmp)
friedmanTest(tmp)  # Friedman's chi-squared = 36.32, df = 4, p-value = 2.487e-07
plotCD(tmp, cex = 1.5)
dev.off()
# 3D
pdf("plots/hpo_cd_3.pdf", width = 6, height = 4)
tmp = - as.matrix(dcast(hpo_agg[dim == 3L], problem ~ method, value.var = "mean_best_logloss")[, -1])
tmp = na.omit(tmp)
friedmanTest(tmp)  # Friedman's chi-squared = 34.32, df = 4, p-value = 6.407e-07
plotCD(tmp, cex = 1.5)
dev.off()
# 5D
pdf("plots/hpo_cd_5.pdf", width = 6, height = 4)
tmp = - as.matrix(dcast(hpo_agg[dim == 5L], problem ~ method, value.var = "mean_best_logloss")[, -1])
tmp = na.omit(tmp)
friedmanTest(tmp)  # Friedman's chi-squared = 34.8, df = 4, p-value = 5.106e-07
plotCD(tmp, cex = 1.5)
dev.off()

### Normalized Regret plots
### BBOB
bbob[, best_overall := min(best_y), by = .(problem)]
bbob[, worst_overall := max(best_y), by = .(problem)]
bbob[, regret := (best_y - best_overall) / (worst_overall - best_overall)]
bbob_agg_regret = bbob[, .(mean_regret = mean(regret), se_regret = sd(regret) / sqrt(.N)), by = .(problem, optimizer, dim, fid, iid, iter)]
bbob_agg_regret[, init := 4L * dim]
bbob_agg_regret[, final := 50L * dim]

#g = ggplot(aes(x = iter, y = mean_regret, colour = optimizer, fill = optimizer), data = bbob_agg_regret[iter > init & iter <= final]) +
#  scale_y_log10() +
#  geom_step() +
#  geom_stepribbon(aes(ymin = mean_regret - se_regret, ymax = mean_regret + se_regret), colour = NA, alpha = 0.3) +
#  facet_wrap(~ problem, scales = "free") +
#  xlab("Iter") +
#  ylab("Mean Normalized Regret") +
#  theme(legend.position = "bottom")

bbob_agg_agg = bbob_agg_regret[, .(mean = mean(mean_regret), se = sd(mean_regret) / sqrt(.N)), by = .(optimizer, dim, iter)]
bbob_agg_agg[, init := 4L * dim]
bbob_agg_agg[, final := 50L * dim]
bbob_agg_agg[, optimizer := factor(optimizer, levels = c("cmaes", "gensa", "grid_search", "mbo", "random_search"), labels = c("CMAES", "GENSA", "Grid", "MBO", "Random"))]

g = ggplot(aes(x = iter, y = mean, colour = optimizer, fill = optimizer), data = bbob_agg_agg[iter > init & iter <= final]) +
  scale_y_log10() +
  geom_step() +
  geom_stepribbon(aes(ymin = mean - se, ymax = mean + se), colour = NA, alpha = 0.1) +
  facet_wrap(~ dim, scales = "free") +
  xlab("Iteration") +
  ylab("Mean Normalized Regret") +
  labs(colour = "Optimizer", fill = "Optimizer") +
  theme_minimal(base_size = 18) + 
  theme(legend.position = "bottom")

ggsave("plots/bbob_agg_normalized_regret.pdf", plot = g, width = 18, height = 5, device = "pdf")

### HPO
hpo[, best_overall := min(best_logloss), by = .(problem)]
hpo[, worst_overall := max(best_logloss), by = .(problem)]
hpo[, regret := (best_logloss - best_overall) / (worst_overall - best_overall)]
hpo_agg_regret = hpo[, .(mean_regret = mean(regret), se_regret = sd(regret) / sqrt(.N)), by = .(problem, method, dim, task, batch_nr)]
hpo_agg_regret[, init := 4L * dim]
hpo_agg_regret[, final := 50L * dim]

#g = ggplot(aes(x = batch_nr, y = mean_regret, colour = method, fill = method), data = hpo_agg_regret[batch_nr > init & batch_nr <= final]) +
#  scale_y_log10() +
#  geom_step() +
#  geom_stepribbon(aes(ymin = mean_regret - se_regret, ymax = mean_regret + se_regret), colour = NA, alpha = 0.3) +
#  facet_wrap(~ problem, scales = "free") +
#  xlab("Iter") +
#  ylab("Mean Normalized Regret") +
#  theme(legend.position = "bottom")

hpo_agg_agg = hpo_agg_regret[, .(mean = mean(mean_regret), se = sd(mean_regret) / sqrt(.N)), by = .(method, dim, batch_nr)]
hpo_agg_agg[, init := 4L * dim]
hpo_agg_agg[, final := 50L * dim]
hpo_agg_agg[, method := factor(method, levels = c("cmaes", "gensa", "grid_search", "mbo", "random_search"), labels = c("CMAES", "GENSA", "Grid", "MBO", "Random"))]

g = ggplot(aes(x = batch_nr, y = mean, colour = method, fill = method), data = hpo_agg_agg[batch_nr > init & batch_nr <= final]) +
  scale_y_log10() +
  geom_step() +
  geom_stepribbon(aes(ymin = mean - se, ymax = mean + se), colour = NA, alpha = 0.1) +
  facet_wrap(~ dim, scales = "free") +
  xlab("Iteration") +
  ylab("Mean Normalized Regret") +
  labs(colour = "Optimizer", fill = "Optimizer") +
  theme_minimal(base_size = 18) +
  theme(legend.position = "bottom")

ggsave("plots/hpo_agg_normalized_regret.pdf", plot = g, width = 18, height = 5, device = "pdf")

# BBOB closest to HPO
hpo_nearest_bbob = readRDS("data/hpo_nearest_bbob.rds")
print(xtable(hpo_nearest_bbob), include.rownames = FALSE)
pdf("plots/bbob_cd_closest_hpo.pdf", width = 6, height = 4)
tmp = - as.matrix(dcast(bbob_agg[problem %in% hpo_nearest_bbob$nearest_bbob] , problem ~ optimizer, value.var = "mean_best_y")[, -1])
friedmanTest(tmp)  # Friedman's chi-squared = 42.779, df = 4, p-value = 1.15e-08
plotCD(tmp, cex = 1.5)
dev.off()

bbob[, cumbudget := cumsum(iter), by = .(optimizer, problem, repl)]
bbob[, cumbudget_scaled := cumbudget / max(cumbudget), by = .(optimizer, problem, repl)]

get_incumbent_cumbudget = function(incumbent, cumbudget_scaled) {
  budgets = seq(0, 1, length.out = 101)
  map_dbl(budgets, function(budget) {
    ind = which(cumbudget_scaled <= budget)
    if (length(ind) == 0L) {
      max(incumbent)
    } else {
      min(incumbent[ind])
    }
  })
}

bbob_budget = bbob[, .(incumbent_budget = get_incumbent_cumbudget(regret, cumbudget_scaled), cumbudget_scaled = seq(0, 1, length.out = 101)), by = .(optimizer, problem, repl)]
bbob_agg_budget = bbob_budget[problem %in% hpo_nearest_bbob$nearest_bbob, .(mean = mean(incumbent_budget), se = sd(incumbent_budget) / sqrt(.N)), by = .(optimizer, problem, cumbudget_scaled)]
bbob_agg_budget[, optimizer := factor(optimizer, levels = c("cmaes", "gensa", "grid_search", "mbo", "random_search"), labels = c("CMAES", "GENSA", "Grid", "MBO", "Random"))]

g = ggplot(aes(x = cumbudget_scaled, y = mean, colour = optimizer, fill = optimizer), data = bbob_agg_budget) +
  scale_y_log10() +
  geom_step() +
  geom_stepribbon(aes(ymin = mean - se, ymax = mean + se), colour = NA, alpha = 0.1) +
  facet_wrap(~ problem, scales = "free") +
  xlab("Fraction of Budget used") +
  ylab("Mean Normalized Regret") +
  labs(colour = "Optimizer", fill = "Optimizer") +
  theme_minimal(base_size = 18) + 
  theme(legend.position = "bottom")

bbob_agg_agg_budget = bbob_agg_budget[, .(mean = mean(mean), se = sd(mean) / sqrt(.N)), by = .(optimizer, cumbudget_scaled)]

g = ggplot(aes(x = cumbudget_scaled, y = mean, colour = optimizer, fill = optimizer), data = bbob_agg_agg_budget) +
  scale_y_log10() +
  geom_step() +
  geom_stepribbon(aes(ymin = mean - se, ymax = mean + se), colour = NA, alpha = 0.1) +
  xlab("Fraction of Budget used") +
  ylab("Mean Normalized Regret") +
  labs(colour = "Optimizer", fill = "Optimizer") +
  theme_minimal(base_size = 18) + 
  theme(legend.position = "bottom")

ggsave("plots/bbob_closest_normalized_regret.pdf", plot = g, width = 8, height = 5, device = "pdf")

pdf("plots/hpo_cd.pdf", width = 6, height = 4)
tmp = - as.matrix(dcast(hpo_agg, problem ~ method, value.var = "mean_best_logloss")[, -1])
friedmanTest(tmp)  # Friedman's chi-squared = 104.99, df = 4, p-value < 2.2e-16
plotCD(tmp, cex = 1.5)
dev.off()

hpo[, cumbudget := cumsum(batch_nr), by = .(method, problem, repl)]
hpo[, cumbudget_scaled := cumbudget / max(cumbudget), by = .(method, problem, repl)]
hpo_budget = hpo[, .(incumbent_budget = get_incumbent_cumbudget(regret, cumbudget_scaled), cumbudget_scaled = seq(0, 1, length.out = 101)), by = .(method, problem, repl)]
hpo_agg_budget = hpo_budget[, .(mean = mean(incumbent_budget), se = sd(incumbent_budget) / sqrt(.N)), by = .(method, problem, cumbudget_scaled)]
hpo_agg_budget[, method := factor(method, levels = c("cmaes", "gensa", "grid_search", "mbo", "random_search"), labels = c("CMAES", "GENSA", "Grid", "MBO", "Random"))]

g = ggplot(aes(x = cumbudget_scaled, y = mean, colour = method, fill = method), data = hpo_agg_budget) +
  scale_y_log10() +
  geom_step() +
  geom_stepribbon(aes(ymin = mean - se, ymax = mean + se), colour = NA, alpha = 0.1) +
  facet_wrap(~ problem, scales = "free") +
  xlab("Fraction of Budget used") +
  ylab("Mean Normalized Regret") +
  labs(colour = "Optimizer", fill = "Optimizer") +
  theme_minimal(base_size = 18) +
  theme(legend.position = "bottom")

hpo_agg_agg_budget = hpo_agg_budget[, .(mean = mean(mean), se = sd(mean) / sqrt(.N)), by = .(method, cumbudget_scaled)]

g = ggplot(aes(x = cumbudget_scaled, y = mean, colour = method, fill = method), data = hpo_agg_agg_budget) +
  scale_y_log10() +
  geom_step() +
  geom_stepribbon(aes(ymin = mean - se, ymax = mean + se), colour = NA, alpha = 0.1) +
  xlab("Fraction of Budget used") +
  ylab("Mean Normalized Regret") +
  labs(colour = "Optimizer", fill = "Optimizer") +
  theme_minimal(base_size = 18) +
  theme(legend.position = "bottom")

ggsave("plots/hpo_closest_normalized_regret.pdf", plot = g, width = 8, height = 5, device = "pdf")

