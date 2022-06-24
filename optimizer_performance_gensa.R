library(data.table)
library(mlr3misc)
library(scmamp)
library(ggplot2)
library(pammtools)

### GenSA & HPO
gensa = readRDS("data/ela_newdata_gensa.rds")
gensa = gensa[, c("nrounds", "eta", "best_logloss", "batch_nr", "repl", "task", "dim", "method")]
hpo = readRDS("data/results_all_hpo.rds")
hpo = rbind(gensa, hpo, fill = TRUE)
hpo[, budget := 50L * dim]
hpo[, problem := paste0(task, "_", dim)]
hpo = hpo[batch_nr <= budget & problem %in% c("wilt_2", "ada_2", "car_2") & method != "gensa"]  # "gensa" is "gensa_3"

hpo_agg = hpo[batch_nr == budget, .(mean_best_logloss = mean(best_logloss), se_best_logloss = sd(best_logloss) / sqrt(.N)), by = .(problem, dim, method)]
hpo_agg[, method := factor(method, levels = c("cmaes", "gensa_1", "gensa_2", "gensa_3", "gensa_4", "grid_search", "mbo", "random_search"), labels = c("CMAES", "GENSAv1", "GENSAv2", "GENSAv3", "GENSAv4", "Grid", "MBO", "Random"))]
# All (2D)
tmp_ = dcast(hpo_agg, problem ~ method, value.var = "mean_best_logloss")
tmp = - as.matrix(dcast(hpo_agg, problem ~ method, value.var = "mean_best_logloss")[, -1])
tmp_[, best := colnames(tmp)[apply(tmp, 1, which.max)]]
friedmanTest(tmp)  # Friedman's chi-squared = 20.978, df = 8, p-value = 0.007207
plotCD(tmp, cex = 1.5)

### HPO
hpo[, best_overall := min(best_logloss), by = .(problem)]
hpo[, worst_overall := max(best_logloss), by = .(problem)]
hpo[, regret := (best_logloss - best_overall) / (worst_overall - best_overall)]
hpo_agg_regret = hpo[, .(mean_regret = mean(regret), se_regret = sd(regret) / sqrt(.N)), by = .(problem, method, dim, task, batch_nr)]
hpo_agg_regret[, init := 4L * dim]
hpo_agg_regret[, final := 50L * dim]
hpo_agg_regret[, method := factor(method, levels = c("cmaes", "gensa_1", "gensa_2", "gensa_3", "gensa_4", "grid_search", "mbo", "random_search"), labels = c("CMAES", "GENSAv1", "GENSAv2", "GENSAv3", "GENSAv4", "Grid", "MBO", "Random"))]
hpo_agg_regret[, problem := factor(problem, levels = c("wilt_2", "ada_2", "car_2"))]

g = ggplot(aes(x = batch_nr, y = mean_regret, colour = method, fill = method), data = hpo_agg_regret[batch_nr > init & batch_nr <= final]) +
  scale_y_log10() +
  geom_step(aes(linetype = method)) +
  geom_stepribbon(aes(ymin = mean_regret - se_regret, ymax = mean_regret + se_regret), colour = NA, alpha = 0.1) +
  facet_wrap(~ problem, scales = "free") +
  xlab("Iteration") +
  ylab("Mean Normalized Regret") +
  scale_linetype_manual(values = c(1, 2, 2, 2, 2, 1, 1, 1)) +
  labs(color = "Optimizer", fill = "Optimizer", linetype = "Optimizer") +
  theme_minimal(base_size = 18) +
  theme(legend.position = "bottom")

ggsave("plots/hpo_gensa_normalized_regret.pdf", plot = g, width = 18, height = 5, device = "pdf")

