library(data.table)
library(ggplot2)
library(pammtools)

dat = readRDS("ela_newdata_large_results.rds")
dat = dat[problem %nin% c("1590_xgboost_2", "40685_xgboost_2", "1590_xgboost_3", "1461_xgboost_3", "40685_xgboost_3", "1590_xgboost_5", "1461_xgboost_5", "40685_xgboost_5")]
dat$dim = as.numeric(map_chr(dat$problem, function(prob) strsplit(prob, split = "_")[[1L]][3L]))

design = dat[repl == 1 & method == "design", c("nrounds", "eta", "lambda", "gamma", "alpha", "task", "dim", "best_logloss")]
saveRDS(design, "design.rds")
optimizers = dat[method != "design"]
optimizers[method == "mbo", batch_nr := seq_len(.N), by = .(task, dim, repl)]
saveRDS(optimizers, "optimizers.rds")
agg = optimizers[, .(mean_ll = mean(best_logloss), sd_ll = sd(best_logloss), n = length(best_logloss)), by = .(batch_nr, method, task, dim)]
agg[, init := 4L * dim]
agg[, se_ll := sd_ll / sqrt(n)]
g = ggplot(aes(x = batch_nr, y = mean_ll, colour = method, fill = method), data = agg[batch_nr >= init]) +
  scale_y_log10() +
  geom_step() +
  geom_stepribbon(aes(ymin = mean_ll - se_ll, ymax = mean_ll + se_ll), colour = NA, alpha = 0.5) +
  facet_wrap(task ~ dim, scales = "free")

ggsave("optimizers.png", plot = g, width = 15, height = 8)

