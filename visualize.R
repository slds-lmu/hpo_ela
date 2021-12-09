library(data.table)
library(ggplot2)
library(pammtools)

dat = readRDS("ela_newdata_results.rds")
design = dat[repl == 1 & method == "design", c("classif.glmnet.alpha", "classif.glmnet.s", "classif.logloss", "task")]
saveRDS(design, "design.rds")
optimizers = dat[method != "design"]
optimizers[method == "mbo", batch_nr := 1:100, by = .(task, repl)]
saveRDS(optimizers, "optimizers.rds")
agg = optimizers[, .(mean_ll = mean(best_logloss), sd_ll = sd(best_logloss), n = length(best_logloss)), by = .(batch_nr, method, task)]
agg[, se_ll := sd_ll / sqrt(n)]
ggplot(aes(x = batch_nr, y = mean_ll, colour = method, fill = method), data = agg) +
  geom_step() +
  geom_stepribbon(aes(ymin = mean_ll - se_ll, ymax = mean_ll + se_ll), colour = NA, alpha = 0.5) +
  facet_wrap(~ task, scales = "free")

library(akima)

fld = with(design[task == "dresses-sales", ], interp(x = classif.glmnet.alpha, y = classif.glmnet.s, z = classif.logloss))
filled.contour(x = fld$x, y = fld$y, z = fld$z, xlab = "classif.glmnet.alpha", ylab = "classif.glmnet.s", main = "classif.logloss")

