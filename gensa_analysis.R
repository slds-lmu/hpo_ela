library(data.table)
library(mlr3misc)
library(ggplot2)

# HPO, reference is median best performance of random search with equal 50d budget
hpo = readRDS("data/optimizers.rds")
hpo[, budget := 50L * dim]

gensa = hpo[method == "gensa" & problem == "wilt_2" & repl == 3]
ggplot(aes(x = nrounds, y = eta, colour = -classif.logloss), data = gensa) +
  geom_text(aes(label = batch_nr, colour = -classif.logloss))

mbo = hpo[method == "mbo" & problem == "wilt_2" & repl == 1]
ggplot(aes(x = nrounds, y = eta, colour = -classif.logloss), data = mbo) +
  geom_text(aes(label = batch_nr, colour = -classif.logloss))

