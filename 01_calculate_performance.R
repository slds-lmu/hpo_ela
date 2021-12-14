wd = dirname(rstudioapi::getSourceEditorContext()$path)
setwd(wd)

library(tidyverse)
library(foreach)

REPL = 10
BUDGET = 100
PAR_FACTOR = 10


data = readRDS("data/optimizers.rds") %>%
  as_tibble()


targets = data %>%
  filter(method == "random_search") %>%
  group_by(task, repl) %>%
  summarize(best_performance = min(best_logloss)) %>%
  ungroup() %>%
  group_by(task) %>%
  summarize(median_rs = median(best_performance)) %>%
  ungroup()

merged_data = data %>%
  select(-c("classif.glmnet.alpha", "classif.glmnet.s", "best_logloss")) %>%
  #filter(method != "random_search") %>%
  inner_join(targets, by = "task")
#   group_by(method, task, repl) %>%
# 
#   filter(classif.logloss <= median_rs) %>%
#   filter(batch_nr == min(batch_nr))
# complete(method, task, repl, fill = list(classif.logloss = NA, 
#                                          batch_nr = 0, median_rs = NA))
# mutate(count = length(batch_nr))
#group_modify(~ add_row(.x,.before=0))

individual_runtime_data = foreach(tmp_method = unique(merged_data$method), .combine = rbind) %:%
  foreach(tmp_task = unique(merged_data$task), .combine = rbind) %:%
  foreach(tmp_repl = unique(merged_data$repl), .combine = rbind) %do% {
    tmp = merged_data %>%
      filter(method == tmp_method & task == tmp_task & repl == tmp_repl & classif.logloss <= median_rs) %>%
      filter(batch_nr == min(batch_nr)) %>%
      as.data.frame()
    
    if(nrow(tmp) == 0) {
      tmp = data.frame(classif.logloss = NA, batch_nr = NA, method = tmp_method, task = tmp_task, repl = tmp_repl, median_rs = NA)
    }
    tmp
  }


ert_data = foreach(tmp_method = unique(individual_runtime_data$method), .combine = rbind) %:%
  foreach(tmp_task = unique(individual_runtime_data$task), .combine = rbind) %do% {
    tmp = individual_runtime_data %>%
      filter(method == tmp_method & task == tmp_task)
    
    suc_runs = sum(!is.na(tmp$batch_nr))
    
    if(suc_runs > 0 ) {
      tmp[is.na(tmp$batch_nr), "batch_nr"] = BUDGET
      ert = sum(tmp$batch_nr, na.rm = T)/suc_runs
    } else {
      ert = BUDGET * REPL * PAR_FACTOR
    }
    
    data.frame(method = tmp_method, task = tmp_task, ert = ert, n_succ_runs = suc_runs)
  }


# par10 is 10000, set to NA for heatmap
ert_data[which(ert_data$ert == 10000), "ert"] = NA

ggplot(ert_data, aes(x = method, y = task)) +
  geom_tile(aes(fill = ert)) +
  geom_text(aes(label = round(ert, 2))) +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red") +
  labs(x = "Method", y = "Task", fill = "ERT")


