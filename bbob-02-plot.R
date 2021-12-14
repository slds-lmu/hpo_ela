library(ggplot2)
library(pammtools)

full_data <- readr::read_rds("data/bbob_full.Rds")

data_summary <- full_data %>%
  group_by(batch_nr, method, fid, iid) %>%
  summarize(mean_best_y = mean(best_y),
            se_best_y = sd(best_y) / n())

#### Common Plot ####

plot <- ggplot(data_summary, aes(batch_nr, mean_best_y)) +
  geom_step(aes(color = method)) +
  geom_stepribbon(aes(ymin = mean_best_y - se_best_y,
                      ymax = mean_best_y + se_best_y,
                      fill = method), alpha = 0.5) +
  facet_wrap(~c(fid), scales = "free_y")

ggsave("plots/bbob-all-iid-1.png", plot, width = unit(12, "in"), height = unit(9, "in"))

#### Individual Plots ####

for (p_fid in FIDS) {
  for (p_iid in IIDS) {
    plot <- ggplot(data_summary %>% filter(fid == p_fid & iid == p_iid),
                   aes(batch_nr, mean_best_y)) +
      geom_step(aes(color = method)) +
      geom_stepribbon(aes(ymin = mean_best_y - se_best_y,
                          ymax = mean_best_y + se_best_y,
                          fill = method), alpha = 0.5) +
      labs(title = paste0("BBOB - FID: ", p_fid, ", IID: ", p_iid, " (", N_REPL, " replications)"))

    ggsave(paste0("plots/bbob-", p_fid, "-", p_iid, ".png"), plot, width = unit(9, "in"), height = unit(9, "in"))
  }
}
