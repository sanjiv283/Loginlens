# Measured results for the final report / slides. Run from repo root: Rscript report/final_results.R
# Output: report/results/*.csv and report/figures/*.png. Ground truth is used ONLY for scoring here.
for (f in c("generate_data", "detector", "evaluate")) source(file.path("R", paste0(f, ".R")))
suppressPackageStartupMessages(library(ggplot2))
dir.create("report/results", showWarnings = FALSE); dir.create("report/figures", showWarnings = FALSE)
prof <- threshold_profiles(); types <- c("unusual_time", "high_frequency", "failed_burst")

run_once <- function(ev) {
  fit <- fit_baseline(ev[ev$period == "baseline", ]); e <- ev$period == "evaluation"
  feat <- compute_features(ev, fit)[e, ]
  list(fit = fit, feat = feat, truth = ev$is_injected_anomaly[e], type = ev$anomaly_type[e],
       res = compare_profiles(feat, ev$is_injected_anomaly[e], prof))
}
ev <- read_login_data("data/login_events.csv"); r <- run_once(ev)
write.csv(r$res, "report/results/threshold_metrics.csv", row.names = FALSE)
by_type <- do.call(rbind, lapply(names(prof), function(p) {
  f <- apply_thresholds(r$feat, prof[[p]])$flag
  do.call(rbind, lapply(types, function(t) data.frame(profile = p, type = t, n = sum(r$type %in% t),
    detected = sum(f & r$type %in% t), recall = safe_div(sum(f & r$type %in% t), sum(r$type %in% t)))))
}))
write.csv(by_type, "report/results/recall_by_type.csv", row.names = FALSE)

# Seed-to-seed variability (same fixed thresholds, new synthetic datasets)
seeds <- 1:20
ss <- do.call(rbind, lapply(seeds, function(s) cbind(seed = s, run_once(generate_login_data(seed = s))$res)))
write.csv(ss, "report/results/seed_study.csv", row.names = FALSE)
summ <- do.call(rbind, lapply(names(prof), function(p) { d <- ss[ss$profile == p, ]
  data.frame(profile = p, metric = c("precision", "recall", "fpr", "f1"),
    mean = sapply(c("precision", "recall", "fpr", "f1"), function(m) mean(d[[m]], na.rm = TRUE)),
    sd = sapply(c("precision", "recall", "fpr", "f1"), function(m) sd(d[[m]], na.rm = TRUE)),
    min = sapply(c("precision", "recall", "fpr", "f1"), function(m) min(d[[m]], na.rm = TRUE)),
    max = sapply(c("precision", "recall", "fpr", "f1"), function(m) max(d[[m]], na.rm = TRUE)),
    n_na = sapply(c("precision", "recall", "fpr", "f1"), function(m) sum(is.na(d[[m]])))) }))
write.csv(summ, "report/results/seed_study_summary.csv", row.names = FALSE)

# Worked examples taken from real data (balanced profile)
sc <- apply_thresholds(r$feat, prof$balanced); sc$type <- r$type
ex <- function(t, col) { d <- sc[sc$flag & sc$type %in% t & !is.na(sc[[col]]), ]; d[which.max(d[[col]]), ] }
fz <- ex("high_frequency", "freq_z"); hb <- ex("failed_burst", "fail_burst_count"); ht <- ex("unusual_time", "hour_surprisal_bits")
ex_rows <- rbind(
  data.frame(kind = "frequency", event_id = fz$event_id, user = fz$user_id, ts = format(fz$timestamp), day_count = fz$day_count,
             user_mean = fz$user_mean_daily, user_sd_eff = fz$user_sd_daily, z = fz$freq_z, hour_prob = NA, burst = NA),
  data.frame(kind = "hour", event_id = ht$event_id, user = ht$user_id, ts = format(ht$timestamp), day_count = NA, user_mean = NA,
             user_sd_eff = NA, z = NA, hour_prob = ht$hour_prob, burst = NA),
  data.frame(kind = "burst", event_id = hb$event_id, user = hb$user_id, ts = format(hb$timestamp), day_count = NA, user_mean = NA,
             user_sd_eff = NA, z = NA, hour_prob = NA, burst = hb$fail_burst_count))
write.csv(ex_rows, "report/results/worked_examples.csv", row.names = FALSE)
u <- r$fit$users[[fz$user_id]]
write.csv(data.frame(user = fz$user_id, baseline_events = u$n, raw_baseline_sd = u$sd, mean = u$mean), "report/results/worked_user.csv", row.names = FALSE)

facts <- data.frame(item = c("total_events", "baseline_events", "evaluation_events", "users", "injected_anomalies_eval",
  "unusual_time", "high_frequency", "failed_burst", "users_below_min_events", "start", "end"),
  value = c(nrow(ev), sum(ev$period == "baseline"), sum(ev$period == "evaluation"), length(unique(ev$user_id)), sum(r$truth),
            sum(r$type %in% "unusual_time"), sum(r$type %in% "high_frequency"), sum(r$type %in% "failed_burst"),
            sum(sapply(r$fit$users, function(x) x$n < BASELINE_PARAMS$min_events)), format(min(ev$timestamp)), format(max(ev$timestamp))))
write.csv(facts, "report/results/dataset_facts.csv", row.names = FALSE)

# Figures
theme_set(theme_minimal(base_size = 12))
long <- tidyr::pivot_longer(r$res[, c("profile", "precision", "recall", "specificity", "f1")], -profile)
long$profile <- factor(long$profile, names(prof))
ggsave("report/figures/final_threshold_metrics.png", ggplot(long, aes(profile, value, fill = name)) + geom_col(position = "dodge") +
  scale_fill_manual(values = c(precision = "#1F3A5F", recall = "#3B7DD8", specificity = "#8FB4E3", f1 = "#9AA5B1")) +
  labs(x = NULL, y = "Value", fill = NULL), width = 7, height = 3.8, dpi = 150)
bt <- by_type; bt$profile <- factor(bt$profile, names(prof))
ggsave("report/figures/final_recall_by_type.png", ggplot(bt, aes(type, recall, fill = profile)) + geom_col(position = "dodge") +
  scale_fill_manual(values = c(relaxed = "#8FB4E3", balanced = "#3B7DD8", strict = "#1F3A5F")) + labs(x = NULL, y = "Recall", fill = NULL),
  width = 7, height = 3.8, dpi = 150)
ss$profile <- factor(ss$profile, names(prof))
ggsave("report/figures/final_seed_study.png", ggplot(ss, aes(profile, recall)) + geom_boxplot(fill = "#8FB4E3") +
  geom_jitter(width = .1, size = 1) + labs(x = NULL, y = "Recall over 20 seeds"), width = 5, height = 3.8, dpi = 150)
d <- ev[ev$period == "evaluation", ]; d$flag <- sc$flag
ggsave("report/figures/final_hours.png", ggplot(d, aes(login_hour, fill = flag)) + geom_histogram(binwidth = 1, boundary = -.5) +
  scale_fill_manual(values = c("#9AA5B1", "#1F3A5F"), labels = c("Normal", "Flagged")) + labs(x = "Hour of day (UTC)", y = "Events", fill = NULL),
  width = 7, height = 3.6, dpi = 150)
print(r$res); print(summ[summ$metric %in% c("recall", "precision"), ]); print(ex_rows); print(facts)

# Workflow diagram (editable boxes are in the slides; this PNG is for the report)
bx <- data.frame(x = c(1, 3.5, 6, 6, 3.5, 1, 6, 3.5, 1), y = c(3, 3, 3, 1.8, 1.8, 1.8, 0.6, 0.6, 0.6),
  lab = c("Synthetic generator\nR/generate_data.R (seed 42)", "data/login_events.csv\nbaseline days 1-30\nevaluation days 31-60", "fit_baseline()\nbaseline rows only",
          "compute_features()\nhour prob., daily z, burst count", "apply_thresholds()\nrelaxed / balanced / strict", "Flags, scores, reasons\n(no labels used)",
          "Ground-truth labels\n(evaluation only)", "evaluate.R\nconfusion matrix, metrics", "Shiny dashboard (app.R)\n4 tabs"),
  kind = c("a", "a", "a", "a", "a", "a", "g", "e", "a"))
ar <- data.frame(x = c(1.9, 4.4, 6, 5.1, 2.4, 3.5, 5.1, 1, 6), xend = c(2.6, 5.1, 6, 4.5, 1.7, 3.5, 4.4, 1, 6),
                 y = c(3, 3, 2.7, 1.8, 1.8, 1.5, 0.6, 1.5, 1.5), yend = c(3, 3, 2.1, 1.8, 1.8, 0.9, 0.6, 0.9, 0.9))
ar <- ar[1:7, ]; ar <- rbind(ar, data.frame(x = 1, xend = 1, y = 1.5, yend = 0.9))
ggsave("report/figures/final_architecture.png", ggplot() + geom_segment(data = ar, aes(x = x, xend = xend, y = y, yend = yend), arrow = arrow(length = unit(.12, "in")), colour = "#1F3A5F") +
  geom_label(data = bx, aes(x, y, label = lab, fill = kind), colour = "#1F3A5F", size = 3.4, label.padding = unit(.5, "lines")) +
  scale_fill_manual(values = c(a = "#E8EEF7", g = "#F3F4F6", e = "#C9D7EE"), guide = "none") + xlim(0, 7.2) + ylim(0.1, 3.4) + theme_void(),
  width = 9, height = 4.2, dpi = 150)
