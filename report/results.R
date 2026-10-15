# Computes all reported results (run from repo root). Labels used only for evaluation here.
for (f in c("generate_data", "detector", "evaluate")) source(file.path("R", paste0(f, ".R")))
library(ggplot2)
ev   <- read_login_data("data/login_events.csv")
fit  <- fit_baseline(ev[ev$period == "baseline", ])
e    <- ev$period == "evaluation"
feat <- compute_features(ev, fit)[e, ]
truth <- ev$is_injected_anomaly[e]; type <- ev$anomaly_type[e]
prof <- threshold_profiles()
res  <- compare_profiles(feat, truth, prof)
by_type <- do.call(rbind, lapply(names(prof), function(p) {
  f <- apply_thresholds(feat, prof[[p]])$flag
  do.call(rbind, lapply(c("unusual_time", "high_frequency", "failed_burst"), function(t)
    data.frame(profile = p, type = t, n = sum(type %in% t), detected = sum(f & type %in% t),
               recall = safe_div(sum(f & type %in% t), sum(type %in% t)))))
}))
fmt3 <- function(d) { for (m in intersect(c("precision", "recall", "fpr", "accuracy"), names(d))) d[[m]] <- ifelse(is.na(d[[m]]), "NA", sprintf("%.3f", d[[m]])); d }
dir.create("report/figures", showWarnings = FALSE)
theme_set(theme_minimal(base_size = 12))
long <- tidyr::pivot_longer(res[, c("profile", "precision", "recall", "fpr", "accuracy")], -profile)
long$profile <- factor(long$profile, names(prof))
ggsave("report/figures/threshold_comparison.png",
  ggplot(long, aes(profile, value, fill = name)) + geom_col(position = "dodge") + labs(x = NULL, y = NULL, fill = NULL,
    title = "Threshold profiles on the synthetic evaluation period"), width = 7, height = 4, dpi = 120)
d <- ev[e, ]; d$flag <- apply_thresholds(feat, prof$balanced)$flag
ggsave("report/figures/hour_distribution.png",
  ggplot(d, aes(login_hour, fill = flag)) + geom_histogram(binwidth = 1, boundary = -0.5) +
    scale_fill_manual(values = c("grey60", "firebrick"), labels = c("normal", "flagged")) +
    labs(x = "Hour (UTC)", y = "Events", fill = NULL, title = "Login hours, balanced profile"), width = 7, height = 4, dpi = 120)
d$date <- as.Date(d$timestamp)
ggsave("report/figures/activity_over_time.png",
  ggplot(d, aes(date, fill = flag)) + geom_bar() + scale_fill_manual(values = c("grey60", "firebrick"), labels = c("normal", "flagged")) +
    labs(x = NULL, y = "Events/day", fill = NULL, title = "Daily logins, balanced profile"), width = 7, height = 4, dpi = 120)
# architecture diagram
bx <- data.frame(x = c(1, 3, 5, 7, 9), label = c("generate_data.R\nsynthetic events", "Baseline period\nfit_baseline()", "compute_features()\nhour | z | burst",
      "apply_thresholds()\nscore, flag, reason", "Shiny app + evaluate.R\n(labels used here only)"))
ggsave("report/figures/architecture.png", ggplot(bx) +
  geom_segment(aes(x = x + .8, xend = x + 1.2, y = 1, yend = 1), data = bx[1:4, ], arrow = arrow(length = unit(.15, "in"))) +
  geom_label(aes(x, 1, label = label), size = 3, label.padding = unit(.4, "lines")) + xlim(0, 10.2) + theme_void(),
  width = 11, height = 2.2, dpi = 120)
cat("Total events:", nrow(ev), "| baseline:", sum(!e), "| evaluation:", sum(e), "| injected:", sum(truth), "\n")
