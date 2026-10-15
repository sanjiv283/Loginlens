# Evaluation (uses ground truth AFTER detection) and helpers for the app.

safe_div <- function(a, b) if (is.na(b) || b == 0) NA_real_ else a / b

confusion_metrics <- function(flag, truth) {
  ok <- !is.na(flag) & !is.na(truth)
  f <- as.logical(flag[ok]); t <- as.logical(truth[ok])
  tp <- sum(f & t); fp <- sum(f & !t); tn <- sum(!f & !t); fn <- sum(!f & t)
  data.frame(TP = tp, FP = fp, TN = tn, FN = fn, flagged = tp + fp,
             precision = safe_div(tp, tp + fp), recall = safe_div(tp, tp + fn),
             fpr = safe_div(fp, fp + tn), accuracy = safe_div(tp + tn, tp + fp + tn + fn))
}

# Compare several profiles on the same scored-feature table; truth is supplied only here.
compare_profiles <- function(feat, truth, profiles) {
  do.call(rbind, lapply(names(profiles), function(nm) {
    s <- apply_thresholds(feat, profiles[[nm]])
    cbind(profile = nm, confusion_metrics(s$flag, truth))
  }))
}

# Explorer filtering. date_range: length-2 Dates (NULL = no filter); empty vectors = no filter.
filter_events <- function(df, users = NULL, date_range = NULL, status = NULL, flag = "all", search = "") {
  if (!nrow(df)) return(df)
  if (length(users))  df <- df[df$user_id %in% users, , drop = FALSE]
  if (length(status)) df <- df[df$status %in% status, , drop = FALSE]
  if (length(date_range) == 2 && !anyNA(date_range)) {
    d <- as.Date(df$timestamp)
    df <- df[!is.na(d) & d >= date_range[1] & d <= date_range[2], , drop = FALSE]
  }
  if (flag == "flagged") df <- df[df$flag, , drop = FALSE]
  if (flag == "normal")  df <- df[!df$flag, , drop = FALSE]
  if (nzchar(search)) {
    hit <- grepl(tolower(search), tolower(do.call(paste, df)), fixed = TRUE)
    df <- df[hit, , drop = FALSE]
  }
  df
}

export_csv <- function(df, file) {
  out <- df; out$timestamp <- format(out$timestamp, "%Y-%m-%d %H:%M:%S", tz = "UTC")
  write.csv(out, file, row.names = FALSE)
}
