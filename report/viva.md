# Viva questions (concise answers)
1. **Why synthetic data?** Privacy, a known ground truth, reproducibility. Results are not real-world evidence.
2. **How do you avoid label leakage?** Detector selects only event_id, user_id, timestamp, status; a test shows shuffling/removing labels leaves features identical.
3. **Baseline vs evaluation?** Profiles fit on days 1-30 only; scoring/metrics on days 31-60.
4. **Why smooth hour probabilities?** Unseen hours would get P=0 and infinite surprisal; additive smoothing (alpha=0.5) keeps P>0 and sums to 1.
5. **Why a circular kernel?** Hours wrap: 23:00 and 00:00 are 1 h apart (circular distance min(|a-b|,24-|a-b|)).
6. **What is surprisal?** -log2 P(hour); rarer hours give larger values.
7. **Why a z-score for daily counts, and what is wrong with it?** Simple and explainable, but counts are discrete, skewed and over-dispersed; Poisson/negative binomial is better.
8. **Zero standard deviation?** sd floored at 1 login/day; z capped at 10.
9. **New user with little history?** Pooled hour profile; no frequency evidence.
10. **How is a burst defined?** Failures by one user within the trailing 10 minutes >= burst size.
11. **Combined score?** max of component/threshold ratios; flag when >= 1; reasons list which rules fired.
12. **Relaxed vs strict?** Strict uses lower cut-offs so it flags more; here recall rose and precision fell (measured, not assumed).
13. **Define precision, recall, FPR.** TP/(TP+FP), TP/(TP+FN), FP/(FP+TN); NA if denominator 0.
14. **Why is accuracy misleading?** Most events are normal, so flagging nothing already scores high.
15. **Biggest limitations?** Synthetic/self-injected anomalies, clean-baseline and stable-habit assumptions, device/location unused, subtle anomalies missed.
