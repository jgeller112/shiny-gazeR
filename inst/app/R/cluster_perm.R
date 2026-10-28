# Multilevel cluster-based permutation test for the Results tab (used only by
# the app). A model is fit in each time bin; clusters of adjacent bins past a
# threshold are compared with the largest cluster mass under shuffled
# condition labels (Maris & Oostenveld, 2007).

# Tests where in time two (or more) conditions differ, controlling the
# family-wise error rate across time bins (Maris & Oostenveld, 2007). A
# model is fit in every time bin; adjacent bins whose statistic passes a
# threshold form clusters; each cluster's mass (the sum of its statistics)
# is compared with the largest cluster mass found when the condition labels
# are shuffled.
# * **Model:** `formula` is fit separately in each time bin. With random
#   effects (e.g. `aggbaseline ~ script + (1 | subject)`) it is a mixed model
#   fit with [lme4::lmer()], so trial-level data with by-subject and by-item
#   random effects can be tested directly; without them it is [stats::lm()].
# * **Statistic:** for a term with one coefficient (e.g. a two-level
#   condition), its signed t value; clusters must keep one sign. For a factor
#   with more levels, its F (from [stats::anova()]).
# * **Threshold:** by default the two-sided critical value for `alpha`: the
#   normal quantile for mixed models (e.g. 1.96), the t or F quantile for
#   `lm()`. Or give `threshold` directly.
# * **Permutation:** the labels in `perm_var` are shuffled between units
#   (`unit`, e.g. each subject's condition cells, or trials), and each unit
#   keeps its whole time course, preserving the correlation between time
#   bins. If `perm_var` varies within subjects, labels are shuffled within
#   each subject; if it is constant within subjects (between-subject), the
#   subjects' labels are shuffled.
# * **p-value** of each observed cluster: the proportion of permutations
#   whose largest |cluster mass| is at least as large (counting the observed
#   data as one permutation).
# Rows are first averaged within each unit and time bin, so binned samples
# (e.g. from [downsample_gaze()] or with a `timebins` column) can be passed
# as they are.
# @param data data frame with the response, predictors, subject and time
#   bin columns.
# @param formula model formula, e.g. `aggbaseline ~ script + (1 | subject)`.
# @param term the term to test (e.g. `"script"`). Defaults to `perm_var`.
# @param perm_var the condition whose labels are shuffled (e.g.
#   `"script"`). Defaults to the first fixed-effect predictor.
# @param time_col time bin column.
# @param subject subject column.
# @param unit columns identifying the unit whose label is shuffled.
#   Defaults to subject and trial if there is a `trial` column, otherwise
#   each subject × `perm_var` cell.
# @param n_perm number of permutations (1000 or more for publication).
# @param alpha cluster-forming alpha, used when `threshold` is `NULL`.
# @param threshold cluster-forming threshold on |t| (or F).
# @param seed random seed, for reproducible permutations.
# @param progress show a progress bar.
# @return An object of class `"gazer_cluster_perm"`: a list with
#   `clusters` (one row per observed cluster: `cluster`, `start`, `end`,
#   `bins`, `mass`, `sign`, `p`), `bins` (per time bin: `time`, `stat`,
#   `cluster`), `null` (largest |mass| per permutation), and the settings.
#   It has `print()` and `plot()` methods.
#   testing of EEG- and MEG-data. *Journal of Neuroscience Methods, 164*,
#   177–190.
# \donttest{
# if (requireNamespace("lme4", quietly = TRUE)) {
#   res <- cluster_permutation(cursive_agg_data, aggbaseline ~ script + (1 | subject),
#                              n_perm = 100, seed = 1)
#   res
# }
# }
cluster_permutation <- function(data, formula, term = NULL, perm_var = NULL,
                                time_col = "timebins", subject = "subject", unit = NULL,
                                n_perm = 1000, alpha = 0.05, threshold = NULL,
                                seed = NULL, progress = FALSE, labels = list()) {
  formula <- stats::as.formula(formula)
  mixed <- any(grepl("|", deparse(formula[[3]]), fixed = TRUE))
  if (mixed && !requireNamespace("lme4", quietly = TRUE)) {
    stop("Random effects in `formula` need the lme4 package: install.packages(\"lme4\").", call. = FALSE)
  }
  fixed <- if (mixed) drop_bars(formula) else formula
  resp <- all.vars(formula[[2]])
  preds <- all.vars(fixed[[3]])
  if (is.null(perm_var)) {
    if (!length(preds)) stop("No fixed-effect predictor in `formula`.", call. = FALSE)
    perm_var <- preds[1]
  }
  if (is.null(term)) term <- perm_var
  if (is.null(unit)) unit <- if ("trial" %in% names(data)) c(subject, "trial") else c(subject, perm_var)
  vars <- unique(c(all.vars(formula), time_col, subject, unit))
  missing_cols <- setdiff(vars, names(data))
  if (length(missing_cols)) stop("`data` has no column(s): ", paste(missing_cols, collapse = ", "), call. = FALSE)

  # One row per unit, other model variables and time bin
  d <- as.data.frame(data)[vars]
  d <- d[!is.na(d[[resp]]), , drop = FALSE]
  keys <- setdiff(vars, resp)
  d <- stats::aggregate(d[resp], d[keys], mean)
  d[[perm_var]] <- if (is.numeric(d[[perm_var]])) d[[perm_var]] else factor(d[[perm_var]])
  times <- sort(unique(d[[time_col]]))
  by_bin <- split(seq_len(nrow(d)), match(d[[time_col]], times))

  # Units and their labels; within- or between-subject shuffling
  ukey <- do.call(paste, c(d[unit], sep = "\r"))
  units <- d[!duplicated(ukey), unique(c(unit, subject, perm_var)), drop = FALSE]
  units$.key <- ukey[!duplicated(ukey)]
  if (any(duplicated(units$.key))) stop("`perm_var` must be constant within each unit.", call. = FALSE)
  within <- any(tapply(as.character(units[[perm_var]]), units[[subject]], function(v) length(unique(v)) > 1))
  row_unit <- match(ukey, units$.key)
  shuffle <- function() {
    lab <- units[[perm_var]]
    if (within) {
      for (idx in split(seq_len(nrow(units)), units[[subject]])) lab[idx] <- lab[idx][sample.int(length(idx))]
    } else {
      subs <- unique(units[[subject]])
      slab <- lab[match(subs, units[[subject]])][sample.int(length(subs))]
      lab <- slab[match(units[[subject]], subs)]
    }
    lab[row_unit]
  }

  # Statistic in each bin
  ctrl <- if (mixed) lme4::lmerControl(calc.derivs = FALSE, check.conv.singular = "ignore",
                                       check.conv.grad = "ignore", check.conv.hess = "ignore")
  one_df <- NULL
  fit_stat <- function(dd) {
    m <- if (mixed) suppressMessages(suppressWarnings(lme4::lmer(formula, data = dd, REML = FALSE, control = ctrl)))
         else stats::lm(formula, data = dd)
    co <- if (mixed) stats::coef(summary(m)) else summary(m)$coefficients
    rows <- grep(paste0("^", term), rownames(co))
    rows <- rows[!grepl(":", rownames(co)[rows])]
    if (is.null(one_df)) one_df <<- length(rows) == 1
    if (one_df) {
      if (!length(rows)) return(NA_real_)
      return(unname(co[rows[1], 3]))   # t value
    }
    a <- stats::anova(m)
    unname(a[term, "F value"])
  }
  stats_for <- function(lab = NULL) {
    vapply(by_bin, function(i) {
      dd <- d[i, , drop = FALSE]
      if (!is.null(lab)) dd[[perm_var]] <- lab[i]
      tryCatch(fit_stat(dd), error = function(e) NA_real_)
    }, numeric(1))
  }
  obs <- stats_for()
  if (all(is.na(obs))) stop("The model could not be fit in any time bin. Check `formula` and `term`.", call. = FALSE)

  if (is.null(threshold)) {
    threshold <- if (one_df) {
      if (mixed) stats::qnorm(1 - alpha / 2) else {
        m0 <- stats::lm(formula, data = d[by_bin[[1]], , drop = FALSE])
        stats::qt(1 - alpha / 2, m0$df.residual)
      }
    } else {
      df1 <- nlevels(d[[perm_var]]) - 1
      if (mixed) stats::qchisq(1 - alpha, df1) / df1 else {
        m0 <- stats::lm(formula, data = d[by_bin[[1]], , drop = FALSE])
        stats::qf(1 - alpha, df1, m0$df.residual)
      }
    }
  }
  find_clusters <- function(s) {
    pass <- !is.na(s) & abs(s) > threshold
    sgn <- ifelse(pass, sign(s), 0)
    id <- cumsum(c(TRUE, sgn[-1] != sgn[-length(sgn)])) * (sgn != 0)
    if (!any(id > 0)) return(list(id = id, mass = numeric()))
    mass <- tapply(s[id > 0], id[id > 0], sum)
    list(id = id, mass = mass)
  }
  oc <- find_clusters(obs)
  means <- condition_means(d, resp, perm_var, subject, time_col, within)

  if (!is.null(seed)) set.seed(seed)
  null <- numeric(n_perm)
  # `progress`: TRUE for a text bar, or a function(k, n_perm) (e.g. Shiny's)
  pb <- if (isTRUE(progress)) utils::txtProgressBar(min = 0, max = n_perm, style = 3)
  for (k in seq_len(n_perm)) {
    pc <- find_clusters(stats_for(shuffle()))
    null[k] <- if (length(pc$mass)) max(abs(pc$mass)) else 0
    if (isTRUE(progress)) utils::setTxtProgressBar(pb, k) else if (is.function(progress)) progress(k, n_perm)
  }
  if (isTRUE(progress)) close(pb)

  ids <- as.integer(names(oc$mass))
  clusters <- data.frame(
    cluster = seq_along(ids),
    start = vapply(ids, function(i) min(times[oc$id == i]), numeric(1)),
    end = vapply(ids, function(i) max(times[oc$id == i]), numeric(1)),
    bins = vapply(ids, function(i) sum(oc$id == i), integer(1)),
    mass = unname(as.numeric(oc$mass)),
    sign = unname(sign(as.numeric(oc$mass))),
    p = vapply(abs(as.numeric(oc$mass)), function(m) (sum(null >= m) + 1) / (n_perm + 1), numeric(1))
  )
  bins <- data.frame(time = times, stat = unname(obs),
                     cluster = ifelse(oc$id > 0, match(oc$id, ids), NA_integer_))
  structure(list(clusters = tibble::as_tibble(clusters), bins = tibble::as_tibble(bins), null = null,
                 means = means, response = if (is.null(labels$response)) resp else labels$response,
                 cond_label = if (is.null(labels$condition)) perm_var else labels$condition,
                 formula = formula, term = term, perm_var = perm_var, statistic = if (one_df) "t" else "F",
                 threshold = threshold, n_perm = n_perm, within = within, mixed = mixed,
                 n_units = nrow(units), n_subjects = length(unique(units[[subject]]))),
            class = "gazer_cluster_perm")
}

print_cluster_perm <- function(x, ...) {
  cat("Cluster-based permutation test (", if (x$mixed) "mixed model" else "linear model", " per time bin)\n", sep = "")
  cat("  ", deparse(x$formula), " | term: ", x$term, " | ", x$statistic, " threshold: ", round(x$threshold, 3), "\n", sep = "")
  cat("  ", x$n_perm, " permutations of ", x$perm_var, " (", if (x$within) "within" else "between", " subjects; ",
      x$n_units, " units, ", x$n_subjects, " subjects)\n", sep = "")
  if (!nrow(x$clusters)) {
    cat("  No clusters passed the threshold.\n")
  } else {
    print(as.data.frame(x$clusters), row.names = FALSE, digits = 4)
  }
  invisible(x)
}

# Plot a cluster-based permutation test
# `type = "timecourse"` (default): the mean of each condition over time
# (mean of subject means), with error ribbons (within-subject SE, Cousineau-
# Morey, for within-subject conditions; between-subject SE otherwise), and
# the clusters as bars underneath: coloured and labelled with their p-value
# if significant, grey otherwise. `type = "statistic"`: the statistic in
# each time bin with the threshold.
# @param x result of [cluster_permutation()].
# @param type `"timecourse"` or `"statistic"`.
# @param alpha significance level for highlighting clusters.
# @param ... unused.
# @return A ggplot.
plot_cluster_perm <- function(x, type = c("timecourse", "statistic"), alpha = 0.05, ...) {
  type <- match.arg(type)
  cl <- as.data.frame(x$clusters)
  b <- x$bins
  step <- if (nrow(b) > 1) stats::median(diff(b$time)) else 1
  cl$sig <- cl$p < alpha
  cl$label <- ifelse(cl$p < .001, "p < .001", paste0("p = ", sub("^0", "", sprintf("%.3f", cl$p))))
  if (type == "statistic" || is.null(x$means)) {
    thr <- if (x$statistic == "t") c(-x$threshold, x$threshold) else x$threshold
    return(ggplot2::ggplot(b, ggplot2::aes(time, stat)) +
      ggplot2::geom_rect(data = cl[cl$sig, ], inherit.aes = FALSE,
                         ggplot2::aes(xmin = start - step / 2, xmax = end + step / 2, ymin = -Inf, ymax = Inf),
                         fill = "#3b5bdb", alpha = 0.15) +
      ggplot2::geom_hline(yintercept = thr, linetype = "dashed", colour = "#c92a2a") +
      ggplot2::geom_hline(yintercept = 0, colour = "grey70") +
      ggplot2::geom_line() + ggplot2::geom_point(size = 1) +
      ggplot2::labs(x = "Time (ms)", y = paste0(x$statistic, " (", x$term, ")"),
                    subtitle = sprintf("Shaded: clusters with p < %.2f (%d permutations). Dashed: threshold.", alpha, x$n_perm)) +
      ggplot2::theme_minimal(base_size = 12))
  }
  m <- x$means
  lo <- min(m$mean - m$se, na.rm = TRUE); hi <- max(m$mean + m$se, na.rm = TRUE)
  span <- hi - lo
  bar_y <- lo - 0.12 * span
  cl$y <- bar_y
  ggplot2::ggplot(m, ggplot2::aes(time, mean, colour = condition, fill = condition)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = mean - se, ymax = mean + se), alpha = 0.2, colour = NA) +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::geom_segment(data = cl, inherit.aes = FALSE,
                          ggplot2::aes(x = start - step / 2, xend = end + step / 2, y = y, yend = y),
                          linewidth = 3.5, lineend = "butt",
                          colour = ifelse(cl$sig, "#212529", "#ced4da")) +
    ggplot2::geom_text(data = cl[cl$sig, ], inherit.aes = FALSE,
                       ggplot2::aes(x = (start + end) / 2, y = y, label = label),
                       vjust = 2, size = 3.5) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0.12, 0.05))) +
    ggplot2::labs(x = "Time (ms)", y = x$response, colour = x$cond_label, fill = x$cond_label,
                  caption = sprintf("Bars: clusters (|%s| > %.2f); dark = p < %.2f, %d permutations. Ribbons: %s SE.",
                                    x$statistic, x$threshold, alpha, x$n_perm,
                                    if (x$within) "within-subject" else "between-subject")) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(legend.position = "top", plot.caption = ggplot2::element_text(colour = "grey40"))
}

# Drop random-effect terms (e.g. (1 | subject)) from a formula
drop_bars <- function(f) {
  tl <- attr(stats::terms(f), "term.labels")
  keep <- tl[!grepl("|", tl, fixed = TRUE)]
  stats::reformulate(if (length(keep)) keep else "1", response = f[[2]])
}

# Mean of subject means per condition and time bin, with SE: within-subject
# (Cousineau-Morey) for within-subject conditions, between-subject otherwise.
# NULL if the condition is numeric.
condition_means <- function(d, resp, cond, subject, time_col, within) {
  if (is.numeric(d[[cond]])) return(NULL)
  s <- stats::aggregate(d[resp], d[c(subject, cond, time_col)], mean)
  names(s) <- c("subject", "condition", "time", "y")
  if (within) {
    J <- length(unique(s$condition))
    sm <- stats::ave(s$y, s$subject, s$time, FUN = mean)
    gm <- stats::ave(s$y, s$time, FUN = mean)
    s$yn <- s$y - sm + gm
    corr <- sqrt(J / (J - 1))
  } else {
    s$yn <- s$y
    corr <- 1
  }
  out <- stats::aggregate(cbind(mean = s$y) ~ condition + time, data = s, FUN = mean)
  se <- stats::aggregate(cbind(se = s$yn) ~ condition + time, data = s,
                         FUN = function(v) if (length(v) > 1) stats::sd(v) / sqrt(length(v)) else NA_real_)
  out <- merge(out, se, by = c("condition", "time"))
  out$se <- out$se * corr
  out$condition <- factor(out$condition)
  out[order(out$condition, out$time), ]
}

