##########################################################################################
# ROC CURVE AND CONFUSION MATRIX
##########################################################################################
#' ROC curve with cost-optimal cutoff
#'
#' Computes the ROC curve and an associated cost curve for a binary classifier.
#' The optimal decision threshold is the cutoff that minimises the weighted sum
#' of false-positive and false-negative costs.  Returns a side-by-side plot of
#' the ROC and cost curves, the optimal cutoff, total cost, AUC, sensitivity,
#' and specificity.
#'
#' @param data A \code{data.frame} or \code{data.table} containing at least the
#'   predicted score column and the actual outcome column.
#' @param predict Character. Name of the column holding the predicted
#'   probability or score (numeric, typically 0–1).
#' @param actual Character. Name of the column holding the binary actual outcome
#'   (\code{0} = negative, \code{1} = positive).
#' @param cost.fp Numeric. Cost incurred for each false positive.
#' @param cost.fn Numeric. Cost incurred for each false negative.
#'
#' @return A named list with six elements:
#' \describe{
#'   \item{plot}{A \code{gtable} object (from \code{gridExtra::arrangeGrob})
#'     containing the ROC curve and the cost curve side by side.  Pass to
#'     \code{grid::grid.draw()} or \code{gridExtra::grid.arrange()} to display.}
#'   \item{cutoff}{Numeric. The optimal decision threshold.}
#'   \item{totalcost}{Numeric. Total cost at the optimal cutoff.}
#'   \item{auc}{Numeric. Area under the ROC curve.}
#'   \item{sensitivity}{Numeric. True positive rate at the optimal cutoff
#'     (TP / (TP + FN)).}
#'   \item{specificity}{Numeric. True negative rate at the optimal cutoff
#'     (TN / (FP + TN)).}
#' }
#'
#' @details
#' Total cost at each threshold is computed as:
#' \deqn{cost = FPR \times N_{neg} \times cost_{fp} + FNR \times N_{pos} \times cost_{fn}}
#' where \eqn{N_{neg}} and \eqn{N_{pos}} are the number of negative and positive
#' instances respectively.  The threshold that minimises this expression is
#' returned as \code{cutoff}.
#'
#' Points on both curves are coloured on a green → orange → red → black gradient
#' according to their normalised cost, making high-cost regions immediately
#' visible.
#'
#' @importFrom ROCR prediction performance
#' @importFrom ggplot2 ggplot aes geom_line geom_point geom_segment geom_hline
#'   geom_vline labs ggtitle scale_y_continuous theme_bw
#' @importFrom scales comma
#' @importFrom gridExtra arrangeGrob
#' @importFrom grid textGrob gpar
#' @importFrom grDevices colorRampPalette rgb
#'
#' @export
#'
#' @examples
#' # Fit a logistic regression on the built-in admission dataset,
#' # then evaluate the classifier with equal fp/fn costs.
#' data(df_admission)
#' model <- glm(admit ~ gre + gpa + rank,
#'              data   = df_admission,
#'              family = binomial)
#' df_admission$score <- predict(model, type = "response")
#'
#' result <- plot_roc_info(
#'   data     = df_admission,
#'   predict  = "score",
#'   actual   = "admit",
#'   cost.fp  = 1,
#'   cost.fn  = 1
#' )
#'
#' grid::grid.draw(result$plot)
#' cat("Optimal cutoff :", round(result$cutoff,     3), "\n")
#' cat("AUC            :", round(result$auc,         3), "\n")
#' cat("Sensitivity    :", round(result$sensitivity, 3), "\n")
#' cat("Specificity    :", round(result$specificity, 3), "\n")
#'
#' # False negatives cost twice as much as false positives
#' result2 <- plot_roc_info(
#'   data     = df_admission,
#'   predict  = "score",
#'   actual   = "admit",
#'   cost.fp  = 1,
#'   cost.fn  = 2
#' )
#' cat("Cutoff shifts to:", round(result2$cutoff, 3),
#'     "(lower threshold catches more positives)\n")
plot_roc_info <- function(data, predict, actual, cost.fp, cost.fn) {
  pred    <- ROCR::prediction(data[[predict]], data[[actual]])
  perf    <- ROCR::performance(pred, "tpr", "fpr")
  roc_dt  <- data.frame(fpr = perf@x.values[[1]], tpr = perf@y.values[[1]])

  cost <- perf@x.values[[1]] * cost.fp  * sum(data[[actual]] == 0) +
         (1 - perf@y.values[[1]]) * cost.fn * sum(data[[actual]] == 1)
  cost_dt <- data.frame(cutoff = pred@cutoffs[[1]], cost = cost)

  best_index  <- which.min(cost)
  best_cost   <- cost_dt[best_index, "cost"]
  best_tpr    <- roc_dt[best_index,  "tpr"]
  best_fpr    <- roc_dt[best_index,  "fpr"]
  best_cutoff <- pred@cutoffs[[1]][best_index]

  auc <- ROCR::performance(pred, "auc")@y.values[[1]]

  normalize   <- function(v) (v - min(v)) / diff(range(v))
  col_ramp    <- grDevices::colorRampPalette(c("green", "orange", "red", "black"))(100)
  col_by_cost <- col_ramp[ceiling(normalize(cost) * 99) + 1]

  roc_plot <- ggplot2::ggplot(roc_dt, ggplot2::aes(fpr, tpr)) +
    ggplot2::geom_line(color = grDevices::rgb(0, 0, 1, alpha = 0.1)) +
    ggplot2::geom_point(color = col_by_cost, size = 1, alpha = 0.1) +
    ggplot2::geom_segment(ggplot2::aes(x = 0, y = 0, xend = 1, yend = 1), alpha = 0.5) +
    ggplot2::labs(title = "ROC", x = "False Positive Rate", y = "True Positive Rate") +
    ggplot2::geom_hline(yintercept = best_tpr, alpha = 0.8, linetype = "dashed", color = "steelblue4") +
    ggplot2::geom_vline(xintercept = best_fpr, alpha = 0.8, linetype = "dashed", color = "steelblue4") +
    ggplot2::theme_bw()

  cost_plot <- ggplot2::ggplot(cost_dt, ggplot2::aes(cutoff, cost)) +
    ggplot2::geom_line(color = "blue", alpha = 0.5) +
    ggplot2::geom_point(color = col_by_cost, size = 4, alpha = 0.5) +
    ggplot2::ggtitle("Cost") +
    ggplot2::scale_y_continuous(labels = scales::comma) +
    ggplot2::geom_vline(xintercept = best_cutoff, alpha = 0.8, linetype = "dashed", color = "steelblue4")

  sub_title <- sprintf("Cutoff at %.2f - Total Cost = %d, AUC = %.3f",
                       best_cutoff, best_cost, auc)
  plot <- gridExtra::arrangeGrob(roc_plot, cost_plot, ncol = 2,
                                 top = grid::textGrob(sub_title,
                                                      gp = grid::gpar(fontsize = 16,
                                                                      fontface = "bold")))
  list(
    plot        = plot,
    cutoff      = best_cutoff,
    totalcost   = best_cost,
    auc         = auc,
    sensitivity = best_tpr,
    specificity = 1 - best_fpr
  )
}

##########################################################################################
# CONFUSION MATRIX
##########################################################################################
#' Confusion matrix visualisation
#'
#' Classifies every observation in \code{data} as a true positive (TP), false
#' positive (FP), true negative (TN), or false negative (FN) based on a chosen
#' decision threshold, then produces a jittered violin plot that shows the
#' distribution of predicted scores for each actual class, coloured by
#' classification type.
#'
#' @param data A \code{data.frame} or \code{data.table} containing at least the
#'   predicted score column and the actual outcome column.
#' @param predict Character. Name of the column holding the predicted
#'   probability or score.
#' @param actual Character. Name of the column holding the binary actual outcome
#'   (\code{0} = negative, \code{1} = positive).
#' @param cutoff Numeric. Decision threshold.  Observations with a predicted
#'   score \eqn{\ge} \code{cutoff} are classified as positive.  Typically
#'   obtained from \code{\link{plot_roc_info}}.
#'
#' @return A named list with two elements:
#' \describe{
#'   \item{data}{A \code{data.table} with columns \code{actual}, \code{predict},
#'     and \code{type} (factor with levels TP, FP, TN, FN).}
#'   \item{plot}{A \code{ggplot} object showing the jittered violin plot.}
#' }
#'
#' @details
#' The \code{actual} column is relevelled so that \code{1} (positive class)
#' appears first — matching the conventional top-left position of a two-by-two
#' confusion matrix.
#'
#' The horizontal dashed line marks the decision threshold.  Points above the
#' line are predicted positive; points below are predicted negative.  Colour
#' encodes whether each prediction was correct (TP/TN) or incorrect (FP/FN).
#'
#' @importFrom data.table data.table
#' @importFrom ggplot2 ggplot aes geom_violin geom_jitter geom_hline
#'   scale_y_continuous scale_color_discrete guides guide_legend ggtitle
#' @importFrom dplyr mutate
#'
#' @export
#'
#' @examples
#' # Continued from plot_roc_info example
#' data(df_admission)
#' model <- glm(admit ~ gre + gpa + rank,
#'              data   = df_admission,
#'              family = binomial)
#' df_admission$score <- predict(model, type = "response")
#'
#' # Use the cost-optimal cutoff from plot_roc_info
#' roc_result <- plot_roc_info(
#'   data    = df_admission,
#'   predict = "score",
#'   actual  = "admit",
#'   cost.fp = 1,
#'   cost.fn = 1
#' )
#'
#' cm <- compute_cm_info(
#'   data    = df_admission,
#'   predict = "score",
#'   actual  = "admit",
#'   cutoff  = roc_result$cutoff
#' )
#'
#' print(cm$plot)
#'
#' # Tabulate the four categories
#' table(cm$data$type)
#'
#' # Try a different cutoff and compare
#' cm_strict <- compute_cm_info(
#'   data    = df_admission,
#'   predict = "score",
#'   actual  = "admit",
#'   cutoff  = 0.7
#' )
#' print(cm_strict$plot)
compute_cm_info <- function(data, predict, actual, cutoff) {
  predict_col <- data[[predict]]
  actual_col  <- relevel(as.factor(data[[actual]]), "1")

  result <- data.table::data.table(actual = actual_col, predict = predict_col)
  result[, type := as.factor(
    ifelse(predict >= cutoff & actual == 1, "TP",
    ifelse(predict >= cutoff & actual == 0, "FP",
    ifelse(predict <  cutoff & actual == 1, "FN", "TN")))
  )]

  plot <- ggplot2::ggplot(result, ggplot2::aes(actual, predict, color = type)) +
    ggplot2::geom_violin(fill = "white", color = NA) +
    ggplot2::geom_jitter(shape = 1) +
    ggplot2::geom_hline(yintercept = cutoff, color = "blue", alpha = 0.6) +
    ggplot2::scale_y_continuous(limits = c(0, 1)) +
    ggplot2::scale_color_discrete(breaks = c("TP", "FN", "FP", "TN")) +
    ggplot2::guides(col = ggplot2::guide_legend(nrow = 2)) +
    ggplot2::ggtitle(sprintf("Confusion Matrix with Cutoff at %.2f", cutoff))

  list(data = result, plot = plot)
}
