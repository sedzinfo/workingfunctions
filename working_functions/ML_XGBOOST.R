#########################################################################################
# TREE PLOT
#########################################################################################
#' @title Plot trees for xgboost::xgb.train
#' @param model object from xgboost::xgb.train
#' @param train Train dataset
#' @param file output filename
#' @importFrom xgboost xgb.plot.multi.trees
#' @keywords ML
#' @export
#' @examples
#' infert_formula<-formula(case~education+spontaneous+induced)
#' boston_formula<-formula(medv~crim+zn+indus+chas+nox+rm+age+dis+rad+tax+ptratio+black+lstat)
#' train_test_classification<-k_fold(df=infert,model_formula=infert_formula)
#' train_test_regression<-k_fold(df=MASS::Boston,model_formula=boston_formula)
#' xgb_classification<-xgboost::xgb.train(
#'                     data=train_test_classification$xgb$f1$train,
#'                     watchlist=train_test_classification$xgb$f1$watchlist,
#'                     eta=.1,
#'                     nthread=8,
#'                     nround=20,
#'                     objective="binary:logistic")
#' xgb_regression<-xgboost::xgb.train(
#'                 data=train_test_regression$xgb$f1$train,
#'                 watchlist=train_test_regression$xgb$f1$watchlist,
#'                 eta=.3,
#'                 nthread=8,
#'                 nround=20)
#' # xgboost::xgb.plot.multi.trees(model=xgb_classification,features_keep=2)
#' # plot_trees_xgboost(model=xgb_classification,
#' #                    train=train_test_classification$xgb$f1,
#' #                    file="Classification")
#' # plot_trees_xgboost(model=xgb_regression,
#' #                    train=train_test_regression$xbg$f1,
#' #                    file="Regression")
plot_trees_xgboost<-function(model,train,file="xgboost") {
  xgboost_trees<-xgboost::xgb.plot.multi.trees(model=model,feature_names=colnames(train),features_keep=10,fill=TRUE,use.names=FALSE)
  htmlwidgets::saveWidget(xgboost_trees,invisible(paste0(toString(getwd()),"/",file,".html")),selfcontained=TRUE)
}
##########################################################################################
# XGBOOST
##########################################################################################
#' @title Report for xgboost::xgb.train
#' @param model object from xgboost::xgb.train
#' @param validation_data validation data
#' @param label outcome variable name
#' @param file output filename
#' @param w width of pdf file
#' @param h height of pdf file
#' @param base_size base font size
#' @param title plot title
#' @param fast if TRUE error values are not saved in output
#' @import ggplot2
#' @importFrom openxlsx createWorkbook saveWorkbook
#' @importFrom stringr str_replace_all fixed
#' @importFrom xgboost xgb.DMatrix xgb.plot.deepness xgb.importance xgb.ggplot.importance
#' @importFrom reshape2 melt
#' @keywords ML
#' @export
#' @examples
#' infert_formula<-formula(case~education+spontaneous+induced)
#' boston_formula<-formula(medv~crim+zn+indus+chas+nox+rm+age+dis+rad+tax+ptratio+black+lstat)
#' train_test_classification<-k_fold(df=infert,model_formula=infert_formula)
#' train_test_regression<-k_fold(df=MASS::Boston,model_formula=boston_formula)
#' xgb_classification<-xgboost::xgb.train(
#'                     params=xgboost::xgb.params(objective="binary:logistic"),
#'                     data=train_test_classification$xgb$f1$train,
#'                     evals=train_test_classification$xgb$f1$watchlist,
#'                     nround=20)
#' xgb_regression<-xgboost::xgb.train(
#'                 data=train_test_regression$xgb$f1$train,
#'                 evals=train_test_regression$xgb$f1$watchlist,
#'                 nround=20)
#' \dontrun{
#' report_xgboost(model=xgb_classification,
#'                validation_data=train_test_classification$f$test$f1,
#'                label=train_test_classification$outcome,
#'                file="Classification")
#' report_xgboost(model=xgb_regression,
#'                validation_data=train_test_regression$f$test$f1,
#'                label=train_test_regression$outcome,
#'                file="Regression")
#' }
report_xgboost <- function(model,
                           validation_data = NULL,
                           label = NULL,
                           file = "xgboost",
                           w = 10,
                           h = 10,
                           base_size = 10,
                           title = "",
                           fast = FALSE) {
  Depth <- Tree <- Cover <- Weight <- value <- Iteration <- Metric <- Factor <- NULL
  
  if (!inherits(model, "xgb.Booster")) {
    stop("model must be an xgboost booster object from xgboost::xgb.train")
  }
  
  plots <- list()
  result <- list()
  observed <- predicted <- NULL
  
  objective <- tryCatch(model$params$objective, error = function(e) NULL)
  is_regression <- !is.null(objective) && grepl("^reg:|^count:|^survival:", objective)
  
  # helper: robust feature-name resolution
  resolve_feature_names <- function(model, validation_data = NULL, label = NULL) {
    fn <- tryCatch(model$feature_names, error = function(e) NULL)
    if (!is.null(fn) && length(fn) > 0) return(fn)
    
    # fallback from tree dump
    fn_tree <- tryCatch({
      dt <- xgboost::xgb.model.dt.tree(model = model)
      unique(dt$Feature[dt$Feature != "Leaf"])
    }, error = function(e) NULL)
    if (!is.null(fn_tree) && length(fn_tree) > 0) return(fn_tree)
    
    # fallback from validation data
    if (!is.null(validation_data)) {
      if (!is.null(label) && label %in% names(validation_data)) {
        return(setdiff(names(validation_data), label))
      }
      return(names(validation_data))
    }
    
    character(0)
  }
  
  feature_names <- resolve_feature_names(model, validation_data, label)
  
  if (!is.null(validation_data)) {
    if (is.null(label) || !(label %in% names(validation_data))) {
      stop("label must be provided and exist in validation_data")
    }
    if (length(feature_names) == 0) {
      stop("Could not infer predictor columns. Provide validation_data with predictor columns and label.")
    }
    
    missing_features <- setdiff(feature_names, names(validation_data))
    if (length(missing_features) > 0) {
      stop("validation_data is missing required features: ",
           paste(missing_features, collapse = ", "))
    }
    
    observed <- validation_data[, label]
    vx <- data.matrix(validation_data[, feature_names, drop = FALSE])
    predicted <- predict(model, newdata = xgboost::xgb.DMatrix(data = vx))
    
    plots$regression <- plot_scatterplot(data.frame(observed = observed, predicted = predicted))
    
    if (!is_regression) {
      perf_obj <- tryCatch(
        result_confusion_performance(observed = observed, predicted = predicted),
        error = function(e) NULL
      )
      if (!is.null(perf_obj)) plots$performance <- perf_obj
    }
  }
  
  params_vec <- tryCatch(unlist(model$params), error = function(e) NULL)
  if (!is.null(params_vec) && length(params_vec) > 0) {
    result$parameters <- data.frame(
      Hyperparameter = names(params_vec),
      value = as.character(params_vec),
      stringsAsFactors = FALSE
    )
  } else {
    result$parameters <- data.frame(Hyperparameter = character(0), value = character(0))
  }
  
  result$model_call <- tryCatch(
    data.frame(
      Parameters = "Call",
      value = gsub(" ", "", toString(deparse(model$call))),
      stringsAsFactors = FALSE
    ),
    error = function(e) data.frame(Parameters = "Call", value = NA_character_)
  )
  
  evaluation_log <- tryCatch(data.frame(model$evaluation_log), error = function(e) data.frame())
  result$evaluation_log <- evaluation_log
  
  xgboost_model_depth <- tryCatch(
    data.frame(xgboost::xgb.plot.deepness(model, which = "max.depth", plot = FALSE)),
    error = function(e) NULL
  )
  
  if (!is.null(xgboost_model_depth) && nrow(xgboost_model_depth) > 0) {
    plots$depth <- ggplot2::ggplot(xgboost_model_depth, ggplot2::aes(y = Depth, x = Tree)) +
      ggplot2::geom_point(alpha = 0.1) +
      ggplot2::labs(x = "Tree", y = "Depth", title = "") +
      ggplot2::theme_bw(base_size = base_size)
    
    plots$cover <- ggplot2::ggplot(xgboost_model_depth, ggplot2::aes(y = Cover, x = Tree)) +
      ggplot2::geom_point(alpha = 0.1) +
      ggplot2::labs(x = "Tree", y = "Cover", title = "") +
      ggplot2::theme_bw(base_size = base_size)
    
    plots$weight <- ggplot2::ggplot(xgboost_model_depth, ggplot2::aes(y = Weight, x = Tree)) +
      ggplot2::geom_point(alpha = 0.1) +
      ggplot2::labs(x = "Tree", y = "Weight", title = "") +
      ggplot2::theme_bw(base_size = base_size)
  }
  
  if (nrow(evaluation_log) > 0 && "iter" %in% names(evaluation_log)) {
    error_df <- reshape2::melt(evaluation_log, id.vars = "iter", variable.name = "Metric")
    names(error_df) <- c("Iteration", "Metric", "value")
    error_df$Metric <- str_aes(error_df$Metric)
    
    plots$error <- ggplot2::ggplot(error_df, ggplot2::aes(y = value, x = Iteration, color = Metric)) +
      ggplot2::geom_line(linewidth = base_size / 15) +
      ggplot2::labs(x = "Iteration", y = "Metric Value", title = paste0("Training Log: ", title)) +
      ggplot2::theme_bw(base_size = base_size)
  }
  
  importance_data <- tryCatch(
    as.data.frame(xgboost::xgb.importance(model = model, feature_names = feature_names)),
    error = function(e) data.frame()
  )
  
  if (nrow(importance_data) > 0) {
    if (!("Feature" %in% names(importance_data))) {
      names(importance_data)[1] <- "Feature"
    }
    
    metric_cols <- intersect(
      c("Importance", "Gain", "Cover", "Frequency", "Weight", "TotalGain", "TotalCover"),
      names(importance_data)
    )
    
    if (length(metric_cols) > 0) {
      ord_col <- metric_cols[1]
      importance_data$Feature <- factor(
        importance_data$Feature,
        levels = importance_data[order(importance_data[[ord_col]]), "Feature"]
      )
      
      importance_long <- reshape2::melt(
        importance_data[, c("Feature", metric_cols), drop = FALSE],
        id.vars = "Feature"
      )
      names(importance_long)[1:2] <- c("Factor", "Metric")
      
      plots$importance <- ggplot2::ggplot(importance_long, ggplot2::aes(y = value, x = Factor, fill = Metric)) +
        ggplot2::geom_bar(stat = "identity", position = ggplot2::position_dodge(), colour = "white") +
        ggplot2::labs(x = "Predictor", y = "Relative Importance", title = paste("Importance", title)) +
        ggplot2::theme_bw(base_size = base_size) +
        ggplot2::coord_flip()
    }
  }
  
  if (!is.null(file) && length(plots) > 0) {
    report_pdf(plotlist = plots, file = file, title = title, w = w, h = h, print_plot = TRUE)
  }
  
  if (!is.null(file)) {
    filename <- paste0(file, ".xlsx")
    if (file.exists(filename)) file.remove(filename)
    wb <- openxlsx::createWorkbook()
    
    if (!is.null(plots$performance) && !is.null(plots$performance$confusion_matrix)) {
      excel_confusion_matrix(plots$performance$confusion_matrix, wb)
    }
    
    if (nrow(importance_data) > 0) {
      excel_critical_value(importance_data, wb, "Feature Importance", numFmt = "#0.00")
    }
    
    if (nrow(result$parameters) > 0) {
      excel_critical_value(result$parameters, wb, "Hyperparameters", numFmt = "#0.00")
    }
    
    if (!fast && nrow(result$evaluation_log) > 0) {
      excel_critical_value(result$evaluation_log, wb, "Evaluation Log", numFmt = "#0.00")
    }
    
    openxlsx::saveWorkbook(wb = wb, file = filename, overwrite = TRUE)
  }
  
  invisible(list(
    plots = plots,
    result = result,
    observed = observed,
    predicted = predicted,
    feature_names = feature_names
  ))
}
