#' Utility for doing univariate analysis
#'
#' \code{univariate_analysis} returns a list of summaries and plots for univariate analysis.
#'
#' Used to help ease the process of doing univariate analysis.
#' For help with plotting of the results @seealso \code{\link{plot_univariates}}
#' @param input_data Any object that can be converted into a data.table, will be what the univariate analysis is conducted on
#' @param continuous_cols A character vector containing the names of all columns that are considered continuous,
#'     if not provided this will be calculated
#' @param continuous_threshold Integer, representing the number of unique values a column must have in order to be
#'     considered continuous.  This will default to 5 and will only be used if continuous_cols was not provided.
#' @return A list of summaries and plots.  The summaries are split into continuous_summaries for the summary statistics of
#'     continuous columns and categorical_summaries the categorical columns. The plots are divided into:
#'     \itemize{
#'       \item categorical_bar_charts - bar charts for categorical columns
#'       \item continuous_box_plots - box plots for continuous variables
#'       \item continuous_histograms - histograms for continuous variables
#'     }
#' @import data.table
#'
#' @export
#'
#' @examples
#' results = univariate_analysis(mtcars)
#' results = univariate_analysis(mtcars, c('cyl', 'am', 'gear', 'carb'))
#' results = univariate_analysis(mtcars, continuous_threshold=3)
univariate_analysis <-function(input_data, continuous_cols, continuous_threshold = 5){
  #Make sure we are dealing with a data.table
  input_data = data.table::data.table(input_data)
  #If continuous_cols is not specified we are going to decide based on unique values and types
  if (missing(continuous_cols)){
    unique_values = sapply(input_data, function(x)length(unique(x)))
    numeric = sapply(input_data, is.numeric)
    gt_threshold = unique_values > continuous_threshold
    continuous_cols = names(input_data[,gt_threshold & numeric, with=FALSE])
  }
  #Categorical columns are simply those that aren't continuous then
  not_continuous = !(names(input_data) %in% continuous_cols)
  cat_cols = names(input_data[,not_continuous,with=FALSE])
  summaries = univariate_summaries(input_data, continuous_cols, cat_cols)
  plots = univariate_plots(input_data, continuous_cols, cat_cols)
  return(list(summaries=summaries, plots=plots))
}

univariate_summaries <- function(input_data, continuous_cols, categorical_cols){
  continuous_summaries = lapply(continuous_cols, function(x){
    x = as.name(x)
    input_data[,list(min=min(eval(x)),
                     lower=quantile(eval(x), 0.25),
                     median = median(eval(x)),
                     upper=quantile(eval(x), 0.75),
                     max=max(eval(x)),
                     mean=mean(eval(x)),
                     std_dev=sd(eval(x)),
                     skew=ifelse(requireNamespace("moments", quietly = TRUE), moments::skewness(eval(x)), NA),
                     kurtosis=ifelse(requireNamespace("moments", quietly = TRUE), moments::kurtosis(eval(x)), NA)),]
  })

  categorical_summaries = lapply(categorical_cols, function(x){input_data[,.N, x]})
  return(list(categorical_summaries=categorical_summaries, continuous_summaries=continuous_summaries))
}

univariate_plots <- function(input_data, continuous_cols, categorical_cols){
  #Produce are categorical plots
  cat_bars = lapply(categorical_cols, function(x){
    ggplot2::ggplot(input_data) +
      #aes_q and as.name are to get ggplot2 to work well with a string variable
      ggplot2::geom_bar(ggplot2::aes_q(as.name(x)), fill='skyblue')+
      ggplot2::theme(panel.background = ggplot2::element_rect(fill='white'))
  })

  continuous_boxes = lapply(continuous_cols, function(x){
    ggplot2::ggplot(input_data) +
      #geom_boxplot needs an x and y in its aesthetic, factor(0) just gives it a dummy factor
      ggplot2::geom_boxplot(ggplot2::aes_q(x=factor(0), as.name(x)), fill='skyblue', varwidth = TRUE)+
      #I like boxplots to be going side to side not up and down, you decide
      ggplot2::coord_flip()+
      ggplot2::theme(panel.background = ggplot2::element_rect(fill='white'),
                     axis.title.y = ggplot2::element_blank())
  })

  continuous_hists = lapply(continuous_cols, function(x){
    ggplot2::ggplot(input_data) +
      ggplot2::geom_histogram(ggplot2::aes_q(as.name(x)), fill='skyblue')+
      ggplot2::theme(panel.background = ggplot2::element_rect(fill='white'))
  })

  return(list(categorical_bar_charts = cat_bars,
              continuous_box_plots = continuous_boxes,
              continuous_histograms = continuous_hists))
}

#' Helper function for rendering univariate plots
#'
#' \code{plot_univariates} Renders univariate plots into 3 plots, one per plot type
#'
#' This function is intended to be used with @seealso \code{\link{univariate_analysis}}
#' @param plot_list The plots list that univariate_analysis generated
#'
#' @export
#'
#' @examples
#' \dontrun{results = univariate_analysis(mtcars)}
#' \dontrun{plot_univariates(results$plots)}
plot_univariates <- function(plot_list){
  for (plots in results$plots) {
    n <- length(plots)
    n_col <- floor(sqrt(n))
    #getting a handle of the gridExtra::grid.arrange function so that we can use it to generate plots
    grid_arange = get('grid.arrange', asNamespace('gridExtra'))
    do.call(grid_arange, c(plots, ncol=n_col))
  }
}
