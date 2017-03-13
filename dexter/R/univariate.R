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
                     skew=moments::skewness(eval(x)), 
                     kurtosis=moments::kurtosis(eval(x))),]
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