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