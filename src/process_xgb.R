library(xgboost)

prepare_xgb = function(df, to_drop = NA, target = NA){
  df = df %>% select(setdiff(colnames(.), to_drop))
  
  if("character" %in% (df %>% sapply(FUN = class))){
    warning("A character column was transformed into an integer one. 
            Same string in different datasets might have different encoding!
            Can not be used in production!")
  }

  xgDf = df %>%
    select(-one_of(target)) %>% 
    mutate_if(is.character, as.factor) %>% 
    mutate_if(is.factor, as.integer) %>%
    as.matrix() %>% 
    xgb.DMatrix(label=df[[target]])
  
  return(xgDf)
}
