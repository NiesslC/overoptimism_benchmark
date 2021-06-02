# helper_fcts_generate_rankdata.R ------------------------------------------------------------------

# helper function to generate method ranking with specified options:
# - benchmark_ranking_fct
# helper functions for imputation methods:
# - impute.missings.by.threshold, impute.missings.data.independent, 
#   impute.missings.mean.value, impute.missings.weighted
#---------------------------------------------------------------------------------------------------
benchmark_ranking_fct <- function(cvdata, 
                                  zeros_as_missings,
                                  imputation_method = c("threshold", "independent", "mean", "weighted"), 
                                  imputation_threshold = NULL,
                                  dataset_grouping = c("none", "groupby_p", "groupby_N", "groupby_clin", "groupby_neff", "groupby_reff"),
                                  dataset_grouping_smaller = NULL,
                                  eval_measure = c("cindex.uno", "ibrier"), 
                                  eval_aggregation = c("mean", "median", "best0.05", "rank"),
                                  aggreg_over_datsets = TRUE){
  cvdata_result = cvdata
  
  
  ### 1. imputation method and zeros as missings 
  if(zeros_as_missings == TRUE){
    cvdata_result$cindex.uno <- ifelse(cvdata_result$cindex.uno == 0, NA, cvdata_result$cindex.uno)
  }
  if(imputation_method == "threshold"){
    cvdata_result = impute.missings.by.threshold(cvdata_result, threshold = imputation_threshold)
  }else if(imputation_method == "independent"){
    cvdata_result = impute.missings.data.independent(cvdata_result)
  }else if(imputation_method == "mean"){
    cvdata_result = impute.missings.mean.value(cvdata_result)
  }else if(imputation_method == "weighted"){
    cvdata_result = impute.missings.weighted(cvdata_result)
  }
  
  ### 2. Selection of datasets
  if(dataset_grouping != "none"){
    grouping =  unlist(strsplit(dataset_grouping,"groupby_"))[2]
    
    median_cat  = median(unlist(cvdata_result %>% select(task.id, !!as.name(grouping)) %>% distinct() %>% select(!!as.name(grouping))))
    cvdata_result[,dataset_grouping] <- ifelse(cvdata_result[,grouping] < median_cat, "less than median", "greater than median or equal")
    if(dataset_grouping_smaller == TRUE){
      cvdata_result = cvdata_result[cvdata_result[,dataset_grouping] == "less than median",]
    }else if(dataset_grouping_smaller == FALSE){
      cvdata_result = cvdata_result[cvdata_result[,dataset_grouping] == "greater than median or equal",]
    }
  }
  ### 3. Ibrier vs. Cindex + Mean/Median/best0.05/rank
  if(aggreg_over_datsets == TRUE){
    cvdata_result = cvdata_result %>% group_by(task.id, learner.id) %>% 
      mutate(cindex.uno = mean(cindex.uno),ibrier = mean(ibrier)) %>%
      distinct(cindex.uno,ibrier, .keep_all=TRUE) %>% ungroup() %>% 
      select (-c(iter, id))
    
    if(eval_aggregation == "mean"){
      cvdata_result = cvdata_result %>% group_by(learner.id, abbreviation, method, group.structure) %>%
        summarise(!!paste0("aggreg_",eval_aggregation) :=  mean(!!as.name(eval_measure))) %>% ungroup()
    } else if(eval_aggregation == "median"){
      cvdata_result = cvdata_result %>% group_by(learner.id, abbreviation, method, group.structure) %>%
        summarise(!!paste0("aggreg_",eval_aggregation) :=  median(!!as.name(eval_measure))) %>% ungroup()
    } else if(eval_aggregation == "best0.05"){
      if(eval_measure == "cindex.uno"){
        cvdata_result = cvdata_result %>% group_by(task.id) %>% mutate(bestvalue_cindex = max(cindex.uno), 
                                                                       isbest_cindex = cindex.uno == max(cindex.uno),
                                                                       env005_cindex = abs(max(cindex.uno)-cindex.uno)/max(cindex.uno) <= 0.05) %>% ungroup()
        cvdata_result = cvdata_result %>% group_by(learner.id, abbreviation, method, group.structure) %>%
          summarise(sum_isbest = sum(isbest_cindex),
                    sum_env005 = sum(env005_cindex)) %>% ungroup()
        
      } else if(eval_measure == "ibrier"){
        cvdata_result = cvdata_result %>% group_by(task.id) %>% mutate(bestvalue_ibrier = min(ibrier),
                                                                       isbest_ibrier = ibrier == min(ibrier),
                                                                       env005_ibrier = abs(min(ibrier)-ibrier)/min(ibrier) <= 0.05) %>% ungroup()
        cvdata_result = cvdata_result %>% group_by(learner.id, abbreviation, method, group.structure) %>%
          summarise(sum_isbest = sum(isbest_ibrier),
                    sum_env005 = sum(env005_ibrier)) %>% ungroup()
        
      }
    } else if(eval_aggregation == "rank"){
      if(eval_measure == "cindex.uno"){
        cvdata_result = cvdata_result %>% group_by(task.id) %>% mutate(rank = rank(-cindex.uno)) %>% ungroup()
        cvdata_result = cvdata_result %>% group_by(learner.id, abbreviation, method, group.structure) %>% 
          summarise(mean_rank = mean(rank)) %>% ungroup()
        
      } else if(eval_measure == "ibrier"){
        cvdata_result = cvdata_result %>% group_by(task.id) %>% mutate(rank = rank(ibrier)) %>% ungroup()
        cvdata_result = cvdata_result %>% group_by(learner.id, abbreviation, method, group.structure) %>% 
          summarise(mean_rank = mean(rank)) %>% ungroup()
        
      }
      
    }
    # final ranking
    if(eval_measure == "cindex.uno" & eval_aggregation %in% c("mean","median")){
      cvdata_result$ranking = rank(-cvdata_result[,paste0("aggreg_",eval_aggregation)], ties.method = "average")
    } else if(eval_measure == "ibrier" & eval_aggregation %in% c("mean","median")){
      cvdata_result$ranking = rank(cvdata_result[,paste0("aggreg_",eval_aggregation)], ties.method = "average")
    } else if(eval_aggregation == "best0.05"){
      cvdata_result$ranking_helpvar = order(order(-cvdata_result$sum_isbest, -cvdata_result$sum_env005))
      cvdata_result = cvdata_result %>% group_by(sum_isbest, sum_env005) %>% 
        mutate(ranking = mean(ranking_helpvar)) %>% select(-ranking_helpvar) # ties.method = average
    } else if(eval_aggregation == "rank"){
      cvdata_result$ranking = rank(cvdata_result$mean_rank, ties.method = "average")
    } 
  }
  
  # add parameter combination
  cvdata_result$combinations <- paste(ifelse(zeros_as_missings, "0=NA", "0!=NA"), 
                                  paste0(imputation_method, imputation_threshold),
                                  dataset_grouping, paste0("<",dataset_grouping_smaller),
                                  eval_measure, eval_aggregation, sep = "/")
  
  return(cvdata_result)
}


#---------------------------------------------------------------------------------------------------
# Impute missings by threshold:
# If a learner fails in more than the set threshold of the CV Iterations, the values of cindex and 
# ibrier for the failing iterations are replaced by the data independent values 
# (0.25 for ibrier and 0.5 for cindex). If a learner fails in less than 
# than the set threshold, the NA values of cindex and ibrier are replaced by the performance means 
# of the successful iterations of this learner. 
impute.missings.by.threshold <- function(df.res, threshold) {
  cindex <- 0.5
  ibrier <- 0.25
  df.res.by.threshold <- df.res
  for (task in unique(df.res$task.id)) {
    for (lrn in unique(df.res$learner.id)) {
      df.temp <- df.res[df.res$learner.id == lrn & df.res$task.id == task,]
      if (any(is.na(df.temp))) {
        n.fails <- sum(apply(df.temp,1, FUN= function(x) {any(is.na(x))}))
        r.fails <- n.fails/nrow(df.temp)
        if(r.fails> threshold) {
          df.temp[is.na(df.temp[, "cindex.uno"]), "cindex.uno"] <- cindex  
          df.temp[is.na(df.temp[, "ibrier"]), "ibrier"] <- ibrier  
        } else {
          mean.cindex <- mean(df.temp$cindex.uno, na.rm = TRUE)
          mean.ibrier <- mean(df.temp$ibrier, na.rm = TRUE)
          df.temp[is.na(df.temp[, "cindex.uno"]), "cindex.uno"] <- mean.cindex
          df.temp[is.na(df.temp[, "ibrier"]), "ibrier"] <- mean.ibrier 
        }
        df.res.by.threshold[df.res$learner.id == lrn & df.res$task.id == task,] <- df.temp
      }  
    }
  }
  df.res.by.threshold
}
#---------------------------------------------------------------------------------------------------
# Impute missings by the data independent value 
# Every CV Iteration failure will be imputed by the data independent values of the prediction 
# performance measures (0.25 for ibrier and 0.5 for cindex)
impute.missings.data.independent <- function(df.res) {
  cindex <- 0.5
  ibrier <- 0.25
  df.res.data.independent <- df.res
  
  for (lrn in unique(df.res$learner.id)) {
    df.temp <- df.res[df.res$learner.id == lrn,]
    if (any(is.na(df.temp))) {
      df.temp[is.na(df.temp[, "cindex.uno"]), "cindex.uno"] <- cindex
      df.temp[is.na(df.temp[, "ibrier"]), "ibrier"] <- ibrier
    }
    df.res.data.independent[df.res$learner.id == lrn,] <- df.temp
  }
  df.res.data.independent
}
#---------------------------------------------------------------------------------------------------
# Impute missings by the mean value 
# Every CV Iteration failure will be imputed by the performance means of the successful iterations of this learner
impute.missings.mean.value <- function(df.res) {
  df.res.mean.value <- df.res
  
for (task in unique(df.res$task.id)) {
  for (lrn in unique(df.res$learner.id)) {
    df.temp <- df.res[df.res$learner.id == lrn & df.res$task.id == task,]
    if (any(is.na(df.temp))) {
      mean.cindex <- ifelse(is.na(mean(df.temp$cindex.uno, na.rm = TRUE)),0.5,mean(df.temp$cindex.uno, na.rm = TRUE))
      mean.ibrier <- ifelse(is.na(mean(df.temp$ibrier, na.rm = TRUE)), 0.25, mean(df.temp$ibrier, na.rm = TRUE))
      df.temp[is.na(df.temp[, "cindex.uno"]), "cindex.uno"] <- mean.cindex
      df.temp[is.na(df.temp[, "ibrier"]), "ibrier"] <- mean.ibrier 
    }
    df.res.mean.value[df.res$learner.id == lrn & df.res$task.id == task,] <- df.temp
  }
}
  df.res.mean.value
}
#---------------------------------------------------------------------------------------------------
# Impute missings by the weighted method
# Every CV Iteration failure will be imputed by a weighted calculation of the performance means of 
# the successful iterations of this learner and the proportion of failures
impute.missings.weighted <- function(df.res) {
  df.res.weighted <- df.res
for (task in unique(df.res$task.id)) {
  for (lrn in unique(df.res$learner.id)) {
    df.temp <- df.res[df.res$learner.id == lrn & df.res$task.id == task,]
    if (any(is.na(df.temp))) {
      n.fails <- sum(apply(df.temp,1, FUN= function(x) {any(is.na(x))}))
      r.fails <- n.fails/nrow(df.temp)
      mean.cindex <- ifelse(is.na(mean(df.temp$cindex.uno, na.rm = TRUE)), 0.5, mean(df.temp$cindex.uno, na.rm = TRUE))
      mean.ibrier <- ifelse(is.na(mean(df.temp$ibrier, na.rm = TRUE)),0.25,mean(df.temp$ibrier, na.rm = TRUE))
      I_cindex <- ifelse(mean.cindex > 0.5,1,0)
      I_ibrier <- ifelse(mean.ibrier < 0.25,1,0)
      df.temp[is.na(df.temp[, "cindex.uno"]), "cindex.uno"] <- 0.5 + (mean.cindex - 0.5) * (1- r.fails) * I_cindex
      df.temp[is.na(df.temp[, "ibrier"]), "ibrier"] <- 0.25 - (0.25 - mean.ibrier) * (1- r.fails) * I_ibrier
      
    }
    df.res.weighted[df.res$learner.id == lrn & df.res$task.id == task,] <- df.temp
  }
}
  df.res.weighted
}
