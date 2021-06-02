# 01_generate_data.R -------------------------------------------------------------------------------

# load and prepare data ----------------------------------------------------------------------------

# load packages and functions
library(reshape2)
library(dplyr)

source("./R/helper_fcts_generate_rankdata.R")


# read data from original benchmark study by Herrmann et al. (2020)
load("./Data/merged-results_na.RData")
datasets_overview <- read.csv("./Data/datasets_overview.csv", sep = ";", header = TRUE, check.names = TRUE)
colnames(datasets_overview)[grep("cancer", colnames(datasets_overview))] <- "task.id"

# the analysis will focus on cindex and ibrier
df_res <- df_res[, c("task.id", "iter", "learner.id", "cindex.uno", "ibrier")]

df_res$abbreviation <- ifelse(df_res$learner.id == "Kaplan-Meier", "KM",
                        ifelse(df_res$learner.id == "CoxBoost", "CoxB",
                        ifelse(df_res$learner.id == "CoxBoost favoring", "CoxB_f",
                        ifelse(df_res$learner.id == "blockForest", "BF",
                        ifelse(df_res$learner.id == "Clinical only", "CoxPH",
                        ifelse(df_res$learner.id == "ipflasso", "IPF",
                        ifelse(df_res$learner.id == "glmboost", "glmB",
                        ifelse(df_res$learner.id == "prioritylasso", "prior",
                        ifelse(df_res$learner.id == "prioritylasso favoring", "prior_f",
                        ifelse(df_res$learner.id == "ranger", "ranger",
                        ifelse(df_res$learner.id == "Lasso", "Lasso",
                        ifelse(df_res$learner.id == "grridge", "GRr", "rfsrc" ))))))))))))


df_res$id <- paste(df_res$task.id, df_res$iter, sep = "-")

# add method and group structure
df_res$method <- ifelse(df_res$learner.id %in% c("Lasso", "ipflasso", "prioritylasso", "prioritylasso favoring", "grridge"), "Penalised Regression",
                        ifelse(df_res$learner.id %in% c("CoxBoost", "CoxBoost favoring", "glmboost"), "Boosting",
                        ifelse(df_res$learner.id %in% c("rfsrc", "ranger", "blockForest"), "Random Forest", 
                        ifelse(df_res$learner.id == "Clinical only", "Clinical only", "Kaplan-Meier"))))


df_res$group.structure<- ifelse(df_res$learner.id %in% c("ipflasso", "prioritylasso", "blockForest", "grridge"), "gs without fav",
                          ifelse(df_res$learner.id %in% c("prioritylasso favoring", "CoxBoost favoring"), "gs fav clin", 
                          ifelse(df_res$learner.id %in% c("Lasso", "CoxBoost", "glmboost", "rfsrc", "ranger"), "no gs", 
                          ifelse(df_res$learner.id == "Clinical only", "Clinical only", "Kaplan-Meier"))))


df_res <- data.frame(df_res)

# add overview datasets
df_res <- merge(df_res, datasets_overview[, c("task.id","clin", "p","N","neff", "reff")], by = "task.id", all = TRUE)

# design and analysis options ----------------------------------------------------------------------
params = list(
zeros_as_missings = TRUE, # could also be FALSE
imputation_method = c("threshold", "independent", "mean", "weighted"),
imputation_threshold = 0.2, # only for threshold
dataset_grouping = c("none", "groupby_p", "groupby_N", "groupby_clin", "groupby_neff"), # "groupby_reff"
dataset_grouping_smaller = c(TRUE, FALSE), # only if grouping != none
eval_measure = c("cindex.uno", "ibrier"),
eval_aggregation = c("mean", "median", "best0.05", "rank"))

combinations = expand.grid(params, stringsAsFactors = FALSE)
# only need imputaton_threshold if imputation_method = imputation
combinations[combinations$imputation_method != "threshold","imputation_threshold"] <- NA
# only need dataset_grouping smaller if grouping != none
combinations[combinations$dataset_grouping == "none","dataset_grouping_smaller"] <- NA

combinations = combinations %>% distinct()

# generate rankdata with all possible combinations -------------------------------------------------
rankdata  = lapply(1:nrow(combinations), FUN = function(i){
  benchmark_ranking_fct(cvdata = df_res, zeros_as_missings = combinations$zeros_as_missings[i],
                        imputation_method = combinations$imputation_method[i],
                        imputation_threshold = combinations$imputation_threshold[i],
                        dataset_grouping = combinations$dataset_grouping[i], 
                        dataset_grouping_smaller = combinations$dataset_grouping_smaller[i],
                        eval_measure = combinations$eval_measure[i],
                        eval_aggregation = combinations$eval_aggregation[i])
  
})
rankdata = bind_rows(rankdata)

# add column for each parameter 
combinations_data = strsplit(rankdata$combinations, "/")
combinations_data = do.call(rbind,combinations_data)
combinations_data = as.data.frame(combinations_data)
colnames(combinations_data) = c("zeroasmissing", "imputation_method", "grouping", "smaller_group",
                     "eval_measure", "aggregation_method")
rankdata = cbind(rankdata, combinations_data)
rm(combinations_data)

# melt aggregation_methods
rankdata = melt(rankdata, measure.vars = c("aggreg_mean", "aggreg_median", "mean_rank", "sum_isbest"),
              value.name = "aggreg_value", variable.name = "helpervar") # sum_env005 as extra column
rankdata = rankdata %>% filter(!is.na(aggreg_value)) %>% select(-helpervar) %>% relocate(aggreg_value, .before = ranking)

# variable that indicates which group of data sets is used
rankdata$grouping_cat  = paste0(gsub("groupby_", "", rankdata$grouping), rankdata$smaller_group)


# method names and ordering 
rankdata$learner.id = factor(rankdata$learner.id, levels = c("blockForest", "Clinical only", "CoxBoost", "CoxBoost favoring",
                                                         "glmboost", "grridge", "ipflasso", "Kaplan-Meier", "Lasso", 
                                                         "prioritylasso", "prioritylasso favoring", "ranger", "rfsrc"),
                           labels = c("BlockForest", "Clinical Only", "CoxBoost", "CoxBoost Favoring",
                                      "Glmboost", "Grridge", "Ipflasso", "Kaplan-Meier", "Lasso", 
                                      "Prioritylasso", "Prioritylasso Favoring", "Ranger", "Rfsrc"))

save(rankdata, file = "./Data/rankdata.RData")


# generate rankdata with random groups of data sets ------------------------------------------------
combinations = combinations %>% filter(dataset_grouping == "none") %>% distinct()
set.seed(123)
rankdata_datasample_1_18_all = lapply(1:50,FUN = function(k){
  datasample = sample(datasets_overview$task.id, size = 18, replace = FALSE)
  rankdata_datasample_1_18 = lapply(1:18, FUN = function(j){
    rankdata_datasample = lapply(1:nrow(combinations), FUN = function(i){
      benchmark_ranking_fct(cvdata = df_res %>% filter(task.id %in% datasample[1:j]),
                            zeros_as_missings = combinations$zeros_as_missings[i],
                            imputation_method = combinations$imputation_method[i],
                            imputation_threshold = combinations$imputation_threshold[i],
                            dataset_grouping = combinations$dataset_grouping[i],
                            dataset_grouping_smaller = combinations$dataset_grouping_smaller[i],
                            eval_measure = combinations$eval_measure[i],
                            eval_aggregation = combinations$eval_aggregation[i])

    })

    rankdata_datasample = bind_rows(rankdata_datasample)

    # add column for each parameter
    combinations_data = strsplit(rankdata_datasample$combinations, "/")
    combinations_data = do.call(rbind,combinations_data)
    combinations_data = as.data.frame(combinations_data)
    colnames(combinations_data) = c("zeroasmissing", "imputation_method", "grouping", "smaller_group",
                              "eval_measure", "aggregation_method")
    rankdata_datasample = cbind(rankdata_datasample, combinations_data)

    rankdata_datasample$nrdatasets = as.character(j)
    rankdata_datasample$rep = as.character(k)
    return(rankdata_datasample)
  })
  rankdata_datasample_1_18 = bind_rows(rankdata_datasample_1_18)
  return(list("datasample" = datasample, "rankdata_datasample_1_18" = rankdata_datasample_1_18))
})



# two components: information about sampled datasets and rankdata
datasample = lapply(rankdata_datasample_1_18_all, '[[', 1)
rankdata_datasample_1_18 = lapply(rankdata_datasample_1_18_all, '[[', 2)
rankdata_datasample_1_18 = do.call("rbind", rankdata_datasample_1_18)

# variable that indicates which group of data sets is used
rankdata_datasample_1_18$grouping_cat  = paste0(gsub("groupby_", "", rankdata_datasample_1_18$grouping),
                                                rankdata_datasample_1_18$smaller_group)

# method names and ordering
rankdata_datasample_1_18$learner.id = factor(rankdata_datasample_1_18$learner.id, levels = c("blockForest", "Clinical only", "CoxBoost", "CoxBoost favoring",
                                                         "glmboost", "grridge", "ipflasso", "Kaplan-Meier", "Lasso", 
                                                         "prioritylasso", "prioritylasso favoring", "ranger", "rfsrc"),
                           labels = c("BlockForest", "Clinical Only", "CoxBoost", "CoxBoost Favoring",
                                      "Glmboost", "Grridge", "Ipflasso", "Kaplan-Meier", "Lasso", 
                                      "Prioritylasso", "Prioritylasso Favoring", "Ranger", "Rfsrc"))
rankdata_datasample_1_18$learner.id = as.character(rankdata_datasample_1_18$learner.id)

# store again as list 
rankdata_datasample_1_18_all = list(datasample, rankdata_datasample_1_18)

save(rankdata_datasample_1_18_all, file = "./Data/rankdata_datasample.RData")
