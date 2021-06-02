# 02_unfolding_models.R ----------------------------------------------------------------------------

# load packages, functions and data ----------------------------------------------------------------
library(smacof)
library(gridExtra)
library(colorspace)
library(ggrepel)
library(ggplot2) ; theme_set(theme_bw())
library(dplyr)
library(tidyr)
library(scales)
library(forcats)

load("./Data/rankdata.RData")
load("./Data/rankdata_datasample.RData")
datasample = rankdata_datasample_1_18_all[[1]]
rankdata_datasample = rankdata_datasample_1_18_all[[2]]
rm(rankdata_datasample_1_18_all)

# unfolding model with 288 combinations as subjects ------------------------------------------------
unfolddata = rankdata %>% select(learner.id, combinations, ranking) %>%
  pivot_wider(names_from = learner.id, values_from = ranking) %>% select(-combinations)
params = rankdata %>% select(imputation_method, grouping_cat, eval_measure, aggregation_method) %>% distinct()

unfold_ord = unfolding(unfolddata, type = "ordinal", conditionality = "unconditional",
                       ties = "primary")
save(params, unfold_ord, file = "./Data/unfolding_model.RData")

# unfolding model with 774 combinations as subjects ------------------------------------------------
datasample_df  = lapply(1:length(datasample), FUN = function(i) {
  df = lapply(1:18, FUN = function(j){
    data.frame(rep = i, nrdatasets = j, datasets = paste(sort(datasample[[i]][1:j]), collapse = " "))
  })
  df = bind_rows(df)
})
datasample_df  = bind_rows(datasample_df)

# ibrier 
unfolddata_ibrier = rankdata_datasample %>% 
  filter(eval_measure == "ibrier" & imputation_method == "threshold0.2" & aggregation_method == "mean") %>% 
  select(learner.id, ranking, rep, nrdatasets) %>%
  pivot_wider(names_from = learner.id, values_from = ranking) 
unfolddata_ibrier = merge(unfolddata_ibrier, datasample_df, by = c("rep", "nrdatasets") )
unfolddata_ibrier = unfolddata_ibrier %>% distinct(datasets, .keep_all = TRUE)
table(unfolddata_ibrier$nrdatasets)
params_ibrier = unfolddata_ibrier %>% select(rep, nrdatasets)
unfolddata_ibrier = unfolddata_ibrier %>% select(-rep, -nrdatasets, -datasets)
unfold_ord_sample_ibrier = unfolding(unfolddata_ibrier, type = "ordinal", conditionality = "unconditional",
                                     ties = "primary")
save(params_ibrier, unfold_ord_sample_ibrier, file = "./Data/unfolding_model_datasample_ibrier.RData")

# cindex
unfolddata_cindex = rankdata_datasample %>% 
  filter(eval_measure == "cindex.uno" & imputation_method == "threshold0.2" & aggregation_method == "mean") %>% 
  select(learner.id, ranking, rep, nrdatasets) %>%
  pivot_wider(names_from = learner.id, values_from = ranking) 
unfolddata_cindex = merge(unfolddata_cindex, datasample_df, by = c("rep", "nrdatasets") )
unfolddata_cindex = unfolddata_cindex %>% distinct(datasets, .keep_all = TRUE)
table(unfolddata_cindex$nrdatasets)
params_cindex = unfolddata_cindex %>% select(rep, nrdatasets)
unfolddata_cindex = unfolddata_cindex %>% select(-rep, -nrdatasets, -datasets)
unfold_ord_sample_cindex = unfolding(unfolddata_cindex, type = "ordinal", conditionality = "unconditional",
                                     ties = "primary")
save(params_cindex,unfold_ord_sample_cindex, file = "./Data/unfolding_model_datasample_cindex.RData")



# goodness of fit ----------------------------------------------------------------------------------
# unfold_ord, unfold_ord_sample_ibrier, unfold_ord_sample_cindex
# screeplot
screeplot_fct = function(data){
  stressvals <- NULL
  dimmax <- 12 # number of methods - 1
  for (i in 1:dimmax) stressvals[i] <- unfolding(data, 
                                                 ndim = i, type = "ordinal", 
                                                 conditionality = "unconditional",
                                                 ties = "primary")$stress
  p = ggplot(data.frame(nrdim = 1:dimmax, stress = stressvals), aes(x = nrdim, y = stressvals))+
    geom_line()+
    geom_point(col = c("black", "red", rep("black", 10)))+
        labs(x = "Number of Dimensions", y = "Stress")+
    scale_x_continuous(breaks = 1:dimmax, minor_breaks = NULL)
  return(p)
}
height1 = 4
width1 = 7
pdf("./Plots/goodness_of_fit/screeplot_unfolding.pdf", height = height1, width = width1)
screeplot_fct(unfolddata)
dev.off()

pdf("./Plots/goodness_of_fit/screeplot_unfolding_sample_ibrier.pdf", height = height1, width = width1)
screeplot_fct(unfolddata_ibrier)
dev.off()

# for dim => 6, iteration limit is reached
pdf("./Plots/goodness_of_fit/screeplot_unfolding_sample_cindex.pdf", height = height1, width = width1)
screeplot_fct(unfolddata_cindex)
dev.off()

# person and item spp
spp_fct = function(model){
  rspp <- sort(model$spp.row, decreasing = TRUE) /100               
  cspp <- sort(model$spp.col, decreasing = TRUE) /100              
  p1 = ggplot(data.frame(spp = cspp, id = names(cspp)), 
              aes(x = fct_reorder(id, spp, .desc = TRUE), 
                  label = fct_reorder(id, spp, .desc = TRUE),
                  y = spp))+
    geom_text_repel(min.segment.length = Inf)+
    geom_point()+
    scale_y_continuous(labels = scales::label_percent(accuracy = 0.1), breaks = seq(0,0.14,0.02),
                       limits = c(0,0.14))+
    scale_x_discrete(expand = expansion(mult = c(0.1,0.1)))+
    geom_hline(yintercept = mean(cspp), linetype = 2)+
    labs(y = "Stress Proportion (%)", x = "Methods")+
    theme(axis.text.x=element_blank(),
          panel.grid = element_blank())
  p2 = ggplot(data.frame(spp = rspp, id = names(rspp)), 
              aes(x = fct_reorder(id, spp, .desc = TRUE), 
                  label = fct_reorder(id, spp, .desc = TRUE),
                  y = spp))+
    #geom_text(vjust = -1)+
    geom_point()+
    scale_y_continuous(labels = scales::label_percent(accuracy = 0.1))+
    scale_x_discrete(expand = expansion(mult = c(0.1,0.1)))+
    geom_hline(yintercept = mean(rspp), linetype = 2)+
    labs(y = "Stress Proportion (%)", x = "Combinations of design and analysis options")+
    theme(axis.text.x=element_blank(),
          panel.grid = element_blank())
  p = grid.arrange(p1, p2, nrow = 1)
  return(p)
}
height2 = 6
width2 = 10
pdf("./Plots/goodness_of_fit/spp_unfolding.pdf", height = height2, width = width2)
spp_fct(unfold_ord)
dev.off()
pdf("./Plots/goodness_of_fit/spp_unfolding_sample_ibrier.pdf", height = height2, width = width2)
spp_fct(unfold_ord_sample_ibrier)
dev.off()
pdf("./Plots/goodness_of_fit/spp_unfolding_sample_cindex.pdf", height = height2, width = width2)
spp_fct(unfold_ord_sample_cindex)
dev.off()



# permutation test
permtest(unfold_ord, nrep = 500) 
# Observed stress value: 0.244 
# p-value: <0.001 
permtest(unfold_ord_sample_ibrier, nrep = 500) 
#Observed stress value: 0.202 
#p-value: <0.001 
permtest(unfold_ord_sample_cindex, nrep = 500) # p<0.0001
#Observed stress value: 0.133 
#p-value: <0.001 





