# 03_results.R -------------------------------------------------------------------------------------

# load packages, functions and data ----------------------------------------------------------------
library(ggplot2) ; theme_set(theme_bw())
library(dplyr)
library(grid)
library(tidyr)
library(RColorBrewer)
library(reshape2)
library(stringi)
library(ggrepel)
library(latex2exp)
library(forcats)
library(shades)

source("./R/helper_fcts_results.R")

load("./Data/rankdata.RData")
load("./Data/rankdata_datasample.RData")
rankdata_datasample = rankdata_datasample_1_18_all[[2]]
rm(rankdata_datasample_1_18_all)
load("./Data/unfolding_model.RData")
load("./Data/unfolding_model_datasample_ibrier.RData")
load("./Data/unfolding_model_datasample_cindex.RData")

if (!dir.exists("Plots")) {dir.create("Plots")} # Folder for Plots

# overall variability (Figure 2 in Section 4.1) ----------------------------------------------------
pdf("./Plots/overall_variability.pdf", width = 6, height = 6)
rankdata %>% group_by(learner.id, ranking) %>%
  summarise(count = n(),perc_count = count/(nrow(rankdata)/13)) %>% ungroup() %>%
  ggplot(aes(x = ranking, y = perc_count))+
  geom_bar(stat = "identity")+
  #  geom_histogram(binwidth = 1)+
  facet_wrap(~learner.id, ncol = 3)+
  labs(x = "Rank", y  = "Relative frequency")+
  scale_x_continuous(breaks = 1:13, minor_breaks = 1:13)
dev.off()

# stepwise optimisation (Figure 3 in Section 4.1) --------------------------------------------------

# different step orders and default values 
pdf("./Plots/stepwise_imp_agg_eval_group.pdf", height = 6, width = 8.5)
step_order = c("imputation_method", "aggregation_method", "eval_measure", "grouping")
plotstepwise_fct(data = rankdata, step_order = step_order, eval_free = FALSE, eval_default = "ibrier")
dev.off()


pdf("./Plots/stepwise_group_eval_agg_imp.pdf", height = 6, width = 8.5)
step_order = c("grouping", "eval_measure", "aggregation_method", "imputation_method")
plotstepwise_fct(data = rankdata, step_order = step_order, eval_free = FALSE, eval_default = "ibrier")
dev.off()

pdf("./Plots/stepwise_imp_agg_eval_group_cindex.pdf", height = 6, width = 8.5)
step_order = c("imputation_method", "aggregation_method", "eval_measure", "grouping")
plotstepwise_fct(data = rankdata, step_order = step_order, eval_free = FALSE, eval_default = "cindex.uno")
dev.off()
rm(step_order)
# unfolding solution (Figure 4 in Section 4.2) -----------------------------------------------------
# plot parameters
width = 6
height = 6

pdf("./Plots/unfolding_imputation_method.pdf", height = height, width = width)
plotunfolding_fct(unfold_solution = unfold_ord, rankdata = rankdata,
                  params = params, var = "imputation_method")
dev.off()
pdf("./Plots/unfolding_grouping_cat.pdf", height = height, width = width)
plotunfolding_fct(unfold_solution = unfold_ord, rankdata = rankdata,
                  params = params, var = "grouping_cat")
dev.off()
pdf("./Plots/unfolding_eval_measure.pdf", height = height, width = width)
plotunfolding_fct(unfold_solution = unfold_ord, rankdata = rankdata,
                  params = params, var = "eval_measure")
dev.off()
pdf("./Plots/unfolding_aggregation_method.pdf", height = height, width = width)
plotunfolding_fct(unfold_solution = unfold_ord, rankdata = rankdata,
                  params = params, var = "aggregation_method")
dev.off()

# distances between ideal points (Figure 5 in Section 4.2) -----------------------------------------

# get data from unfolding solution 
defaultdist_data = rbind(plotunfolding_fct(unfold_solution = unfold_ord, rankdata = rankdata,
                                           params = params, var = "grouping_cat", output = "dist") %>% 
                           select(grouping_cat, isdefault, D1, D2, dist_from_first) %>%
                           rename(option = grouping_cat) %>% mutate(choice = "data sets"),
                         plotunfolding_fct(unfold_solution = unfold_ord, rankdata = rankdata,
                                           params = params, var = "eval_measure", output = "dist")%>% 
                           select(eval_measure, isdefault, D1, D2, dist_from_first)%>%
                           rename(option = eval_measure) %>% mutate(choice = "performance measure"),
                         plotunfolding_fct(unfold_solution = unfold_ord, rankdata = rankdata,
                                           params = params, var = "imputation_method", output = "dist")%>% 
                           select(imputation_method, isdefault, D1, D2, dist_from_first)%>%
                           rename(option = imputation_method) %>% mutate(choice = "imputation method"),
                         plotunfolding_fct(unfold_solution = unfold_ord, rankdata = rankdata, 
                                           params = params, var = "aggregation_method", output = "dist")%>% 
                           select(aggregation_method, isdefault, D1, D2, dist_from_first)%>%
                           rename(option = aggregation_method) %>% mutate(choice = "aggregation method"))
defaultdist_data$choice = factor(defaultdist_data$choice, levels = c("performance measure","data sets",
                                                                     "imputation method","aggregation method"),
                                 labels = c("Perform. \nmeas.","Data sets","Imputation method","Aggregation method"))
defaultdist_data = defaultdist_data %>% filter(!isdefault) %>% droplevels()

defaultdist_data$option = factor(defaultdist_data$option, levels = levels(defaultdist_data$option),
                                 labels = c(paste0("all - ", c("clin large", "clin small", "n large", "n small ", 
                                                               "n_e large", "n_e small", 
                                                               "p large", "p small")), 
                                            "ibrier - cindex", 
                                            paste0("threshold0.2 - ",c("random", "mean","weighted")), 
                                            paste0("mean - ", c("best0.05","median","rank") )))

# plot the distances
p = ggplot(defaultdist_data, aes(x = option, y = dist_from_first))+
  geom_boxplot()+
  facet_grid(. ~ choice, scales = "free", space='free') +
  guides(x = guide_axis(angle = 30))+
  labs(y = "Distance", x = "Default option - Alternative option")
gt = ggplot_gtable(ggplot_build(p))
gt$widths[5] = 1.1*gt$widths[5]
pdf("./Plots/unfolding_defaultdist.pdf", height = 4.5, width = 7.5)
grid.draw(gt)
dev.off()

rm(p, gt, defaultdist_data)

# unfolding solution with sampled groups of data sets - ibrier (Figure 6 in Section 4.2) -----------
conf_items <- as.data.frame(unfold_ord_sample_ibrier$conf.col)
conf_items$learner.id = rownames(conf_items)
conf_items = merge(conf_items, 
                   rankdata_datasample %>% select(learner.id, method, group.structure) %>% distinct(),
                   by = "learner.id")
conf_persons = as.data.frame(unfold_ord_sample_ibrier$conf.row)
conf_persons = cbind(conf_persons, params_ibrier)  
conf_persons$id = rownames(conf_persons)
conf_persons$nrdatasets = factor(conf_persons$nrdatasets, levels = paste(1:18))


pdf("./Plots/unfolding_datasample_ibrier.pdf", height = 6, width = 9)
ggplot(conf_persons, aes(x = D2, y = D1)) +
  coord_fixed(ratio = 1)+
  geom_point(aes(col = as.numeric(as.character(nrdatasets))), alpha = 0.6)+
  geom_text_repel(data = conf_items, aes(x = D2, y = D1, label = learner.id), col = "grey29", 
            min.segment.length = Inf,
            nudge_y = 0.05)+
  geom_point(data = conf_items, aes(x = D2, y = D1), col = "grey29", shape = 17, size = 1.8)+
  labs(color = "Number of \ndata sets", x = "Dimension 1", y = "Dimension 2")+
  scale_x_continuous(expand = expansion(mult = 0.1))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        text = element_text(size = 13))+
  scale_color_viridis_c(begin = 1, end = 0.35,
                        breaks=c(1,5,10,15,18),
                        limits=c(1,18))
dev.off()

rm(conf_items, conf_persons)
# unfolding solution with sampled groups of data sets - cindex  ------------------------------------
conf_items <- as.data.frame(unfold_ord_sample_cindex$conf.col)
conf_items$learner.id = rownames(conf_items)
conf_items = merge(conf_items, 
                   rankdata_datasample %>% select(learner.id, method, group.structure) %>% distinct(),
                   by = "learner.id")
conf_persons = as.data.frame(unfold_ord_sample_cindex$conf.row)
conf_persons = cbind(conf_persons, params_cindex)  
conf_persons$id = rownames(conf_persons)
conf_persons$nrdatasets = factor(conf_persons$nrdatasets, levels = paste(1:18))


pdf("./Plots/unfolding_datasample_cindex.pdf", height = 6, width = 9)
ggplot(conf_persons, aes(x = D2, y = D1)) +
  coord_fixed(ratio = 1)+
  geom_point(aes(col = as.numeric(as.character(nrdatasets))), alpha = 0.7)+
  geom_text_repel(data = conf_items, aes(x = D2, y = D1, label = learner.id), col = "grey29",
                  min.segment.length = Inf,
                  nudge_y = 0.05)+
  geom_point(data = conf_items, aes(x = D2, y = D1), col = "grey29", shape = 17, size = 1.8)+
  labs(color = "Number of \ndata sets", x = "Dimension 1", y = "Dimension 2")+
  scale_x_continuous(expand = expansion(mult = 0.1))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        text = element_text(size = 13))+
  scale_color_viridis_c(begin = 1, end = 0.3,
                        breaks=c(1,5,10,15,18),
                        limits=c(1,18))

dev.off()

rm(conf_items, conf_persons)
















