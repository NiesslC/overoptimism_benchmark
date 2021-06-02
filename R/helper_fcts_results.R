# helper_fcts_results.R ----------------------------------------------------------------------------

# helper functions for step-wise optimisation plots:
# - stewise_opt, nextdat, merge_and_filter, findopt, plotstepwise_fct
# helper function for unfolding plots:
# - plotunfolding_fct
# --------------------------------------------------------------------------------------------------
stepwise_opt <- function(data, step_order, eval_free, eval_default = NULL){
  whichstepevalmeas = which(step_order == "eval_measure")
  #data = result_red3
  data$grouping <- paste0(data$grouping, data$smaller_group)
  
  optinfo = vector("list", length = 4)
  
  default_values_list = list("imputation_method" = "threshold0.2",
                             "grouping" = "none<NA", "aggregation_method" = "mean", 
                             "eval_measure" = eval_default)
  
  # 1. 
  step1 = nextdat(stepnr = 1, step_order = step_order, data = data, eval_free = eval_free, 
                  default_values_list = default_values_list)
  opt1 = findopt(step = step1, stepnr = 1, step_order = step_order, whichstepevalmeas = whichstepevalmeas, 
                 eval_free = eval_free, default_values_list = default_values_list)
  optinfo[[1]] = opt1[[2]]
  opt1 = opt1[[1]]
  # 2. 
  step2 = nextdat(stepnr = 2, step_order = step_order, data = data, eval_free = eval_free, 
                  default_values_list = default_values_list)
  step2 = merge_and_filter(step2, stepnr = 2, step_order = step_order, 
                           whichstepevalmeas = whichstepevalmeas, eval_free = eval_free,
                           opt1 = opt1) 
  opt2 = findopt(step = step2, stepnr = 2, step_order = step_order, whichstepevalmeas = whichstepevalmeas,
                 eval_free = eval_free, default_values_list = default_values_list)
  optinfo[[2]] = opt2[[2]]
  opt2 = opt2[[1]]
  # 3.
  step3 =  nextdat(stepnr = 3, step_order = step_order, data = data, eval_free = eval_free, 
                   default_values_list = default_values_list)
  step3 = merge_and_filter(step3, stepnr = 3, step_order = step_order, 
                           whichstepevalmeas = whichstepevalmeas, eval_free = eval_free,
                           opt1 = opt1, opt2 = opt2) 
  opt3 = findopt(step = step3, stepnr = 3, step_order = step_order, whichstepevalmeas = whichstepevalmeas,
                 eval_free = eval_free, default_values_list = default_values_list)
  optinfo[[3]] = opt3[[2]]
  opt3 = opt3[[1]]
  # 4. 
  step4 = nextdat(stepnr = 4, step_order = step_order, data = data, eval_free = eval_free, 
                  default_values_list = default_values_list)
  step4 = merge_and_filter(step4, stepnr = 4, step_order = step_order, 
                           whichstepevalmeas = whichstepevalmeas, eval_free = eval_free,
                           opt1 = opt1, opt2 = opt2, opt3 = opt3) 
  opt4 = findopt(step = step4, stepnr = 4, step_order = step_order, whichstepevalmeas = whichstepevalmeas,
                 eval_free = eval_free, default_values_list = default_values_list)
  optinfo[[4]] = opt4[[2]]
  opt4 = opt4[[1]]
  # final data
  final = data
  final = merge_and_filter(final, stepnr = 5, step_order = step_order, 
                           whichstepevalmeas = whichstepevalmeas, eval_free = eval_free,
                           opt1 = opt1, opt2 = opt2, opt3 = opt3, opt4 = opt4) 
  final_short = final %>% select(learner.id, ranking, starts_with("opt")) %>% rename(best_rank = ranking)
  
  # merge rankings of each optimization step
  if(whichstepevalmeas > 1 & eval_free == TRUE){
    for(i in 1:(whichstepevalmeas-1)){
      optinfo[[i]] = merge(final_short[,c("learner.id", "opteval_measure")], optinfo[[i]], by = "learner.id")
      optinfo[[i]] = optinfo[[i]] %>% filter(eval_measure == opteval_measure) %>%
        select(learner.id, ranking, optstep, stepnr)
    }
  }
  
  
  return(list("final_short" = final_short, "optinfo" = optinfo))
}

#---------------------------------------------------------------------------------------------------
nextdat <- function(stepnr, step_order, data, eval_free, default_values_list){
  
  if(stepnr == 4 |
     (eval_free == TRUE & stepnr == 3 & step_order[4] == "eval_measure")){
    nextdat = data
  } else{
    next_steps = step_order[(stepnr+1):length(step_order)]
    if(eval_free == TRUE){
      next_steps = setdiff(next_steps, "eval_measure")
    }
    default_values = unname(unlist(default_values_list[next_steps]))
    
    if(length(next_steps) == 3){
      nextdat = data  %>% filter(!!as.name(next_steps[1]) == default_values[1] &
                                   !!as.name(next_steps[2]) == default_values[2] &
                                   !!as.name(next_steps[3]) == default_values[3] )
    } else if(length(next_steps) == 2){
      nextdat = data  %>% filter(!!as.name(next_steps[1]) == default_values[1] &
                                   !!as.name(next_steps[2]) == default_values[2])
    } else if(length(next_steps) == 1){
      nextdat = data  %>% filter(!!as.name(next_steps[1]) == default_values[1])
    }
  }
  return(nextdat)
}
#---------------------------------------------------------------------------------------------------
merge_and_filter <- function(step, stepnr, step_order, whichstepevalmeas, eval_free,
                             opt1 = NULL, opt2 = NULL, opt3 = NULL, opt4 = NULL){
  
  vec1 <- rep("learner.id", 4)
  if(eval_free == TRUE){
    vec2 <- c(rep("eval_measure", whichstepevalmeas - 1), rep("", 4-(whichstepevalmeas-1)))
    vecs <- lapply(1:(stepnr-1), FUN = function(i) stri_remove_empty(c(vec1[i], vec2[i])))
  } else{
    vecs = vec1
  }
  
  if(stepnr == 2){
    nextstep = merge(step, opt1, by = vecs[[1]]) 
    nextstep = nextstep %>% filter(!!as.name(step_order[[1]]) == !!as.name(paste0("opt",step_order[[1]])))
  } else if(stepnr == 3){
    nextstep = merge(step, opt1, by = vecs[[1]])
    nextstep = merge(nextstep, opt2, by = vecs[[2]])
    nextstep = nextstep %>% filter(!!as.name(step_order[[1]]) == !!as.name(paste0("opt",step_order[[1]])) &
                                     !!as.name(step_order[[2]]) == !!as.name(paste0("opt",step_order[[2]])) )
    
  } else if(stepnr == 4){
    nextstep = merge(step, opt1, by = vecs[[1]])
    nextstep = merge(nextstep, opt2, by = vecs[[2]])
    nextstep = merge(nextstep, opt3, by = vecs[[3]])
    nextstep = nextstep %>% filter(!!as.name(step_order[[1]]) == !!as.name(paste0("opt",step_order[[1]])) &
                                     !!as.name(step_order[[2]]) == !!as.name(paste0("opt",step_order[[2]])) &
                                     !!as.name(step_order[[3]]) == !!as.name(paste0("opt",step_order[[3]])))
  } else if(stepnr == 5){
    nextstep = merge(step, opt1, by = vecs[[1]])
    nextstep = merge(nextstep, opt2, by = vecs[[2]])
    nextstep = merge(nextstep, opt3, by = vecs[[3]])
    nextstep = merge(nextstep, opt4, by = vecs[[4]])
    nextstep = nextstep %>% filter(!!as.name(step_order[[1]]) == !!as.name(paste0("opt",step_order[[1]])) &
                                     !!as.name(step_order[[2]]) == !!as.name(paste0("opt",step_order[[2]])) &
                                     !!as.name(step_order[[3]]) == !!as.name(paste0("opt",step_order[[3]])) &
                                     !!as.name(step_order[[4]]) == !!as.name(paste0("opt",step_order[[4]])))
  }
  return(nextstep)
}
#---------------------------------------------------------------------------------------------------
findopt <- function(step, stepnr, step_order, whichstepevalmeas, eval_free,
                    default_values_list){
  optvar = step_order[stepnr]
  optname = paste0("opt",optvar)
  eval_chosen = stepnr >= whichstepevalmeas
  
  if(eval_chosen == TRUE | eval_free == FALSE){
    if(optvar %in% c("imputation_method", "grouping")){
      opt = step %>% group_by(learner.id) %>% 
        mutate(eval_sgn = ifelse(eval_measure == "cindex.uno", -1,1)) %>% # sign depends on eval_measure
        # rank according to ranking, if ranking is equal, according to aggregated value
        arrange(learner.id,ranking,ifelse(aggregation_method %in% c("mean","median"), eval_sgn * aggreg_value,
                                          ifelse(aggregation_method == "rank", aggreg_value, -aggreg_value)), -sum_env005) %>%
        mutate(optrank = 1:n(), isopt = optrank == 1 | (aggreg_value == aggreg_value[1] & ranking == ranking[1] & 
                                                          (sum_env005 == sum_env005[1] | is.na(sum_env005))),
               !!optname := !!as.name(optvar)) %>% filter(isopt) 
      # if still more than one option left -> use default, if not possib.: sample
      if(nrow(opt) > length(unique(opt$learner.id))){ 
        if(optvar == "imputation_method"){
          opt = opt %>% group_by(learner.id) %>% 
            filter(if(any(imputation_method == default_values_list$imputation_method)) 
              imputation_method == default_values_list$imputation_method else TRUE) %>%
            sample_n(1) %>% select(learner.id, !!optname, ranking)
          
        } else if(optvar == "grouping"){
          opt = opt %>% group_by(learner.id) %>% 
            filter(if(any(grouping == default_values_list$grouping)) 
              grouping == default_values_list$grouping else TRUE) %>%
            sample_n(1) %>% select(learner.id, !!optname, ranking)
        }
      } else{
        opt = opt %>% select(learner.id, !!optname, ranking)
      }
    } else if(optvar %in% c("eval_measure","aggregation_method")){
      if(optvar == "eval_measure"){
        eval_measures = c("cindex.uno", "ibrier")
        eval_default = default_values_list$eval_measure
        eval_measures = c(eval_measures[which(eval_measures == eval_default)],eval_measures[which(eval_measures != eval_default)])
        step[,optvar] = factor(step[,optvar], levels = eval_measures)
      } else if(optvar == "aggregation_method"){
        step[,optvar] = factor(step[,optvar], levels = c("mean","median","best0.05","rank"))
      }
      opt = step %>% group_by(learner.id) %>% 
        # rank according to ranking, if ranking is equal, according to default order of aggregation_method / eval_measure
        arrange(learner.id,ranking,!!as.name(optvar)) %>% mutate(optrank = 1:n(), isopt = optrank == 1,
                                                                 !!optname := !!as.name(optvar)) %>%
        filter(isopt) %>% sample_n(1) %>% select(learner.id, !!optname, ranking)
    }
    
    
  } else{
    if(optvar %in% c("imputation_method", "grouping")){
      opt = step %>% group_by(learner.id, eval_measure) %>% 
        mutate(eval_sgn = ifelse(eval_measure == "cindex.uno", -1,1)) %>% # sign depends on eval_measure
        # rank according to ranking, if ranking is equal, according to aggregated value
        arrange(learner.id,ranking,ifelse(aggregation_method %in% c("mean","median"), eval_sgn * aggreg_value,
                                          ifelse(aggregation_method == "rank", aggreg_value, -aggreg_value)), -sum_env005) %>%
        mutate(optrank = 1:n(), isopt = optrank == 1 | (aggreg_value == aggreg_value[1] & ranking == ranking[1] & 
                                                          (sum_env005 == sum_env005[1] | is.na(sum_env005))),
               !!optname := !!as.name(optvar)) %>% filter(isopt) 
      # if still more than one option left -> use default, if not possib.: sample
      if(nrow(opt) > length(unique(opt$learner.id))){ 
        if(optvar == "imputation_method"){
          opt = opt %>% group_by(learner.id, eval_measure) %>% 
            filter(if(any(imputation_method == default_values_list$imputation_method)) 
              imputation_method == default_values_list$imputation_method else TRUE) %>%
            sample_n(1) %>% select(learner.id, eval_measure, !!optname, ranking)
          
        } else if(optvar == "grouping"){
          opt = opt %>% group_by(learner.id, eval_measure) %>% 
            filter(if(any(grouping == default_values_list$grouping)) 
              grouping == default_values_list$grouping else TRUE) %>%
            sample_n(1) %>% select(learner.id, eval_measure, !!optname, ranking)
        }
      } else{
        opt = opt %>% select(learner.id, eval_measure, !!optname, ranking)
      }
    } else if(optvar == "aggregation_method"){
      step[,optvar] = factor(step[,optvar], levels = c("mean","median","best0.05","rank"))
      opt = step %>% group_by(learner.id, eval_measure) %>% 
        # rank according to ranking, if ranking is equal, according to default order
        # of aggregation_method / eval_measure
        arrange(learner.id,ranking,!!as.name(optvar)) %>% mutate(optrank = 1:n(), isopt = optrank == 1,
                                                                 !!optname := !!as.name(optvar)) %>%
        filter(isopt) %>% sample_n(1) %>% select(learner.id, eval_measure, !!optname, ranking)
    }
  }
  optinfo = opt
  optinfo$optstep = optname
  optinfo$stepnr = stepnr
  optinfo = optinfo %>% select(-!!optname)
  opt = opt %>% select(-ranking)
  return(list("opt" = opt, "optinfo" = optinfo))
}


#---------------------------------------------------------------------------------------------------
# plot
plotstepwise_fct <- function(data, step_order, eval_free, eval_default = NULL){
  data$learner.id = as.character(data$learner.id)

  # stepwise optimization 
  optdata = stepwise_opt(data = data, step_order = step_order, 
                         eval_free = eval_free, eval_default = eval_default)
  
  final = optdata[[1]]
  optinfo = optdata[[2]]
  optinfo = bind_rows(optinfo)
  final = merge(final, optinfo, by = "learner.id")
  final$optstep = factor(final$optstep, levels= paste0("opt",step_order))
  final$optstep = fct_rev(final$optstep)

  # plot data
  plotdata = data %>% group_by(learner.id) %>% mutate(minrank = min(ranking), maxrank = max(ranking),
                                                      range = maxrank - minrank) %>%
    select(learner.id, minrank, maxrank, range) %>% distinct()
  plotdata = melt(plotdata, measure.vars = c("minrank","maxrank"), 
                  variable.name = "minmax", value.name = "minmaxrank")
  # default ranks
  default = data %>% filter(imputation_method == "independentNA" &
                              grouping == "none" &  # vorsicht wegen grouping (nur kat, nicht </>)
                              aggregation_method == "mean" &
                              eval_measure == eval_default) %>% select(learner.id,ranking)
  default2 = final %>% select(-optstep,-stepnr,-ranking) %>% distinct() %>% 
    mutate(optstep = "default", stepnr = 0)
  default = merge(default, default2, by = "learner.id")
  default = default[match(colnames(final), colnames(default))]
  final$optstep = as.character(final$optstep)
  final = bind_rows(final, default)
  # merge final with plotdata
  plotdata = merge(plotdata, final, by = "learner.id")
  plotdata$optstep = factor(plotdata$optstep, levels = c("default", paste0("opt",step_order)))
  plotdata = plotdata %>% group_by(learner.id) %>% mutate(default_value = max(ranking))
  #plotdata$learner.id = fct_reorder(plotdata$learner.id, -plotdata$default_value) 
  plotdata$learner.id = factor(plotdata$learner.id, 
                               levels=unique(plotdata$learner.id[order(-plotdata$best_rank, -plotdata$default_value)]))
  eval_lab <- ifelse(!is.null(eval_default), eval_default, "not specified")
  plotdata$linevar = "linevar"

  # Plot
  size_text = 13
  cols = list("default" = "#F8766D", "grouping" = "#A3A500" , "eval_measure" = "#00BF7D",
              "imputation_method" = "#00B0F6", "aggregation_method" = "orchid3")
  cols_vec = unname(c(cols$default, cols[[step_order[1]]],cols[[step_order[2]]],cols[[step_order[3]]],cols[[step_order[4]]]))
  linesize = 0.9
  arrowsize = 1
  steplabel_list = list("imputation_method" = "imputation method" , "eval_measure" = "performance measure",
                        "aggregation_method" = "aggregation method", "grouping" = "data sets")
  steplabel =  unname(c(steplabel_list[[step_order[1]]],steplabel_list[[step_order[2]]],
                        steplabel_list[[step_order[3]]],steplabel_list[[step_order[4]]]))
  
  plotdata1 = plotdata %>% select(learner.id, ranking, stepnr)%>% arrange(stepnr) %>%
    distinct() %>% pivot_wider(names_from = stepnr, values_from = ranking, names_prefix = "stepnr") %>%
    mutate(meanrank1 = ifelse(stepnr0!= stepnr1,stepnr0-((stepnr0-stepnr1)/2), NA), 
           meanrank2 = ifelse(stepnr1!= stepnr2,stepnr1-((stepnr1-stepnr2)/2), NA), 
           meanrank3 = ifelse(stepnr2!= stepnr3,stepnr2-((stepnr2-stepnr3)/2), NA), 
           meanrank4 = ifelse(stepnr3!= stepnr4,stepnr3-((stepnr3-stepnr4)/2), NA))
  plotdata2 = melt(plotdata1, measure.vars = paste0("meanrank", 1:4), variable.name = "stepnr")
  plotdata2 = plotdata2 %>% drop_na(value)
  p = ggplot(plotdata, aes(x = learner.id))+
    geom_line(data = plotdata, aes(y = minmaxrank, x = learner.id), linetype = 2, size = 0.4)+
    geom_point(aes(y = ranking, col = optstep), size = 0.2)+
    
    geom_segment(data = plotdata1 %>% filter(stepnr0 != stepnr1), aes(x = learner.id, xend = learner.id, y = stepnr0, yend = stepnr1),
                 arrow = arrow(length = unit(0.4, "cm")),  col = cols[[step_order[1]]], size = arrowsize) +
    geom_segment(data = plotdata1 %>% filter(stepnr1 != stepnr2), aes(x = learner.id, xend = learner.id, y = stepnr1, yend = stepnr2),
                 arrow = arrow(length = unit(0.4, "cm")),  col = cols[[step_order[2]]], size = arrowsize) +
    geom_segment(data = plotdata1 %>% filter(stepnr2 != stepnr3), aes(x = learner.id, xend = learner.id, y = stepnr2, yend = stepnr3),
                 arrow = arrow(length = unit(0.4, "cm")),  col = cols[[step_order[3]]], size = arrowsize) +
    geom_segment(data = plotdata1 %>% filter(stepnr3 != stepnr4), aes(x = learner.id, xend = learner.id, y = stepnr3, yend = stepnr4),
                 arrow = arrow(length = unit(0.4, "cm")),  col = cols[[step_order[4]]], size = arrowsize) +
    geom_text(data = plotdata2, aes(x = learner.id, label = paste0("(", gsub("meanrank", "",stepnr),")"),
                                    y = value), vjust= -1, show.legend = FALSE)+
    coord_flip()+
    geom_point(aes(y = best_rank), size = 1.9, col = "black")+
    geom_point(data = plotdata %>% filter(stepnr ==0),   
               aes(y = ranking), size = 2.2, col = cols[["default"]])+
    scale_color_manual(values= cols_vec, labels = c("default option", paste0("(", 1:4, ") ",steplabel)))+
    labs(shape = "", col = "Optimisation steps", y = "Rank", linetype = "", x  =  "Method")+
    guides(colour = guide_legend(override.aes = list(size=1)))+
    scale_y_continuous(breaks = 1:13, minor_breaks = 1:13)+
    theme(text = element_text(size = size_text))
  
  return(p)
  
}
#---------------------------------------------------------------------------------------------------
plotunfolding_fct = function(unfold_solution, rankdata, params, var,
                   default_imputation = "threshold0.2",
                   default_eval = "ibrier",
                   default_dataset = "all",
                   default_aggregation = "mean",
                   output = "plot"){
  
  # generate person (combinations) and item (methods) data 
  conf_items <- as.data.frame(unfold_solution$conf.col)
  conf_items$learner.id = rownames(conf_items)
  conf_items = merge(conf_items, 
                     rankdata %>% select(learner.id, method, group.structure) %>% distinct(),
                     by = "learner.id")
  conf_persons <- as.data.frame(unfold_solution$conf.row)
  conf_persons = cbind(conf_persons, params)  
  conf_persons$id = rownames(conf_persons)
  
  # set names and defaults for each choice 
  conf_persons$imputation_method = gsub("NA", "",conf_persons$imputation_method)
  conf_persons$imputation_method = relevel(factor(conf_persons$imputation_method), ref =default_imputation)
  conf_persons$eval_measure = relevel(factor(conf_persons$eval_measure), ref = default_eval)
  conf_persons$grouping_cat = gsub("none<NA", "all", conf_persons$grouping_cat)
  conf_persons$grouping_cat = factor(conf_persons$grouping_cat,
                                     levels = c("clin<FALSE","clin<TRUE","N<FALSE","N<TRUE",
                                                "neff<FALSE","neff<TRUE",
                                                "p<FALSE",  "p<TRUE", "all"))
  conf_persons$aggregation_method = relevel(factor(conf_persons$aggregation_method), ref = default_aggregation)
  
  # plot parameters 
  col1 <- c("#999999", "#E69F00", "#56B4E9", "#009E73",
            "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
  col2 = c(brewer.pal(3, "YlOrBr")[3:2], brewer.pal(3, "RdPu")[3:2],
           brewer.pal(3, "Greens")[3:2], brewer.pal(3, "Blues")[3:2], "#999999")
  col3 = c("#29BF12","#00A5CF","#DE1A1A", "#574AE2","#FFBF00")
  legend_position = "top"
  alpha_seg = 0.5
  expand_x =  c(0.15,0.15)
  expand_y = c(0.05, 0.1)
  col_seg = "grey"
  size_seg = 0.2
  size_text = 13
  size_legendtitle = 11.7
  size_legendtext = 11.7
  shape_items = 17
  size_items = 1.8
  
  # basic plot 
  p =  ggplot(conf_persons, aes(x = D1, y = D2)) +
    theme(legend.position = legend_position)+
    coord_fixed(ratio = 1)+
    scale_y_continuous(expand = expansion(mult = expand_y))+
    scale_x_continuous(expand = expansion(mult = expand_x))+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank())+
    labs(x = "Dimension 1", y = "Dimension 2")
  
  # color plot according to choice that is being considered
  if(var == "imputation_method"){
    conf_persons = conf_persons %>%
      group_by(eval_measure, grouping_cat, aggregation_method) %>% 
      mutate(isdefault = imputation_method %in% default_imputation,
             x = D1[isdefault],
             y = D2[isdefault]) %>%
      mutate(group_id = cur_group_id()) %>%
      arrange(group_id, desc(isdefault)) %>%
      mutate(dist_from_first = sqrt((D1 - first(D1))^2 + (D2 - first(D2))^2))%>%
      ungroup()
    p = p +  
      geom_segment(data = conf_persons, aes(x = x, y = y, xend = D1, yend = D2), col = col_seg,
                   alpha = alpha_seg, size = size_seg+0.3)+
      geom_point(data = conf_persons, aes(x = D1, y = D2, col = imputation_method)) +
      geom_point(data = conf_persons %>% filter(imputation_method %in% default_imputation),
                 aes(x = D1, y = D2), col = col1[1]) +
      geom_text_repel(data = conf_items, aes(x = D1, y = D2, label = learner.id), col = "grey29", vjust = -0.8)+
      geom_point(data = conf_items, aes(x = D1, y = D2), col = "grey29", shape = shape_items, size = size_items )+
      scale_color_manual(name = "Option", values = col1, labels = c("threshold0.2", "random", "mean","weighted"))+
      theme(legend.margin=margin(0,0,0,0),
            legend.box.margin=margin(18,0,0,0),
            text = element_text(size=size_text),
            legend.title=element_text(size=size_legendtitle),
            legend.text=element_text(size=size_legendtext))
    
    
  } else if(var == "eval_measure"){
    conf_persons = conf_persons %>%
      group_by(imputation_method, grouping_cat, aggregation_method) %>% 
      mutate(isdefault = eval_measure %in% default_eval,
             x = D1[isdefault],
             y = D2[isdefault]) %>%
      mutate(group_id = cur_group_id()) %>%
      arrange(group_id, desc(isdefault)) %>%
      mutate(dist_from_first = sqrt((D1 - first(D1))^2 + (D2 - first(D2))^2))%>%
      ungroup()
    p = p + 
      geom_segment(data = conf_persons, aes(x = x, y = y, xend = D1, yend = D2), col = col_seg,
                   alpha = alpha_seg, size = size_seg)+
      geom_point(data = conf_persons, aes(x = D1, y = D2, col = eval_measure))  +
      geom_point(data = conf_persons %>% filter(eval_measure %in% default_eval),
                 aes(x = D1, y = D2), col = col1[1]) +
      geom_text_repel(data = conf_items, aes(x = D1, y = D2, label = learner.id), col = "grey29", vjust = -0.8) +
      geom_point(data = conf_items, aes(x = D1, y = D2), col = "grey29", shape = shape_items, size = size_items )+
      scale_color_manual(name = "Option", values = col1, labels = c("ibrier", "cindex"))+
      theme(legend.margin=margin(0,0,0,0),
            legend.box.margin=margin(18,0,0,0),
            text = element_text(size=size_text),
            legend.title=element_text(size=size_legendtitle),
            legend.text=element_text(size=size_legendtext))
    
  } else if(var == "grouping_cat"){
    conf_persons = conf_persons %>%
      group_by(imputation_method, eval_measure, aggregation_method) %>% 
      mutate(isdefault = grouping_cat %in% default_dataset,
             x = D1[isdefault],
             y = D2[isdefault]) %>%
      mutate(group_id = cur_group_id()) %>%
      arrange(group_id, desc(isdefault)) %>%
      mutate(dist_from_first = sqrt((D1 - first(D1))^2 + (D2 - first(D2))^2))%>%
      ungroup()
      p = p + 
      geom_segment(data = conf_persons, aes(x = x, y = y, xend = D1, yend = D2), col = col_seg,
                   alpha = alpha_seg, size = size_seg)+
      geom_point(data = conf_persons, aes(x = D1, y = D2, col = grouping_cat))  +
      geom_point(data = conf_persons %>% filter(grouping_cat %in% default_dataset),
                 aes(x = D1, y = D2), col = rev(col2)[1]) +
      scale_color_manual(name = "Option", values = col2, 
                         labels = c("clin large", "clin small", "n large", "n small ", 
                                    unname(TeX('$n_e$ large')), unname(TeX('$n_e$ small')), 
                                    "p large", "p small", "all"))+
      geom_text_repel(data = conf_items, aes(x = D1, y = D2, label = learner.id), col = "grey29", vjust = -0.8) +
      geom_point(data = conf_items, aes(x = D1, y = D2), col = "grey29", shape = shape_items, size = size_items )+
      theme(legend.margin=margin(0,0,0,0),
            legend.box.margin=margin(0,0,0,0),
            legend.text.align = 0,
            text = element_text(size=size_text),
            legend.title=element_text(size=size_legendtitle),
            legend.text=element_text(size=size_legendtext))+
      guides(col=guide_legend(nrow=2,byrow=FALSE))
      
  } else if(var == "aggregation_method"){
    conf_persons = conf_persons %>%
      group_by(imputation_method, eval_measure, grouping_cat) %>% 
      mutate(isdefault = aggregation_method %in% default_aggregation,
             x = D1[isdefault],
             y = D2[isdefault]) %>%
      mutate(group_id = cur_group_id()) %>%
      arrange(group_id, desc(isdefault)) %>%
      mutate(dist_from_first = sqrt((D1 - first(D1))^2 + (D2 - first(D2))^2))%>%
      ungroup()
    
    p = p +  
      geom_segment(data = conf_persons, aes(x = x, y = y, xend = D1, yend = D2), col = col_seg,
                   alpha = alpha_seg, size = size_seg+0.3)+
      geom_point(data = conf_persons, aes(x = D1, y = D2, col= aggregation_method))  +
      geom_point(data = conf_persons %>% filter(aggregation_method %in% default_aggregation),
                 aes(x = D1, y = D2), col = col1[1]) +
      geom_text_repel(data = conf_items, aes(x = D1, y = D2, label = learner.id), col = "grey29", vjust = -0.8) +
      geom_point(data = conf_items, aes(x = D1, y = D2), col = "grey29", shape = shape_items, size = size_items )+
      scale_color_manual(name = "Option", values = col1)+
      theme(legend.margin=margin(0,0,0,0),
            legend.box.margin=margin(18,0,0,0),
            text = element_text(size=size_text),
            legend.title=element_text(size=size_legendtitle),
            legend.text=element_text(size=size_legendtext))
  }
  if(output == "plot"){
    return(p)
  } else if(output == "dist"){
    return(conf_persons)
  }
  
}


