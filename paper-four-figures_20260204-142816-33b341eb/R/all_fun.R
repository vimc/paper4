#all fun 

lowerbound <- 1e-8
per <- 1e3

fun_simple_ridges <- function(dfin, outc="deaths", cols=vaccine_colours) {
  
  if (any(dfin$activity_type == "all")){
    y_lab <- "Disease"
    dfin <- dfin %>% group_by(disease) %>% mutate(thing=disease)
    
  } else {
    y_lab <- "Vaccine"
    dfin <- dfin %>% group_by(vaccine)%>% mutate(thing=vaccine) 
  }
  
  dfin %>% 
    group_by(new_id, thing) %>%
    summarise(mean_outc = weighted.mean(!!as.name(paste0(outc,"_averted_rate")),fvps, na.rm=TRUE)) %>%
    
    ggplot()+
    aes(x = mean_outc*per, 
        y=reorder(thing, mean_outc), fill = thing)+
    geom_density_ridges(alpha=0.7, stat="binline", bins=200)+
    scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                  labels = trans_format("log10", math_format(10^.x)))+
    theme_ridges()+
    labs(x = paste0("Impact ratio (", ifelse(outc=="dalys","DALYs",outc)," averted per thousand vaccinated)"), 
         y = y_lab)+
    theme(legend.position = "none")+
    scale_fill_manual(values=cols)
}

fun_simple_ridges_alloutc <- function(dfin, cols=vaccine_colours) {
  
  if (any(dfin$activity_type == "all")){
    y_lab <- "Disease"
    dfin <- dfin %>% group_by(disease) %>% mutate(thing=disease)
    
  } else {
    y_lab <- "Vaccine"
    dfin <- dfin %>% group_by(vaccine)%>% mutate(thing=vaccine) 
  }
  
  tmp <- dfin %>% 
    select(-c(yll_averted_rate, cases_averted_rate)) %>%
    tidyr::pivot_longer(names_to = "outc", values_to = "value", cols = c(deaths_averted_rate, dalys_averted_rate)) %>%
    group_by(new_id, thing, outc) %>%
    summarise(mean_outc = weighted.mean(value,fvps, na.rm=TRUE)) %>%
    mutate(outc = if_else(outc=="dalys_averted_rate", "DALYs", "Deaths")) %>%
    mutate(outc = factor(outc, levels=c("Deaths", "DALYs"))) 
  
  #order by deaths
  levelsorder <- tmp %>% filter(outc=="Deaths") %>% group_by(thing) %>% 
    summarise(oot = mean(mean_outc)) %>% arrange(oot) %>% pull(thing)
  
  tmp%>%
    mutate(thing = factor(thing, levels = levelsorder)) %>%
    
    ggplot()+
    aes(x = mean_outc*per, 
        y=thing, fill = thing)+
    geom_density_ridges(alpha=0.7, stat="binline", bins=200)+
    scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                  labels = trans_format("log10", math_format(10^.x)))+
    theme_ridges()+
    labs(x = paste0("Impact ratio (per thousand vaccinated)"), 
         y = y_lab)+
    theme(legend.position = "none")+
    scale_fill_manual(values=cols)+
    facet_wrap(.~ outc, ncol=2, scales="free_x")
}

fun_density_subregion <- function(dfin, dis= TRUE, outc="deaths", act=FALSE){
  if (dis){
    y_lab <- "Disease"
    dfin <- dfin %>% group_by(disease) %>% mutate(thing=disease)
  } else {
    y_lab <- "Vaccine"
    dfin <- dfin %>% group_by(vaccine)%>% mutate(thing=vaccine) 
  }
  
  if(act)dfin <- dfin %>% group_by(new_id, thing, subregion, activity_type) else dfin <- dfin %>% group_by(new_id, thing, subregion)
  
  p <- dfin %>% 
    summarise(mean_outc = weighted.mean(!!as.name(paste0(outc,"_averted_rate")),fvps, na.rm=TRUE)) %>% 
    filter(mean_outc>lowerbound) %>%
    
    ggplot()+
    aes(y = subregion, x=mean_outc*per, fill=subregion)+
    geom_density_ridges( alpha=0.9, stat="binline", bins=200, scale=7)+
    scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                  labels = trans_format("log10", math_format(10^.x)))+
    scale_fill_manual(values = subregion_colours)+
    theme_ridges()+
    theme( axis.text.x = element_text(angle=45,hjust=1),
           axis.text.y=element_blank(),
           legend.position = "bottom")+
    labs(y = y_lab, 
         x = paste0("Impact ratio (", ifelse(outc=="dalys","DALYs",outc)," averted per thousand vaccinated)"), 
         fill = "Subregion")+
    guides(fill=guide_legend(nrow=3))
  if(act){
    p <- p+ facet_wrap(thing~activity_type, ncol=4, strip.position="right")
  } else {
    p <- p+ facet_wrap(thing~., ncol=4, strip.position="right")
  }
  p
}

fun_ladder_subregion <- function(dfin, dis= TRUE, outc="deaths", act=FALSE){
  if (dis){
    y_lab <- "Disease"
    dfin <- dfin %>% group_by(disease) %>% mutate(thing=disease)
  } else {
    y_lab <- "Vaccine"
    dfin <- dfin %>% group_by(vaccine)%>% mutate(thing=vaccine) 
  }
  
  if(act)dfin <- dfin %>% group_by(new_id, thing, subregion, activity_type) else dfin <- dfin %>% group_by(new_id, thing, subregion)
  
  tmp <- dfin %>% 
    summarise(mean_outc = weighted.mean(!!as.name(paste0(outc,"_averted_rate")),fvps, na.rm=TRUE)) %>% 
    filter(mean_outc>lowerbound) 
  
  if(act) {tmp <- tmp %>% group_by(thing, subregion, activity_type)
  } else {tmp <- tmp %>% group_by(thing, subregion)} 
  
  p <- tmp %>%
    summarise(lowerq = quantile(mean_outc, 0.025),
              meanq = mean(mean_outc),
              higherq = quantile(mean_outc, 0.975)) %>%
    
    ggplot()+
    aes(y = subregion, colour=subregion)+
    geom_linerange(aes(xmin = lowerq*per, xmax = higherq*per), linewidth = 1)+
    geom_point(aes(x = meanq*per), size=2) +
    scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                  labels = trans_format("log10", math_format(10^.x)))+
    scale_colour_manual(values = subregion_colours)+
    theme_ridges()+
    theme( axis.text.x = element_text(angle=45,hjust=1),
           axis.text.y=element_blank(),
           legend.position = "bottom")+
    labs(y = "Subregion", 
         x = paste0("Impact ratio (", ifelse(outc=="dalys","DALYs",outc)," averted per thousand vaccinated)"), 
         colour = "Subregion")+
    guides(colour=guide_legend(nrow=3))
  if(act){
    p <- p+ facet_wrap(thing~activity_type, ncol=4, strip.position="right")
  } else {
    p <- p+ facet_wrap(thing~., ncol=4, strip.position="right")
  }
  p
  
}

plot_density_vaccine <- function(df2_mod_ave, outc="deaths"){
  p <- lapply(unique(df2_mod_ave$activity_type),
              FUN = function(a) df2_mod_ave %>% filter(activity_type==a) %>%
                group_by(subregion, vaccine, run_id, disease, activity_type) %>%
                summarise(outc_averted_rate = weighted.mean(!!as.name(paste0(outc,"_averted_rate")),fvps,na.rm=TRUE)) %>%
                
                ggplot()+
                aes(y = vaccine, x = outc_averted_rate*per, fill = vaccine)+
                geom_density_ridges( alpha=0.9 ,stat="binline", bins=200, scale=7)+
                scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                              labels = trans_format("log10", math_format(10^.x)))+
                scale_fill_manual(values = vaccine_colours, aesthetics = c("fill", "colour"))+
                theme_ridges()+
                theme( axis.text.x = element_text(angle=45,hjust=1),
                       axis.text.y = element_blank(),
                       legend.position = "bottom")+
                labs(y = "Subregion", 
                     x = paste0("Impact ratio (", ifelse(outc=="dalys","DALYs",outc)," averted per thousand vaccinated)"), 
                     fill = "Vaccine")+
                facet_wrap(.~subregion, ncol=3, scales="free_y")+
                guides( fill = guide_legend("Vaccine", nrow=3))+
                ggtitle(R.utils::capitalize(a))
  )
  p
}

plot_density_disease <- function(df2_mod_ave, outc="deaths"){
  p <- lapply(unique(df2_mod_ave$activity_type),
              FUN = function(a) df2_mod_ave %>% filter(activity_type==a) %>%
                group_by(subregion, run_id, disease, activity_type) %>%
                summarise(outc_averted_rate = weighted.mean(!!as.name(paste0(outc,"_averted_rate")),fvps,na.rm=TRUE)) %>%
                filter(outc_averted_rate >lowerbound) %>%
                
                ggplot()+
                aes(y = disease, x = outc_averted_rate*per, fill = disease)+
                geom_density_ridges( alpha=0.9 ,stat="binline", bins=200, scale=7)+
                scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                              labels = trans_format("log10", math_format(10^.x)))+
                scale_fill_manual(values = disease_colours, aesthetics = c("fill", "colour"))+
                theme_ridges()+
                theme( axis.text.x = element_text(angle=45,hjust=1),
                       axis.text.y = element_blank(),
                       legend.position = "bottom")+
                labs(y = "Subregion", 
                     x = paste0("Impact ratio (", ifelse(outc=="dalys","DALYs",outc)," averted per thousand vaccinated)"), 
                     fill = "Disease")+
                facet_wrap(.~subregion, ncol=3, scales="free_y")+
                guides( fill = guide_legend("Disease", nrow=3))+
                ggtitle(R.utils::capitalize(a))
  )
  p
}

plot_ladder_disease <- function(df2_mod_ave, outc="deaths"){
  p <- lapply(unique(df2_mod_ave$activity_type),
              FUN = function(a) df2_mod_ave %>% filter(activity_type==a) %>%
                group_by(subregion, run_id, disease, activity_type) %>%
                summarise(outc_averted_rate = weighted.mean(!!as.name(paste0(outc,"_averted_rate")),fvps,na.rm=TRUE)) %>%
                filter(outc_averted_rate >lowerbound) %>%
                group_by(subregion, disease, activity_type) %>%
                summarise(lower = quantile(outc_averted_rate, 0.025), 
                          mean = mean(outc_averted_rate), upper = quantile(outc_averted_rate, 0.975)) %>%
                
                ggplot()+
                aes(y = disease, colour = disease)+
                geom_linerange(aes(xmin = lower*per, xmax = upper*per), linewidth=1)+
                geom_point(aes(x = mean*per)) +
                scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                              labels = trans_format("log10", math_format(10^.x)))+
                scale_fill_manual(values = disease_colours, aesthetics = c("fill", "colour"))+
                theme_ridges()+
                theme( axis.text.x = element_text(angle=45,hjust=1),
                       axis.text.y = element_blank(),
                       legend.position = "bottom")+
                labs(y = "Disease", 
                     x = paste0("Impact ratio (", ifelse(outc=="dalys","DALYs",outc)," averted per thousand vaccinated)"), 
                     fill = "Disease")+
                facet_wrap(.~subregion, ncol=3, scales="free_y")+
                guides( fill = guide_legend("Disease", nrow=3))+
                ggtitle(R.utils::capitalize(a))
  )
  p
}

plot_ladder_disease_act <- function(df2_mod_ave, outc="deaths"){
  df2_mod_ave  %>% filter(method=="standard", activity_type != "all") %>%
    filter(!disease %in% c("MenA", "MenCWYX")) %>% 
    group_by(subregion, run_id, disease, activity_type) %>%
    summarise(outc_averted_rate = weighted.mean(!!as.name(paste0(outc,"_averted_rate")),fvps,na.rm=TRUE)) %>%
    filter(outc_averted_rate >lowerbound) %>%
    group_by(subregion, disease, activity_type) %>%
    summarise(lower = quantile(outc_averted_rate, 0.025), 
              mean = mean(outc_averted_rate), upper = quantile(outc_averted_rate, 0.975)) %>%
    
    ggplot()+
    aes(y = disease, colour = disease, alpha = activity_type, shape=activity_type)+
    geom_linerange(aes(xmin = lower*per, xmax = upper*per, x=mean*per), linewidth=1, 
                   position = position_jitter(seed = 123, width =0.3))+
    geom_point(aes(x = mean*per), position = position_jitter(seed = 123, width =0.3), size=2) +
    scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                  labels = trans_format("log10", math_format(10^.x)))+
    scale_fill_manual(values = disease_colours, aesthetics = c("fill", "colour"))+
    theme_ridges()+
    theme( axis.text.x = element_text(angle=45,hjust=1),
           axis.text.y = element_blank(),
           legend.position = "bottom")+
    labs(y = "Disease", 
         x = paste0("Impact ratio (", ifelse(outc=="dalys","DALYs",outc)," averted per thousand vaccinated)"), 
         Colour = "Disease", alpha="Activity type", shape="Activity type")+
    facet_wrap(.~subregion, ncol=3, scales="free_y")+
    guides( colour = guide_legend("Disease", nrow=3))+
    scale_alpha_discrete(range=c(0.9,1))
  
}

plot_modelling_group_variation <- function(df2, df3, outc="deaths"){ 
  
  df2 %>% left_join(df3, by = join_by(modelling_group, disease)) %>% 
    group_by(disease) %>% 
    mutate(mean_outc = weighted.mean(!!as.name(paste0(outc,"_averted_rate")),fvps, na.rm=TRUE)) %>%
    filter(!!as.name(paste0(outc,"_averted_rate"))>lowerbound) %>%
    ggplot()+
    aes(fill = as.character(mod_num), x = !!as.name(paste0(outc,"_averted_rate")), y=reorder(disease, mean_outc))+
    geom_density_ridges(alpha=0.5, stat="binline", bins=200, draw_baseline=FALSE)+
    facet_grid(.~activity_type, scales="free")+
    theme_bw()+
    theme(legend.position = "none", axis.text.x = element_text(angle=90, hjust=1))+
    scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                  labels = trans_format("log10", math_format(10^.x)))+
    scale_fill_viridis_d()+
    labs(x = paste0("Impact ratio (", ifelse(outc=="dalys","DALYs",outc),")"), y = "Vaccine")
}

