#all fun 

lowerbound <- 1e-8
per <- 1e3

fun_dot_whisker <- function(dfin, outc="deaths", cols=disease_colours, highlight = TRUE) {
  if(highlight){
    dfin %>% 
      group_by(disease) %>% 
      mutate(mean_outc = weighted.mean(!!as.name(paste0(outc,"_averted_rate")),fvps, na.rm=TRUE)) %>% ungroup() %>%
      filter(!!as.name(paste0(outc,"_averted_rate"))>lowerbound) %>% ungroup() %>%
      
      ggplot()+
      aes(y=reorder(disease,mean_outc), 
          x=!!as.name(paste0(outc,"_averted_rate")),fill=disease, colour=disease)+
      geom_violin()+
      scale_colour_manual(values = cols, aesthetics = c("fill", "colour"))+
      theme_ridges()+
      scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                    labels = trans_format("log10", math_format(10^.x)))+
      gghighlight::gghighlight(disease %in% c("COVID", "Malaria"))+
      labs(x = paste0("Impact ratio (", ifelse(outc=="dalys","DALYs",outc),")"), y = "Disease")+
      theme(legend.position = "none")   
  } else {
    dfin %>% 
      group_by(vaccine) %>% mutate(mean_outc = weighted.mean(!!as.name(paste0(outc,"_averted_rate")),fvps, na.rm=TRUE)) %>%
      filter(!!as.name(paste0(outc,"_averted_rate"))>lowerbound) %>% ungroup() %>%
      ggplot()+
      aes(y=reorder(disease, mean_outc), 
          x=!!as.name(paste0(outc,"_averted_rate")),fill=disease, colour=disease)+
      geom_violin()+
      scale_colour_manual(values = cols, aesthetics = c("fill", "colour"))+
      theme_ridges()+
      scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                    labels = trans_format("log10", math_format(10^.x)))+
      labs(x = paste0("Impact ratio (", ifelse(outc=="dalys","DALYs",outc),")"), y = "Disease")+
      theme(legend.position = "none")
  }
  
} #not used

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

plot_density_vaccine <- function(df2_mod_ave, outc="deaths"){
  p <- lapply(unique(df2_mod_ave$activity_type),
              FUN = function(a) df2_mod_ave %>% filter(activity_type==a) %>%
                group_by(subregion, vaccine, run_id, disease, activity_type) %>%
                summarise(outc_averted_rate = weighted.mean(!!as.name(paste0(outc,"_averted_rate")),fvps,na.rm=TRUE)) %>%
                #filter(outc_averted_rate >lowerbound) %>%
                
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
                group_by(subregion, vaccine, run_id, disease, activity_type) %>%
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

plot_modelling_group_variation <- function(df2, df3, outc="deaths"){ #note this is internal so I have left all the uncertainty in
  
  df2 %>% left_join(df3, by = join_by(modelling_group, vaccine)) %>% 
    #filter(!disease %in% c("MenA", "COVID"), !!as.name(paste0(outc,"_averted_rate"))>lowerbound) %>%
    
    group_by(vaccine) %>% mutate(mean_outc = weighted.mean(!!as.name(paste0(outc,"_averted_rate")),fvps, na.rm=TRUE)) %>%
    filter(!!as.name(paste0(outc,"_averted_rate"))>lowerbound) %>%
    ggplot()+
    aes(fill = as.character(mod_num), x = !!as.name(paste0(outc,"_averted_rate")), y=reorder(vaccine, mean_outc))+
    geom_density_ridges(alpha=0.5, stat="binline", bins=200, draw_baseline=FALSE)+
    facet_grid(.~activity_type, scales="free")+
    theme_bw()+
    theme(legend.position = "none", axis.text.x = element_text(angle=90, hjust=1))+
    scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                  labels = trans_format("log10", math_format(10^.x)))+
    scale_fill_viridis_d()+
    labs(x = paste0("Impact ratio (", ifelse(outc=="dalys","DALYs",outc),")"), y = "Vaccine")
}

fun_get_pair_rank <- function(tmp, combo){
  comb <- as.vector(combo)
  tmp2 <- tmp %>% 
    filter(disease %in% comb) %>%
    pivot_wider(names_from = disease, values_from = dar)
  tmp2 <- tmp2[,c("run_id", comb)]
  sum(tmp2[,2]>tmp2[,3])/nrow(tmp2)
}

plot_pairwise_rank <- function(tmp){
  combos <- t(combn(sort(unique(tmp$disease)), 2))
  
  oot <- sapply(1:nrow(combos), function(x) fun_get_pair_rank(tmp, combos[x,]))
  
  df_oot <- data.frame(disease1 = combos[,1], disease2 =combos[,2], outp = oot)
  
  df_oot %>% bind_rows(df_oot %>% rename(disease1=disease2, disease2=disease1) %>% mutate(outp=1-outp)) %>%
    ggplot()+
    aes(x = disease1, y=disease2, fill=outp*100)+
    geom_tile()+
    theme_minimal()+
    labs(x = "Disease 1", y= "Disease 2", 
         fill = "Percentage of samples \nwhere Disease 1 impact ratio > Disease 2 impact ratio")+
    theme(legend.position = "bottom")+
    MetBrewer::scale_fill_met_c("Johnson")
}
