fun_slice_data_viz <- function(data_viz_df_in, 
                               lowerbound = 1e-8,
                               outc = "dalys",
                               grps_in=grps_ls[[2]],
                               login = FALSE,
                               per = 1e3,
                               head_dir = "dataviz",
                               filter_out_malaria = TRUE){
  
  
  tmp <- data_viz_df_in %>% 
    mutate(new_var = !!as.name(paste0(outc, "_averted_rate", collapse=""))) %>%
    mutate(new_var = new_var*per)
  
  
  if("activity_type" %in% grps_in){
    tmp <- tmp %>% filter(activity_type!= "all")
  } else {
    tmp <- tmp %>% filter(activity_type== "all")
  }
  
  tmp <- tmp %>%   
    group_by(across(all_of(c(grps_in, "new_id")))) %>%
    summarise(new_var = weighted.mean(new_var,fvps, na.rm = TRUE)) 
  
  if(login)  tmp <- tmp %>% filter(new_var>lowerbound) %>%
    mutate(new_var = log10(new_var)) 
  
  tmp <- tmp %>%
    group_by(across(all_of(grps_in))) %>%
    mutate(new_cat = cut(new_var, 200 , dig.lab=4)) %>%
    group_by(across(all_of(c(grps_in, "new_cat")))) %>%
    summarise(Counts = n(), .groups="keep") %>% 
    rowwise() %>% mutate(new_cat = as.character(new_cat)) %>%
    mutate(lower_bound = as.numeric(gsub("\\(", "", strsplit(new_cat, ",")[[1]][1])),
           upper_bound = as.numeric(gsub("\\]", "", strsplit(new_cat, ",")[[1]][2]))) %>%
    ungroup() %>% select(-new_cat) %>%
    arrange(lower_bound)
  
  if(filter_out_malaria){
    if("country" %in% grps_in) tmp <- tmp %>% filter(disease != "Malaria")
  }
  
  oot_name <- paste0(head_dir,"/hist_counts_", outc, "_", paste0(grps_in, collapse="_"),ifelse(login, "_log", ""), ".csv")
  
  write.csv(tmp,oot_name, row.names = FALSE)
}

fun_slice_summary_table <- function(tmp, 
                                    outc = "dalys",
                                    grps_in=grps_ls[[8]],
                                    head_dir = "dataviz", 
                                    per=1e3,
                                    filter_out_malaria = TRUE){
  
  if(filter_out_malaria){
    if("country" %in% grps_in) tmp <- tmp %>% filter(disease != "Malaria")
  }
  
  if("activity_type" %in% grps_in){
    tmp <- tmp %>% filter(activity_type!= "all")
  } else {
    tmp <- tmp %>% filter(activity_type== "all")
  }
  
  tmp <- tmp %>%
    mutate(new_var = !!as.name(paste0(outc, "_averted_rate", collapse=""))*per) %>% 
    group_by(across(all_of(c(grps_in, "new_id")))) %>%
    summarise(new_var = weighted.mean(new_var,fvps, na.rm = TRUE)) %>%
    group_by(across(all_of(grps_in))) %>%
    summarise(mean_value =    round( mean(new_var, na.rm=TRUE), 6),
              lower_95 =   round(quantile(new_var, na.rm=TRUE, 0.025), 6),
              upper_95 =   round(quantile(new_var, na.rm=TRUE, 0.975), 6),
              median_value = round(median(new_var, na.rm=TRUE), 6))
  
  if(!filter_out_malaria){
    tmp <- tmp %>% filter(!vaccine %in% c("R21", "RTS,S")) %>%
                            bind_rows(tmp %>% filter( vaccine %in% c("R21", "RTS,S")) %>%
                                        group_by(across(all_of(grps_in[grps_in!="vaccine"])))%>%
                                        reframe(mean_value = mean(mean_value), 
                                                  lower_95 = mean(lower_95),
                                                  upper_95 = mean(upper_95),
                                                  median_value = mean(median_value)) %>%
                                        mutate(vaccine = "malaria"))
  }
  
  write.csv(tmp,
            paste0(head_dir,"/summary_table_", outc, "_", paste0(grps_in, collapse="_"), ".csv"), row.names = FALSE)
}
