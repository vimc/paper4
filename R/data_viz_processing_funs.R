fun_slice_data_viz <- function(data_viz_df_in, 
                               lowerbound = 1e-8,
                               outc = "dalys",
                               grps_in=grps_ls[[2]],
                               login = FALSE,
                               per = 1e3,
                               head_dir = "dataviz"){
  
  
  tmp <- data_viz_df_in %>% 
    filter(!!as.name(paste0(outc, "_averted_rate", collapse=""))>lowerbound) %>%
    mutate(new_var = !!as.name(paste0(outc, "_averted_rate", collapse=""))) %>%
    mutate(new_var = new_var*per)
  
  if(login)  tmp <- tmp %>% 
      mutate(new_var = log10(new_var)) 
  
  tmp <- tmp %>%   
    group_by(across(all_of(c(grps_in, "new_id")))) %>%
    summarise(new_var = weighted.mean(new_var,fvps, na.rm = TRUE)) %>%
    group_by(across(all_of(grps_in))) %>%
    mutate(new_cat = cut(new_var, 200 , dig.lab=4)) %>%
    group_by(across(all_of(c(grps_in, "new_cat")))) %>%
    summarise(Counts = n(), .groups="keep") %>% 
    rowwise() %>% mutate(new_cat = as.character(new_cat)) %>%
    mutate(lower_bound = as.numeric(gsub("\\(", "", strsplit(new_cat, ",")[[1]][1])),
           upper_bound = as.numeric(gsub("\\]", "", strsplit(new_cat, ",")[[1]][2]))) %>%
    ungroup() %>% select(-new_cat)
  
  if("country" %in% grps_in) tmp <- tmp %>% filter(disease != "Malaria")
  
  if("activity_type" %in% grps_in)tmp <- tmp %>% filter(activity_type!= "all")
  
  oot_name <- paste0(head_dir,"/hist_counts_", outc, "_", paste0(grps_in, collapse="_"),ifelse(login, "_log", ""), ".csv")
  
  write.csv(tmp,oot_name, row.names = FALSE)
}

fun_slice_summary_table <- function(tmp, 
                                    outc = "dalys",
                                    grps_in=grps_ls[[8]],
                                    head_dir = "dataviz", 
                                    per=1e3){
  
  
  if("country" %in% grps_in) tmp <- tmp %>% filter(disease != "Malaria")
  
  tmp <- tmp %>%
    mutate(new_var = !!as.name(paste0(outc, "_averted_rate", collapse=""))*per) %>% #need to fix the output for meningitis
    group_by(across(all_of(c(grps_in, "new_id")))) %>%
    summarise(new_var = weighted.mean(new_var,fvps, na.rm = TRUE)) %>%
    group_by(across(all_of(grps_in))) %>%
    summarise(mean_value =    round( mean(new_var, na.rm=TRUE), 6),
              lower_95 =   round(quantile(new_var, na.rm=TRUE, 0.025), 6),
              upper_95 =   round(quantile(new_var, na.rm=TRUE, 0.975), 6),
              median_value = round(median(new_var, na.rm=TRUE), 6))
  
  if("activity_type" %in% grps_in)tmp <- tmp %>% filter(activity_type!= "all")
  
  write.csv(tmp,
            paste0(head_dir,"/summary_table_", outc, "_", paste0(grps_in, collapse="_"), ".csv"), row.names = FALSE)
}
