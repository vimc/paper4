# Function to create FVP maps
create_fvp_map <- function(disease_name) {
  
  fvp_data <- df4
  map_data <- world_map

  # Filter for disease name
  df_disease <- fvp_data %>% filter(disease == disease_name)
  
  # Append the filtered dataset to world map to keep all polygons
  df_map <- map_data %>% 
    left_join(df_disease, by = c("iso_3_code" = "iso_a3")) %>% 
    select(iso_3_code, disease, country_name, fvps_all, geometry) %>% 
    st_as_sf() 
  
  # Define range for disease-specific FVPs to define map colors
  fvps_range <- range(0, max(df_disease$fvps_all, na.rm = TRUE))

  # Define year range to show title esp for Cholera
  year_range <- ifelse(disease_name == "Cholera", "2000:2040", "2000:2030")
  
  # draw out map
  p <- ggplot(df_map) +
    geom_sf_who_poly(aes(fill = fvps_all)) +
    MetBrewer::scale_fill_met_c("Nizami",
      limits    = fvps_range
    ) +
    labs(
      title = paste("Fully Vaccinated Persons (FVPs) for", 
                    disease_name, "between",year_range),
      fill = "FVPs (millions)"
    ) +
    who_map_pipeline(no_annotation = TRUE, na_scale = FALSE, no_data_scale = FALSE ) +
    theme_minimal() +
    theme(
      plot.background = element_rect(fill = "white", colour = NA),
      axis.text = element_blank(),
    )
  p
}
