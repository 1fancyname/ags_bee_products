#06_transform_wachsmonitoring.r


#05_transform.r

base::print("% Transforming wachsmonitoring results table to long format.")

tbl_results_wm <- tbl_results_wm %>% 
  tidyr::pivot_longer(
    cols = "Acetamiprid":"Thymol", 
    names_to = "substance", 
    values_to = "concentration",
    values_drop_na = FALSE
  )



#replace all NA values with 0
tbl_results_wm$concentration <- base::replace(tbl_results_wm$concentration, base::is.na(tbl_results_wm$concentration), 0)

