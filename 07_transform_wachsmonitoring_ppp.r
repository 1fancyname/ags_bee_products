#07_transform_wachsmonitoring_ppp.r





# tbl_wm_ppp_pool ---------------------------------------------------------


base::print("% Transforming PPP wachsmonitoring pool results table to long format.")

tbl_wm_ppp_pool <- tbl_wm_ppp_pool %>% 
  tidyr::pivot_longer(
    cols = "Fludioxonil":"DEET", 
    names_to = "substance", 
    values_to = "concentration",
    values_drop_na = FALSE
  )



#replace all NA values with 0
tbl_wm_ppp_pool$concentration <- base::replace(tbl_wm_ppp_pool$concentration, base::is.na(tbl_wm_ppp_pool$concentration), 0)





# tbl_wm_ppp_yearly -------------------------------------------------------

base::print("% Transforming PPP wachsmonitoring yearly results table to long format.")

tbl_wm_ppp_yearly <- tbl_wm_ppp_yearly %>% 
  tidyr::pivot_longer(
    cols = "Fludioxonil":"Thymol", 
    names_to = "substance", 
    values_to = "concentration",
    values_drop_na = FALSE
  )



#replace all NA values with 0
tbl_wm_ppp_yearly$concentration <- base::replace(tbl_wm_ppp_yearly$concentration, base::is.na(tbl_wm_ppp_yearly$concentration), 0)







myvar.wm_processed <- TRUE
base::print("% processing of PPP Wachsmonitoring data successfull.")
