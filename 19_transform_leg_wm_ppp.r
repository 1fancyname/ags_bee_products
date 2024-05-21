#19_transform_leg_wm_ppp.r


base::print("% Transforming legacy wachsmonitoring results table to long format.")

tbl_leg_wm_ppp_results %<>% 
  tidyr::pivot_longer(
    cols = "Acetamiprid":"Thymol", 
    names_to = "substance", 
    values_to = "concentration",
    values_drop_na = FALSE)


tbl_leg_wm_ppp_results$concentration <- base::replace(tbl_leg_wm_ppp_results$concentration, base::is.na(tbl_leg_wm_ppp_results$concentration), 0)

myvar.leg_wm_ppp_processed <- TRUE

base::print("% Transformation of legacy PPP Wachsmonitoring data completed")

