#12_transform_pollenmonitoring_ppp.r



# change datatypes --------------------------------------------------------


#tbl_pm_ppp_colonies
tbl_pm_ppp_colonies$ags_col_number <- base::as.character(tbl_pm_ppp_colonies$ags_col_number)


#tbl_pm_ppp_experiments
tbl_pm_ppp_experiments$sampling_start_exp <- base::as.Date(tbl_pm_ppp_experiments$sampling_start_exp, format = "%d.%m.%Y")
tbl_pm_ppp_experiments$sampling_end_exp <- base::as.Date(tbl_pm_ppp_experiments$sampling_end_exp, format = "%d.%m.%Y")
tbl_pm_ppp_experiments$colony_1 <- base::as.character(tbl_pm_ppp_experiments$colony_1)
tbl_pm_ppp_experiments$colony_2 <- base::as.character(tbl_pm_ppp_experiments$colony_2)
tbl_pm_ppp_experiments$colony_3 <- base::as.character(tbl_pm_ppp_experiments$colony_3)
tbl_pm_ppp_experiments$colony_4 <- base::as.character(tbl_pm_ppp_experiments$colony_4)



# tbl_pm_ppp_results ------------------------------------------------------



base::print("% Joining PPP Pollenmonitoring results with sample list.")
#join with different metadata tables
tbl_pm_ppp_results <- tbl_pm_ppp_results %>% 
  dplyr::inner_join(tbl_pm_ppp_samples, by = dplyr::join_by(fk_id_p == pk_id_p))

base::print("% Joining PPP Pollenmonitoring results with location information.")

tbl_pm_ppp_results <- tbl_pm_ppp_results %>% 
  dplyr::inner_join(tbl_pm_ppp_locations, by = dplyr::join_by(location_short == location_short))



#tbl_pm_ppp_samples



