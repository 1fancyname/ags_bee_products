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


#add information from tbl_pm_ppp_experiments to tbl_pm_ppp_results

tbl_pm_ppp_results <- myfun.pm_ppp_add_experiment_to_results(fun_input_table = tbl_pm_ppp_results,
                                                  fun_date_column = tbl_pm_ppp_results$sample_date)

#re-calculate week in tbl_pm_ppp from $sample date

tbl_pm_ppp_results <- tbl_pm_ppp_results %>%
  dplyr::mutate(week = lubridate::week(sample_date))



base::print("% Transforming beebread results table to long format.")

#pivot longer
tbl_pm_ppp_results <- tbl_pm_ppp_results %>% 
  tidyr::pivot_longer(
    cols = "Thiamethoxam":"Flumethrin", 
    names_to = "substance", 
    values_to = "concentration",
    values_drop_na = FALSE
  )

#replace all NA values with 0
tbl_pm_ppp_results$concentration <- base::replace(tbl_pm_ppp_results$concentration, base::is.na(tbl_pm_ppp_results$concentration), 0)



#create new columns for lod and loq information
tbl_pm_ppp_results <- tibble::rowid_to_column(tbl_pm_ppp_results, "ID")
tbl_pm_ppp_results <- dplyr::mutate(tbl_pm_ppp_results, 
                                lod = NA,
                                loq = NA,
                                greater_than_lod = NA,
                                greater_than_loq = NA)




base::print("% Adding LOD and LOQ information to pm ppp results.")

#add lod and loq to tbl_pm_ppp by checking tbl_pm_ppp_limits
myvar.unique_meth <- base::unique(tbl_pm_ppp_results$fk_method)
for (i in base::seq_along(myvar.unique_meth)) {
  tmp_tbl_res_meth <- dplyr::filter(tbl_pm_ppp_results, fk_method == myvar.unique_meth[i])
  for (j in 1:base::NROW(tmp_tbl_res_meth)) {
    myvar.cur_row <- tmp_tbl_res_meth$ID[j]
    myvar.cur_sub <- tmp_tbl_res_meth$substance[j]
    tmp_tbl_lim <- dplyr::filter(tbl_pm_ppp_limits, fk_method_id == myvar.unique_meth[i], substance_name == myvar.cur_sub)
    tbl_pm_ppp_results$lod[myvar.cur_row] <- tmp_tbl_lim$lod[1]
    tbl_pm_ppp_results$loq[myvar.cur_row] <- tmp_tbl_lim$loq[1]
  }
}
rm(i, j)
rm(myvar.unique_meth,
   tmp_tbl_res_meth,
   myvar.cur_row,
   myvar.cur_sub,
   tmp_tbl_lim)



base::print("% Checking LOD for pm ppp results.")


#check if concentration is greater than LOD
tbl_pm_ppp_results <- myfun.check_lod(tbl_pm_ppp_results)



base::print("% Checking LOQ for pm ppp results.")

#check if concentration is greater than LOQ
tbl_pm_ppp_results <- myfun.check_loq(tbl_pm_ppp_results)


base::print("% Creating new tibble for PPP Pollenmonitoring prevalence information.")

# tbl_pm_ppp_prevalence -------------------------------------------------------

tbl_pm_ppp_prevalence <- myfun.create_tbl_prevalence(input_table = tbl_pm_ppp_results,
                                                 output_table = tbl_pm_ppp_prevalence)



# tbl_pm_ppp_percentage -------------------------------------------------------

tbl_pm_ppp_percentage <- myfun.create_tbl_percentage(input_table = tbl_pm_ppp_prevalence,
                                                 output_table = tbl_pm_ppp_percentage)



# tbl_pm_ppp_prevalence_ch ------------------------------------------------



base::print("% Creating new tibble for PPP Pollenmonitoring prevalence CH information.")

tbl_pm_ppp_prevalence_ch <- myfun.create_tbl_prevalence_ch(input_table = tbl_pm_ppp_results,
                                                           output_table = tbl_pm_ppp_prevalence_ch)


# tbl_pm_ppp_percentage_ch -------------------------------------------------------

tbl_pm_ppp_percentage_ch <- myfun.create_tbl_percentage(input_table = tbl_pm_ppp_prevalence_ch,
                                                     output_table = tbl_pm_ppp_percentage_ch)




myvar.pm_ppp_processed <- TRUE

base::print("% Transformation of PPP Pollenmonitoring data completed")





#tbl_pm_ppp_samples



