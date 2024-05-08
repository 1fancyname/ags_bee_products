#12_transform_pollenmonitoring_ppp.r



# change datatypes --------------------------------------------------------


# tbl_pm_ppp_samples ------------------------------------------------------

tbl_pm_ppp_samples$sample_date <- base::as.Date(tbl_pm_ppp_samples$sample_date, format = "%d.%m.%Y")



#tbl_pm_ppp_colonies
tbl_pm_ppp_colonies$ags_col_number <- base::as.character(tbl_pm_ppp_colonies$ags_col_number)


#tbl_pm_ppp_experiments
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

#add information from tbl_pm_ppp_substances
tbl_pm_ppp_results <- tbl_pm_ppp_results %>% 
  dplyr::inner_join(tbl_pm_ppp_substances, by = dplyr::join_by(substance == substance_name))


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




# create table without missing samples ------------------------------------
#This table MUST NOT be used for anything else other than for ch6

tbl_pm_ppp_nms <- tbl_pm_ppp_results



  myvar.unique_exp <- unique(tbl_pm_ppp_nms$pk_id_exp)
  for (j in seq_along(myvar.unique_exp)) {
    tbl_tmp <- dplyr::filter(tbl_pm_ppp_nms, pk_id_exp == myvar.unique_exp[j])
    myvar.tmp_min_week <- tbl_tmp %$% min(week)
    myvar.tmp_max_week <- tbl_tmp %$% max(week)
    myvar.tmp_weeks <- myvar.tmp_min_week:myvar.tmp_max_week
    for (k in seq_along(myvar.tmp_weeks)) {
      tbl_tmp %<>% dplyr::filter(week == myvar.tmp_weeks[k])
      if (nrow(tbl_tmp) > 0) {
        
      } else {
        tbl_tmp_info <- dplyr::filter(tbl_pm_ppp_nms, pk_id_exp == myvar.unique_exp[j])
        tbl_pm_ppp_nms %<>% add_row(tbl_tmp_info[1,])
        tbl_pm_ppp_nms$week[nrow(tbl_pm_ppp_nms)] <- myvar.tmp_weeks[k]
        tbl_pm_ppp_nms$concentration[nrow(tbl_pm_ppp_nms)] <- 0.0000000000001
      }
    }
  }

rm(j, k)
rm(myvar.unique_exp,
   myvar.tmp_min_week, myvar.tmp_max_week, myvar.tmp_weeks,
   tbl_tmp_info)



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



# create tbl_pm_ppp ------------------------------------------------------------

base::print("% Creating new tibble tbl_pm_ppp.")



tbl_pm_ppp <- dplyr::tibble(substance = NA,
                                 class = NA,
                                 location = NA,
                                 year = NA,
                                 lod = NA,
                                 loq = NA,
                                 n = NA,
                                 prct_gt_lod_lt_loq = NA,
                                 prct_gt_loq = NA,
                                 max_concentration = NA,
                                 max_date = NA,
                                 mean_concentration = NA)



myvar.unique_substances <- unique(tbl_pm_ppp_results$substance)
for (i in seq_along(myvar.unique_substances)) {
  tbl_tmp_sub <- dplyr::filter(tbl_pm_ppp_results, substance == myvar.unique_substances[i])
  myvar.unique_years <- unique(tbl_tmp_sub$year)
  for (j in seq_along(myvar.unique_years)) {
    tbl_tmp_year <- dplyr::filter(tbl_tmp_sub, year == myvar.unique_years[j])
    myvar.unique_locations <- unique(tbl_tmp_year$location_short)
    for (h in seq_along(myvar.unique_locations)) {
      tbl_tmp_location <- dplyr::filter(tbl_tmp_year, location_short == myvar.unique_locations[h])
      tbl_tmp_prevalence <- dplyr::filter(tbl_pm_ppp_prevalence, substance == myvar.unique_substances[i], year == myvar.unique_years[j], location_short == myvar.unique_locations[h])
      myvar.max_conc <- max(tbl_tmp_location$concentration)
      tbl_tmp_max_conc <- dplyr::filter(tbl_tmp_location, concentration == myvar.max_conc)
      tbl_tmp_mean <- dplyr::filter(tbl_tmp_location, greater_than_loq == TRUE)
      myvar.mean_conc <-  mean(tbl_tmp_mean$concentration)
      
      
      tbl_pm_ppp[nrow(tbl_pm_ppp) + 1,] = list(myvar.unique_substances[i],
                                               tbl_tmp_location$class[1],
                                               myvar.unique_locations[h],
                                               myvar.unique_years[j],
                                               tbl_tmp_location$lod[1],
                                               tbl_tmp_location$loq[1],
                                               tbl_tmp_prevalence$n_samples[1],
                                               tbl_tmp_prevalence$prct_gt_lod_lt_loq[1],
                                               tbl_tmp_prevalence$prct_gt_loq[1],
                                               myvar.max_conc,
                                               tbl_tmp_max_conc$sample_date[1],
                                               myvar.mean_conc)
      
    }
  }
}


rm(h, i, j)
rm(myvar.unique_substances, 
   tbl_tmp_sub,
   myvar.unique_years,
   tbl_tmp_year,
   myvar.unique_locations,
   tbl_tmp_location,
   tbl_tmp_prevalence,
   myvar.max_conc,
   tbl_tmp_max_conc,
   tbl_tmp_mean,
   myvar.mean_conc)


tbl_pm_ppp <- tbl_pm_ppp[-1,]






# tbl_pm_ppp_ch --------------------------------------------------------------

base::print("% Creating new tibble tbl_pm_ppp_ch.")

tbl_pm_ppp_ch <- dplyr::tibble(substance = NA,
                            class = NA,
                            year = NA,
                            lod = NA,
                            loq = NA,
                            n = NA,
                            prct_gt_lod_lt_loq = NA,
                            prct_gt_loq = NA,
                            max_concentration = NA,
                            max_greater_lod = NA,
                            max_greater_loq = NA,
                            max_date = NA,
                            max_location = NA,
                            min_concentration = NA,
                            min_greater_lod = NA,
                            min_greater_loq = NA,
                            mean_concentration = NA,
                            median_concentration = NA,
                            sd_concentration = NA)


myvar.unique_substances <- unique(tbl_pm_ppp_results$substance)
for (i in seq_along(myvar.unique_substances)) {
  tbl_tmp_sub <- dplyr::filter(tbl_pm_ppp_results, substance == myvar.unique_substances[i])
  myvar.unique_years <- unique(tbl_tmp_sub$year)
  for (j in seq_along(myvar.unique_years)) {
    tbl_tmp_year <- dplyr::filter(tbl_tmp_sub, year == myvar.unique_years[j])
    tbl_tmp_year_loq <- dplyr::filter(tbl_tmp_year, greater_than_loq == TRUE)
    tbl_tmp_prevalence_ch <- dplyr::filter(tbl_pm_ppp_prevalence_ch, year == myvar.unique_years[j], substance == myvar.unique_substances[i])
    myvar.max_conc <- max(tbl_tmp_year$concentration)
    tbl_tmp_max_conc <- dplyr::filter(tbl_tmp_year, concentration == myvar.max_conc)
    tbl_tmp_non_zero <- dplyr::filter(tbl_tmp_year, concentration != 0)
    myvar.min_conc <- min(tbl_tmp_non_zero$concentration)
    tbl_tmp_min_conc <- dplyr::filter(tbl_tmp_year, concentration == myvar.min_conc)
    myvar.mean_conc <- mean(tbl_tmp_year_loq$concentration)
    myvar.median_conc <- median(tbl_tmp_year_loq$concentration)
    myvar.sd_conc <- sd(tbl_tmp_year_loq$concentration)
    
    tbl_pm_ppp_ch[nrow(tbl_pm_ppp_ch) + 1,] = list(myvar.unique_substances[i],
                                                   tbl_tmp_year$class[1],
                                                   myvar.unique_years[j],
                                                   tbl_tmp_year$lod[1],
                                                   tbl_tmp_year$loq[1],
                                                   tbl_tmp_prevalence_ch$n_samples[1],
                                                   tbl_tmp_prevalence_ch$prct_gt_lod_lt_loq[1],
                                                   tbl_tmp_prevalence_ch$prct_gt_loq[1],
                                                   myvar.max_conc,
                                                   tbl_tmp_max_conc$greater_than_lod[1],
                                                   tbl_tmp_max_conc$greater_than_lod[1],
                                                   tbl_tmp_max_conc$sample_date[1],
                                                   tbl_tmp_max_conc$location_short[1],
                                                   myvar.min_conc,
                                                   tbl_tmp_min_conc$greater_than_lod[1],
                                                   tbl_tmp_min_conc$greater_than_lod[1],
                                                   myvar.mean_conc,
                                                   myvar.median_conc,
                                                   myvar.sd_conc)
    
  }
}
rm(i, j)
rm(myvar.unique_substances,
   tbl_tmp_sub,
   myvar.unique_years,
   tbl_tmp_year,
   tbl_tmp_year_loq,
   tbl_tmp_prevalence_ch,
   myvar.max_conc,
   tbl_tmp_max_conc,
   myvar.min_conc,
   myvar.mean_conc,
   myvar.median_conc,
   myvar.sd_conc)


tbl_pm_ppp_ch <- tbl_pm_ppp_ch[-1,]



myvar.pm_ppp_processed <- TRUE

base::print("% Transformation of PPP Pollenmonitoring data completed")





