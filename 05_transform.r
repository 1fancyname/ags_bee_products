#05_transform.r



#stop user interface in case it is running
myvar.user_interface_state <- "off"


# change datatypes --------------------------------------------------------


base::print("% Changing datatypes in beebread data.")

#tbl_results_bb
tbl_results_bb$date_extr <- base::as.Date(tbl_results_bb$date_extr)
tbl_results_bb$date_analyse <- base::as.Date(tbl_results_bb$date_analyse)


#tbl_samples_bb
tbl_samples_bb$sample_date <- base::as.Date(tbl_samples_bb$sample_date, format="%d.%m.%Y")
tbl_samples_bb$colony <- base::as.character(tbl_samples_bb$colony)


base::print("% Changing datatypes in experiment data.")

#tbl_experiments
tbl_experiments$sampling_start_exp <- base::as.Date(tbl_experiments$sampling_start_exp, format="%d.%m.%Y")
tbl_experiments$sampling_end_exp <- base::as.Date(tbl_experiments$sampling_end_exp, format="%d.%m.%Y")
tbl_experiments$colony <- base::as.character(tbl_experiments$colony)


base::print("% Changing datatypes in pollen data.")

#tbl_results_p
tbl_results_p$date_extr <- base::as.Date(tbl_results_p$date_extr, format="%d.%m.%Y")
tbl_results_p$date_analyse <- base::as.Date(tbl_results_p$date_analyse, format="%d.%m.%Y")


#tbl_samples_p
tbl_samples_p$colony <- base::as.character(tbl_samples_p$colony)
tbl_samples_p$sample_date_start <- base::as.Date(tbl_samples_p$sample_date_start, format="%d.%m.%Y")
tbl_samples_p$sample_date_end <- base::as.Date(tbl_samples_p$sample_date_end, format="%d.%m.%Y")

base::print("% Changing datatypes in colony data.")

#tbl_colonies
tbl_colonies$col_number <- base::as.character(tbl_colonies$col_number)

base::print("% Changing datatypes in wax data.")

#tbl_samples_w
tbl_samples_w$sample_date <- base::as.Date(tbl_samples_w$sample_date, format="%d.%m.%Y")
tbl_samples_w$colony <- base::as.character(tbl_samples_w$colony)


#tbl_results_w
tbl_results_w$date_extr <- base::as.Date(tbl_results_w$date_extr, format="%d.%m.%Y")
tbl_results_w$date_analyse <- base::as.Date(tbl_results_w$date_analyse, format="%d.%m.%Y")

base::print("% Changing datatypes in apistrip data.")

#tbl_samples_a
tbl_samples_a$sample_date <- base::as.Date(tbl_samples_a$sample_date, format="%d.%m.%Y")
tbl_samples_a$colony <- base::as.character(tbl_samples_a$colony)


#tbl_results_a
tbl_results_a$date_extr <- base::as.Date(tbl_results_a$date_extr, format="%d.%m.%Y")
tbl_results_a$date_analyse <- base::as.Date(tbl_results_a$date_analyse, format="%d.%m.%Y")



# tbl_limits --------------------------------------------------------------




# tbl_results_bb ----------------------------------------------------------

base::print("% Joining beebread results with sample list.")
#join with different metadata tables
#tbl_results_bb
tbl_results_bb <- tbl_results_bb %>% 
  dplyr::inner_join(tbl_samples_bb, by = dplyr::join_by(fk_id_sam_bb == pk_id_sam_bb))

base::print("% Joining beebread results with location information.")

tbl_results_bb <- tbl_results_bb %>% 
  dplyr::inner_join(tbl_locations, by = dplyr::join_by(location == location_long))

base::print("% Joining beebread results with experiment information.")

#add information from tbl_experiments to tbl_results
tbl_results_bb <- myfun.add_experiment_to_results(fun_input_table = tbl_results_bb,
                                                  fun_date_column = tbl_results_bb$sample_date,
                                                  fun_matrix = "beebread")

base::print("% Transforming beebread results table to long format.")

#pivot longer
tbl_results_bb <- tbl_results_bb %>% 
  tidyr::pivot_longer(
    cols = "Thiamethoxam":"Flumethrin", 
    names_to = "substance", 
    values_to = "concentration",
    values_drop_na = FALSE
  )


#replace all NA values with 0
tbl_results_bb$concentration <- base::replace(tbl_results_bb$concentration, base::is.na(tbl_results_bb$concentration), 0)



#create new columns for lod and loq information
tbl_results_bb <- tibble::rowid_to_column(tbl_results_bb, "ID")
tbl_results_bb <- dplyr::mutate(tbl_results_bb, 
                                lod = NA,
                                loq = NA,
                                greater_than_lod = NA,
                                greater_than_loq = NA)

base::print("% Adding LOD and LOQ information to bebread results.")

#add lod and loq to tbl_results_bb by checking tbl_limits
myvar.unique_meth <- base::unique(tbl_results_bb$fk_method)
for (i in base::seq_along(myvar.unique_meth)) {
  tmp_tbl_res_meth <- dplyr::filter(tbl_results_bb, fk_method == myvar.unique_meth[i])
  for (j in 1:base::NROW(tmp_tbl_res_meth)) {
    myvar.cur_row <- tmp_tbl_res_meth$ID[j]
    myvar.cur_sub <- tmp_tbl_res_meth$substance[j]
    tmp_tbl_lim <- dplyr::filter(tbl_limits, fk_method_id == myvar.unique_meth[i], substance_name == myvar.cur_sub)
    tbl_results_bb$lod[myvar.cur_row] <- tmp_tbl_lim$lod[1]
    tbl_results_bb$loq[myvar.cur_row] <- tmp_tbl_lim$loq[1]
  }
}
rm(i, j)
rm(myvar.unique_meth,
   tmp_tbl_res_meth,
   myvar.cur_row,
   myvar.cur_sub,
   tmp_tbl_lim)



base::print("% Checking LOD for beebread results.")


#check if concentration is greater than LOD
tbl_results_bb <- myfun.check_lod(tbl_results_bb)


base::print("% Checking LOQ for beebread results.")

#check if concentration is greater than LOQ
tbl_results_bb <- myfun.check_loq(tbl_results_bb)



# tbl_avg_cum_bb --------------------------------------------------------------

base::print("% Creating new tibble for beebread mean and cumulative values.")


tbl_avg_cum_bb <- dplyr::tibble(id = NA,
                                matrix = NA,
                                location = NA,
                                year = NA,
                                sample_date = NA,
                                location_short = NA,
                                substance = NA,
                                lod = NA,
                                loq = NA,
                                concentration_avg = NA,
                                concentration_cum = NA)




#create table with average and cumulative values for each year && date && location && substance
myvar.unique_year <- base::unique(tbl_results_bb$year)
for (l in base::seq_along(myvar.unique_year)) {
  myvar.unique_dates_bb_results <- base::unique(tbl_results_bb$sample_date)
  for (i in base::seq_along(myvar.unique_dates_bb_results)) {
    tbl_tmp_results <- dplyr::filter(tbl_results_bb, sample_date == myvar.unique_dates_bb_results[i])
    myvar.unique_locations_bb_results <- base::unique(tbl_tmp_results$location)
    for (j in base::seq_along(myvar.unique_locations_bb_results)) {
      tbl_tmp_results_location <- dplyr::filter(tbl_tmp_results, location == myvar.unique_locations_bb_results[j])
      myvar.unique_substances_bb_results <- base::unique(tbl_tmp_results_location$substance)
      for (k in base::seq_along(myvar.unique_substances_bb_results)) {
        tbl_tmp_results_substances <- dplyr::filter(tbl_tmp_results_location, substance == myvar.unique_substances_bb_results[k])
        for (h in 1:base::NROW(tbl_tmp_results_substances)) {
          if (tbl_tmp_results_substances$concentration[h] < tbl_tmp_results_substances$loq[h]) {
            tbl_tmp_results_substances$concentration[h] = 0
          }
        }
        tbl_avg_cum_bb[nrow( tbl_avg_cum_bb) + 1,] = list(tbl_tmp_results_substances$fk_id_sam_bb[1],
                                                          tbl_tmp_results_substances$matrix[1],
                                                          tbl_tmp_results_substances$location[1],
                                                          tbl_tmp_results_substances$year[1],
                                                          tbl_tmp_results_substances$sample_date[1],
                                                          tbl_tmp_results_substances$location_short[1],
                                                          tbl_tmp_results_substances$substance[1],
                                                          tbl_tmp_results_substances$lod[1],
                                                          tbl_tmp_results_substances$loq[1],
                                                          mean(tbl_tmp_results_substances$concentration),
                                                          sum(tbl_tmp_results_substances$concentration))
      }
      
    }
  }
}

rm(i,j,h,l,k)
rm(myvar.unique_dates_bb_results,
   tbl_tmp_results,
   myvar.unique_locations_bb_results,
   tbl_tmp_results_location,
   myvar.unique_substances_bb_results,
   tbl_tmp_results_substances,
   myvar.unique_year)


#remove first observation
tbl_avg_cum_bb <- tbl_avg_cum_bb[-1,]



# tbl_results_p -----------------------------------------------------------

base::print("% Joining pollen results with sample list.")

#join with different metadata tables
tbl_results_p <- tbl_results_p %>% 
  dplyr::inner_join(tbl_samples_p, by = dplyr::join_by(fk_id_sam_p == pk_id_sam_p))

base::print("% Joining pollen results with location information.")

tbl_results_p <- tbl_results_p %>% 
  dplyr::inner_join(tbl_locations, by = dplyr::join_by(location == location_long))

base::print("% Joining pollen results with experiment information.")

#add information from tbl_experiments to tbl_results
tbl_results_p <- myfun.add_experiment_to_results(fun_input_table = tbl_results_p,
                                                 fun_date_column = tbl_results_p$sample_date_start,
                                                 fun_matrix = "pollen")


base::print("% Transforming pollen results table to long format.")

#pivot longer
tbl_results_p <- tbl_results_p %>% 
  tidyr::pivot_longer(
    cols = "Thiamethoxam":"Flumethrin", 
    names_to = "substance", 
    values_to = "concentration",
    values_drop_na = FALSE
  )


#replace all NA values with 0
tbl_results_p$concentration <- base::replace(tbl_results_p$concentration, is.na(tbl_results_p$concentration), 0)



#create new columns for lod and loq information
tbl_results_p <- tibble::rowid_to_column(tbl_results_p, "ID")
tbl_results_p <- dplyr::mutate(tbl_results_p, 
                               lod = NA,
                               loq = NA,
                               greater_than_lod = NA,
                               greater_than_loq = NA)

base::print("% Adding LOD and LOQ information to pollen results.")

#add lod and loq to tbl_results_bb by checking tbl_limits
myvar.unique_meth <- base::unique(tbl_results_p$fk_method)
for (i in base::seq_along(myvar.unique_meth)) {
  tmp_tbl_res_meth <- dplyr::filter(tbl_results_p, fk_method == myvar.unique_meth[i])
  for (j in 1:base::NROW(tmp_tbl_res_meth)) {
    myvar.cur_row <- tmp_tbl_res_meth$ID[j]
    myvar.cur_sub <- tmp_tbl_res_meth$substance[j]
    tmp_tbl_lim <- dplyr::filter(tbl_limits, fk_method_id == myvar.unique_meth[i], substance_name == myvar.cur_sub)
    tbl_results_p$lod[myvar.cur_row] <- tmp_tbl_lim$lod[1]
    tbl_results_p$loq[myvar.cur_row] <- tmp_tbl_lim$loq[1]
  }
}
rm(i, j)
rm(myvar.unique_meth,
   tmp_tbl_res_meth,
   myvar.cur_row,
   myvar.cur_sub,
   tmp_tbl_lim)


base::print("% Checking LOD for pollen results.")

#check if concentration is greater than lod

tbl_results_p <- myfun.check_lod(tbl_results_p)


base::print("% Checking LOQ for pollen results.")

#check if concentration is greater than loq

tbl_results_p <- myfun.check_loq(tbl_results_p)




# tbl_avg_cum_p -----------------------------------------------------------

base::print("% Creating new tibble for pollen mean and cumulative values.")

tbl_avg_cum_p <- dplyr::tibble(id = NA,
                               matrix = NA,
                               location = NA,
                               year = NA,
                               sample_date = NA,
                               location_short = NA,
                               substance = NA,
                               lod = NA,
                               loq = NA,
                               concentration_avg = NA,
                               concentration_cum = NA)




#create table with average and cumulative values for each year && date && location && substance
myvar.unique_year <- base::unique(tbl_results_p$year)
for (l in base::seq_along(myvar.unique_year)) {
  myvar.unique_dates_p_results <- base::unique(tbl_results_p$sample_date_start)
  for (i in base::seq_along(myvar.unique_dates_p_results)) {
    tbl_tmp_results <- dplyr::filter(tbl_results_p, sample_date_start == myvar.unique_dates_p_results[i])
    myvar.unique_locations_p_results <- base::unique(tbl_tmp_results$location)
    for (j in base::seq_along(myvar.unique_locations_p_results)) {
      tbl_tmp_results_location <- dplyr::filter(tbl_tmp_results, location == myvar.unique_locations_p_results[j])
      myvar.unique_substances_p_results <- base::unique(tbl_tmp_results_location$substance)
      for (k in base::seq_along(myvar.unique_substances_p_results)) {
        tbl_tmp_results_substances <- dplyr::filter(tbl_tmp_results_location, substance == myvar.unique_substances_p_results[k])
        for (h in 1:base::NROW(tbl_tmp_results_substances)) {
          if (tbl_tmp_results_substances$concentration[h] < tbl_tmp_results_substances$loq[h]) {
            tbl_tmp_results_substances$concentration[h] = 0
          }
        }
        tbl_avg_cum_p[nrow( tbl_avg_cum_p) + 1,] = list(tbl_tmp_results_substances$fk_id_sam_p[1],
                                                        tbl_tmp_results_substances$matrix[1],
                                                        tbl_tmp_results_substances$location[1],
                                                        tbl_tmp_results_substances$year[1],
                                                        tbl_tmp_results_substances$sample_date_start[1],
                                                        tbl_tmp_results_substances$location_short[1],
                                                        tbl_tmp_results_substances$substance[1],
                                                        tbl_tmp_results_substances$lod[1],
                                                        tbl_tmp_results_substances$loq[1],
                                                        mean(tbl_tmp_results_substances$concentration),
                                                        sum(tbl_tmp_results_substances$concentration))
      }
      
    }
  }
}

rm(i,j,h,l,k)
rm(myvar.unique_dates_p_results,
   tbl_tmp_results,
   myvar.unique_locations_p_results,
   tbl_tmp_results_location,
   myvar.unique_substances_p_results,
   tbl_tmp_results_substances,
   myvar.unique_year)


#remove first observation (NA values)
tbl_avg_cum_p <- tbl_avg_cum_p[-1,]







# tbl_results_w -----------------------------------------------------------

base::print("% Joining wax results with sample list.")

#join with different metadata tables
#tbl_results_bb
tbl_results_w <- tbl_results_w %>% 
  dplyr::inner_join(tbl_samples_w, by = dplyr::join_by(fk_id_sam_w == pk_id_sam_w))

base::print("% Joining wax results with location information.")

tbl_results_w <- tbl_results_w %>% 
  dplyr::inner_join(tbl_locations, by = dplyr::join_by(location == location_long))

base::print("% Joining wax results with experiment information.")

#add information from tbl_experiments to tbl_results
tbl_results_w <- myfun.add_experiment_to_results(fun_input_table = tbl_results_w,
                                                 fun_date_column = tbl_results_w$sample_date,
                                                 fun_matrix = "wax")

base::print("% Transforming wax results table to long format.")

#pivot longer
tbl_results_w <- tbl_results_w %>% 
  tidyr::pivot_longer(
    cols = "Thiamethoxam":"Flumethrin", 
    names_to = "substance", 
    values_to = "concentration",
    values_drop_na = FALSE
  )


#replace all NA values with 0
tbl_results_w$concentration <- base::replace(tbl_results_w$concentration, base::is.na(tbl_results_w$concentration), 0)



#create new columns for lod and loq information
tbl_results_w <- tibble::rowid_to_column(tbl_results_w, "ID")
tbl_results_w <- dplyr::mutate(tbl_results_w, 
                               lod = NA,
                               loq = NA,
                               greater_than_lod = NA,
                               greater_than_loq = NA)

base::print("% Adding LOD and LOQ information to wax results.")


#add lod and loq to tbl_results_bb by checking tbl_limits
myvar.unique_meth <- base::unique(tbl_results_w$fk_method)
for (i in base::seq_along(myvar.unique_meth)) {
  tmp_tbl_res_meth <- dplyr::filter(tbl_results_w, fk_method == myvar.unique_meth[i])
  for (j in 1:base::NROW(tmp_tbl_res_meth)) {
    myvar.cur_row <- tmp_tbl_res_meth$ID[j]
    myvar.cur_sub <- tmp_tbl_res_meth$substance[j]
    tmp_tbl_lim <- dplyr::filter(tbl_limits, fk_method_id == myvar.unique_meth[i], substance_name == myvar.cur_sub)
    tbl_results_w$lod[myvar.cur_row] <- tmp_tbl_lim$lod[1]
    tbl_results_w$loq[myvar.cur_row] <- tmp_tbl_lim$loq[1]
  }
}
rm(i, j)
rm(myvar.unique_meth,
   tmp_tbl_res_meth,
   myvar.cur_row,
   myvar.cur_sub,
   tmp_tbl_lim)

base::print("% Checking LOD for wax results.")

#check if concentration is greater than LOD
tbl_results_w <- myfun.check_lod(tbl_results_w)

base::print("% Checking LOQ for wax results.")

#check if concentration is greater than LOQ
tbl_results_w <- myfun.check_loq(tbl_results_w)







# tbl_avg_cum_w -----------------------------------------------------------

base::print("% Creating new tibble for wax mean and cumulative values.")

tbl_avg_cum_w <- dplyr::tibble(id = NA,
                               matrix = NA,
                               location = NA,
                               year = NA,
                               sample_date = NA,
                               location_short = NA,
                               substance = NA,
                               lod = NA,
                               loq = NA,
                               concentration_avg = NA,
                               concentration_cum = NA)




#create table with average and cumulative values for each year && date && location && substance
myvar.unique_year <- base::unique(tbl_results_w$year)
for (l in base::seq_along(myvar.unique_year)) {
  myvar.unique_dates_w_results <- base::unique(tbl_results_w$sample_date)
  for (i in base::seq_along(myvar.unique_dates_w_results)) {
    tbl_tmp_results <- dplyr::filter(tbl_results_w, sample_date == myvar.unique_dates_w_results[i])
    myvar.unique_locations_w_results <- base::unique(tbl_tmp_results$location)
    for (j in base::seq_along(myvar.unique_locations_w_results)) {
      tbl_tmp_results_location <- dplyr::filter(tbl_tmp_results, location == myvar.unique_locations_w_results[j])
      myvar.unique_substances_w_results <- base::unique(tbl_tmp_results_location$substance)
      for (k in base::seq_along(myvar.unique_substances_w_results)) {
        tbl_tmp_results_substances <- dplyr::filter(tbl_tmp_results_location, substance == myvar.unique_substances_w_results[k])
        for (h in 1:base::NROW(tbl_tmp_results_substances)) {
          if (tbl_tmp_results_substances$concentration[h] < tbl_tmp_results_substances$loq[h]) {
            tbl_tmp_results_substances$concentration[h] = 0
          }
        }
        tbl_avg_cum_w[nrow( tbl_avg_cum_w) + 1,] = list(tbl_tmp_results_substances$fk_id_sam_w[1],
                                                        tbl_tmp_results_substances$matrix[1],
                                                        tbl_tmp_results_substances$location[1],
                                                        tbl_tmp_results_substances$year[1],
                                                        tbl_tmp_results_substances$sample_date[1],
                                                        tbl_tmp_results_substances$location_short[1],
                                                        tbl_tmp_results_substances$substance[1],
                                                        tbl_tmp_results_substances$lod[1],
                                                        tbl_tmp_results_substances$loq[1],
                                                        mean(tbl_tmp_results_substances$concentration),
                                                        sum(tbl_tmp_results_substances$concentration))
      }
      
    }
  }
}

rm(i,j,h,l,k)
rm(myvar.unique_dates_w_results,
   tbl_tmp_results,
   myvar.unique_locations_w_results,
   tbl_tmp_results_location,
   myvar.unique_substances_w_results,
   tbl_tmp_results_substances,
   myvar.unique_year)


#remove first observation (NA Values)
tbl_avg_cum_w <- tbl_avg_cum_w[-1,]






# tbl_results_a -----------------------------------------------------------

base::print("% Joining apistrip results with sample list.")

tbl_results_a <- tbl_results_a %>% 
  dplyr::inner_join(tbl_samples_a, by = dplyr::join_by(fk_id_sam_a == pk_id_sam_a))

base::print("% Joining apistrip results with location information.")

tbl_results_a <- tbl_results_a %>% 
  dplyr::inner_join(tbl_locations, by = dplyr::join_by(location == location_long))

base::print("% Joining apistrip results with experiment information.")

#add information from tbl_experiments to tbl_results
tbl_results_a <- myfun.add_experiment_to_results(fun_input_table = tbl_results_a,
                                                 fun_date_column = tbl_results_a$sample_date,
                                                 fun_matrix = "apistrip")

base::print("% Transforming apistrip results table to long format.")

#pivot longer
tbl_results_a <- tbl_results_a %>% 
  tidyr::pivot_longer(
    cols = "Thiamethoxam":"Flumethrin", 
    names_to = "substance", 
    values_to = "concentration",
    values_drop_na = FALSE
  )


#replace all NA values with 0
tbl_results_a$concentration <- base::replace(tbl_results_a$concentration, base::is.na(tbl_results_a$concentration), 0)



#create new columns for lod and loq information
tbl_results_a <- tibble::rowid_to_column(tbl_results_a, "ID")
tbl_results_a <- dplyr::mutate(tbl_results_a, 
                               lod = NA,
                               loq = NA,
                               greater_than_lod = NA,
                               greater_than_loq = NA)

base::print("% Adding LOD and LOQ information to apistrip results.")

#add lod and loq to tbl_results_a by checking tbl_limits
myvar.unique_meth <- base::unique(tbl_results_a$fk_method)
for (i in base::seq_along(myvar.unique_meth)) {
  tmp_tbl_res_meth <- dplyr::filter(tbl_results_a, fk_method == myvar.unique_meth[i])
  for (j in 1:base::NROW(tmp_tbl_res_meth)) {
    myvar.cur_row <- tmp_tbl_res_meth$ID[j]
    myvar.cur_sub <- tmp_tbl_res_meth$substance[j]
    tmp_tbl_lim <- dplyr::filter(tbl_limits, fk_method_id == myvar.unique_meth[i], substance_name == myvar.cur_sub)
    tbl_results_a$lod[myvar.cur_row] <- tmp_tbl_lim$lod[1]
    tbl_results_a$loq[myvar.cur_row] <- tmp_tbl_lim$loq[1]
  }
}
rm(i, j)
rm(myvar.unique_meth,
   tmp_tbl_res_meth,
   myvar.cur_row,
   myvar.cur_sub,
   tmp_tbl_lim)


base::print("% Checking LOD for apistrip results.")

#check if concentration is greater than LOD
tbl_results_a <- myfun.check_lod(tbl_results_a)

base::print("% Checking LOQ for apistrip results.")

#check if concentration is greater than LOQ
tbl_results_a <- myfun.check_loq(tbl_results_a)




# tbl_avg_cum_a -----------------------------------------------------------

base::print("% Creating new tibble for apistrip mean and cumulative values.")

tbl_avg_cum_a <- dplyr::tibble(id = NA,
                               matrix = NA,
                               location = NA,
                               year = NA,
                               sample_date = NA,
                               location_short = NA,
                               substance = NA,
                               lod = NA,
                               loq = NA,
                               concentration_avg = NA,
                               concentration_cum = NA)



#create table with average and cumulative values for each year && date && location && substance
myvar.unique_year <- base::unique(tbl_results_a$year)
for (l in base::seq_along(myvar.unique_year)) {
  myvar.unique_dates_a_results <- base::unique(tbl_results_a$sample_date)
  for (i in base::seq_along(myvar.unique_dates_a_results)) {
    tbl_tmp_results <- dplyr::filter(tbl_results_a, sample_date == myvar.unique_dates_a_results[i])
    myvar.unique_locations_a_results <- base::unique(tbl_tmp_results$location)
    for (j in base::seq_along(myvar.unique_locations_a_results)) {
      tbl_tmp_results_location <- dplyr::filter(tbl_tmp_results, location == myvar.unique_locations_a_results[j])
      myvar.unique_substances_a_results <- base::unique(tbl_tmp_results_location$substance)
      for (k in base::seq_along(myvar.unique_substances_a_results)) {
        tbl_tmp_results_substances <- dplyr::filter(tbl_tmp_results_location, substance == myvar.unique_substances_a_results[k])
        for (h in 1:base::NROW(tbl_tmp_results_substances)) {
          if (tbl_tmp_results_substances$concentration[h] < tbl_tmp_results_substances$loq[h]) {
            tbl_tmp_results_substances$concentration[h] = 0
          }
        }
        tbl_avg_cum_a[nrow( tbl_avg_cum_a) + 1,] = list(tbl_tmp_results_substances$fk_id_sam_a[1],
                                                        tbl_tmp_results_substances$matrix[1],
                                                        tbl_tmp_results_substances$location[1],
                                                        tbl_tmp_results_substances$year[1],
                                                        tbl_tmp_results_substances$sample_date[1],
                                                        tbl_tmp_results_substances$location_short[1],
                                                        tbl_tmp_results_substances$substance[1],
                                                        tbl_tmp_results_substances$lod[1],
                                                        tbl_tmp_results_substances$loq[1],
                                                        mean(tbl_tmp_results_substances$concentration),
                                                        sum(tbl_tmp_results_substances$concentration))
      }
      
    }
  }
}

rm(i,j,h,l,k)
rm(myvar.unique_dates_a_results,
   tbl_tmp_results,
   myvar.unique_locations_a_results,
   tbl_tmp_results_location,
   myvar.unique_substances_a_results,
   tbl_tmp_results_substances,
   myvar.unique_year)


#remove first observation (NA values)

tbl_avg_cum_a <- tbl_avg_cum_a[-1,]


base::print("% Creating new tibble for beebread prevalence information.")

# tbl_prevalence_bb -------------------------------------------------------

tbl_prevalence_bb <- myfun.create_tbl_prevalence(input_table = tbl_results_bb,
                                                 output_table = tbl_prevalence_bb)


# tbl_percentage_bb -------------------------------------------------------

tbl_percentage_bb <- myfun.create_tbl_percentage(input_table = tbl_prevalence_bb,
                                                 output_table = tbl_percentage_bb)

base::print("% Creating new tibble for pollen prevalence information.")

# tbl_prevalence_p --------------------------------------------------------

tbl_prevalence_p <- myfun.create_tbl_prevalence(input_table = tbl_results_p,
                                                output_table = tbl_prevalence_p)


# tbl_percentage_p --------------------------------------------------------

tbl_percentage_p <- myfun.create_tbl_percentage(input_table = tbl_prevalence_p,
                                                output_table = tbl_percentage_p)

base::print("% Creating new tibble for wax prevalence information.")

# tbl_prevalence_w --------------------------------------------------------

tbl_prevalence_w <- myfun.create_tbl_prevalence(input_table = tbl_results_w,
                                                output_table = tbl_prevalence_w)


# tbl_percentage_w --------------------------------------------------------

tbl_percentage_w <- myfun.create_tbl_percentage(input_table = tbl_prevalence_w,
                                                output_table = tbl_percentage_w)

base::print("% Creating new tibble for apistrip prevalence information.")

# tbl_prevalence_a --------------------------------------------------------

tbl_prevalence_a <- myfun.create_tbl_prevalence(input_table = tbl_results_a,
                                                output_table = tbl_prevalence_a)


# tbl_percentage_a --------------------------------------------------------

tbl_percentage_a <- myfun.create_tbl_percentage(input_table = tbl_prevalence_a,
                                                output_table = tbl_percentage_a)


# create tbl_avg_cum_agr --------------------------------------------------

base::print("% Binding mean/cumulative tables of all matrixes.")

tbl_avg_cum_agr <- dplyr::bind_rows(tbl_avg_cum_bb, tbl_avg_cum_p, tbl_avg_cum_w, tbl_avg_cum_a)



# create tbl_matrix_diff ------------------------------------------------------------

base::print("% Creating new tibble for matrix information.")

#create tibble

tbl_matrix_diff <- dplyr::tibble(substance = NA,
                                 log_kow = NA,
                                 log_koa = NA,
                                 log_kaw = NA,
                                 location = NA,
                                 year = NA,
                                 bb_loq = NA,
                                 bb_n = NA,
                                 bb_max_concentration = NA,
                                 bb_mean_concentration = NA,
                                 p_loq = NA,
                                 p_n = NA,
                                 p_max_concentration = NA,
                                 p_mean_concentration = NA,
                                 w_loq = NA,
                                 w_n = NA,
                                 w_max_concentration = NA,
                                 w_mean_concentration = NA,
                                 a_loq = NA,
                                 a_n = NA,
                                 a_max_concentration = NA,
                                 a_mean_concentration = NA)


myvar.unique_year <- base::unique(tbl_experiments$year)
myvar.unique_substance <- base::unique(tbl_substances$substance_name)
myvar.unique_location <- base::unique(tbl_locations$location_short)


for (i in base::seq_along(myvar.unique_year)) {
  
  for (j in base::seq_along(myvar.unique_substance)) {
    
    for (h in seq_along(myvar.unique_location)) {
      tmp_tbl_avg_cum_bb <- dplyr::filter(tbl_avg_cum_bb, location_short == myvar.unique_location[h], year == myvar.unique_year[i], substance == myvar.unique_substance[j])
      tmp_tbl_avg_cum_p <- dplyr::filter(tbl_avg_cum_p, location_short == myvar.unique_location[h], year == myvar.unique_year[i], substance == myvar.unique_substance[j])
      tmp_tbl_avg_cum_w <- dplyr::filter(tbl_avg_cum_w, location_short == myvar.unique_location[h], year == myvar.unique_year[i], substance == myvar.unique_substance[j])
      tmp_tbl_avg_cum_a <- dplyr::filter(tbl_avg_cum_a, location_short == myvar.unique_location[h], year == myvar.unique_year[i], substance == myvar.unique_substance[j])
      tmp_tbl_prevalence_bb <- dplyr::filter(tbl_prevalence_bb, location_short == myvar.unique_location[h], year == myvar.unique_year[i], substance == myvar.unique_substance[j])
      tmp_tbl_prevalence_p <- dplyr::filter(tbl_prevalence_p, location_short == myvar.unique_location[h], year == myvar.unique_year[i], substance == myvar.unique_substance[j])
      tmp_tbl_prevalence_w <- dplyr::filter(tbl_prevalence_w, location_short == myvar.unique_location[h], year == myvar.unique_year[i], substance == myvar.unique_substance[j])
      tmp_tbl_prevalence_a <- dplyr::filter(tbl_prevalence_a, location_short == myvar.unique_location[h], year == myvar.unique_year[i], substance == myvar.unique_substance[j])
      tmp_tbl_results_bb <- dplyr::filter(tbl_results_bb, location_short == myvar.unique_location[h], year == myvar.unique_year[i], substance == myvar.unique_substance[j], greater_than_loq == TRUE)
      tmp_tbl_results_p <- dplyr::filter(tbl_results_p, location_short == myvar.unique_location[h], year == myvar.unique_year[i], substance == myvar.unique_substance[j], greater_than_loq == TRUE)
      tmp_tbl_results_w <- dplyr::filter(tbl_results_w, location_short == myvar.unique_location[h], year == myvar.unique_year[i], substance == myvar.unique_substance[j], greater_than_loq == TRUE)
      tmp_tbl_results_a <- dplyr::filter(tbl_results_a, location_short == myvar.unique_location[h], year == myvar.unique_year[i], substance == myvar.unique_substance[j], greater_than_loq == TRUE)
      tmp_tbl_results_lod_bb <- dplyr::filter(tbl_results_bb, location_short == myvar.unique_location[h], year == myvar.unique_year[i], substance == myvar.unique_substance[j])
      tmp_tbl_results_lod_p <- dplyr::filter(tbl_results_p, location_short == myvar.unique_location[h], year == myvar.unique_year[i], substance == myvar.unique_substance[j])
      tmp_tbl_results_lod_w <- dplyr::filter(tbl_results_w, location_short == myvar.unique_location[h], year == myvar.unique_year[i], substance == myvar.unique_substance[j])
      tmp_tbl_results_lod_a <- dplyr::filter(tbl_results_a, location_short == myvar.unique_location[h], year == myvar.unique_year[i], substance == myvar.unique_substance[j])
      tmp_tbl_substances <- dplyr::filter(tbl_substances, substance_name == myvar.unique_substance[j])
      tbl_matrix_diff[nrow(tbl_matrix_diff) + 1,] = list(myvar.unique_substance[j],
                                                         tmp_tbl_substances$log_kow[1],
                                                         tmp_tbl_substances$log_koa[1],
                                                         tmp_tbl_substances$log_kaw[1],
                                                         myvar.unique_location[h],
                                                         myvar.unique_year[i],
                                                         tmp_tbl_results_lod_bb$loq[1],
                                                         tmp_tbl_prevalence_bb$prevalence_gt_loq[1],
                                                         base::max(tmp_tbl_results_bb$concentration),
                                                         base::mean(tmp_tbl_results_bb$concentration),
                                                         tmp_tbl_results_lod_p$loq[1],
                                                         tmp_tbl_prevalence_p$prevalence_gt_loq[1],
                                                         base::max(tmp_tbl_results_p$concentration),
                                                         base::mean(tmp_tbl_results_p$concentration),
                                                         tmp_tbl_results_lod_w$loq[1],
                                                         tmp_tbl_prevalence_w$prevalence_gt_loq[1],
                                                         base::max(tmp_tbl_results_w$concentration),
                                                         base::mean(tmp_tbl_results_w$concentration),
                                                         tmp_tbl_results_lod_a$loq[1],
                                                         tmp_tbl_prevalence_a$prevalence_gt_loq[1],
                                                         base::max(tmp_tbl_results_a$concentration),
                                                         base::mean(tmp_tbl_results_a$concentration))
      
    }
  }
}
rm(i, j, h)
rm(myvar.unique_year,
   myvar.unique_substance,
   myvar.unique_location,
   tmp_tbl_avg_cum_bb,
   tmp_tbl_avg_cum_p,
   tmp_tbl_avg_cum_w,
   tmp_tbl_avg_cum_a,
   tmp_tbl_prevalence_bb,
   tmp_tbl_prevalence_p,
   tmp_tbl_prevalence_w,
   tmp_tbl_prevalence_a,
   tmp_tbl_results_bb,
   tmp_tbl_results_p,
   tmp_tbl_results_w,
   tmp_tbl_results_a,
   tmp_tbl_substances)


#remove first observation (NA Values)

tbl_matrix_diff <- tbl_matrix_diff[-1,]


base::print("% Transformation of project AP22 - 25 data completed")
