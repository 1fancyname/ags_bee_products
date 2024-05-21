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
    cols = "Fludioxonil":"DEET", 
    names_to = "substance", 
    values_to = "concentration",
    values_drop_na = FALSE
  )



#replace all NA values with 0
tbl_wm_ppp_yearly$concentration <- base::replace(tbl_wm_ppp_yearly$concentration, base::is.na(tbl_wm_ppp_yearly$concentration), 0)





# tbl_wm_ppp_export -------------------------------------------------------



tbl_wm_ppp_export <- dplyr::tibble(year = NA,
                                   substance = NA,
                                   min = NA,
                                   mean = NA,
                                   median = NA,
                                   max = NA,
                                   max_company = NA)


myvar.tmp_unique_year <- unique(tbl_wm_ppp_pool$year)

for (i in seq_along(myvar.tmp_unique_year)) {
  tbl_tmp_year <- dplyr::filter(tbl_wm_ppp_pool, year == myvar.tmp_unique_year[i])
  myvar.tmp_unique_sub <- unique(tbl_tmp_year$substance)
  for (j in seq_along(myvar.tmp_unique_sub)) {
    tbl_tmp_sub <- dplyr::filter(tbl_tmp_year, substance == myvar.tmp_unique_sub[j])
    if (max(tbl_tmp_sub$concentration) > 0) {
      tbl_tmp_max_conc <- dplyr::filter(tbl_tmp_sub, concentration == max(tbl_tmp_sub$concentration))
      myvar.tmp_max_company <- tbl_tmp_max_conc$wax_man[1]
    } else {
      myvar.tmp_max_company <- "None"
    }
    tbl_wm_ppp_export[nrow( tbl_wm_ppp_export) + 1,] = list(tbl_tmp_sub$year[1],
                                                            tbl_tmp_sub$substance[1],
                                                            min(tbl_tmp_sub$concentration),
                                                            mean(tbl_tmp_sub$concentration),
                                                            median(tbl_tmp_sub$concentration),
                                                            max(tbl_tmp_sub$concentration),
                                                            myvar.tmp_max_company)
  }
  
  
}
rm(i, j)
rm(myvar.tmp_unique_year, myvar.tmp_unique_sub, myvar.tmp_max_company,
   tbl_tmp_year, tbl_tmp_sub)


tbl_wm_ppp_export <- tbl_wm_ppp_export[-1,]


myvar.wm_processed <- TRUE
base::print("% processing of PPP Wachsmonitoring data successfull.")
