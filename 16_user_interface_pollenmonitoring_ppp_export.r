#16_user_interface_pollenmonitoring_ppp_export.r


myvar.user_interface_pm_ppp_export_state <- "on"

while (myvar.user_interface_pm_ppp_export_state != "off") {
  
  base::print("PPP Pollenmonitoring export")
  base::print("To stop the user interface, press 'esc'. To exit PPP Pollenmonitoring export, type 'exit'.")
  base::print("")
  base::print("")
  base::print("Would you like to create charts, or export files generated from the imported data?")
  base::print("Type ")
  base::print("1 to create charts")
  base::print("2 to export data")
  myvar.usr.chart_export <- base::as.character(base::readline("Enter here:")) 
  
  switch(myvar.usr.chart_export,
         "1" = {
           base::print("Which chart would you like to create? There is a comprehensive list of all types in the user manual.")
           myvar.usr.chart_type <- stringr::str_to_lower(base::as.character(base::readline("Enter chart-id here:")))
           switch(myvar.usr.chart_type,
                  "ch1" = {
                    base::print("For which year do you want to create the charts? Example: 2024")
                    myvar.usr_prevalence_year  <- base::as.numeric(base::readline("Enter here:"))
                    
                    myfun.pm_ppp_plot_prevalence(myvar.usr_prevalence_year)
                    base::print("DONE")
                    base::print(paste0("The files are located at: ./Grafik/PPP_Pollenmonitoring/Prevalence/",myvar.usr_prevalence_year,"/"))
                    base::print("%")
                    base::print("%")
                    base::print("%")
                    base::print("%")
                    

                  },
                  "ch2" = {
                    base::print("For which year do you want to create the charts? Example: 2024")
                    myvar.usr_prevalence_year  <- base::as.numeric(base::readline("Enter here:"))
                    
                    myfun.pm_ppp_plot_prevalence_ch(myvar.usr_prevalence_year)
                    base::print("DONE")
                    base::print(paste0("The files are located at: ./Grafik/PPP_Pollenmonitoring/Prevalence/",myvar.usr_prevalence_year,"/"))
                    base::print("%")
                    base::print("%")
                    base::print("%")
                    base::print("%")
                    
                  },
                  "ch3" = {
                    base::print("For which year do you want to createthe charts? Example: 2024")
                    myvar.usr_year  <- base::as.numeric(base::readline("Enter here:"))
                    
                    base::print("For which location do you want to create the charts? Example: BC2")
                    myvar.usr_results_location  <- stringr::str_to_upper(base::as.character(base::readline("Enter here:"))) 
                    
                    
                    base::print("Type 1 to plot values greater than LOD. Type 2 to plot values greater than LOQ")
                    myvar.usr_lo_filter  <- base::as.character(base::readline("Enter here:"))
                    
                    
                    switch(myvar.usr_lo_filter,
                           "1" = {
                             myfun.plot_pm_ppp_substance_gt_lod(fun_year = myvar.usr_year,
                                                            fun_location = myvar.usr_results_location)
                             base::print("DONE")
                             base::print(paste0("The files are located at: ./Grafik/PPP_Pollenmonitoring/Substances/", myvar.usr_year,"/", myvar.usr_results_location, "/greater_than_lod/"))
                             base::print("%")
                             base::print("%")
                             base::print("%")
                             base::print("%")
                           },
                           "2" = {
                             #call function for plotting graphs
                             myfun.plot_pm_ppp_substance_gt_loq(fun_year = myvar.usr_year,
                                                            fun_location = myvar.usr_results_location)
                             
                             base::print("DONE")
                             base::print(paste0("The files are located at: ./Grafik/PPP_Pollenmonitoring/Substances/", myvar.usr_year,"/", myvar.usr_results_location, "/greater_than_loq/"))
                             base::print("%")
                             base::print("%")
                             base::print("%")
                             base::print("%")
                           }
                    )
                  },
                  "ch4" = {
                    base::print("For which year do you want to create a plot? Example: 2024")
                    myvar.usr_results_year  <- base::as.numeric(base::readline("Enter here:"))
                    
                    
                    base::print("Choose which locations you want to compare. Example: BC2")
                    base::print("Leave prompt empty and hit enter if the desired locations have been chosen.")
                    myvar.usr_location1  <- stringr::str_to_upper(base::as.character(base::readline("Enter first location here:")))
                    myvar.usr_location2 <- stringr::str_to_upper(base::as.character(base::readline("Enter first location here:")))
                    myvar.usr_location3  <- stringr::str_to_upper(base::as.character(base::readline("Enter third location here:")))
                    myvar.usr_location4  <- stringr::str_to_upper(base::as.character(base::readline("Enter fourth location here:"))) 
                    myvar.usr_location5  <- stringr::str_to_upper(base::as.character(base::readline("Enter fifth location here:"))) 
                    myvar.usr_location <- unique(myvar.usr_location)
                  
                    
                    myfun.plot_pm_ppp_location_comp_def(myvar.usr_results_year,
                                                        fun_location1 = myvar.usr_location1,
                                                        fun_location2 = myvar.usr_location2,
                                                        fun_location3 = myvar.usr_location3,
                                                        fun_location4 = myvar.usr_location4,
                                                        fun_location5 = myvar.usr_location5)
                    
                    base::print("DONE")
                    base::print(paste0("The files are located at: ./Grafik/PPP_Pollenmonitoring/Location_Comparison/", myvar.usr_year))
                    base::print("%")
                    base::print("%")
                    base::print("%")
                    base::print("%")
                  },
                  "ch5" = {
                    base::print("For which year do you want to create a plot? Example: 2024")
                    myvar.usr_results_year  <- base::as.numeric(base::readline("Enter here:"))
                    
                    
                    base::print("Choose which locations you want to compare. Example: BC2")
                    base::print("Leave prompt empty and hit enter if the desired locations have been chosen.")
                    myvar.usr_location1  <- stringr::str_to_upper(base::as.character(base::readline("Enter first location here:")))
                    myvar.usr_location2 <- stringr::str_to_upper(base::as.character(base::readline("Enter first location here:")))
                    myvar.usr_location3  <- stringr::str_to_upper(base::as.character(base::readline("Enter third location here:")))
                    myvar.usr_location4  <- stringr::str_to_upper(base::as.character(base::readline("Enter fourth location here:"))) 
                    myvar.usr_location5  <- stringr::str_to_upper(base::as.character(base::readline("Enter fifth location here:"))) 
                    myvar.usr_location <- unique(myvar.usr_location)
                    base::print("Select the timeframe.")
                    myvar.usr_start_week <- as.numeric(base::readline("Enter first calendar week:"))
                    myvar.usr_end_week <- as.numeric(base::readline("Enter last calendar week:"))
                    
                    myfun.plot_pm_ppp_location_comp_time(myvar.usr_results_year,
                                                         fun_location1 = myvar.usr_location1,
                                                         fun_location2 = myvar.usr_location2,
                                                         fun_location3 = myvar.usr_location3,
                                                         fun_location4 = myvar.usr_location4,
                                                         fun_location5 = myvar.usr_location5,
                                                         fun_start_week = myvar.usr_start_week,
                                                         fun_end_week = myvar.usr_end_week)
                    
                    base::print("DONE")
                    base::print(paste0("The files are located at: ./Grafik/PPP_Pollenmonitoring/Location_Comparison_Custom/", myvar.usr_year))
                    base::print("%")
                    base::print("%")
                    base::print("%")
                    base::print("%")
                  },
                  "ch6" = {
                    base::print("For which year do you want to create a plot? Example: 2024")
                    myvar.usr_results_year  <- base::as.numeric(base::readline("Enter here:"))
                    
                    
                    base::print("Choose which substances you want to compare. Example: Azoxystrobin")
                    base::print("INFO: This input is case sensitive. Azoxystrobin is not the same as azoxystrobin.")
                    base::print("Leave prompt empty and hit enter if the desired substances have been chosen.")
                    myvar.usr_substance1  <- base::as.character(base::readline("Enter first substance here:"))
                    myvar.usr_substance2 <- base::as.character(base::readline("Enter first substance here:"))
                    myvar.usr_substance3  <- base::as.character(base::readline("Enter third substance here:"))
                    myvar.usr_substance4  <- base::as.character(base::readline("Enter fourth substance here:"))
                    myvar.usr_substance5  <- base::as.character(base::readline("Enter fifth substance here:"))

                    
                    myfun.plot_pm_ppp_box_substance(myvar.usr_results_year,
                                                    fun_sub1 = myvar.usr_substance1,
                                                    fun_sub2 = myvar.usr_substance2,
                                                    fun_sub3 = myvar.usr_substance3,
                                                    fun_sub4 = myvar.usr_substance4,
                                                    fun_sub5 = myvar.usr_substance5)
                    
                    base::print("DONE")
                    base::print(paste0("The files are located at: ./Grafik/PPP_Pollenmonitoring/Substance_Comparison/", myvar.usr_year))
                    base::print("%")
                    base::print("%")
                    base::print("%")
                    base::print("%")
                  },
                  "ch7" = {
                    base::print("For which year do you want to create a plot? Example: 2024")
                    myvar.usr_results_year  <- base::as.numeric(base::readline("Enter here:"))
                    
                    myfun.plot_pm_ppp_substance_trend_ch(myvar.usr_results_year)
                    base::print("DONE")
                    base::print(paste0("The files are located at: ./Grafik/PPP_Pollenmonitoring/Trent/", myvar.usr_year))
                    base::print("%")
                    base::print("%")
                    base::print("%")
                    base::print("%")
                  },
                  "ch8" = {
                    base::print("For which year do you want to create a plot? Example: 2024")
                    myvar.usr_results_year  <- base::as.numeric(base::readline("Enter here:"))
                    
                    myfun.plot_pm_ppp_trend_ch(myvar.usr_results_year)
                    base::print("DONE")
                    base::print(paste0("The files are located at: ./Grafik/PPP_Pollenmonitoring/Trent/", myvar.usr_year))
                    base::print("%")
                    base::print("%")
                    base::print("%")
                    base::print("%")
                  }
           )
         },
         "2" = {
           base::print("Which table would you like to export? There is a comprehensive list of all types in the user manual.")
           myvar.usr.table_type <- stringr::str_to_lower(base::as.character(base::readline("Enter table-id here:")))
           
           switch(myvar.usr.table_type,
                  "tbl1" = {
                    #results export
                    tbl_pm_ppp_results_export <- dplyr::select(tbl_pm_ppp_results, 
                                                       fk_id_p,
                                                       sample_date,
                                                       location_long,
                                                       year,
                                                       substance,
                                                       concentration) %>%
                    pivot_wider(names_from = substance, values_from = concentration)
                  
                  
                  readr::write_excel_csv(
                    tbl_pm_ppp_results_export,
                    base::paste0("./Export/PPP_Pollenmonitoring/tbl_results_pm_ppp.csv"),
                    delim = ";"
                  )
                  rm(tbl_pm_ppp_results_export)
                  base::print("DONE")
                  base::print(base::paste0("The file ist stored in ./Export/PPP_Pollenmonitoring/"))
                  
                  },
                  "tbl2" = {
                    #tbl_pm_ppp
                    readr::write_excel_csv(
                      tbl_pm_ppp,
                      base::paste0("./Export/PPP_Pollenmonitoring/tbl_pm_ppp.csv"),
                      delim = ";"
                    )
                    base::print("DONE")
                    base::print(base::paste0("The file ist stored in ./Export/PPP_Pollenmonitoring/"))
                    
                  },
                  "tbl3" = {
                    #tbl_pm_ppp_ch
                    readr::write_excel_csv(
                      tbl_pm_ppp_ch,
                      base::paste0("./Export/PPP_Pollenmonitoring/tbl_pm_ppp_ch.csv"),
                      delim = ";"
                    )
                    base::print("DONE")
                    base::print(base::paste0("The file ist stored in ./Export/PPP_Pollenmonitoring/"))
                    
                  },
                  "tbl4" = {
                    
                  },
                  "tbl5" = {
                    
                  }
           )
         },
         "exit" = {
           base::print("% Stopping user interface...")
           base::print("%")
           base::print("%")
           base::print("%")
           break
         }
  )
  
  
}



