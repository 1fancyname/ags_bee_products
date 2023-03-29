#08_user_interface.r

myvar.user_interface_state <- "on"

while (myvar.user_interface_state != "off") {
  base::print("Wellcome to the R script for plotting Data of the AP22 - 25 Project. To exit the user interface press esc.")
  base::print("")
  base::print("")
  base::print("Would you like to create charts with the imported data or export files generated from the imported data?")
  base::print("Type ")
  base::print("1 to create charts")
  base::print("2 to export files")
  myvar.usr.chart_export <- base::as.character(base::readline("Enter here:")) 
  
  switch(myvar.usr.chart_export,
         "1" = {
           base::print("Which chart would you like to create? There is a comprehensive list of all types in the user manual.")
           myvar.usr.chart_type <- stringr::str_to_lower(base::as.character(base::readline("Enter chart-id here:")))
           switch(myvar.usr.chart_type,
                  "ch1" = {
                    
                    # ch1 ---------------------------------------------------------------------
                    base::print("For which matrix do you want to create the charts? Example: BB")
                    myvar.usr_matrix <- stringr::str_to_upper(base::as.character(base::readline("Enter here:")))
                    
                    switch(myvar.usr_matrix,
                           "BB" = {
                             base::print("For which year do you want to create the charts? Example: 2022")
                             myvar.usr_prevalence_year  <- base::as.numeric(base::readline("Enter here:"))
                             
                             myfun.plot_prevalence_bb(myvar.usr_prevalence_year)
                             base::print("DONE")
                             base::print(paste0("The files are located at: ./Grafik/Beebread/Prevalence/",myvar.usr_prevalence_year,"/"))
                             base::print("%")
                             base::print("%")
                             base::print("%")
                             base::print("%")
                             
                           },
                           "P" = {
                             base::print("For which year do you want to create the charts? Example: 2022")
                             myvar.usr_prevalence_year  <- base::as.numeric(base::readline("Enter here:"))
                             
                             myfun.plot_prevalence_p(myvar.usr_prevalence_year)
                             base::print("DONE")
                             base::print(paste0("The files are located at: ./Grafik/Pollen/Prevalence/",myvar.usr_prevalence_year,"/"))
                             base::print("%")
                             base::print("%")
                             base::print("%")
                             base::print("%")
                           },
                           "W" = {
                             base::print("For which year do you want to create the charts? Example: 2022")
                             myvar.usr_prevalence_year  <- base::as.numeric(base::readline("Enter here:"))
                             
                             myfun.plot_prevalence_w(myvar.usr_prevalence_year)
                             base::print("DONE")
                             base::print(paste0("The files are located at: ./Grafik/Wax/Prevalence/",myvar.usr_prevalence_year,"/"))
                             base::print("%")
                             base::print("%")
                             base::print("%")
                             base::print("%")
                           },
                           "A_L2" = {
                             base::print("For which year do you want to create the charts? Example: 2022")
                             myvar.usr_prevalence_year  <- base::as.numeric(base::readline("Enter here:"))
                             
                             myfun.plot_prevalence_a(myvar.usr_prevalence_year)
                             base::print("DONE")
                             base::print(paste0("The files are located at: ./Grafik/Apistrip_L2/Prevalence/",myvar.usr_prevalence_year,"/"))
                             base::print("%")
                             base::print("%")
                             base::print("%")
                             base::print("%")
                           },
                           "A_L1" = {
                             base::print("For which year do you want to create the charts? Example: 2022")
                             myvar.usr_prevalence_year  <- base::as.numeric(base::readline("Enter here:"))
                             
                             myfun.plot_prevalence_a_sp(myvar.usr_prevalence_year)
                             base::print("DONE")
                             base::print(paste0("The files are located at: ./Grafik/Apistrip_L1/Prevalence/",myvar.usr_prevalence_year,"/"))
                             base::print("%")
                             base::print("%")
                             base::print("%")
                             base::print("%")
                           }
                    )
                  },
                  "ch2" = {
                    
                    # ch2 ---------------------------------------------------------------------
                    
                    base::print("For which matrix do you want to the charts? Example: BB")
                    myvar.usr_matrix <- stringr::str_to_upper(base::as.character(base::readline("Enter here:")))
                    
                    switch(myvar.usr_matrix,
                           "BB" = {
                             base::print("For which year do you want to createthe charts? Example: 2022")
                             myvar.usr_year  <- base::as.numeric(base::readline("Enter here:"))
                             
                             base::print("For which location do you want to create the charts? Example: BC2")
                             myvar.usr_results_location  <- stringr::str_to_upper(base::as.character(base::readline("Enter here:"))) 
                             
                             
                             base::print("Type 1 to plot values greater than LOD. Type 2 to plot values greater than LOQ")
                             myvar.usr_lo_filter  <- base::as.character(base::readline("Enter here:"))
                             
                             
                             switch(myvar.usr_lo_filter,
                                    "1" = {
                                      myfun.plot_substance_gt_lod_bb(fun_year = myvar.usr_year,
                                                                     fun_location = myvar.usr_results_location)
                                      base::print("DONE")
                                      base::print(paste0("The files are located at: ./Grafik/Beebread/Substances/", myvar.usr_year,"/", myvar.usr_results_location, "/greater_than_lod/"))
                                      base::print("%")
                                      base::print("%")
                                      base::print("%")
                                      base::print("%")
                                    },
                                    "2" = {
                                      #call function for plotting graphs
                                      myfun.plot_substance_gt_loq_bb(fun_year = myvar.usr_year,
                                                                     fun_location = myvar.usr_results_location)
                                      
                                      base::print("DONE")
                                      base::print(paste0("The files are located at: ./Grafik/Beebread/Substances/", myvar.usr_year,"/", myvar.usr_results_location, "/greater_than_loq/"))
                                      base::print("%")
                                      base::print("%")
                                      base::print("%")
                                      base::print("%")
                                    }
                             )
                           },
                           "P" = {
                             base::print("For which year do you want to create the charts? Example: 2022")
                             myvar.usr_results_year  <- base::as.numeric(base::readline("Enter here:"))
                             
                             base::print("For which location do you want to create the charts? Example: BC2")
                             myvar.usr_results_location  <- stringr::str_to_upper(base::as.character(base::readline("Enter here:"))) 
                             
                             base::print("Type 1 to plot values greater than LOD. Type 2 to plot values greater than LOQ")
                             myvar.usr_lo_filter  <- base::as.character(base::readline("Enter here:"))
                             
                             
                             switch(myvar.usr_lo_filter,
                                    "1" = {
                                      myfun.plot_substance_gt_lod_p(fun_year = myvar.usr_results_year,
                                                                    fun_location = myvar.usr_results_location)
                                      base::print("DONE")
                                      base::print(paste0("The files are located at: ./Grafik/Pollen/Substances/", myvar.usr_results_year,"/", myvar.usr_results_location, "/greater_than_lod/"))
                                      base::print("%")
                                      base::print("%")
                                      base::print("%")
                                      base::print("%")
                                    },
                                    "2" = {
                                      #call function for plotting graphs
                                      myfun.plot_substance_gt_loq_p(fun_year = myvar.usr_results_year,
                                                                    fun_location = myvar.usr_results_location)
                                      
                                      base::print("DONE")
                                      base::print(paste0("The files are located at: ./Grafik/Pollen/Substances/", myvar.usr_results_year,"/", myvar.usr_results_location, "/greater_than_loq/"))
                                      base::print("%")
                                      base::print("%")
                                      base::print("%")
                                      base::print("%")
                                    }
                             )
                             
                             
                           },
                           "W" = {
                             base::print("For which year do you want to create the charts? Example: 2022")
                             myvar.usr_results_year  <- base::as.numeric(base::readline("Enter here:"))
                             
                             base::print("For which location do you want to create the charts? Example: BC2")
                             myvar.usr_results_location  <- stringr::str_to_upper(base::as.character(base::readline("Enter here:"))) 
                             
                             base::print("Type 1 to plot values greater than LOD. Type 2 to plot values greater than LOQ")
                             myvar.usr_lo_filter  <- base::as.character(base::readline("Enter here:"))
                             
                             
                             switch(myvar.usr_lo_filter,
                                    "1" = {
                                      myfun.plot_substance_gt_lod_w(fun_year = myvar.usr_results_year,
                                                                    fun_location = myvar.usr_results_location)
                                      base::print("DONE")
                                      base::print(paste0("The files are located at: ./Grafik/Wax/Substances/", myvar.usr_results_year,"/", myvar.usr_results_location, "/greater_than_lod/"))
                                      base::print("%")
                                      base::print("%")
                                      base::print("%")
                                      base::print("%")
                                    },
                                    "2" = {
                                      #call function for plotting graphs
                                      myfun.plot_substance_gt_loq_w(fun_year = myvar.usr_results_year,
                                                                    fun_location = myvar.usr_results_location)
                                      
                                      base::print("DONE")
                                      base::print(paste0("The files are located at: ./Grafik/Wax/Substances/", myvar.usr_results_year,"/", myvar.usr_results_location, "/greater_than_loq/"))
                                      base::print("%")
                                      base::print("%")
                                      base::print("%")
                                      base::print("%")
                                    }
                             )
                           },
                           "A_L2" = {
                             base::print("For which year do you want to create the charts? Example: 2022")
                             myvar.usr_results_year  <- base::as.numeric(base::readline("Enter here:"))
                             
                             base::print("For which location do you want to create the charts? Example: BC2")
                             myvar.usr_results_location  <- stringr::str_to_upper(base::as.character(base::readline("Enter here:"))) 
                             
                             myfun.plot_substance_gt_lod_a(fun_year = myvar.usr_results_year,
                                                           fun_location = myvar.usr_results_location)
                             base::print("DONE")
                             base::print(paste0("The files are located at: ./Grafik/Apistrip/Substances/", myvar.usr_results_year,"/", myvar.usr_results_location, "/greater_than_lod/"))
                             base::print("%")
                             base::print("%")
                             base::print("%")
                             base::print("%")
                             
                           },
                           "A_L1" = {
                             base::print("For which year do you want to create the charts? Example: 2022")
                             myvar.usr_results_year  <- base::as.numeric(base::readline("Enter here:"))
                             
                             base::print("For which location do you want to create the charts? Example: BC2")
                             myvar.usr_results_location  <- stringr::str_to_upper(base::as.character(base::readline("Enter here:"))) 
                             
                             #call function for plotting graphs
                             myfun.plot_substance_gt_loq_a_sp(fun_year = myvar.usr_results_year,
                                                           fun_location = myvar.usr_results_location)
                             
                             base::print("DONE")
                             base::print(paste0("The files are located at: ./Grafik/Apistrip_Spain/Substances/", myvar.usr_results_year,"/", myvar.usr_results_location, "/greater_than_loq/"))
                             base::print("%")
                             base::print("%")
                             base::print("%")
                             base::print("%")
                             
                             
                           }
                    )
                  },
                  "ch3" = {
                    
                    # ch3 ---------------------------------------------------------------------
                    
                    base::print("For which matrix do you want to create the charts? Example: bb")
                    myvar.usr_matrix <- stringr::str_to_upper(base::as.character(base::readline("Enter here:")))
                    
                    switch(myvar.usr_matrix,
                           "BB" = {
                             base::print("For which year do you want to create a plot? Example: 2022")
                             myvar.usr_results_year  <- base::as.numeric(base::readline("Enter here:"))
                             
                             
                             myfun.plot_avg_bb(fun_year = myvar.usr_results_year)
                             
                             base::print("DONE")
                             base::print(paste0("The files are located at: ./Grafik/Beebread/Average/",myvar.usr_results_year,"/"))
                             base::print("%")
                             base::print("%")
                             base::print("%")
                             base::print("%")
                           },
                           "P" = {
                             base::print("For which year do you want to create a plot? Example: 2022")
                             myvar.usr_results_year  <- base::as.numeric(base::readline("Enter here:"))
                             
                             myfun.plot_avg_p(myvar.usr_results_year)
                             
                             base::print("DONE")
                             base::print(paste0("The files are located at: ./Grafik/Pollen/Average/",myvar.usr_results_year,"/"))
                             base::print("%")
                             base::print("%")
                             base::print("%")
                             base::print("%")
                           },
                           "W" = {
                             base::print("For which year do you want to create a plot? Example: 2022")
                             myvar.usr_results_year  <- base::as.numeric(base::readline("Enter here:"))
                             
                             myfun.plot_avg_w(myvar.usr_results_year)
                             
                             base::print("DONE")
                             base::print(paste0("The files are located at: ./Grafik/Wax/Average/",myvar.usr_results_year,"/"))
                             base::print("%")
                             base::print("%")
                             base::print("%")
                             base::print("%")
                           },
                           "A_L2" = {
                             base::print("For which year do you want to create a plot? Example: 2022")
                             myvar.usr_results_year  <- base::as.numeric(base::readline("Enter here:"))
                             
                             myfun.plot_avg_a(myvar.usr_results_year)
                             
                             base::print("DONE")
                             base::print(paste0("The files are located at: ./Grafik/Apistrip/Average/",myvar.usr_results_year,"/"))
                             base::print("%")
                             base::print("%")
                             base::print("%")
                             base::print("%")
                           },
                           "A_L1" = {
                             base::print("For which year do you want to create a plot? Example: 2022")
                             myvar.usr_results_year  <- base::as.numeric(base::readline("Enter here:"))
                             
                             myfun.plot_avg_a_sp(myvar.usr_results_year)
                             
                             base::print("DONE")
                             base::print(paste0("The files are located at: ./Grafik/Apistrip_Spain/Average/",myvar.usr_results_year,"/"))
                             base::print("%")
                             base::print("%")
                             base::print("%")
                             base::print("%")
                           }
                    )
                  },
                  "ch4" = {
                    
                    # ch4 ---------------------------------------------------------------------
                    
                    base::print("For which matrix do you want to create a graph? Example: BB")
                    myvar.usr_matrix <- stringr::str_to_upper(base::as.character(base::readline("Enter here:")))
                    
                    switch(myvar.usr_matrix,
                           "BB" = {
                             base::print("For which year do you want to create a plot? Example: 2022")
                             myvar.usr_results_year  <- base::as.numeric(base::readline("Enter here:"))
                             
                             myfun.plot_cum_bb(myvar.usr_results_year)
                             
                             base::print("DONE")
                             base::print(paste0("The files are located at: ./Grafik/Beebread/Cumulative/",myvar.usr_results_year,"/"))
                             base::print("%")
                             base::print("%")
                             base::print("%")
                             base::print("%")
                           },
                           "P" = {
                             base::print("For which year do you want to create a plot? Example: 2022")
                             myvar.usr_results_year  <- base::as.numeric(base::readline("Enter here:"))
                             
                             myfun.plot_cum_p(myvar.usr_results_year)
                             
                             base::print("DONE")
                             base::print(paste0("The files are located at: ./Grafik/Pollen/Cumulative/",myvar.usr_results_year,"/"))
                             base::print("%")
                             base::print("%")
                             base::print("%")
                             base::print("%")
                           },
                           "W" = {
                             base::print("For which year do you want to create a plot? Example: 2022")
                             myvar.usr_results_year  <- base::as.numeric(base::readline("Enter here:"))
                             
                             myfun.plot_cum_w(myvar.usr_results_year)
                             
                             base::print("DONE")
                             base::print(paste0("The files are located at: ./Grafik/Wax/Cumulative/",myvar.usr_results_year,"/"))
                             base::print("%")
                             base::print("%")
                             base::print("%")
                             base::print("%")
                           },
                           "A_L2" = {
                             base::print("For which year do you want to create a plot? Example: 2022")
                             myvar.usr_results_year  <- base::as.numeric(base::readline("Enter here:"))
                             
                             myfun.plot_cum_a(myvar.usr_results_year)
                             
                             base::print("DONE")
                             base::print(paste0("The files are located at: ./Grafik/Apistrip/Cumulative/",myvar.usr_results_year,"/"))
                             base::print("%")
                             base::print("%")
                             base::print("%")
                             base::print("%")
                           },
                           "A_L1" = {
                             base::print("For which year do you want to create a plot? Example: 2022")
                             myvar.usr_results_year  <- base::as.numeric(base::readline("Enter here:"))
                             
                             myfun.plot_cum_a_sp(myvar.usr_results_year)
                             
                             base::print("DONE")
                             base::print(paste0("The files are located at: ./Grafik/Apistrip_Spain/Cumulative/",myvar.usr_results_year,"/"))
                             base::print("%")
                             base::print("%")
                             base::print("%")
                             base::print("%")
                           }
                    )
                  },
                  "ch5" = {
                    
                    # ch5 ---------------------------------------------------------------------
                    
                    base::print("For which year do you want to create the charts? Example: 2022")
                    myvar.usr_results_year  <- base::as.numeric(base::readline("Enter here:"))
                    
                    base::print("For which location do you want to create the charts? Example: BC2")
                    myvar.usr_results_location  <- stringr::str_to_upper(base::as.character(base::readline("Enter here:"))) 
                    
                    base::print("Choose which matrixes you want to compare. Example: BB")
                    base::print("Leave prompt empty and hit enter if the desired matrixes have been chosen.")
                    
                    myvar.usr_matrix[1]  <- as.vector(stringr::str_to_upper(base::as.character(base::readline("Enter first matrix here:")))) 
                    myvar.usr_matrix[2]  <- as.vector(stringr::str_to_upper(base::as.character(base::readline("Enter second matrix here:")))) 
                    myvar.usr_matrix[3]  <- as.vector(stringr::str_to_upper(base::as.character(base::readline("Enter third matrix here:")))) 
                    myvar.usr_matrix <- unique(myvar.usr_matrix)
                    myvar.usr_matrix <- str_replace_all(myvar.usr_matrix, "A_L2", "")
                    myvar.usr_matrix <- str_replace_all(myvar.usr_matrix, "A_L1", "")
                    
                    myfun.plot_matrix_avg_comparison(fun_year = myvar.usr_results_year,
                                                     fun_location = myvar.usr_results_location,
                                                     fun_matrix1 = myvar.usr_matrix[1],
                                                     fun_matrix2 = myvar.usr_matrix[2],
                                                     fun_matrix3 = myvar.usr_matrix[3])
                    
                    base::print("DONE")
                    base::print(paste0("The files are located at: ./Grafik/Matrix_Comparison/Mean/",myvar.usr_results_year,"/", myvar.usr_results_location, "/"))
                    base::print("%")
                    base::print("%")
                    base::print("%")
                    base::print("%")
                  },
                  "ch6" = {
                    # ch6 ---------------------------------------------------------------------
                    
                    base::print("For which year do you want to create the charts? Example: 2022")
                    myvar.usr_results_year  <- base::as.numeric(base::readline("Enter here:"))
                    
                    base::print("For which location do you want to create the charts? Example: BC2")
                    myvar.usr_results_location  <- stringr::str_to_upper(base::as.character(base::readline("Enter here:"))) 
                    
                    
                    myfun.plot_matrix_avg_comparison_a(fun_year = myvar.usr_results_year,
                                                     fun_location = myvar.usr_results_location)
                    
                    base::print("DONE")
                    base::print(paste0("The files are located at: ./Grafik/Matrix_Comparison/Mean/",myvar.usr_results_year,"/", myvar.usr_results_location, "/"))
                    base::print("%")
                    base::print("%")
                    base::print("%")
                    base::print("%")
                  },
                  "ch7" = {
                    
                    base::print("For what period of time do you want to create the charts?")
                    myvar.usr_results_year1  <- base::as.numeric(base::readline("Enter first year here:"))
                    myvar.usr_results_year2  <- base::as.numeric(base::readline("Enter last year here:"))
                    
                    
                    myfun.create_standard_substance_plot_wm(fun_year_start = myvar.usr_results_year1,
                                                            fun_year_end = myvar.usr_results_year2)
                    
                    base::print("DONE")
                    base::print(paste0("The files are located at: ./Grafik/Wachsmonitoring/Substances/"))
                    base::print("%")
                    base::print("%")
                    base::print("%")
                    base::print("%")
                    
                  },
                  "ch8" = {
                    
                    base::print("Choose which substances you want to include in the chart. Example: Acetamiprid")
                    base::print("Leave prompt empty and hit enter if the desired substances have been chosen.")
                    
                    
                    myvar.usr_substance[1]  <- as.vector(base::as.character(base::readline("Enter first matrix here:")))
                    myvar.usr_substance[2]  <- as.vector(base::as.character(base::readline("Enter second matrix here:"))) 
                    myvar.usr_substance[3]  <- as.vector(base::as.character(base::readline("Enter third matrix here:")))
                    myvar.usr_substance[4]  <- as.vector(base::as.character(base::readline("Enter fourth matrix here:"))) 
                    myvar.usr_substance[5]  <- as.vector(base::as.character(base::readline("Enter fifth matrix here:"))) 
                    myvar.usr_substance <- unique(myvar.usr_substance)
                    
                    
                    
                    base::print("For what period of time do you want to create the charts?")
                    myvar.usr_results_year1  <- base::as.numeric(base::readline("Enter first year here:"))
                    myvar.usr_results_year2  <- base::as.numeric(base::readline("Enter last year here:"))
                    
                    
                    myfun.create_sub_comparison_chart_wm(fun_sub1 = myvar.usr_substance[1],
                                                     fun_sub2 = myvar.usr_substance[2],
                                                     fun_sub3 = myvar.usr_substance[3],
                                                     fun_sub4 = myvar.usr_substance[4],
                                                     fun_sub5 = myvar.usr_substance[5],
                                                     fun_year_start = myvar.usr_results_year1,
                                                     fun_year_end = myvar.usr_results_year2)
                    
                    base::print("DONE")
                    base::print(paste0("The files are located at: ./Grafik/Wachsmonitoring/Substance_Comparison/"))
                    base::print("%")
                    base::print("%")
                    base::print("%")
                    base::print("%")
                    
                  },
                  "ch9" = {
                    
                  },
                  "ch10" = {
                    
                  },
                  "ch11" = {
                    
                  },
                  "ch12" = {
                    
                  }
           )
         },
         "2" = {
           base::print("Which table would you like to export? There is a comprehensive list of all types in the user manual.")
           myvar.usr.table_type <- stringr::str_to_lower(base::as.character(base::readline("Enter table-id here:")))
           
           switch(myvar.usr.table_type,
                  "tbl1" = {
                    
                    # tbl1 --------------------------------------------------------------------
                    
                    base::print("For which matrix would you like to export the data? Example: BB")
                    myvar.usr_matrix <- stringr::str_to_upper(base::as.character(base::readline("Enter here:")))
                    switch(myvar.usr_matrix,
                           "BB" = {
                             tbl_tmp_results_bb <- dplyr::select(tbl_results_bb, 
                                                                 fk_id_sam_bb,
                                                                 colony,
                                                                 matrix,
                                                                 sample_date,
                                                                 location,
                                                                 year,
                                                                 substance,
                                                                 concentration,
                                                                 lod,
                                                                 loq) %>%
                               pivot_wider(names_from = substance, values_from = concentration)
                               
                             
                             readr::write_excel_csv(
                               tbl_tmp_results_bb,
                               base::paste0("tbl_results_", myvar.usr_matrix, ".csv"),
                               delim = ";"
                             )
                             rm(tbl_tmp_results_bb)
                             base::print("DONE")
                             base::print(base::paste0("The file ist stored in the working directory."))
                             
                           },
                           "P" = {
                             readr::write_excel_csv(
                               dplyr::select(tbl_results_p, 
                                             fk_id_sam_p,
                                             colony,
                                             matrix,
                                             sample_date_start,
                                             sample_date_end,
                                             location,
                                             year,
                                             substance,
                                             concentration,
                                             lod,
                                             loq),
                               base::paste0("tbl_results_", myvar.usr_matrix, ".csv"),
                               delim = ";"
                             )
                             base::print("DONE")
                             base::print(base::paste0("The file ist stored in the working directory."))
                             
                           },
                           "W" = {
                             readr::write_excel_csv(
                               dplyr::select(tbl_results_w, 
                                             fk_id_sam_w,
                                             colony,
                                             matrix,
                                             sample_date,
                                             location,
                                             year,
                                             substance,
                                             concentration,
                                             lod,
                                             loq),
                               base::paste0("tbl_results_", myvar.usr_matrix, ".csv"),
                               delim = ";"
                             )
                             base::print("DONE")
                             base::print(base::paste0("The file ist stored in the working directory."))
                             
                           },
                           "A_L2" = {
                             readr::write_excel_csv(
                               dplyr::select(tbl_results_a, 
                                             fk_id_sam_a,
                                             colony,
                                             matrix,
                                             sample_date,
                                             location,
                                             year,
                                             substance,
                                             concentration,
                                             lod,
                                             loq),
                               base::paste0("tbl_results_", myvar.usr_matrix, ".csv"),
                               delim = ";"
                             )
                             base::print("DONE")
                             base::print(base::paste0("The file ist stored in the working directory."))
                             
                           },
                           "A_L1" = {
                             readr::write_excel_csv(
                               dplyr::select(tbl_results_a_sp, 
                                             fk_id_sam_a_sp,
                                             colony,
                                             matrix,
                                             sample_date,
                                             location,
                                             year,
                                             substance,
                                             concentration,
                                             lod,
                                             loq),
                               base::paste0("tbl_results_", myvar.usr_matrix, ".csv"),
                               delim = ";"
                             )
                             base::print("DONE")
                             base::print(base::paste0("The file ist stored in the working directory."))
                             
                           }
                    )
                  },
                  "tbl2" = {
                    
                    # tbl2 --------------------------------------------------------------------
                    
                    base::print("For which year do you want export the table? Example: 2022")
                    myvar.usr_results_year  <- base::as.numeric(base::readline("Enter here:"))
                    
                    myfun.export_tbl_matrix_diff(myvar.usr_results_year)
                    base::print("DONE")
                    base::print(base::paste0("The file ist stored inthe working directory"))
                    
                  },
                  "tbl3" = {
                    
                  },
                  "tbl4" = {
                    
                  },
                  "tbl5" = {
                    
                  }
           )
           
         }
  )
}



switch(variable,
       "case1" = {
         
       },
       "case2" = {
         
       },
       "case3" = {
         
       }
)









