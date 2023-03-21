#06_user_interface.r

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
                    base::print("For which matrix do you want to create the charts? Example: bb")
                    myvar.usr_matrix <- stringr::str_to_lower(base::as.character(base::readline("Enter here:")))
                    
                    switch(myvar.usr_matrix,
                           "bb" = {
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
                           "p" = {
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
                           "w" = {
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
                           "a" = {
                             base::print("For which year do you want to create the charts? Example: 2022")
                             myvar.usr_prevalence_year  <- base::as.numeric(base::readline("Enter here:"))
                             
                             myfun.plot_prevalence_a(myvar.usr_prevalence_year)
                             base::print("DONE")
                             base::print(paste0("The files are located at: ./Grafik/Apistrip/Prevalence/",myvar.usr_prevalence_year,"/"))
                             base::print("%")
                             base::print("%")
                             base::print("%")
                             base::print("%")
                           }
                    )
                  },
                  "ch2" = {

# ch2 ---------------------------------------------------------------------

                    base::print("For which matrix do you want to the charts? Example: bb")
                    myvar.usr_matrix <- stringr::str_to_lower(base::as.character(base::readline("Enter here:")))
                    
                    switch(myvar.usr_matrix,
                           "bb" = {
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
                           "p" = {
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
                           "w" = {
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
                           "a" = {
                             base::print("For which year do you want to create the charts? Example: 2022")
                             myvar.usr_results_year  <- base::as.numeric(base::readline("Enter here:"))
                             
                             base::print("For which location do you want to create the charts? Example: BC2")
                             myvar.usr_results_location  <- stringr::str_to_upper(base::as.character(base::readline("Enter here:"))) 
                             
                             base::print("Type 1 to plot values greater than LOD. Type 2 to plot values greater than LOQ")
                             myvar.usr_lo_filter  <- base::as.character(base::readline("Enter here:"))
                             
                             
                             switch(myvar.usr_lo_filter,
                                    "1" = {
                                      myfun.plot_substance_gt_lod_a(fun_year = myvar.usr_results_year,
                                                                    fun_location = myvar.usr_results_location)
                                      base::print("DONE")
                                      base::print(paste0("The files are located at: ./Grafik/Apistrip/Substances/", myvar.usr_results_year,"/", myvar.usr_results_location, "/greater_than_lod/"))
                                      base::print("%")
                                      base::print("%")
                                      base::print("%")
                                      base::print("%")
                                    },
                                    "2" = {
                                      #call function for plotting graphs
                                      myfun.plot_substance_gt_loq_a(fun_year = myvar.usr_results_year,
                                                                    fun_location = myvar.usr_results_location)
                                      
                                      base::print("DONE")
                                      base::print(paste0("The files are located at: ./Grafik/Apistrip/Substances/", myvar.usr_results_year,"/", myvar.usr_results_location, "/greater_than_loq/"))
                                      base::print("%")
                                      base::print("%")
                                      base::print("%")
                                      base::print("%")
                                    }
                             )
                           }
                    )
                  },
                  "ch3" = {

# ch3 ---------------------------------------------------------------------

                    base::print("For which matrix do you want to create the charts? Example: bb")
                    myvar.usr_matrix <- stringr::str_to_lower(base::as.character(base::readline("Enter here:")))
                    
                    switch(myvar.usr_matrix,
                           "bb" = {
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
                           "p" = {
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
                           "w" = {
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
                           "a"= {
                             base::print("For which year do you want to create a plot? Example: 2022")
                             myvar.usr_results_year  <- base::as.numeric(base::readline("Enter here:"))
                             
                             myfun.plot_avg_a(myvar.usr_results_year)
                             
                             base::print("DONE")
                             base::print(paste0("The files are located at: ./Grafik/Apistrip/Average/",myvar.usr_results_year,"/"))
                             base::print("%")
                             base::print("%")
                             base::print("%")
                             base::print("%")
                           }
                    )
                  },
                  "ch4" = {

# ch4 ---------------------------------------------------------------------

                    base::print("For which matrix do you want to create a graph? Example: bb")
                    myvar.usr_matrix <- stringr::str_to_lower(base::as.character(base::readline("Enter here:")))
                    
                    switch(myvar.usr_matrix,
                           "bb" = {
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
                           "p" = {
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
                           "w" = {
                             base::print("For which year do you want to create a plot? Example: 2022")
                             myvar.usr_results_year  <- base::as.numeric(base::readline("Enter here:"))
                             
                             myfun.plot_cum_w(myvar.usr_results_year)
                             
                             base::print("DONE")
                             base::print(paste0("The files are located at: ./Grafik/Wax/Cumulytive/",myvar.usr_results_year,"/"))
                             base::print("%")
                             base::print("%")
                             base::print("%")
                             base::print("%")
                           },
                           "a" = {
                             base::print("For which year do you want to create a plot? Example: 2022")
                             myvar.usr_results_year  <- base::as.numeric(base::readline("Enter here:"))
                             
                             myfun.plot_cum_a(myvar.usr_results_year)
                             
                             base::print("DONE")
                             base::print(paste0("The files are located at: ./Grafik/Apistrip/Cumulytive/",myvar.usr_results_year,"/"))
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
                    
                    myfun.plot_matrix_avg_comparison(fun_year = myvar.usr_results_year,
                                                     fun_location = myvar.usr_results_location)
                    
                    base::print("DONE")
                    base::print(paste0("The files are located at: ./Grafik/Matrix_Comparison/Mean/",myvar.usr_results_year,"/", myvar.usr_results_location, "/"))
                    base::print("%")
                    base::print("%")
                    base::print("%")
                    base::print("%")
                  },
                  "ch6" = {
                    
                  },
                  "ch7" = {
                    
                  },
                  "ch8" = {
                    
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
           myvar.usr.table_type <- base::as.character(base::readline("Enter table-id here:"))
           
           switch(myvar.usr.table_type,
                  "tbl1" = {

# tbl1 --------------------------------------------------------------------

                    base::print("For which matrix would you like to export the data? Example: bb")
                    myvar.usr_matrix <- stringr::str_to_lower(base::as.character(base::readline("Enter here:")))
                    switch(myvar.usr_matrix,
                           "bb" = {
                             readr::write_excel_csv(
                               dplyr::select(tbl_results_bb, 
                                             fk_id_sam_bb,
                                             colony,
                                             matrix,
                                             sample_date,
                                             location,
                                             year,
                                             substance,
                                             concentration,
                                             lod,
                                             loq),
                               base::paste0("./Export/Results/", myvar.usr_matrix, "/tbl_results_", myvar.usr_matrix, ".csv"),
                               delim = ";"
                             )
                             base::print("DONE")
                             base::print(base::paste0("The file ist stored in: ./Export/Results/", myvar.usr_matrix, "/"))
                             
                           },
                           "p" = {
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
                               base::paste0("./Export/Results/", myvar.usr_matrix, "/tbl_results_", myvar.usr_matrix, ".csv"),
                               delim = ";"
                             )
                             base::print("DONE")
                             base::print(base::paste0("The file ist stored in: ./Export/Results/", myvar.usr_matrix, "/"))
                             
                           },
                           "w" = {
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
                               base::paste0("./Export/Results/", myvar.usr_matrix, "/tbl_results_", myvar.usr_matrix, ".csv"),
                               delim = ";"
                             )
                             base::print("DONE")
                             base::print(base::paste0("The file ist stored in: ./Export/Results/", myvar.usr_matrix, "/"))
                             
                           },
                           "a" = {
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
                               base::paste0("./Export/Results/", myvar.usr_matrix, "/tbl_results_", myvar.usr_matrix, ".csv"),
                               delim = ";"
                             )
                             base::print("DONE")
                             base::print(base::paste0("The file ist stored in: ./Export/Results/", myvar.usr_matrix, "/"))
                             
                           }
                    )
                  },
                  "tbl2" = {

# tbl2 --------------------------------------------------------------------

                    base::print("For which year do you want export the table? Example: 2022")
                    myvar.usr_results_year  <- base::as.numeric(base::readline("Enter here:"))
                    
                    myfun.export_tbl_matrix_diff(myvar.usr_results_year)
                    base::print("DONE")
                    base::print(base::paste0("The file ist stored in: ./Export/Matrix_Comparison/", myvar.usr_results_year, "/"))
                    
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
