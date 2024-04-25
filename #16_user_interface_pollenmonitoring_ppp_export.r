#16_user_interface_pollenmonitoring_ppp_export.r


myvar.user_interface_pm_ppp_export_state <- "on"

while (myvar.user_interface_pm_ppp_export_state != "off") {
  
  base::print("PPP Pollenmonitoring export")
  base::print("To stop the user interface, press 'esc'. To exit PPP Pollenmonitoring export, type 'exit'.")
  base::print("")
  base::print("")
  base::print("Would you like to create charts with the imported data?")
  base::print("Type ")
  base::print("1 to create charts")
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
                    base::print("prevalence CH not yet ready")
                   
                    
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
                    myvar.usr_location[1]  <- as.vector(stringr::str_to_upper(base::as.character(base::readline("Enter first location here:")))) 
                    myvar.usr_location[2]  <- as.vector(stringr::str_to_upper(base::as.character(base::readline("Enter first location here:")))) 
                    myvar.usr_location[3]  <- as.vector(stringr::str_to_upper(base::as.character(base::readline("Enter third location here:"))))
                    myvar.usr_location[4]  <- as.vector(stringr::str_to_upper(base::as.character(base::readline("Enter fourth location here:")))) 
                    myvar.usr_location[5]  <- as.vector(stringr::str_to_upper(base::as.character(base::readline("Enter fifth location here:")))) 
                    myvar.usr_location <- unique(myvar.usr_location)
                    
                    myfun.plot_pm_ppp_substance_comp(myvar.usr_results_year,
                                      fun_location1 = myvar.usr_location[1],
                                      fun_location2 = myvar.usr_location[2],
                                      fun_location3 = myvar.usr_location[3],
                                      fun_location4 = myvar.usr_location[4],
                                      fun_location5 = myvar.usr_location[5])
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



