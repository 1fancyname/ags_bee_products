#15_user_interface_wm_ppp_export.r



myvar.user_interface_wm_ppp_export_state <- "on"

while (myvar.user_interface_wm_ppp_export_state != "off") {
  
  base::print("PPP Wachsmonitoring export")
  base::print("To stop the user interface, press 'esc'. To exit PPP Wachsmonitoring export, type 'exit'.")
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
                    base::print("For what period of time do you want to create the charts?")
                    myvar.usr_results_year1  <- base::as.numeric(base::readline("Enter first year here:"))
                    myvar.usr_results_year2  <- base::as.numeric(base::readline("Enter last year here:"))
                    
                    
                    myfun.create_standard_substance_plot_wm(fun_year_start = myvar.usr_results_year1,
                                                            fun_year_end = myvar.usr_results_year2)
                    
                    base::print("DONE")
                    base::print(paste0("The files are located at: ./Grafik/Wachsmonitoring_PPP/Substances/"))
                    base::print("%")
                    base::print("%")
                    base::print("%")
                    base::print("%")
                  },
                  "ch2" = {
                    
                    base::print("Choose which substances you want to include in the chart. Example: Acetamiprid")
                    base::print("Leave prompt empty and hit enter if the desired substances have been chosen.")
                    
                    
                    myvar.usr_substance[1]  <- as.vector(base::as.character(base::readline("Enter first substance here:")))
                    myvar.usr_substance[2]  <- as.vector(base::as.character(base::readline("Enter second substance here:"))) 
                    myvar.usr_substance[3]  <- as.vector(base::as.character(base::readline("Enter third substance here:")))
                    myvar.usr_substance[4]  <- as.vector(base::as.character(base::readline("Enter fourth substance here:"))) 
                    myvar.usr_substance[5]  <- as.vector(base::as.character(base::readline("Enter fifth substance here:"))) 
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
                    base::print(paste0("The files are located at: ./Grafik/Wachsmonitoring_PPP/Substance_Comparison/"))
                    base::print("%")
                    base::print("%")
                    base::print("%")
                    base::print("%")
                    
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



