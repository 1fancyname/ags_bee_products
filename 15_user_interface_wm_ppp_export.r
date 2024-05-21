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
  base::print("2 to export files")
  myvar.usr.chart_export <- base::as.character(base::readline("Enter here:")) 
  
  switch(myvar.usr.chart_export,
         "1" = {
           base::print("Which chart would you like to create? There is a comprehensive list of all types in the user manual.")
           myvar.usr.chart_type <- stringr::str_to_lower(base::as.character(base::readline("Enter chart-id here:")))
           switch(myvar.usr.chart_type,
                  "ch1" = {
                    myfun.plot_wm_ppp_sub_yearly()
                    
                    base::print("DONE")
                    base::print(paste0("The files are located at: ./Grafik/PPP_Wachsmonitoring/Pool/"))
                    base::print("%")
                    base::print("%")
                    base::print("%")
                    base::print("%")
                  },
                  "ch2" = {
                    
                    myfun.plot_wm_ppp_sub_box_pool()
                    
                    base::print("DONE")
                    base::print(paste0("The files are located at: ./Grafik/PPP_Wachsmonitoring/Yearly/"))
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
                    readr::write_excel_csv(
                      tbl_wm_ppp_export,
                      glue("Export/PPP_Wachsmonitoring/ppp_wachsmonitoring.csv"),
                      delim = ";"
                    )
                    
                    base::print("DONE")
                    base::print(paste0("The file is located at: ./Data/Wachsmonitoring_PPP/"))
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



