#08_user_interface_ap22-25.r

myvar.user_interface_ap22_25_state <- "on"

while (myvar.user_interface_ap22_25_state != "off") {
  base::print("Project AP22-25")
  base::print("To stop the user interface, press 'esc'. To and exit Project AP22-25, type 'exit'.")
  base::print("")
  base::print("")
  base::print("Current status:")
  base::print(paste0("Project AP22-25 data is loaded:    ", myvar.ap22_25_loaded))
  base::print(paste0("Project AP22-25 data is processed: ", myvar.ap22_25_processed))
  base::print("What would you like to do?")
  base::print("Type ")
  base::print("1 to re-load the data from Project AP22-25.")
  base::print("2 to process the data from Project AP22-25")
  base::print("3 to visualize and export the data from Project AP22-25")
  myvar.usr.ap22_25_choose_action <- base::as.character(base::readline("Enter here:")) 
  switch(myvar.usr.ap22_25_choose_action,
         "1" = {
           base::print("% Loading Project AP22-25 data... ")
           myvar.ap22_25_re_load <- TRUE
           source("./99_modules/load_ap22-25.r")
         },
         "2" = {
           base::print("% Processing Project AP22-25 data... ")
           source("./99_modules/06_transform_ap22-25.r")
         },
         "3" = {
           if (myvar.ap22_25_processed == TRUE) {
             
             base::print("% Starting AP22-25 export user interface... ")
             source("./99_modules/13_user_interface_ap22-25_export.r")
             
           } else {
             
             base::print("Error, data is not ready for export. ")
             base::print("Please process data before visualizing/exporting. ")
             base::print("%")
             base::print("%")
             base::print("%")
           }
           
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













