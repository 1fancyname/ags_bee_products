#14_user_interface_wm_ppp.r



myvar.user_interface_state <- "on"

while (myvar.user_interface_state != "off") {
  base::print("PPP Wachsmonitoring")
  base::print("To stop the user interface, press 'esc'. To and exit project PPP Wachsmonitoring, type 'exit'.")
  base::print("")
  base::print("")
  base::print("Current status:")
  base::print(paste0("PPP Wachsmonitoring data is loaded:    ", myvar.wm_loaded))
  base::print(paste0("PPP wachsmonitoring data is processed: ", myvar.wm_processed))
  base::print("What would you like to do?")
  base::print("Type ")
  base::print("1 to re-load the data from Project PPP Wachsmonitoring")
  base::print("2 to process the data from Project PPP Wachsmonitoring")
  base::print("3 to visualize or export the data from Project PPP Wachsmonitoring")
  myvar.usr.wm_ppp_choose_action <- base::as.character(base::readline("Enter here:")) 
  switch(myvar.usr.wm_ppp_choose_action,
         "1" = {
           base::print("Loading PPP Wachsmonitoring data... ")
           myvar.wm_re_load <- TRUE
           source("./99_modules/load_wachsmonitoring_ppp.r")
         },
         "2" = {
           base::print("% Processing PPP Wachsmonitoring data... ")
           source("./99_modules/07_transform_wachsmonitoring_ppp.r")
         },
         "3" = {
           if (myvar.wm_processed == TRUE) {
             
             base::print("% Starting PPP Wachsmonitoring export user interface... ")
             source("./99_modules/15_user_interface_wm_ppp_export.r")
             
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
         },
         "end" = {
           base::print("% Stopping user interface...")
           base::print("%")
           base::print("%")
           base::print("%")
           break
         }
  )
}

