#21_user_interface_leg_wm_ppp.r



myvar.user_interface_state <- "on"

while (myvar.user_interface_state != "off") {
  base::print("Legacy PPP Wachsmonitoring")
  base::print("To stop the user interface, press 'esc'. To and exit project PPP Wachsmonitoring, type 'exit'.")
  base::print("")
  base::print("")
  base::print("Current status:")
  base::print(paste0("Legacy PPP Wachsmonitoring data is loaded:    ", myvar.leg_wm_ppp_loaded))
  base::print(paste0("Legacy PPP wachsmonitoring data is processed: ", myvar.leg_wm_ppp_processed))
  base::print("What would you like to do?")
  base::print("Type ")
  base::print("1 to re-load the data from Project Legacy PPP Wachsmonitoring")
  base::print("2 to process the data from Project Legacy PPP Wachsmonitoring")
  base::print("3 to visualize the data from Project Legacy PPP Wachsmonitoring")
  myvar.usr.leg_wm_ppp_choose_action <- base::as.character(base::readline("Enter here:")) 
  switch(myvar.usr.leg_wm_ppp_choose_action,
         "1" = {
           base::print("Loading Legacy PPP Wachsmonitoring data... ")
           myvar.leg_wm_ppp_re_load <- TRUE
           source("./99_modules/load_leg_wachsmonitoring_ppp.r")
         },
         "2" = {
           base::print("% Processing PPP Wachsmonitoring data... ")
           source("./99_modules/19_transform_leg_wm_ppp.r")
         },
         "3" = {
           if (myvar.leg_wm_ppp_processed == TRUE) {
             
             base::print("% Starting Legacy PPP Wachsmonitoring export user interface... ")
             source("./99_modules/18_user_interface_leg_wm_ppp_export.r")
             
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

