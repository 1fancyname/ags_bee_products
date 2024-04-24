#11_user-interface_pollenmonitoring_ppp.r



myvar.user_interface_pm_ppp_state <- "on"

while (myvar.user_interface_pm_ppp_state != "off") {
  base::print("PPP Pollenmonitoring")
  base::print("To stop the user interface, press 'esc'. To and exit project PPP Pollenmonitoring, type 'exit'.")
  base::print("")
  base::print("")
  base::print("Current status:")
  base::print(paste0("PPP Pollenmonitoring data is loaded:    ", myvar.pm_ppp_loaded))
  base::print(paste0("PPP Pollenmonitoring data is processed: ", myvar.pm_ppp_processed))
  base::print("What would you like to do?")
  base::print("Type ")
  base::print("1 to re-load the data from Project PPP Pollenmonitoring.")
  base::print("2 to process the data from Project PPP Pollenmonitoring")
  base::print("3 to visualize the data from Project PPP Pollenmonitoring")
  myvar.usr.pm_ppp_choose_action <- base::as.character(base::readline("Enter here:")) 
  switch(myvar.usr.pm_ppp_choose_action,
         "1" = {
           base::print("Loading PPP Pollenmonitoring data... ")
           myvar.pm_ppp_re_load<- TRUE
           source("./99_modules/load_pollenmonitoring_ppp.r")
         },
         "2" = {
           base::print("% Processing PPP Pollenmonitoring data... ")
           source("./99_modules/12_transform_pollenmonitoring_ppp.r.r")
         },
         "3" = {
           #
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

