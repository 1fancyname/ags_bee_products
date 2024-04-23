#11_user-interface_pm_ppp.r



myvar.user_interface_state <- "on"

while (myvar.user_interface_state != "off") {
  base::print("PPP Pollenmonitoring")
  base::print("To stop the user interface, press 'esc'. To and exit Project PPP Pollenmonitoring, type 'exit'.")
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
  myvar.usr.choose_project <- base::as.character(base::readline("Enter here:")) 
  switch(myvar.usr.choose_project,
         "1" = {
           base::print("Loading PPP Pollenmonitoring data... ")
           source("./99_modules/load_pollenmonitoring_ppp.r")
         },
         "2" = {
           #
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

