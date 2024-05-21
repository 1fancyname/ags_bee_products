#user-interface.r



source("./99_modules/10_states.r")

myvar.user_interface_state <- "on"

while (myvar.user_interface_state != "off") {
  base::print("Wellcome to the R script of the AGS bee products group.")
  base::print("Version 1.6")
  base::print("To stop the user interface and exit the program, type 'exit' in the menu or press esc.")
  base::print("")
  base::print("")
  base::print("Current status:")
  base::print(paste0("PPP Wachsmonitoring data is loaded:           ", myvar.wm_loaded))
  base::print(paste0("PPP Wachsmonitoring data is processed:        ", myvar.wm_processed))
  base::print(paste0("Project AP22-25 data is loaded:               ", myvar.ap22_25_loaded))
  base::print(paste0("Project AP22-25 data is processed:            ", myvar.ap22_25_processed))
  base::print(paste0("PPP Pollenmonitoring data is loaded:          ", myvar.pm_ppp_loaded))
  base::print(paste0("PPP Pollenmonitoring data is processed:       ", myvar.pm_ppp_processed))
  base::print(paste0("Legacy PPP Pollenmonitoring data is loaded:   ", myvar.leg_wm_ppp_loaded))
  base::print(paste0("Legacy PPP Pollenmonitoring data is processed:", myvar.leg_wm_ppp_processed))
  base::print("What project would you like to work on?")
  base::print("Type ")
  base::print("1 to work with data from Project PPP Wachsmonitoring.")
  base::print("2 to work with data from Project AP22-25")
  base::print("3 to work with data from Project PPP Pollenmonitoring")
  base::print("4 to work with data from Project Legacy PPP Wachsmonitoring")
  myvar.usr.choose_project <- base::as.character(base::readline("Enter here:")) 
  switch(myvar.usr.choose_project,
         "1" = {
           base::print("Loading PPP Wachsmonitoring data... ")
           source("./99_modules/load_wachsmonitoring_ppp.r")
           source("./99_modules/14_user_interface_wm_ppp.r")
         },
         "2" = {
           base::print("% Loading Project AP22-25 data... ")
           source("./99_modules/load_ap22-25.r")
           source("./99_modules/08_user_interface_ap22-25.r")
         },
         "3" = {
           base::print("% Loading PPP Pollenmonitoring data... ")
           source("./99_modules/load_pollenmonitoring_ppp.r")
           source("./99_modules/11_user-interface_pollenmonitoring_ppp.r")
         },
         "4" = {
           base::print("% Loading PPP legacy Wachsmonitoring data... ")
           source("./99_modules/load_leg_wachsmonitoring_ppp.r")
           source("./99_modules/21_user_interface_leg_wm_ppp.r")
           
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
  
