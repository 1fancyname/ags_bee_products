#user-interface.r



source("./99_modules/10_states.r")

myvar.user_interface_state <- "on"

while (myvar.user_interface_state != "off") {
  base::print("Wellcome to the R script of the bee products group.")
  base::print("Version 1.0.1")
  base::print("To stop the user interface and exit the program, type 'exit' in the menu or press esc.")
  base::print("")
  base::print("")
  base::print("Current status:")
  base::print(paste0("Wachsmonitoring data is loaded:         ", myvar.wm_loaded))
  base::print(paste0("Wachsmonitoring data is processed:      ", myvar.wm_processed))
  base::print(paste0("Project AP22-25 data is loaded:         ", myvar.ap22_25_loaded))
  base::print(paste0("Project AP22-25 data is processed:      ", myvar.ap22_25_processed))
  base::print(paste0("PPP Pollenmonitoring data is loaded:    ", myvar.pm_ppp_loaded))
  base::print(paste0("PPP Pollenmonitoring data is processed: ", myvar.pm_ppp_processed))
  base::print("What project would you like to work on?")
  base::print("Type ")
  base::print("1 to work with data from Project Wachsmonitoring.")
  base::print("2 to work with data from Project AP22-25")
  base::print("3 to work with data from Project PPP Pollenmonitoring")
  myvar.usr.choose_project <- base::as.character(base::readline("Enter here:")) 
  switch(myvar.usr.choose_project,
         "1" = {
           base::print("Loading Wachsmonitoring data... ")
           
           
         },
         "2" = {
           #
         },
         "3" = {
           base::print("Loading PPP Pollenmonitoring data... ")
           source("./99_modules/load_pollenmonitoring_ppp.r")
           source("./99_modules/11_user-interface_pm_ppp.r")
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
  
