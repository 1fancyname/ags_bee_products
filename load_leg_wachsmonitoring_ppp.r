#load_leg_wachsmonitoring_ppp.r



if (myvar.packages_loaded == TRUE) {
  base::print("% Packages already loaded.")
  
} else {
  source("./99_modules/01_load_packages.r")
  myvar.packages_loaded <- TRUE
}

if (myvar.gvar_loaded == TRUE) {
  base::print("% Global variables already loaded.")
  
} else {
  source("./99_modules/04_global_variables.r")
  myvar.gvar_loaded <- TRUE
}

if (myvar.fun_loaded == TRUE) {
  base::print("% Functions already loaded.")
  
} else {
  source("./99_modules/05_functions.r")
  myvar.fun_loaded <- TRUE
}

if (myvar.leg_wm_ppp_re_load == TRUE) {
  source("./99_modules/20_import_leg_wachsmonitoring_ppp.r")
  myvar.leg_wm_ppp_loaded <-  TRUE
  myvar.leg_wm_ppp_processed <- FALSE
  myvar.leg_wm_ppp_load <- FALSE
  base::print("% Legacy PPP Wachsmonitoring data successfully loaded")
  
} else {
  if (myvar.leg_wm_ppp_loaded == TRUE) {
    base::print("% Data already loaded.")
    
  } else {
    source("./99_modules/20_import_leg_wachsmonitoring_ppp.r")
    myvar.leg_wm_ppp_loaded <-  TRUE
    myvar.leg_wm_ppp_processed <- FALSE
    base::print("% Legacy PPP Wachsmonitoring data successfully loaded")
  }
}






