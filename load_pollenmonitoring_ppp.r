#load_pollenmonitoring_ppp.r


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

if (myvar.pm_ppp_loaded == TRUE) {
  base::print("% Data already loaded.")
  
} else {
  source("./99_modules/09_import_pollenmonitoring_ppp.r")
  myvar.pm_ppp_loaded <-  TRUE
  base::print("% PPP pollenmonitoring data successfully loaded")
}







