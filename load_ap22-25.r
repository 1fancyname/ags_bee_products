#load_ap22-25.r


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

if (myvar.ap22_25_re_load == TRUE) {
  source("./99_modules/02_import_ap22-25.r")
  myvar.ap22_25_loaded <-  TRUE
  myvar.ap22_25_processed <- FALSE
  myvar.ap22_25_re_load <- FALSE
  base::print("% AP22-25 data successfully loaded")
  
} else {
  if (myvar.ap22_25_loaded == TRUE) {
    base::print("% Data already loaded.")
    
  } else {
    source("./99_modules/02_import_ap22-25.r")
    myvar.ap22_25_loaded <-  TRUE
    myvar.ap22_25_processed <- FALSE
    base::print("% AP22-25 data successfully loaded")
  }
}









