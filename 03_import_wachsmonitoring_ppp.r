#03_import_wachsmonitoring_ppp.r

base::print("% Getting working directory")
myvar.cur_wd <- as.character(getwd())

#import excel file
tbl_results_wm <- readxl::read_excel(paste0(myvar.cur_wd, "/Data/Wachsmonitoring_PPP/results_wachsmonitoring.xlsx"), sheet = "w")

