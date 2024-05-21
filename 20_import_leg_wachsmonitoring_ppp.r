#20_import_leg_wachsmonitoring_ppp.r



base::print("% Getting working directory")
myvar.cur_wd <- as.character(getwd())


myvar.cur_wd <- as.character(getwd())

base::print("% Importing ppp wachsmonitoring project data.")



#load excel file with user input
script_parameters <- readxl::read_excel("script_parameters.xlsx")


#get user input from excel by filtering on variable names and subsetting
tmp_tbl_params <- dplyr::filter(script_parameters, parameter_name == "myvar.leg_wm_ppp_project_data")
myvar.leg_wm_ppp_project_data <- tmp_tbl_params$user_input[1]



tmp_tbl_params <- dplyr::filter(script_parameters, parameter_name == "myvar.leg_wm_ppp_results_doc")
myvar.leg_wm_ppp_results_doc <- tmp_tbl_params$user_input[1]


tmp_tbl_params <- dplyr::filter(script_parameters, parameter_name == "myvar.leg_wm_ppp_results_sheet")
myvar.leg_wm_ppp_results_sheet <- tmp_tbl_params$user_input[1]

# import excel files ------------------------------------------------------


tbl_leg_wm_ppp_results <- readxl::read_excel(paste0(myvar.cur_wd, myvar.leg_wm_ppp_project_data, myvar.leg_wm_ppp_results_doc), sheet = myvar.leg_wm_ppp_results_sheet)
