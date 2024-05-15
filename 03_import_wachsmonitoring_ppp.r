#03_import_wachsmonitoring_ppp.r

base::print("% Getting working directory")
myvar.cur_wd <- as.character(getwd())



base::print("% Getting working directory")
myvar.cur_wd <- as.character(getwd())

base::print("% Importing ppp wachsmonitoring project data.")



#load excel file with user input
script_parameters <- readxl::read_excel("script_parameters.xlsx")


#get user input from excel by filtering on variable names and subsetting
tmp_tbl_params <- dplyr::filter(script_parameters, parameter_name == "myvar.wm_ppp_project_data")
myvar.wm_ppp_project_data <- tmp_tbl_params$user_input[1]


tmp_tbl_params <- dplyr::filter(script_parameters, parameter_name == "myvar.wm_ppp_results_doc")
myvar.wm_ppp_results_doc <- tmp_tbl_params$user_input[1]

tmp_tbl_params <- dplyr::filter(script_parameters, parameter_name == "myvar.wm_ppp_results_pool_sheet")
myvar.wm_ppp_results_pool_sheet <- tmp_tbl_params$user_input[1]

tmp_tbl_params <- dplyr::filter(script_parameters, parameter_name == "myvar.wm_ppp_results_yearly_sheet")
myvar.wm_ppp_results_yearly_sheet <- tmp_tbl_params$user_input[1]




# import excel files ------------------------------------------------------


#metadata tables
tbl_wm_ppp_pool <- readxl::read_excel(paste0(myvar.cur_wd, myvar.wm_ppp_project_data, myvar.wm_ppp_results_doc), sheet = myvar.wm_ppp_results_pool_sheet)
tbl_wm_ppp_yearly <- readxl::read_excel(paste0(myvar.cur_wd, myvar.wm_ppp_project_data, myvar.wm_ppp_results_doc), sheet = myvar.wm_ppp_results_yearly_sheet)
