#09_import_pollenmonitoring_ppp.r

#stop user interface in case it is running
myvar.user_interface_state <- "off"


base::print("% Getting working directory")
myvar.cur_wd <- as.character(getwd())

base::print("% Importing ppp pollenmonitoring project data.")



#load excel file with user input
script_parameters <- readxl::read_excel("script_parameters.xlsx")


#get user input from excel by filtering on variable names and subsetting
tmp_tbl_params <- dplyr::filter(script_parameters, parameter_name == "myvar.pm_ppp_project_data")
myvar.pm_ppp_project_data <- tmp_tbl_params$user_input[1]

tmp_tbl_params <- dplyr::filter(script_parameters, parameter_name == "myvar.pm_ppp_samples_doc")
myvar.pm_ppp_samples_doc <- tmp_tbl_params$user_input[1]

tmp_tbl_params <- dplyr::filter(script_parameters, parameter_name == "myvar.pm_ppp_samples_sheet")
myvar.pm_ppp_samples_sheet <- tmp_tbl_params$user_input[1]

tmp_tbl_params <- dplyr::filter(script_parameters, parameter_name == "myvar.pm_ppp_results_doc")
myvar.pm_ppp_results_doc <- tmp_tbl_params$user_input[1]

tmp_tbl_params <- dplyr::filter(script_parameters, parameter_name == "myvar.pm_ppp_results_sheet")
myvar.pm_ppp_results_sheet <- tmp_tbl_params$user_input[1]

tmp_tbl_params <- dplyr::filter(script_parameters, parameter_name == "myvar.pm_ppp_meta_doc")
myvar.pm_ppp_meta_doc <- tmp_tbl_params$user_input[1]

tmp_tbl_params <- dplyr::filter(script_parameters, parameter_name == "myvar.pm_ppp_meta_loc_sheet")
myvar.pm_ppp_meta_loc_sheet <- tmp_tbl_params$user_input[1]

tmp_tbl_params <- dplyr::filter(script_parameters, parameter_name == "myvar.pm_ppp_meta_col_sheet")
myvar.pm_ppp_meta_col_sheet <- tmp_tbl_params$user_input[1]

tmp_tbl_params <- dplyr::filter(script_parameters, parameter_name == "myvar.pm_ppp_meta_exp_sheet")
myvar.pm_ppp_meta_exp_sheet <- tmp_tbl_params$user_input[1]

tmp_tbl_params <- dplyr::filter(script_parameters, parameter_name == "myvar.pm_ppp_meta_lim_sheet")
myvar.pm_ppp_meta_lim_sheet <- tmp_tbl_params$user_input[1]

tmp_tbl_params <- dplyr::filter(script_parameters, parameter_name == "myvar.pm_ppp_meta_bk_sheet")
myvar.pm_ppp_meta_bk_sheet <- tmp_tbl_params$user_input[1]

tmp_tbl_params <- dplyr::filter(script_parameters, parameter_name == "myvar.pm_ppp_meta_sub_sheet")
myvar.pm_ppp_meta_sub_sheet <- tmp_tbl_params$user_input[1]

rm(tmp_tbl_params)



# import excel files ------------------------------------------------------


#metadata tables
tbl_pm_ppp_colonies <- readxl::read_excel(paste0(myvar.cur_wd, myvar.pm_ppp_project_data, myvar.pm_ppp_meta_doc), sheet = myvar.pm_ppp_meta_col_sheet)
tbl_pm_ppp_locations <- readxl::read_excel(paste0(myvar.cur_wd, myvar.pm_ppp_project_data, myvar.pm_ppp_meta_doc), sheet = myvar.pm_ppp_meta_loc_sheet)
tbl_pm_ppp_experiments <- readxl::read_excel(paste0(myvar.cur_wd, myvar.pm_ppp_project_data, myvar.pm_ppp_meta_doc), sheet = myvar.pm_ppp_meta_exp_sheet)
tbl_pm_ppp_limits <- readxl::read_excel(paste0(myvar.cur_wd, myvar.pm_ppp_project_data, myvar.pm_ppp_meta_doc), sheet = myvar.pm_ppp_meta_lim_sheet)
tbl_pm_ppp_beekepers <- readxl::read_excel(paste0(myvar.cur_wd, myvar.pm_ppp_project_data, myvar.pm_ppp_meta_doc), sheet = myvar.pm_ppp_meta_bk_sheet)
tbl_pm_ppp_substances <- readxl::read_excel(paste0(myvar.cur_wd, myvar.pm_ppp_project_data, myvar.pm_ppp_meta_doc), sheet = myvar.pm_ppp_meta_sub_sheet)


#results table
tbl_pm_ppp_results <- readxl::read_excel(paste0(myvar.cur_wd, myvar.pm_ppp_project_data, myvar.pm_ppp_results_doc), sheet = myvar.pm_ppp_results_sheet)


#samples table
tbl_pm_ppp_samples <- readxl::read_excel(paste0(myvar.cur_wd, myvar.pm_ppp_project_data, myvar.pm_ppp_samples_doc), sheet = myvar.pm_ppp_samples_sheet)




rm(myvar.pm_ppp_samples_doc, myvar.pm_ppp_samples_sheet, myvar.pm_ppp_results_doc, 
   myvar.pm_ppp_results_sheet, myvar.pm_ppp_meta_doc, myvar.pm_ppp_meta_loc_sheet, 
   myvar.pm_ppp_meta_col_sheet, myvar.pm_ppp_meta_exp_sheet, myvar.pm_ppp_meta_lim_sheet,
   myvar.pm_ppp_meta_bk_sheet, myvar.pm_ppp_meta_sub_sheet, myvar.cur_wd, myvar.pm_ppp_project_data, myvar.pm_ppp_meta_doc)
