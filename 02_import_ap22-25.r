#02_import_ap22-25.r


base::print("% Getting working directory")
myvar.cur_wd <- as.character(getwd())


base::print("% Importing project data.")



#load excel file with user input
script_parameters <- readxl::read_excel("script_parameters.xlsx")


#get user input from excel by filtering on variable names and subsetting
tmp_tbl_params <- dplyr::filter(script_parameters, parameter_name == "myvar.ap22_25_project_data")
myvar.ap22_25_project_data <- tmp_tbl_params$user_input[1]

#get user input from excel by filtering on variable names and subsetting
tmp_tbl_params <- dplyr::filter(script_parameters, parameter_name == "myvar.ap22_25_samples_doc")
myvar.ap22_25_samples_doc <- tmp_tbl_params$user_input[1]

tmp_tbl_params <- dplyr::filter(script_parameters, parameter_name == "myvar.ap22_25_samples_bb_sheet")
myvar.ap22_25_samples_bb_sheet <- tmp_tbl_params$user_input[1]

tmp_tbl_params <- dplyr::filter(script_parameters, parameter_name == "myvar.ap22_25_samples_p_sheet")
myvar.ap22_25_samples_p_sheet <- tmp_tbl_params$user_input[1]

tmp_tbl_params <- dplyr::filter(script_parameters, parameter_name == "myvar.ap22_25_samples_w_sheet")
myvar.ap22_25_samples_w_sheet <- tmp_tbl_params$user_input[1]

tmp_tbl_params <- dplyr::filter(script_parameters, parameter_name == "myvar.ap22_25_samples_a_sp_sheet")
myvar.ap22_25_samples_a_sp_sheet <- tmp_tbl_params$user_input[1]

tmp_tbl_params <- dplyr::filter(script_parameters, parameter_name == "myvar.ap22_25_samples_a_sheet")
myvar.ap22_25_samples_a_sheet <- tmp_tbl_params$user_input[1]

tmp_tbl_params <- dplyr::filter(script_parameters, parameter_name == "myvar.ap22_25_results_doc")
myvar.ap22_25_results_doc <- tmp_tbl_params$user_input[1]

tmp_tbl_params <- dplyr::filter(script_parameters, parameter_name == "myvar.ap22_25_results_bb_sheet")
myvar.ap22_25_results_bb_sheet <- tmp_tbl_params$user_input[1]

tmp_tbl_params <- dplyr::filter(script_parameters, parameter_name == "myvar.ap22_25_results_p_sheet")
myvar.ap22_25_results_p_sheet <- tmp_tbl_params$user_input[1]

tmp_tbl_params <- dplyr::filter(script_parameters, parameter_name == "myvar.ap22_25_results_w_sheet")
myvar.ap22_25_results_w_sheet <- tmp_tbl_params$user_input[1]

tmp_tbl_params <- dplyr::filter(script_parameters, parameter_name == "myvar.ap22_25_results_a_sheet")
myvar.ap22_25_results_a_sheet <- tmp_tbl_params$user_input[1]

tmp_tbl_params <- dplyr::filter(script_parameters, parameter_name == "myvar.ap22_25_results_a_sp_sheet")
myvar.ap22_25_results_a_sp_sheet <- tmp_tbl_params$user_input[1]

tmp_tbl_params <- dplyr::filter(script_parameters, parameter_name == "myvar.ap22_25_meta_doc")
myvar.ap22_25_meta_doc <- tmp_tbl_params$user_input[1]

tmp_tbl_params <- dplyr::filter(script_parameters, parameter_name == "myvar.ap22_25_meta_loc_sheet")
myvar.ap22_25_meta_loc_sheet <- tmp_tbl_params$user_input[1]

tmp_tbl_params <- dplyr::filter(script_parameters, parameter_name == "myvar.ap22_25_meta_col_sheet")
myvar.ap22_25_meta_col_sheet <- tmp_tbl_params$user_input[1]

tmp_tbl_params <- dplyr::filter(script_parameters, parameter_name == "myvar.ap22_25_meta_exp_sheet")
myvar.ap22_25_meta_exp_sheet <- tmp_tbl_params$user_input[1]

tmp_tbl_params <- dplyr::filter(script_parameters, parameter_name == "myvar.ap22_25_meta_lim_sheet")
myvar.ap22_25_meta_lim_sheet <- tmp_tbl_params$user_input[1]

tmp_tbl_params <- dplyr::filter(script_parameters, parameter_name == "myvar.ap22_25_meta_mat_sheet")
myvar.ap22_25_meta_mat_sheet <- tmp_tbl_params$user_input[1]

tmp_tbl_params <- dplyr::filter(script_parameters, parameter_name == "myvar.ap22_25_meta_sub_sheet")
myvar.ap22_25_meta_sub_sheet <- tmp_tbl_params$user_input[1]

rm(tmp_tbl_params)




# import excel files ------------------------------------------------------



#metadata tables
tbl_colonies <- readxl::read_excel(paste0(myvar.cur_wd, myvar.ap22_25_project_data, myvar.ap22_25_meta_doc), sheet = myvar.ap22_25_meta_col_sheet)
tbl_locations <- readxl::read_excel(paste0(myvar.cur_wd, myvar.ap22_25_project_data, myvar.ap22_25_meta_doc), sheet = myvar.ap22_25_meta_loc_sheet)
tbl_experiments <- readxl::read_excel(paste0(myvar.cur_wd, myvar.ap22_25_project_data, myvar.ap22_25_meta_doc), sheet = myvar.ap22_25_meta_exp_sheet)
tbl_limits <- readxl::read_excel(paste0(myvar.cur_wd, myvar.ap22_25_project_data, myvar.ap22_25_meta_doc), sheet = myvar.ap22_25_meta_lim_sheet)
tbl_matrixes <- readxl::read_excel(paste0(myvar.cur_wd, myvar.ap22_25_project_data, myvar.ap22_25_meta_doc), sheet = myvar.ap22_25_meta_mat_sheet)
tbl_substances <- readxl::read_excel(paste0(myvar.cur_wd, myvar.ap22_25_project_data, myvar.ap22_25_meta_doc), sheet = myvar.ap22_25_meta_sub_sheet)



#result tables
tbl_results_bb <- readxl::read_excel(paste0(myvar.cur_wd, myvar.ap22_25_project_data, myvar.ap22_25_results_doc), sheet = myvar.ap22_25_results_bb_sheet)
tbl_results_p <- readxl::read_excel(paste0(myvar.cur_wd, myvar.ap22_25_project_data, myvar.ap22_25_results_doc), sheet = myvar.ap22_25_results_p_sheet)
tbl_results_w <- readxl::read_excel(paste0(myvar.cur_wd, myvar.ap22_25_project_data, myvar.ap22_25_results_doc), sheet = myvar.ap22_25_results_w_sheet)
tbl_results_a <- readxl::read_excel(paste0(myvar.cur_wd, myvar.ap22_25_project_data, myvar.ap22_25_results_doc), sheet = myvar.ap22_25_results_a_sheet)
tbl_results_a_sp <- readxl::read_excel(paste0(myvar.cur_wd, myvar.ap22_25_project_data, myvar.ap22_25_results_doc), sheet = myvar.ap22_25_results_a_sp_sheet)



#sample tables
tbl_samples_bb <- readxl::read_excel(paste0(myvar.cur_wd, myvar.ap22_25_project_data, myvar.ap22_25_samples_doc), sheet = myvar.ap22_25_samples_bb_sheet)
tbl_samples_p <- readxl::read_excel(paste0(myvar.cur_wd, myvar.ap22_25_project_data, myvar.ap22_25_samples_doc), sheet = myvar.ap22_25_samples_p_sheet)
tbl_samples_w <- readxl::read_excel(paste0(myvar.cur_wd, myvar.ap22_25_project_data, myvar.ap22_25_samples_doc), sheet = myvar.ap22_25_samples_w_sheet)
tbl_samples_a <- readxl::read_excel(paste0(myvar.cur_wd, myvar.ap22_25_project_data, myvar.ap22_25_samples_doc), sheet = myvar.ap22_25_samples_a_sheet)
tbl_samples_a_sp <- readxl::read_excel(paste0(myvar.cur_wd, myvar.ap22_25_project_data, myvar.ap22_25_samples_doc), sheet = myvar.ap22_25_samples_a_sp_sheet)


rm(myvar.ap22_25_project_data, myvar.ap22_25_samples_doc, myvar.ap22_25_samples_bb_sheet,
   myvar.ap22_25_samples_p_sheet, myvar.ap22_25_samples_w_sheet, myvar.ap22_25_samples_a_sp_sheet,
   myvar.ap22_25_samples_a_sheet, myvar.ap22_25_results_doc, myvar.ap22_25_results_bb_sheet,
   myvar.ap22_25_results_p_sheet, myvar.ap22_25_results_w_sheet, myvar.ap22_25_results_a_sheet,
   myvar.ap22_25_results_a_sp_sheet, myvar.ap22_25_meta_doc, myvar.ap22_25_meta_loc_sheet, 
   myvar.ap22_25_meta_col_sheet, myvar.ap22_25_meta_exp_sheet, myvar.ap22_25_meta_lim_sheet,
   myvar.ap22_25_meta_mat_sheet, myvar.ap22_25_meta_sub_sheet, myvar.cur_wd)




