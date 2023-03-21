#02_import_data.r

#stop user interface in case it is running
myvar.user_interface_state <- "off"


base::print("% Importing project data.")

pb = txtProgressBar(min = 0, max = 100, initial = 0) 
setTxtProgressBar(pb,1)

#load excel file with user input
script_parameters <- readxl::read_excel("script_parameters.xlsx")

setTxtProgressBar(pb,25)

#get user input from excel by filtering on variable names and subsetting
tmp_tbl_params <- dplyr::filter(script_parameters, parameter_name == "myvar.samples_doc")
myvar.sample_doc <- tmp_tbl_params$user_input[1]

tmp_tbl_params <- dplyr::filter(script_parameters, parameter_name == "myvar.samples_bb_sheet")
myvar.samples_bb_sheet <- tmp_tbl_params$user_input[1]

tmp_tbl_params <- dplyr::filter(script_parameters, parameter_name == "myvar.samples_p_sheet")
myvar.samples_p_sheet <- tmp_tbl_params$user_input[1]

tmp_tbl_params <- dplyr::filter(script_parameters, parameter_name == "myvar.samples_w_sheet")
myvar.samples_w_sheet <- tmp_tbl_params$user_input[1]

tmp_tbl_params <- dplyr::filter(script_parameters, parameter_name == "myvar.samples_a_sheet")
myvar.samples_a_sheet <- tmp_tbl_params$user_input[1]

tmp_tbl_params <- dplyr::filter(script_parameters, parameter_name == "myvar.results_doc")
myvar.results_doc <- tmp_tbl_params$user_input[1]

tmp_tbl_params <- dplyr::filter(script_parameters, parameter_name == "myvar.results_bb_sheet")
myvar.results_bb_sheet <- tmp_tbl_params$user_input[1]

tmp_tbl_params <- dplyr::filter(script_parameters, parameter_name == "myvar.results_p_sheet")
myvar.results_p_sheet <- tmp_tbl_params$user_input[1]

tmp_tbl_params <- dplyr::filter(script_parameters, parameter_name == "myvar.results_w_sheet")
myvar.results_w_sheet <- tmp_tbl_params$user_input[1]

tmp_tbl_params <- dplyr::filter(script_parameters, parameter_name == "myvar.results_a_sheet")
myvar.results_a_sheet <- tmp_tbl_params$user_input[1]

tmp_tbl_params <- dplyr::filter(script_parameters, parameter_name == "myvar.meta_doc")
myvar.meta_doc <- tmp_tbl_params$user_input[1]

tmp_tbl_params <- dplyr::filter(script_parameters, parameter_name == "myvar.meta_loc_sheet")
myvar.meta_loc_sheet <- tmp_tbl_params$user_input[1]

tmp_tbl_params <- dplyr::filter(script_parameters, parameter_name == "myvar.meta_col_sheet")
myvar.meta_col_sheet <- tmp_tbl_params$user_input[1]

tmp_tbl_params <- dplyr::filter(script_parameters, parameter_name == "myvar.meta_exp_sheet")
myvar.meta_exp_sheet <- tmp_tbl_params$user_input[1]

tmp_tbl_params <- dplyr::filter(script_parameters, parameter_name == "myvar.meta_lim_sheet")
myvar.meta_lim_sheet <- tmp_tbl_params$user_input[1]

tmp_tbl_params <- dplyr::filter(script_parameters, parameter_name == "myvar.meta_mat_sheet")
myvar.meta_mat_sheet <- tmp_tbl_params$user_input[1]

tmp_tbl_params <- dplyr::filter(script_parameters, parameter_name == "myvar.meta_sub_sheet")
myvar.meta_sub_sheet <- tmp_tbl_params$user_input[1]

rm(tmp_tbl_params)




# import excel files ------------------------------------------------------

setTxtProgressBar(pb,60) 

#metadata tables
tbl_colonies <- readxl::read_excel(myvar.meta_doc, sheet = myvar.meta_col_sheet)
tbl_locations <- readxl::read_excel(myvar.meta_doc, sheet = myvar.meta_loc_sheet)
tbl_experiments <- readxl::read_excel(myvar.meta_doc, sheet = myvar.meta_exp_sheet)
tbl_limits <- readxl::read_excel(myvar.meta_doc, sheet = myvar.meta_lim_sheet)
tbl_matrixes <- readxl::read_excel(myvar.meta_doc, sheet = myvar.meta_mat_sheet)
tbl_substances <- readxl::read_excel(myvar.meta_doc, sheet = myvar.meta_sub_sheet)

setTxtProgressBar(pb,75)

#result tables
tbl_results_bb <- readxl::read_excel(myvar.results_doc, sheet = myvar.results_bb_sheet)
tbl_results_p <- readxl::read_excel(myvar.results_doc, sheet = myvar.results_p_sheet)
tbl_results_w <- readxl::read_excel(myvar.results_doc, sheet = myvar.results_w_sheet)
tbl_results_a <- readxl::read_excel(myvar.results_doc, sheet = myvar.results_a_sheet)

setTxtProgressBar(pb,90)

#sample tables
tbl_samples_bb <- readxl::read_excel(myvar.sample_doc, sheet = myvar.samples_bb_sheet)
tbl_samples_p <- readxl::read_excel(myvar.sample_doc, sheet = myvar.samples_p_sheet)
tbl_samples_w <- readxl::read_excel(myvar.sample_doc, sheet = myvar.samples_w_sheet)
tbl_samples_a <- readxl::read_excel(myvar.sample_doc, sheet = myvar.samples_a_sheet)


rm(myvar.meta_col_sheet, myvar.meta_doc, myvar.meta_exp_sheet,
   myvar.meta_lim_sheet, myvar.meta_loc_sheet, myvar.meta_mat_sheet,
   myvar.meta_sub_sheet, myvar.results_bb_sheet, myvar.results_doc,
   myvar.results_p_sheet, myvar.results_w_sheet, myvar.results_a_sheet, myvar.sample_doc,
   myvar.samples_bb_sheet, myvar.samples_p_sheet, myvar.samples_w_sheet, myvar.samples_a_sheet)



setTxtProgressBar(pb,100)
