#05_functions.r


#stop user interface in case it is running

myvar.user_interface_state <- "off"


base::print("% Loading functions into session.")

# assign viridis hex codes to vector --------------------------------------

myfun.assign_viridis_to_vec <- function(input_vec){
  tmp_vec <- base::as.character(input_vec)
  myvar.viridis_seq <- base::seq.int(from = 1, to = 10,along.with = input_vec)
  if (base::length(input_vec) <= 10) {
    for (i in seq_along(input_vec)) {
      tmp_vec <- stringr::str_replace(tmp_vec, regex(paste0("^", tmp_vec[i], "$")), myvar.viridis_palette[myvar.viridis_seq[i]])
      tmp_vec <- purrr::set_names(tmp_vec, input_vec)
    }
  } else {
    base::print("Error! There are too many colonies per location.")
  }
  
  base::return(tmp_vec)
}



# create label vector for lab ---------------------------------------------

myfun.create_label_vector <- function(input_vec){
  out_vec <- base::as.character(1:base::length(input_vec))
  base::return(out_vec)
}



# check if concentration is greater than LOD ------------------------------

myfun.check_lod <- function(input_table){
  for (i in 1:base::NROW(input_table)) {
    if (base::is.na(input_table$concentration[i])) {
      input_table$greater_than_lod[i] = NA
    } else if (input_table$concentration[i] < input_table$lod[i]) {
      input_table$greater_than_lod[i] = FALSE
    } else if (input_table$concentration[i] >= input_table$lod[i]) {
      input_table$greater_than_lod[i] = TRUE
    }
  }
  rm(i)
  base::return(input_table)
}



# check if concentration is greater than LOQ -------------------------------

myfun.check_loq <- function(input_table){
  for (i in 1:base::NROW(input_table)) {
    if (base::is.na(input_table$concentration[i])) {
      input_table$greater_than_loq[i] = NA
    } else if (input_table$concentration[i] < input_table$loq[i]) {
      input_table$greater_than_loq[i] = FALSE
    } else if (input_table$concentration[i] >= input_table$loq[i]) {
      input_table$greater_than_loq[i] = TRUE
    }
  }
  rm(i)
  base::return(input_table)
}



# create tbl_prevalence_x -------------------------------------------------

myfun.create_tbl_prevalence <- function(input_table, output_table){
  
  output_table <- dplyr::tibble(substance = NA,
                              location_short = NA,
                              year = NA,
                              n_samples = NA,
                              prevalence_gt_lod_lt_loq = NA,
                              prevalence_gt_lod = NA,
                              prevalence_gt_loq = NA,
                              prct_gt_lod_lt_loq = NA,
                              prct_gt_lod = NA,
                              prct_gt_loq = NA)
  
  
  myvar.substances_unique <- base::unique(input_table$substance)
  myvar.location_short_unique <- base::unique(input_table$location_short)
  
  
  for (i in base::seq_along(myvar.substances_unique)) {
    tbl_tmp_sub <- dplyr::filter(input_table, substance == myvar.substances_unique[i])
    for (j in base::seq_along(myvar.location_short_unique)) {
      tbl_tmp_location <- dplyr::filter(tbl_tmp_sub, location_short == myvar.location_short_unique[j])
      tbl_tmp_location_gt_lod_lt_loq <- dplyr::filter(tbl_tmp_location, greater_than_lod == "TRUE", greater_than_loq == "FALSE")
      tbl_tmp_location_gt_lod <- dplyr::filter(tbl_tmp_location, greater_than_lod == "TRUE")
      tbl_tmp_location_gt_loq <- dplyr::filter(tbl_tmp_location, greater_than_loq == "TRUE")
      myvar.tbl_tmp_location <- base::NROW(tbl_tmp_location)
      myvar.tbl_tmp_location_gt_lod_lt_loq <- base::NROW(tbl_tmp_location_gt_lod_lt_loq)
      myvar.tbl_tmp_location_gt_lod <- base::NROW(tbl_tmp_location_gt_lod)
      myvar.tbl_tmp_location_gt_loq <- base::NROW(tbl_tmp_location_gt_loq)
      output_table[nrow(output_table) + 1,] = base::list(tbl_tmp_location$substance[1],
                                                                   myvar.location_short_unique[j],
                                                                   tbl_tmp_location$year[1],
                                                                   NROW(tbl_tmp_location),
                                                                   NROW(tbl_tmp_location_gt_lod_lt_loq),
                                                                   NROW(tbl_tmp_location_gt_lod),
                                                                   NROW(tbl_tmp_location_gt_loq),
                                                                   myvar.tbl_tmp_location_gt_lod_lt_loq / (myvar.tbl_tmp_location * 0.01),
                                                                   myvar.tbl_tmp_location_gt_lod / (myvar.tbl_tmp_location * 0.01),
                                                                   myvar.tbl_tmp_location_gt_loq / (myvar.tbl_tmp_location * 0.01))
    }
  }
  rm(i, j)
  rm(tbl_tmp_sub,
     tbl_tmp_location,
     tbl_tmp_location_gt_lod_lt_loq,
     tbl_tmp_location_gt_lod,
     tbl_tmp_location_gt_loq,
     myvar.tbl_tmp_location,
     myvar.tbl_tmp_location_gt_lod_lt_loq,
     myvar.tbl_tmp_location_gt_lod,
     myvar.tbl_tmp_location_gt_loq,
     myvar.substances_unique,
     myvar.location_short_unique)
  
  #remove first observation (NA Values)
  output_table <- output_table[-1,]
  

  base::return(output_table)
}



# create_tbl_percentage_x -------------------------------------------------

myfun.create_tbl_percentage <- function(input_table, output_table){
  
  output_table <- dplyr::select(input_table, -n_samples) %>%
    tidyr::pivot_longer(
      cols = "prct_gt_lod_lt_loq":"prct_gt_loq", 
      names_to = "percentage_type", 
      values_to = "percentage",
      values_drop_na = FALSE
    )
  
  base::return(output_table)
}



# plot_prevalence_chart_bb ---------------------------------------------------

myfun.plot_prevalence_bb <- function(fun_year){
  tmp_tbl <- dplyr::filter(tbl_results_bb, year == fun_year)
  myvar.unique_locations <- base::unique(tmp_tbl$location_short)
  for (i in seq_along(myvar.unique_locations)) {
    myvar.cur_location_short <- myvar.unique_locations[i]
    dplyr::filter(tbl_percentage_bb, location_short == myvar.unique_locations[i], percentage_type != "prct_gt_lod", percentage != 0, year == fun_year) %>%
      ggplot(mapping = aes(
        x = fct_rev(
          factor(substance,
                 levels = unique(substance[order(prevalence_gt_loq,
                                               prevalence_gt_lod_lt_loq,
                                               substance)]),
                 ordered = TRUE)),
        y = percentage,
        fill = percentage_type)) +
      geom_col() +
      scale_fill_manual(values = myvar.prct_type_colours_viridis,
                        labels = c(">LOD \n<LOQ", ">LOQ")) +
      ggtitle(paste0(myvar.unique_locations[i])) +
      theme(
        axis.text.x = element_text(
          angle = 45,
          hjust = 1,
          colour = "black",
          size = 14
        ),
        axis.text.y = element_text(size = 14, colour = "black"),
        axis.title.y = element_text(size = 16),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        plot.title = element_text(size = 20),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 13)
      ) +
      ylab("Prevalence [%]") +
      xlab("") +
      labs(fill = "") +
      scale_y_continuous(breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100),
                         expand = expansion(mult = c(0, .1)),
                         limits = c(0, 100))
    ggsave(paste0("bb_",myvar.cur_location_short, "_substance_prevalence.jpg"),
           height = 2000,
           width = 4000,
           units = "px",
           path = base::paste0("./Grafik/Beebread/Prevalence/", fun_year,"/"))
  }
  rm(i)
}



# create prevalence chart for pollen --------------------------------------

myfun.plot_prevalence_p <- function(fun_year){
  tmp_tbl <- dplyr::filter(tbl_results_p, year == fun_year)
  myvar.unique_locations <- base::unique(tmp_tbl$location_short)
  for (i in seq_along(myvar.unique_locations)) {
    myvar.cur_location_short <- myvar.unique_locations[i]
    dplyr::filter(tbl_percentage_p, location_short == myvar.unique_locations[i], percentage_type != "prct_gt_lod", percentage != 0, year == fun_year) %>%
      ggplot(mapping = aes(
        x = fct_rev(
          factor(substance,
                 levels = unique(substance[order(prevalence_gt_loq,
                                                 prevalence_gt_lod_lt_loq,
                                                 substance)]),
                 ordered = TRUE)),
        y = percentage,
        fill = percentage_type)) +
      geom_col() +
      scale_fill_manual(values = myvar.prct_type_colours_viridis,
                        labels = c(">LOD \n<LOQ", ">LOQ")) +
      ggtitle(paste0(myvar.unique_locations[i])) +
      theme(
        axis.text.x = element_text(
          angle = 45,
          hjust = 1,
          colour = "black",
          size = 14
        ),
        axis.text.y = element_text(size = 14, colour = "black"),
        axis.title.y = element_text(size = 16),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        plot.title = element_text(size = 20),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 13)
      ) +
      ylab("Prevalence [%]") +
      xlab("") +
      labs(fill = "") +
      scale_y_continuous(breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100),
                         expand = expansion(mult = c(0, .1)),
                         limits = c(0, 100))
    ggsave(paste0("p_",myvar.cur_location_short, "_substance_prevalence.jpg"),
           height = 2000,
           width = 4000,
           units = "px",
           path = base::paste0("./Grafik/Pollen/Prevalence/", fun_year,"/"))
  }
  rm(i)
}



# create prevalence chart for wax -----------------------------------------

myfun.plot_prevalence_w <- function(fun_year){
  tmp_tbl <- dplyr::filter(tbl_results_w, year == fun_year)
  myvar.unique_locations <- base::unique(tmp_tbl$location_short)
  for (i in seq_along(myvar.unique_locations)) {
    myvar.cur_location_short <- myvar.unique_locations[i]
    dplyr::filter(tbl_percentage_w, location_short == myvar.unique_locations[i], percentage_type != "prct_gt_lod", percentage != 0, year == fun_year) %>%
      ggplot(mapping = aes(
        x = fct_rev(
          factor(substance,
                 levels = unique(substance[order(prevalence_gt_loq,
                                                 prevalence_gt_lod_lt_loq,
                                                 substance)]),
                 ordered = TRUE)),
        y = percentage,
        fill = percentage_type)) +
      geom_col() +
      scale_fill_manual(values = myvar.prct_type_colours_viridis,
                        labels = c(">LOD \n<LOQ", ">LOQ")) +
      ggtitle(paste0(myvar.unique_locations[i])) +
      theme(
        axis.text.x = element_text(
          angle = 45,
          hjust = 1,
          colour = "black",
          size = 14
        ),
        axis.text.y = element_text(size = 14, colour = "black"),
        axis.title.y = element_text(size = 16),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        plot.title = element_text(size = 20),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 13)
      ) +
      ylab("Prevalence [%]") +
      xlab("") +
      labs(fill = "") +
      scale_y_continuous(breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100),
                         expand = expansion(mult = c(0, .1)),
                         limits = c(0, 100))
    ggsave(paste0("w_",myvar.cur_location_short, "_substance_prevalence.jpg"),
           height = 2000,
           width = 4000,
           units = "px",
           path = base::paste0("./Grafik/Wax/Prevalence/", fun_year,"/"))
  }
  rm(i)
}



# create prevalence chart for apistrip ------------------------------------

myfun.plot_prevalence_a <- function(fun_year){
  tmp_tbl <- dplyr::filter(tbl_results_a, year == fun_year)
  myvar.unique_locations <- base::unique(tmp_tbl$location_short)
  for (i in seq_along(myvar.unique_locations)) {
    myvar.cur_location_short <- myvar.unique_locations[i]
    dplyr::filter(tbl_percentage_a, location_short == myvar.unique_locations[i], percentage_type != "prct_gt_lod", percentage != 0, year == fun_year) %>%
      ggplot(mapping = aes(
        x = fct_rev(
          factor(substance,
                 levels = unique(substance[order(prevalence_gt_loq,
                                                 prevalence_gt_lod_lt_loq,
                                                 substance)]),
                 ordered = TRUE)),
        y = percentage,
        fill = percentage_type)) +
      geom_col() +
      scale_fill_manual(values = myvar.prct_type_colours_viridis,
                        labels = c(">LOD \n<LOQ", ">LOQ")) +
      ggtitle(paste0(myvar.unique_locations[i])) +
      theme(
        axis.text.x = element_text(
          angle = 45,
          hjust = 1,
          colour = "black",
          size = 14
        ),
        axis.text.y = element_text(size = 14, colour = "black"),
        axis.title.y = element_text(size = 16),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        plot.title = element_text(size = 20),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 13)
      ) +
      ylab("Prevalence [%]") +
      xlab("") +
      labs(fill = "") +
      scale_y_continuous(breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100),
                         expand = expansion(mult = c(0, .1)),
                         limits = c(0, 100))
    ggsave(paste0("a_",myvar.cur_location_short, "_substance_prevalence.jpg"),
           height = 2000,
           width = 4000,
           units = "px",
           path = base::paste0("./Grafik/Apistrip_L2/Prevalence/", fun_year,"/"))
  }
  rm(i)
}




# create prevalence chart for apistrip_spain ------------------------------------

myfun.plot_prevalence_a_sp <- function(fun_year){
  tmp_tbl <- dplyr::filter(tbl_results_a_sp, year == fun_year)
  myvar.unique_locations <- base::unique(tmp_tbl$location_short)
  for (i in seq_along(myvar.unique_locations)) {
    myvar.cur_location_short <- myvar.unique_locations[i]
    dplyr::filter(tbl_percentage_a_sp, location_short == myvar.unique_locations[i], percentage_type != "prct_gt_lod", percentage != 0, year == fun_year) %>%
      ggplot(mapping = aes(
        x = fct_rev(
          factor(substance,
                 levels = unique(substance[order(prevalence_gt_loq,
                                                 prevalence_gt_lod_lt_loq,
                                                 substance)]),
                 ordered = TRUE)),
        y = percentage,
        fill = percentage_type)) +
      geom_col() +
      scale_fill_manual(values = myvar.prct_type_colours_viridis,
                        labels = c(">LOD \n<LOQ", ">LOQ")) +
      ggtitle(paste0(myvar.unique_locations[i])) +
      theme(
        axis.text.x = element_text(
          angle = 45,
          hjust = 1,
          colour = "black",
          size = 14
        ),
        axis.text.y = element_text(size = 14, colour = "black"),
        axis.title.y = element_text(size = 16),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        plot.title = element_text(size = 20),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 13)
      ) +
      ylab("Prevalence [%]") +
      xlab("") +
      labs(fill = "") +
      scale_y_continuous(breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100),
                         expand = expansion(mult = c(0, .1)),
                         limits = c(0, 100))
    ggsave(paste0("a_sp_",myvar.cur_location_short, "_substance_prevalence.jpg"),
           height = 2000,
           width = 4000,
           units = "px",
           path = base::paste0("./Grafik/Apistrip_L1/Prevalence/", fun_year,"/"))
  }
  rm(i)
}



# #create standard substance graph for bb------------------------------------------

#for values grater than loq

myfun.plot_substance_gt_loq_bb <- function(fun_year, fun_location){
  tmp_tbl <- dplyr::filter(tbl_results_bb, year == fun_year, location_short == fun_location)
  myvar.tmp1_sub_unique <- base::unique(tmp_tbl$substance)
  myvar.tmp_tbl_unique_colonies <- base::unique(tmp_tbl$colony)
  myvar.tmp_labels <- myfun.create_label_vector(myvar.tmp_tbl_unique_colonies)
  myvar.tmp_colony_colours_viridis <- myfun.assign_viridis_to_vec(base::sort(myvar.tmp_tbl_unique_colonies))
  for (i in base::seq_along(myvar.tmp1_sub_unique)) {
    tmp_tbl_sub <- dplyr::filter(tbl_results_bb, year == fun_year, location_short == fun_location, substance == myvar.tmp1_sub_unique[i])
    for (j in 1:NROW(tmp_tbl_sub)) {
      if (tmp_tbl_sub$greater_than_loq[j] == FALSE) {
        tmp_tbl_sub$concentration[j] = 0
      }
    }
    myvar.tmp_date_breaks <- base::unique(tmp_tbl_sub$sample_date)
    myvar.tmp_dates_labels <- base::strftime(base::unique(tmp_tbl_sub$sample_date), format = "%d.%m.")
    myvar.tmp_max_conc <- base::max(tmp_tbl_sub$concentration)
    tmp_tbl_sub %>%
      ggplot(mapping = aes(x = sample_date,y = concentration, fill = colony)) +
      geom_col(position = position_dodge2(preserve = "single")) +
      scale_fill_manual(values = myvar.tmp_colony_colours_viridis,
                        labels = myvar.tmp_labels) +
      ggtitle(paste0(myvar.tmp1_sub_unique[i])) +
      theme(
        axis.text.x = element_text(
          angle = 30,
          hjust = 1,
          colour = "black",
          size = 14
        ),
        axis.text.y = element_text(size = 14, colour = "black"),
        axis.title.y = element_text(size = 16),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        plot.title = element_text(size = 20),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 13)
      ) +
      xlab("") +
      ylab("Conc. [\u00b5g/kg]") +
      labs(fill = "Colony") +
      scale_y_continuous(expand = expansion(mult = c(0, .1)),
                         limits = c(0, myvar.tmp_max_conc)) +
      scale_x_date(breaks = myvar.tmp_date_breaks,
                   labels = myvar.tmp_dates_labels)
    ggsave(paste0(fun_location, "_bb_plot_",myvar.tmp1_sub_unique[i] ,"_gt_loq.jpg"),
           height = 1080,
           width = 2300,
           units = "px",
           path = base::paste0("./Grafik/Beebread/Substances/", fun_year,"/", fun_location, "/greater_than_loq/"))
    
  }
}


#for values greater than lod

myfun.plot_substance_gt_lod_bb <- function(fun_year, fun_location){
  tmp_tbl <- dplyr::filter(tbl_results_bb, year == fun_year, location_short == fun_location)
  myvar.tmp1_sub_unique <- base::unique(tmp_tbl$substance)
  myvar.tmp_tbl_unique_colonies <- base::unique(tmp_tbl$colony)
  myvar.tmp_labels <- myfun.create_label_vector(myvar.tmp_tbl_unique_colonies)
  myvar.tmp_colony_colours_viridis <- myfun.assign_viridis_to_vec(base::sort(myvar.tmp_tbl_unique_colonies))
  for (i in base::seq_along(myvar.tmp1_sub_unique)) {
    tmp_tbl_sub <- dplyr::filter(tbl_results_bb, year == fun_year, location_short == fun_location, substance == myvar.tmp1_sub_unique[i])
    for (j in 1:NROW(tmp_tbl_sub)) {
      if (tmp_tbl_sub$greater_than_lod[j] == FALSE) {
        tmp_tbl_sub$concentration[j] = 0
      }
    }
    myvar.tmp_date_breaks <- base::unique(tmp_tbl_sub$sample_date)
    myvar.tmp_dates_labels <- base::strftime(base::unique(tmp_tbl_sub$sample_date), format = "%d.%m.")
    myvar.tmp_max_conc <- base::max(tmp_tbl_sub$concentration)
    tmp_tbl_sub %>%
      ggplot(mapping = aes(x = sample_date,y = concentration, fill = colony)) +
      geom_col(position = position_dodge2(preserve = "single")) +
      scale_fill_manual(values = myvar.tmp_colony_colours_viridis,
                        labels = myvar.tmp_labels) +
      ggtitle(paste0(myvar.tmp1_sub_unique[i])) +
      theme(
        axis.text.x = element_text(
          angle = 30,
          hjust = 1,
          colour = "black",
          size = 14
        ),
        axis.text.y = element_text(size = 14, colour = "black"),
        axis.title.y = element_text(size = 16),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        plot.title = element_text(size = 20),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 13)
      ) +
      xlab("") +
      ylab("Conc. [\u00b5g/kg]") +
      labs(fill = "Colony") +
      scale_y_continuous(expand = expansion(mult = c(0, .1)),
                         limits = c(0, myvar.tmp_max_conc)) +
      scale_x_date(breaks = myvar.tmp_date_breaks,
                   labels = myvar.tmp_dates_labels)
    ggsave(paste0(fun_location, "_bb_plot_",myvar.tmp1_sub_unique[i] ,"_gt_lod.jpg"),
           height = 1080,
           width = 2300,
           units = "px",
           path = base::paste0("./Grafik/Beebread/Substances/", fun_year,"/", fun_location, "/greater_than_lod/"))
    
  }
}




# create standard substance graph for p -----------------------------------

#for values grater than loq

myfun.plot_substance_gt_loq_p <- function(fun_year, fun_location){
  tmp_tbl <- dplyr::filter(tbl_results_p, year == fun_year, location_short == fun_location)
  myvar.tmp1_sub_unique <- base::unique(tmp_tbl$substance)
  myvar.tmp_tbl_unique_colonies <- base::unique(tmp_tbl$colony)
  myvar.tmp_tbl_unique_colonies <- base::sort(myvar.tmp_tbl_unique_colonies)
  myvar.tmp_labels <- myfun.create_label_vector(myvar.tmp_tbl_unique_colonies)
  myvar.tmp_colony_colours_viridis <- myfun.assign_viridis_to_vec(myvar.tmp_tbl_unique_colonies)
  for (i in base::seq_along(myvar.tmp1_sub_unique)) {
    tmp_tbl_sub <- dplyr::filter(tbl_results_p, year == fun_year, location_short == fun_location, substance == myvar.tmp1_sub_unique[i])
    for (j in 1:NROW(tmp_tbl_sub)) {
      if (tmp_tbl_sub$greater_than_loq[j] == FALSE) {
        tmp_tbl_sub$concentration[j] = 0
      }
    }
    myvar.tmp_date_breaks <- base::unique(tmp_tbl_sub$sample_date_start)
    myvar.tmp_dates_labels <- base::strftime(base::unique(tmp_tbl_sub$sample_date_start), format = "%d.%m.")
    myvar.tmp_max_conc <- base::max(tmp_tbl_sub$concentration)
    tmp_tbl_sub %>%
      ggplot(mapping = aes(x = sample_date_start,y = concentration, fill = colony)) +
      geom_col(position = position_dodge2(preserve = "single")) +
      scale_fill_manual(values = myvar.tmp_colony_colours_viridis,
                        labels = myvar.tmp_labels) +
      ggtitle(paste0(myvar.tmp1_sub_unique[i])) +
      theme(
        axis.text.x = element_text(
          angle = 30,
          hjust = 1,
          colour = "black",
          size = 14
        ),
        axis.text.y = element_text(size = 14, colour = "black"),
        axis.title.y = element_text(size = 16),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        plot.title = element_text(size = 20),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 13)
      ) +
      xlab("") +
      ylab("Conc. [\u00b5g/kg]") +
      labs(fill = "Colony") +
      scale_y_continuous(expand = expansion(mult = c(0, .1)),
                         limits = c(0, myvar.tmp_max_conc)) +
      scale_x_date(breaks = myvar.tmp_date_breaks,
                   labels = myvar.tmp_dates_labels)
    ggsave(paste0(fun_location, "_p_plot_",myvar.tmp1_sub_unique[i] ,"_gt_loq.jpg"),
           height = 1080,
           width = 2300,
           units = "px",
           path = base::paste0("./Grafik/Pollen/Substances/", fun_year,"/", fun_location, "/greater_than_loq/"))
    
  }
}


#for values greater than lod

myfun.plot_substance_gt_lod_p <- function(fun_year, fun_location){
  tmp_tbl <- dplyr::filter(tbl_results_p, year == fun_year, location_short == fun_location)
  myvar.tmp1_sub_unique <- base::unique(tmp_tbl$substance)
  myvar.tmp_tbl_unique_colonies <- base::unique(tmp_tbl$colony)
  myvar.tmp_labels <- myfun.create_label_vector(myvar.tmp_tbl_unique_colonies)
  myvar.tmp_colony_colours_viridis <- myfun.assign_viridis_to_vec(base::sort(myvar.tmp_tbl_unique_colonies))
  for (i in base::seq_along(myvar.tmp1_sub_unique)) {
    tmp_tbl_sub <- dplyr::filter(tbl_results_p, year == fun_year, location_short == fun_location, substance == myvar.tmp1_sub_unique[i])
    for (j in 1:NROW(tmp_tbl_sub)) {
      if (tmp_tbl_sub$greater_than_lod[j] == FALSE) {
        tmp_tbl_sub$concentration[j] = 0
      }
    }
    myvar.tmp_date_breaks <- base::unique(tmp_tbl_sub$sample_date_start)
    myvar.tmp_dates_labels <- base::strftime(base::unique(tmp_tbl_sub$sample_date_start), format = "%d.%m.")
    myvar.tmp_max_conc <- base::max(tmp_tbl_sub$concentration)
    tmp_tbl_sub %>%
      ggplot(mapping = aes(x = sample_date_start,y = concentration, fill = colony)) +
      geom_col(position = position_dodge2(preserve = "single")) +
      scale_fill_manual(values = myvar.tmp_colony_colours_viridis,
                        labels = myvar.tmp_labels) +
      ggtitle(paste0(myvar.tmp1_sub_unique[i])) +
      theme(
        axis.text.x = element_text(
          angle = 30,
          hjust = 1,
          colour = "black",
          size = 14
        ),
        axis.text.y = element_text(size = 14, colour = "black"),
        axis.title.y = element_text(size = 16),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        plot.title = element_text(size = 20),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 13)
      ) +
      xlab("") +
      ylab("Conc. [\u00b5g/kg]") +
      labs(fill = "Colony") +
      scale_y_continuous(expand = expansion(mult = c(0, .1)),
                         limits = c(0, myvar.tmp_max_conc)) +
      scale_x_date(breaks = myvar.tmp_date_breaks,
                   labels = myvar.tmp_dates_labels)
    ggsave(paste0(fun_location, "_p_plot_",myvar.tmp1_sub_unique[i] ,"_gt_lod.jpg"),
           height = 1080,
           width = 2300,
           units = "px",
           path = base::paste0("./Grafik/Pollen/Substances/", fun_year,"/", fun_location, "/greater_than_lod/"))
    
  }
}



# create standard substance graph for w -----------------------------------

#for values_greater than loq

myfun.plot_substance_gt_loq_w <- function(fun_year, fun_location){
  tmp_tbl <- dplyr::filter(tbl_results_w, year == fun_year, location_short == fun_location)
  myvar.tmp1_sub_unique <- base::unique(tmp_tbl$substance)
  myvar.tmp_tbl_unique_colonies <- base::unique(tmp_tbl$colony)
  myvar.tmp_labels <- myfun.create_label_vector(myvar.tmp_tbl_unique_colonies)
  myvar.tmp_colony_colours_viridis <- myfun.assign_viridis_to_vec(base::sort(myvar.tmp_tbl_unique_colonies))
  for (i in base::seq_along(myvar.tmp1_sub_unique)) {
    tmp_tbl_sub <- dplyr::filter(tbl_results_w, year == fun_year, location_short == fun_location, substance == myvar.tmp1_sub_unique[i])
    for (j in 1:NROW(tmp_tbl_sub)) {
      if (tmp_tbl_sub$greater_than_loq[j] == FALSE) {
        tmp_tbl_sub$concentration[j] = 0
      }
    }
    myvar.tmp_date_breaks <- base::unique(tmp_tbl_sub$sample_date)
    myvar.tmp_dates_labels <- base::strftime(base::unique(tmp_tbl_sub$sample_date), format = "%d.%m.")
    myvar.tmp_max_conc <- base::max(tmp_tbl_sub$concentration)
    tmp_tbl_sub %>%
      ggplot(mapping = aes(x = sample_date,y = concentration, fill = colony)) +
      geom_col(position = position_dodge2(preserve = "single")) +
      scale_fill_manual(values = myvar.tmp_colony_colours_viridis,
                        labels = myvar.tmp_labels) +
      ggtitle(paste0(myvar.tmp1_sub_unique[i])) +
      theme(
        axis.text.x = element_text(
          angle = 30,
          hjust = 1,
          colour = "black",
          size = 14
        ),
        axis.text.y = element_text(size = 14, colour = "black"),
        axis.title.y = element_text(size = 16),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        plot.title = element_text(size = 20),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 13)
      ) +
      xlab("") +
      ylab("Conc. [\u00b5g/kg]") +
      labs(fill = "Colony") +
      scale_y_continuous(expand = expansion(mult = c(0, .1)),
                         limits = c(0, myvar.tmp_max_conc)) +
      scale_x_date(breaks = myvar.tmp_date_breaks,
                   labels = myvar.tmp_dates_labels)
    ggsave(paste0(fun_location, "_w_plot_",myvar.tmp1_sub_unique[i] ,"_gt_loq.jpg"),
           height = 1080,
           width = 2300,
           units = "px",
           path = base::paste0("./Grafik/Wax/Substances/", fun_year,"/", fun_location, "/greater_than_loq/"))
    
  }
}


#for values greater than lod

myfun.plot_substance_gt_lod_w <- function(fun_year, fun_location){
  tmp_tbl <- dplyr::filter(tbl_results_w, year == fun_year, location_short == fun_location)
  myvar.tmp1_sub_unique <- base::unique(tmp_tbl$substance)
  myvar.tmp_tbl_unique_colonies <- base::unique(tmp_tbl$colony)
  myvar.tmp_labels <- myfun.create_label_vector(myvar.tmp_tbl_unique_colonies)
  myvar.tmp_colony_colours_viridis <- myfun.assign_viridis_to_vec(base::sort(myvar.tmp_tbl_unique_colonies))
  for (i in base::seq_along(myvar.tmp1_sub_unique)) {
    tmp_tbl_sub <- dplyr::filter(tbl_results_w, year == fun_year, location_short == fun_location, substance == myvar.tmp1_sub_unique[i])
    for (j in 1:NROW(tmp_tbl_sub)) {
      if (tmp_tbl_sub$greater_than_lod[j] == FALSE) {
        tmp_tbl_sub$concentration[j] = 0
      }
    }
    myvar.tmp_date_breaks <- base::unique(tmp_tbl_sub$sample_date)
    myvar.tmp_dates_labels <- base::strftime(base::unique(tmp_tbl_sub$sample_date), format = "%d.%m.")
    myvar.tmp_max_conc <- base::max(tmp_tbl_sub$concentration)
    tmp_tbl_sub %>%
      ggplot(mapping = aes(x = sample_date,y = concentration, fill = colony)) +
      geom_col(position = position_dodge2(preserve = "single")) +
      scale_fill_manual(values = myvar.tmp_colony_colours_viridis,
                        labels = myvar.tmp_labels) +
      ggtitle(paste0(myvar.tmp1_sub_unique[i])) +
      theme(
        axis.text.x = element_text(
          angle = 30,
          hjust = 1,
          colour = "black",
          size = 14
        ),
        axis.text.y = element_text(size = 14, colour = "black"),
        axis.title.y = element_text(size = 16),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        plot.title = element_text(size = 20),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 13)
      ) +
      xlab("") +
      ylab("Conc. [\u00b5g/kg]") +
      labs(fill = "Colony") +
      scale_y_continuous(expand = expansion(mult = c(0, .1)),
                         limits = c(0, myvar.tmp_max_conc)) +
      scale_x_date(breaks = myvar.tmp_date_breaks,
                   labels = myvar.tmp_dates_labels)
    ggsave(paste0(fun_location, "_w_plot_",myvar.tmp1_sub_unique[i] ,"_gt_loq.jpg"),
           height = 1080,
           width = 2300,
           units = "px",
           path = base::paste0("./Grafik/Wax/Substances/", fun_year,"/", fun_location, "/greater_than_lod/"))
    
  }
}



# create standard substance chart for apistrip ----------------------------

#for values grater than loq

myfun.plot_substance_gt_loq_a <- function(fun_year, fun_location){
  tmp_tbl <- dplyr::filter(tbl_results_a, year == fun_year, location_short == fun_location)
  myvar.tmp1_sub_unique <- base::unique(tmp_tbl$substance)
  myvar.tmp_tbl_unique_colonies <- base::unique(tmp_tbl$colony)
  myvar.tmp_labels <- myfun.create_label_vector(myvar.tmp_tbl_unique_colonies)
  myvar.tmp_colony_colours_viridis <- myfun.assign_viridis_to_vec(base::sort(myvar.tmp_tbl_unique_colonies))
  for (i in base::seq_along(myvar.tmp1_sub_unique)) {
    tmp_tbl_sub <- dplyr::filter(tbl_results_a, year == fun_year, location_short == fun_location, substance == myvar.tmp1_sub_unique[i])
    for (j in 1:NROW(tmp_tbl_sub)) {
      if (tmp_tbl_sub$greater_than_loq[j] == FALSE) {
        tmp_tbl_sub$concentration[j] = 0
      }
    }
    myvar.tmp_date_breaks <- base::unique(tmp_tbl_sub$sample_date)
    myvar.tmp_dates_labels <- base::strftime(base::unique(tmp_tbl_sub$sample_date), format = "%d.%m.")
    myvar.tmp_max_conc <- base::max(tmp_tbl_sub$concentration)
    tmp_tbl_sub %>%
      ggplot(mapping = aes(x = sample_date,y = concentration, fill = colony)) +
      geom_col(position = position_dodge2(preserve = "single")) +
      scale_fill_manual(values = myvar.tmp_colony_colours_viridis,
                        labels = myvar.tmp_labels) +
      ggtitle(paste0(myvar.tmp1_sub_unique[i])) +
      theme(
        axis.text.x = element_text(
          angle = 30,
          hjust = 1,
          colour = "black",
          size = 14
        ),
        axis.text.y = element_text(size = 14, colour = "black"),
        axis.title.y = element_text(size = 16),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        plot.title = element_text(size = 20),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 13)
      ) +
      xlab("") +
      ylab("Conc. [ng/Strip]") +
      labs(fill = "Colony") +
      scale_y_continuous(expand = expansion(mult = c(0, .1)),
                         limits = c(0, myvar.tmp_max_conc)) +
      scale_x_date(breaks = myvar.tmp_date_breaks,
                   labels = myvar.tmp_dates_labels)
    ggsave(paste0(fun_location, "_a_plot_",myvar.tmp1_sub_unique[i] ,"_gt_loq.jpg"),
           height = 1080,
           width = 2300,
           units = "px",
           path = base::paste0("./Grafik/Apistrip_L2/Substances/", fun_year,"/", fun_location, "/greater_than_loq/"))
    
  }
}


#for values grater than lod

myfun.plot_substance_gt_lod_a <- function(fun_year, fun_location){
  tmp_tbl <- dplyr::filter(tbl_results_a, year == fun_year, location_short == fun_location)
  myvar.tmp1_sub_unique <- base::unique(tmp_tbl$substance)
  myvar.tmp_tbl_unique_colonies <- base::unique(tmp_tbl$colony)
  myvar.tmp_labels <- myfun.create_label_vector(myvar.tmp_tbl_unique_colonies)
  myvar.tmp_colony_colours_viridis <- myfun.assign_viridis_to_vec(base::sort(myvar.tmp_tbl_unique_colonies))
  for (i in base::seq_along(myvar.tmp1_sub_unique)) {
    tmp_tbl_sub <- dplyr::filter(tbl_results_a, year == fun_year, location_short == fun_location, substance == myvar.tmp1_sub_unique[i])
    for (j in 1:NROW(tmp_tbl_sub)) {
      if (tmp_tbl_sub$greater_than_lod[j] == FALSE) {
        tmp_tbl_sub$concentration[j] = 0
      }
    }
    myvar.tmp_date_breaks <- base::unique(tmp_tbl_sub$sample_date)
    myvar.tmp_dates_labels <- base::strftime(base::unique(tmp_tbl_sub$sample_date), format = "%d.%m.")
    myvar.tmp_max_conc <- base::max(tmp_tbl_sub$concentration)
    tmp_tbl_sub %>%
      ggplot(mapping = aes(x = sample_date,y = concentration, fill = colony)) +
      geom_col(position = position_dodge2(preserve = "single")) +
      scale_fill_manual(values = myvar.tmp_colony_colours_viridis,
                        labels = myvar.tmp_labels) +
      ggtitle(paste0(myvar.tmp1_sub_unique[i])) +
      theme(
        axis.text.x = element_text(
          angle = 30,
          hjust = 1,
          colour = "black",
          size = 14
        ),
        axis.text.y = element_text(size = 14, colour = "black"),
        axis.title.y = element_text(size = 16),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        plot.title = element_text(size = 20),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 13)
      ) +
      xlab("") +
      ylab("Conc. [ng/Strip]") +
      labs(fill = "Colony") +
      scale_y_continuous(expand = expansion(mult = c(0, .1)),
                         limits = c(0, myvar.tmp_max_conc)) +
      scale_x_date(breaks = myvar.tmp_date_breaks,
                   labels = myvar.tmp_dates_labels)
    ggsave(paste0(fun_location, "_a_plot_",myvar.tmp1_sub_unique[i] ,"_gt_lod.jpg"),
           height = 1080,
           width = 2300,
           units = "px",
           path = base::paste0("./Grafik/Apistrip_L2/Substances/", fun_year,"/", fun_location, "/greater_than_lod/"))
    
  }
}




# create standard substance chart for apistrip_spain ----------------------------

#for values grater than loq

myfun.plot_substance_gt_loq_a_sp <- function(fun_year, fun_location){
  tmp_tbl <- dplyr::filter(tbl_results_a_sp, year == fun_year, location_short == fun_location)
  myvar.tmp1_sub_unique <- base::unique(tmp_tbl$substance)
  myvar.tmp_tbl_unique_colonies <- base::unique(tmp_tbl$colony)
  myvar.tmp_labels <- myfun.create_label_vector(myvar.tmp_tbl_unique_colonies)
  myvar.tmp_colony_colours_viridis <- myfun.assign_viridis_to_vec(base::sort(myvar.tmp_tbl_unique_colonies))
  for (i in base::seq_along(myvar.tmp1_sub_unique)) {
    tmp_tbl_sub <- dplyr::filter(tbl_results_a_sp, year == fun_year, location_short == fun_location, substance == myvar.tmp1_sub_unique[i])
    for (j in 1:NROW(tmp_tbl_sub)) {
      if (tmp_tbl_sub$greater_than_loq[j] == FALSE) {
        tmp_tbl_sub$concentration[j] = 0
      }
    }
    myvar.tmp_date_breaks <- base::unique(tmp_tbl_sub$sample_date)
    myvar.tmp_dates_labels <- base::strftime(base::unique(tmp_tbl_sub$sample_date), format = "%d.%m.")
    myvar.tmp_max_conc <- base::max(tmp_tbl_sub$concentration)
    tmp_tbl_sub %>%
      ggplot(mapping = aes(x = sample_date,y = concentration, fill = colony)) +
      geom_col(position = position_dodge2(preserve = "single")) +
      scale_fill_manual(values = myvar.tmp_colony_colours_viridis,
                        labels = myvar.tmp_labels) +
      ggtitle(paste0(myvar.tmp1_sub_unique[i])) +
      theme(
        axis.text.x = element_text(
          angle = 30,
          hjust = 1,
          colour = "black",
          size = 14
        ),
        axis.text.y = element_text(size = 14, colour = "black"),
        axis.title.y = element_text(size = 16),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        plot.title = element_text(size = 20),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 13)
      ) +
      xlab("") +
      ylab("Conc. [ng/Strip]") +
      labs(fill = "Colony") +
      scale_y_continuous(expand = expansion(mult = c(0, .1)),
                         limits = c(0, myvar.tmp_max_conc)) +
      scale_x_date(breaks = myvar.tmp_date_breaks,
                   labels = myvar.tmp_dates_labels)
    ggsave(paste0(fun_location, "_a_sp_plot_",myvar.tmp1_sub_unique[i] ,"_gt_loq.jpg"),
           height = 1080,
           width = 2300,
           units = "px",
           path = base::paste0("./Grafik/Apistrip_L1/Substances/", fun_year,"/", fun_location, "/greater_than_loq/"))
    
  }
}




# plot mean chart for bb -------------------------------------------------------------

myfun.plot_avg_bb <- function(fun_year, fun_location1, fun_location2, fun_location3, fun_location4, fun_location5){
  tbl_tmp <- dplyr::filter(tbl_avg_cum_bb, year == fun_year, location_short == fun_location1 | location_short == fun_location2 | location_short == fun_location3 | location_short == fun_location4 | location_short == fun_location5)
  myvar.tmp_tbl_unique_location_short <- base::unique(tbl_tmp$location_short)
  myvar.tmp_labels_location <- base::sort(base::unique(tbl_tmp$location))
  myvar.tmp_location_short_colours_viridis <- myfun.assign_viridis_to_vec(base::sort(myvar.tmp_tbl_unique_location_short))
  myvar.tmp_substances <- base::unique(tbl_tmp$substance)
  myvar.tmp_date_breaks <- base::unique(tbl_tmp$sample_date)
  myvar.tmp_dates_labels <- base::strftime(base::unique(tbl_tmp$sample_date), format = "%d.%m.")
  
  for (i in base::seq_along(myvar.tmp_substances)) {
    tbl_tmp_cur_sub <- dplyr::filter(tbl_avg_cum_bb, year == fun_year, substance == myvar.tmp_substances[i], location_short == fun_location1 | location_short == fun_location2 | location_short == fun_location3 | location_short == fun_location4 | location_short == fun_location5)
    tbl_tmp_cur_sub %>%
      ggplot(mapping = aes(x = sample_date,y = concentration_avg, fill = location_short)) +
      geom_col(position = position_dodge2(preserve = "single")) +
      scale_fill_manual(values = myvar.tmp_location_short_colours_viridis,
                        labels = myvar.tmp_labels_location) +
      ggtitle(paste0(myvar.tmp_substances[i])) +
      theme(
        axis.text.x = element_text(
          angle = 30,
          hjust = 1,
          colour = "black",
          size = 14
        ),
        axis.text.y = element_text(size = 14, colour = "black"),
        axis.title.y = element_text(size = 16),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        plot.title = element_text(size = 20),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 13)
      ) +
      labs(fill = "Location") +
      xlab("") +
      ylab("Conc. [\u00b5g/kg]") +
      scale_y_continuous(expand = expansion(mult = c(0, .1))) +
      scale_x_date(breaks = myvar.tmp_date_breaks,
                   labels = myvar.tmp_dates_labels)
    ggsave(paste0("bb_location_avg_",myvar.tmp_substances[i],"_gt_loq.jpg"),
           height = 1080,
           width = 2800,
           units = "px",
           path = paste0("./Grafik/Beebread/Mean/", fun_year,"/"))
  }
  rm(i)
}



# plot mean chart for p --------------------------------------------------------------

myfun.plot_avg_p <- function(fun_year, fun_location1, fun_location2, fun_location3, fun_location4, fun_location5){
  tbl_tmp <- dplyr::filter(tbl_avg_cum_p, year == fun_year, location_short == fun_location1 | location_short == fun_location2 | location_short == fun_location3 | location_short == fun_location4 | location_short == fun_location5)
  myvar.tmp_tbl_unique_location_short <- base::unique(tbl_tmp$location_short)
  myvar.tmp_labels_location <- base::sort(base::unique(tbl_tmp$location))
  myvar.tmp_location_short_colours_viridis <- myfun.assign_viridis_to_vec(base::sort(myvar.tmp_tbl_unique_location_short))
  myvar.tmp_substances <- base::unique(tbl_tmp$substance)
  myvar.tmp_date_breaks <- base::unique(tbl_tmp$sample_date)
  myvar.tmp_dates_labels <- base::strftime(base::unique(tbl_tmp$sample_date), format = "%d.%m.")
  
  for (i in base::seq_along(myvar.tmp_substances)) {
    tbl_tmp_cur_sub <- dplyr::filter(tbl_avg_cum_p, year == fun_year, substance == myvar.tmp_substances[i], location_short == fun_location1 | location_short == fun_location2 | location_short == fun_location3 | location_short == fun_location4 | location_short == fun_location5)
    tbl_tmp_cur_sub %>%
      ggplot(mapping = aes(x = sample_date,y = concentration_avg, fill = location_short)) +
      geom_col(position = position_dodge2(preserve = "single")) +
      scale_fill_manual(values = myvar.tmp_location_short_colours_viridis,
                        labels = myvar.tmp_labels_location) +
      ggtitle(paste0(myvar.tmp_substances[i])) +
      theme(
        axis.text.x = element_text(
          angle = 30,
          hjust = 1,
          colour = "black",
          size = 14
        ),
        axis.text.y = element_text(size = 14, colour = "black"),
        axis.title.y = element_text(size = 16),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        plot.title = element_text(size = 20),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 13)
      ) +
      labs(fill = "Location") +
      xlab("") +
      ylab("Conc. [\u00b5g/kg]") +
      scale_y_continuous(expand = expansion(mult = c(0, .1))) +
      scale_x_date(breaks = myvar.tmp_date_breaks,
                   labels = myvar.tmp_dates_labels)
    ggsave(paste0("p_location_avg_",myvar.tmp_substances[i],"_gt_loq.jpg"),
           height = 1080,
           width = 2800,
           units = "px",
           path = paste0("./Grafik/Pollen/Mean/", fun_year,"/"))
  }
  rm(i)
}



# create mean chart for w -----------------------------------------------------

myfun.plot_avg_w <- function(fun_year, fun_location1, fun_location2, fun_location3, fun_location4, fun_location5){
  tbl_tmp <- dplyr::filter(tbl_avg_cum_w, year == fun_year, location_short == fun_location1 | location_short == fun_location2 | location_short == fun_location3 | location_short == fun_location4 | location_short == fun_location5)
  myvar.tmp_tbl_unique_location_short <- base::unique(tbl_tmp$location_short)
  myvar.tmp_labels_location <- base::sort(base::unique(tbl_tmp$location))
  myvar.tmp_location_short_colours_viridis <- myfun.assign_viridis_to_vec(base::sort(myvar.tmp_tbl_unique_location_short))
  myvar.tmp_substances <- base::unique(tbl_tmp$substance)
  myvar.tmp_date_breaks <- base::unique(tbl_tmp$sample_date)
  myvar.tmp_dates_labels <- base::strftime(base::unique(tbl_tmp$sample_date), format = "%d.%m.")
  
  for (i in base::seq_along(myvar.tmp_substances)) {
    tbl_tmp_cur_sub <- dplyr::filter(tbl_avg_cum_w, year == fun_year, substance == myvar.tmp_substances[i], location_short == fun_location1 | location_short == fun_location2 | location_short == fun_location3 | location_short == fun_location4 | location_short == fun_location5)
    tbl_tmp_cur_sub %>%
      ggplot(mapping = aes(x = sample_date,y = concentration_avg, fill = location_short)) +
      geom_col(position = position_dodge2(preserve = "single")) +
      scale_fill_manual(values = myvar.tmp_location_short_colours_viridis,
                        labels = myvar.tmp_labels_location) +
      ggtitle(paste0(myvar.tmp_substances[i])) +
      theme(
        axis.text.x = element_text(
          angle = 30,
          hjust = 1,
          colour = "black",
          size = 14
        ),
        axis.text.y = element_text(size = 14, colour = "black"),
        axis.title.y = element_text(size = 16),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        plot.title = element_text(size = 20),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 13)
      ) +
      labs(fill = "Location") +
      xlab("") +
      ylab("Conc. [\u00b5g/kg]") +
      scale_y_continuous(expand = expansion(mult = c(0, .1))) +
      scale_x_date(breaks = myvar.tmp_date_breaks,
                   labels = myvar.tmp_dates_labels)
    ggsave(paste0("p_location_avg_",myvar.tmp_substances[i],"_gt_loq.jpg"),
           height = 1080,
           width = 2800,
           units = "px",
           path = paste0("./Grafik/Wax/Mean/", fun_year,"/"))
  }
  rm(i)
}



# create mean chart for a ---------------------------------------------------

myfun.plot_avg_a <- function(fun_year, fun_location1, fun_location2, fun_location3, fun_location4, fun_location5){
  tbl_tmp <- dplyr::filter(tbl_avg_cum_a, year == fun_year, location_short == fun_location1 | location_short == fun_location2 | location_short == fun_location3 | location_short == fun_location4 | location_short == fun_location5)
  myvar.tmp_tbl_unique_location_short <- base::unique(tbl_tmp$location_short)
  myvar.tmp_labels_location <- base::sort(base::unique(tbl_tmp$location))
  myvar.tmp_location_short_colours_viridis <- myfun.assign_viridis_to_vec(base::sort(myvar.tmp_tbl_unique_location_short))
  myvar.tmp_substances <- base::unique(tbl_tmp$substance)
  myvar.tmp_date_breaks <- base::unique(tbl_tmp$sample_date)
  myvar.tmp_dates_labels <- base::strftime(base::unique(tbl_tmp$sample_date), format = "%d.%m.")
  
  for (i in base::seq_along(myvar.tmp_substances)) {
    tbl_tmp_cur_sub <- dplyr::filter(tbl_avg_cum_a, year == fun_year, substance == myvar.tmp_substances[i], location_short == fun_location1 | location_short == fun_location2 | location_short == fun_location3 | location_short == fun_location4 | location_short == fun_location5)
    tbl_tmp_cur_sub %>%
      ggplot(mapping = aes(x = sample_date,y = concentration_avg, fill = location_short)) +
      geom_col(position = position_dodge2(preserve = "single")) +
      scale_fill_manual(values = myvar.tmp_location_short_colours_viridis,
                        labels = myvar.tmp_labels_location) +
      ggtitle(paste0(myvar.tmp_substances[i])) +
      theme(
        axis.text.x = element_text(
          angle = 30,
          hjust = 1,
          colour = "black",
          size = 14
        ),
        axis.text.y = element_text(size = 14, colour = "black"),
        axis.title.y = element_text(size = 16),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        plot.title = element_text(size = 20),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 13)
      ) +
      labs(fill = "Location") +
      xlab("") +
      ylab("Conc. [ng/Strip]") +
      scale_y_continuous(expand = expansion(mult = c(0, .1))) +
      scale_x_date(breaks = myvar.tmp_date_breaks,
                   labels = myvar.tmp_dates_labels)
    ggsave(paste0("a_location_avg_",myvar.tmp_substances[i],"_gt_loq.jpg"),
           height = 1080,
           width = 2800,
           units = "px",
           path = paste0("./Grafik/Apistrip_L2/Mean/", fun_year,"/"))
  }
  rm(i)
}




# create mean chart for a_sp ---------------------------------------------------

myfun.plot_avg_a_sp <- function(fun_year, fun_location1, fun_location2, fun_location3, fun_location4, fun_location5){
  tbl_tmp <- dplyr::filter(tbl_avg_cum_a_sp, year == fun_year, location_short == fun_location1 | location_short == fun_location2 | location_short == fun_location3 | location_short == fun_location4 | location_short == fun_location5)
  myvar.tmp_tbl_unique_location_short <- base::unique(tbl_tmp$location_short)
  myvar.tmp_labels_location <- base::sort(base::unique(tbl_tmp$location))
  myvar.tmp_location_short_colours_viridis <- myfun.assign_viridis_to_vec(base::sort(myvar.tmp_tbl_unique_location_short))
  myvar.tmp_substances <- base::unique(tbl_tmp$substance)
  myvar.tmp_date_breaks <- base::unique(tbl_tmp$sample_date)
  myvar.tmp_dates_labels <- base::strftime(base::unique(tbl_tmp$sample_date), format = "%d.%m.")
  
  for (i in base::seq_along(myvar.tmp_substances)) {
    tbl_tmp_cur_sub <- dplyr::filter(tbl_avg_cum_a_sp, year == fun_year, substance == myvar.tmp_substances[i], location_short == fun_location1 | location_short == fun_location2 | location_short == fun_location3 | location_short == fun_location4 | location_short == fun_location5)
    tbl_tmp_cur_sub %>%
      ggplot(mapping = aes(x = sample_date,y = concentration_avg, fill = location_short)) +
      geom_col(position = position_dodge2(preserve = "single")) +
      scale_fill_manual(values = myvar.tmp_location_short_colours_viridis,
                        labels = myvar.tmp_labels_location) +
      ggtitle(paste0(myvar.tmp_substances[i])) +
      theme(
        axis.text.x = element_text(
          angle = 30,
          hjust = 1,
          colour = "black",
          size = 14
        ),
        axis.text.y = element_text(size = 14, colour = "black"),
        axis.title.y = element_text(size = 16),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        plot.title = element_text(size = 20),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 13)
      ) +
      labs(fill = "Location") +
      xlab("") +
      ylab("Conc. [ng/Strip]") +
      scale_y_continuous(expand = expansion(mult = c(0, .1))) +
      scale_x_date(breaks = myvar.tmp_date_breaks,
                   labels = myvar.tmp_dates_labels)
    ggsave(paste0("a_sp_location_avg_",myvar.tmp_substances[i],"_gt_loq.jpg"),
           height = 1080,
           width = 2800,
           units = "px",
           path = paste0("./Grafik/Apistrip_L1/Mean/", fun_year,"/"))
  }
  rm(i)
}



# create cumulative chart for bb --------------------------------------------------

myfun.plot_cum_bb <- function(fun_year, fun_location1, fun_location2, fun_location3, fun_location4, fun_location5){
  tbl_tmp <- dplyr::filter(tbl_avg_cum_bb, year == fun_year, location_short == fun_location1 | location_short == fun_location2 | location_short == fun_location3 | location_short == fun_location4 | location_short == fun_location5)
  myvar.tmp_tbl_unique_location_short <- base::unique(tbl_tmp$location_short)
  myvar.tmp_labels_location <- base::sort(base::unique(tbl_tmp$location))
  myvar.tmp_location_short_colours_viridis <- myfun.assign_viridis_to_vec(base::sort(myvar.tmp_tbl_unique_location_short))
  myvar.tmp_substances <- base::unique(tbl_tmp$substance)
  myvar.tmp_date_breaks <- base::unique(tbl_tmp$sample_date)
  myvar.tmp_dates_labels <- base::strftime(base::unique(tbl_tmp$sample_date), format = "%d.%m.")
  
  for (i in base::seq_along(myvar.tmp_substances)) {
    tbl_tmp_cur_sub <- dplyr::filter(tbl_avg_cum_bb, year == fun_year, substance == myvar.tmp_substances[i], location_short == fun_location1 | location_short == fun_location2 | location_short == fun_location3 | location_short == fun_location4 | location_short == fun_location5)
    tbl_tmp_cur_sub %>%
      ggplot(mapping = aes(x = sample_date,y = concentration_cum, fill = location_short)) +
      geom_col(position = position_dodge2(preserve = "single")) +
      scale_fill_manual(values = myvar.tmp_location_short_colours_viridis,
                        labels = myvar.tmp_labels_location) +
      ggtitle(paste0(myvar.tmp_substances[i])) +
      theme(
        axis.text.x = element_text(
          angle = 30,
          hjust = 1,
          colour = "black",
          size = 14
        ),
        axis.text.y = element_text(size = 14, colour = "black"),
        axis.title.y = element_text(size = 16),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        plot.title = element_text(size = 20),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 13)
      ) +
      labs(fill = "Location") +
      xlab("") +
      ylab("Conc. [\u00b5g/kg]") +
      scale_y_continuous(expand = expansion(mult = c(0, .1))) +
      scale_x_date(breaks = myvar.tmp_date_breaks,
                   labels = myvar.tmp_dates_labels)
    ggsave(paste0("bb_location_cum_",myvar.tmp_substances[i],"_gt_loq.jpg"),
           height = 1080,
           width = 2800,
           units = "px",
           path = paste0("./Grafik/Beebread/Cumulative/", fun_year,"/"))
  }
  rm(i)
}



# create cumulative chart for p ---------------------------------------------------

myfun.plot_cum_p <- function(fun_year, fun_location1, fun_location2, fun_location3, fun_location4, fun_location5){
  tbl_tmp <- dplyr::filter(tbl_avg_cum_p, year == fun_year, location_short == fun_location1 | location_short == fun_location2 | location_short == fun_location3 | location_short == fun_location4 | location_short == fun_location5)
  myvar.tmp_tbl_unique_location_short <- base::unique(tbl_tmp$location_short)
  myvar.tmp_labels_location <- base::sort(base::unique(tbl_tmp$location))
  myvar.tmp_location_short_colours_viridis <- myfun.assign_viridis_to_vec(base::sort(myvar.tmp_tbl_unique_location_short))
  myvar.tmp_substances <- base::unique(tbl_tmp$substance)
  myvar.tmp_date_breaks <- base::unique(tbl_tmp$sample_date)
  myvar.tmp_dates_labels <- base::strftime(base::unique(tbl_tmp$sample_date), format = "%d.%m.")
  
  for (i in base::seq_along(myvar.tmp_substances)) {
    tbl_tmp_cur_sub <- dplyr::filter(tbl_avg_cum_p, year == fun_year, substance == myvar.tmp_substances[i], location_short == fun_location1 | location_short == fun_location2 | location_short == fun_location3 | location_short == fun_location4 | location_short == fun_location5)
    tbl_tmp_cur_sub %>%
      ggplot(mapping = aes(x = sample_date,y = concentration_cum, fill = location_short)) +
      geom_col(position = position_dodge2(preserve = "single")) +
      scale_fill_manual(values = myvar.tmp_location_short_colours_viridis,
                        labels = myvar.tmp_labels_location) +
      ggtitle(paste0(myvar.tmp_substances[i])) +
      theme(
        axis.text.x = element_text(
          angle = 30,
          hjust = 1,
          colour = "black",
          size = 14
        ),
        axis.text.y = element_text(size = 14, colour = "black"),
        axis.title.y = element_text(size = 16),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        plot.title = element_text(size = 20),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 13)
      ) +
      labs(fill = "Location") +
      xlab("") +
      ylab("Conc. [\u00b5g/kg]") +
      scale_y_continuous(expand = expansion(mult = c(0, .1))) +
      scale_x_date(breaks = myvar.tmp_date_breaks,
                   labels = myvar.tmp_dates_labels)
    ggsave(paste0("p_location_cum_",myvar.tmp_substances[i],"_gt_loq.jpg"),
           height = 1080,
           width = 2800,
           units = "px",
           path = paste0("./Grafik/Pollen/Cumulative/", fun_year,"/"))
  }
  rm(i)
}



# create cumulative chart for w ---------------------------------------------------

myfun.plot_cum_w <- function(fun_year, fun_location1, fun_location2, fun_location3, fun_location4, fun_location5){
  tbl_tmp <- dplyr::filter(tbl_avg_cum_w, year == fun_year, location_short == fun_location1 | location_short == fun_location2 | location_short == fun_location3 | location_short == fun_location4 | location_short == fun_location5)
  myvar.tmp_tbl_unique_location_short <- base::unique(tbl_tmp$location_short)
  myvar.tmp_labels_location <- base::sort(base::unique(tbl_tmp$location))
  myvar.tmp_location_short_colours_viridis <- myfun.assign_viridis_to_vec(base::sort(myvar.tmp_tbl_unique_location_short))
  myvar.tmp_substances <- base::unique(tbl_tmp$substance)
  myvar.tmp_date_breaks <- base::unique(tbl_tmp$sample_date)
  myvar.tmp_dates_labels <- base::strftime(base::unique(tbl_tmp$sample_date), format = "%d.%m.")
  
  for (i in base::seq_along(myvar.tmp_substances)) {
    tbl_tmp_cur_sub <- dplyr::filter(tbl_avg_cum_w, year == fun_year, substance == myvar.tmp_substances[i], location_short == fun_location1 | location_short == fun_location2 | location_short == fun_location3 | location_short == fun_location4 | location_short == fun_location5)
    tbl_tmp_cur_sub %>%
      ggplot(mapping = aes(x = sample_date,y = concentration_cum, fill = location_short)) +
      geom_col(position = position_dodge2(preserve = "single")) +
      scale_fill_manual(values = myvar.tmp_location_short_colours_viridis ,
                        labels = myvar.tmp_labels_location) +
      ggtitle(paste0(myvar.tmp_substances[i])) +
      theme(
        axis.text.x = element_text(
          angle = 30,
          hjust = 1,
          colour = "black",
          size = 14
        ),
        axis.text.y = element_text(size = 14, colour = "black"),
        axis.title.y = element_text(size = 16),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        plot.title = element_text(size = 20),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 13)
      ) +
      labs(fill = "Location") +
      xlab("") +
      ylab("Conc. [\u00b5g/kg]") +
      scale_y_continuous(expand = expansion(mult = c(0, .1))) +
      scale_x_date(breaks = myvar.tmp_date_breaks,
                   labels = myvar.tmp_dates_labels)
    ggsave(paste0("w_location_cum_",myvar.tmp_substances[i],"_gt_loq.jpg"),
           height = 1080,
           width = 2800,
           units = "px",
           path = paste0("./Grafik/Wax/Cumulative/", fun_year,"/"))
  }
  rm(i)
}



# create cumulative chart for a --------------------------------------------

myfun.plot_cum_a <- function(fun_year, fun_location1, fun_location2, fun_location3, fun_location4, fun_location5){
  tbl_tmp <- dplyr::filter(tbl_avg_cum_a, year == fun_year, location_short == fun_location1 | location_short == fun_location2 | location_short == fun_location3 | location_short == fun_location4 | location_short == fun_location5)
  myvar.tmp_tbl_unique_location_short <- base::unique(tbl_tmp$location_short)
  myvar.tmp_labels_location <- base::sort(base::unique(tbl_tmp$location))
  myvar.tmp_location_short_colours_viridis <- myfun.assign_viridis_to_vec(base::sort(myvar.tmp_tbl_unique_location_short))
  myvar.tmp_substances <- base::unique(tbl_tmp$substance)
  myvar.tmp_date_breaks <- base::unique(tbl_tmp$sample_date)
  myvar.tmp_dates_labels <- base::strftime(base::unique(tbl_tmp$sample_date), format = "%d.%m.")
  
  for (i in base::seq_along(myvar.tmp_substances)) {
    tbl_tmp_cur_sub <- dplyr::filter(tbl_avg_cum_a, year == fun_year, substance == myvar.tmp_substances[i], location_short == fun_location1 | location_short == fun_location2 | location_short == fun_location3 | location_short == fun_location4 | location_short == fun_location5)
    tbl_tmp_cur_sub %>%
      ggplot(mapping = aes(x = sample_date,y = concentration_cum, fill = location_short)) +
      geom_col(position = position_dodge2(preserve = "single")) +
      scale_fill_manual(values = myvar.tmp_location_short_colours_viridis,
                        labels = myvar.tmp_labels_location) +
      ggtitle(paste0(myvar.tmp_substances[i])) +
      theme(
        axis.text.x = element_text(
          angle = 30,
          hjust = 1,
          colour = "black",
          size = 14
        ),
        axis.text.y = element_text(size = 14, colour = "black"),
        axis.title.y = element_text(size = 16),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        plot.title = element_text(size = 20),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 13)
      ) +
      labs(fill = "Location") +
      xlab("") +
      ylab("Conc. [ng/Strip]") +
      scale_y_continuous(expand = expansion(mult = c(0, .1))) +
      scale_x_date(breaks = myvar.tmp_date_breaks,
                   labels = myvar.tmp_dates_labels)
    ggsave(paste0("a_location_cum_",myvar.tmp_substances[i],"_gt_loq.jpg"),
           height = 1080,
           width = 2800,
           units = "px",
           path = paste0("./Grafik/Apistrip_L2/Cumulative/", fun_year,"/"))
  }
  rm(i)
}





# create cumulative chart for a_sp --------------------------------------------

myfun.plot_cum_a_sp <- function(fun_year, fun_location1, fun_location2, fun_location3, fun_location4, fun_location5){
  tbl_tmp <- dplyr::filter(tbl_avg_cum_a_sp, year == fun_year, location_short == fun_location1 | location_short == fun_location2 | location_short == fun_location3 | location_short == fun_location4 | location_short == fun_location5)
  myvar.tmp_tbl_unique_location_short <- base::unique(tbl_tmp$location_short)
  myvar.tmp_labels_location <- base::sort(base::unique(tbl_tmp$location))
  myvar.tmp_location_short_colours_viridis <- myfun.assign_viridis_to_vec(base::sort(myvar.tmp_tbl_unique_location_short))
  myvar.tmp_substances <- base::unique(tbl_tmp$substance)
  myvar.tmp_date_breaks <- base::unique(tbl_tmp$sample_date)
  myvar.tmp_dates_labels <- base::strftime(base::unique(tbl_tmp$sample_date), format = "%d.%m.")
  
  for (i in base::seq_along(myvar.tmp_substances)) {
    tbl_tmp_cur_sub <- dplyr::filter(tbl_avg_cum_a_sp, year == fun_year, substance == myvar.tmp_substances[i], location_short == fun_location1 | location_short == fun_location2 | location_short == fun_location3 | location_short == fun_location4 | location_short == fun_location5)
    tbl_tmp_cur_sub %>%
      ggplot(mapping = aes(x = sample_date,y = concentration_cum, fill = location_short)) +
      geom_col(position = position_dodge2(preserve = "single")) +
      scale_fill_manual(values = myvar.tmp_location_short_colours_viridis,
                        labels = myvar.tmp_labels_location) +
      ggtitle(paste0(myvar.tmp_substances[i])) +
      theme(
        axis.text.x = element_text(
          angle = 30,
          hjust = 1,
          colour = "black",
          size = 14
        ),
        axis.text.y = element_text(size = 14, colour = "black"),
        axis.title.y = element_text(size = 16),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        plot.title = element_text(size = 20),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 13)
      ) +
      labs(fill = "Location") +
      xlab("") +
      ylab("Conc. [ng/Strip]") +
      scale_y_continuous(expand = expansion(mult = c(0, .1))) +
      scale_x_date(breaks = myvar.tmp_date_breaks,
                   labels = myvar.tmp_dates_labels)
    ggsave(paste0("a_sp_location_cum_",myvar.tmp_substances[i],"_gt_loq.jpg"),
           height = 1080,
           width = 2800,
           units = "px",
           path = paste0("./Grafik/Apistrip_L1/Cumulative/", fun_year,"/"))
  }
  rm(i)
}


# append calculated values from one matrix to tbl_matrix_export -----------

myfun.append_calc_values_to_tbl_matrix_export <- function(fun_tbl_input, fun_tbl_export_name) {
  myvar.tmp_unique_year_bb <- base::unique(fun_tbl_input$year)
  for (i in base::seq_along(myvar.tmp_unique_year_bb)) {
    tmp_tbl_year <- dplyr::filter(fun_tbl_input, year == myvar.tmp_unique_year_bb[i])
    myvar.tmp_unique_substances <- base::unique(tmp_tbl_year$substance)
    for (j in base::seq_along(myvar.tmp_unique_substances)) {
      tmp_tbl_substance <- dplyr::filter(tmp_tbl_year, substance == myvar.tmp_unique_substances[j])
      myvar.tmp_unique_location <- base::unique(tmp_tbl_substance$location_short)
      for (h in base::seq_along(myvar.tmp_unique_location)) {
        tmp_tbl_location <- dplyr::filter(tmp_tbl_substance, location_short == myvar.tmp_unique_location[h])
        fun_tbl_export_name[base::nrow(fun_tbl_export_name) + 1,] = list(tmp_tbl_location$matrix[1],
                                                                         tmp_tbl_location$year[1],
                                                                         tmp_tbl_location$location_short[1],
                                                                         tmp_tbl_location$substance[1],
                                                                         base::max(tmp_tbl_location$concentration),
                                                                         base::mean(tmp_tbl_location$concentration),
                                                                         stats::median(tmp_tbl_location$concentration))
      }
    }
  }
  return(fun_tbl_export_name)
}



# add experiment information to result table ------------------------------

myfun.add_experiment_to_results <- function(fun_input_table, fun_date_column, fun_matrix) {
  fun_input_table <- dplyr::mutate(fun_input_table, year = lubridate::year(fun_date_column))
  fun_input_table <- tibble::rowid_to_column(fun_input_table, "ID")
  
  fun_input_table <- dplyr::mutate(fun_input_table, pk_id_exp = NA,
                                  sampling_start_exp = NA,
                                  sampling_end_exp = NA,
                                  fk_method = NA)
  
  fun_input_table$sampling_start_exp <- as.Date(fun_input_table$sampling_start_exp)
  fun_input_table$sampling_end_exp <- as.Date(fun_input_table$sampling_end_exp)
  
  myvar.tmp_unique_year <- base::unique(fun_input_table$year)
  for (i in base::seq_along(myvar.tmp_unique_year)) {
    tmp_tbl_year <- dplyr::filter(fun_input_table, year == myvar.tmp_unique_year[i])
    myvar.tmp_unique_colonies <- base::unique(tmp_tbl_year$colony)
    for (j in base::seq_along(myvar.tmp_unique_colonies)) {
      tmp_tbl_colony <- dplyr::filter(tmp_tbl_year, colony == myvar.tmp_unique_colonies[j])
      tmp_tbl_experiment  <- dplyr::filter(tbl_experiments, year == myvar.tmp_unique_year[i], colony == myvar.tmp_unique_colonies[j],  matrix == fun_matrix)
      for (h in 1:base::NROW(tmp_tbl_colony)) {
        myvar.cur_row <- tmp_tbl_colony$ID[h]
        fun_input_table$pk_id_exp[myvar.cur_row] <-  tmp_tbl_experiment$pk_id_exp[1]
        fun_input_table$sampling_start_exp[myvar.cur_row] <-  base::as.Date(tmp_tbl_experiment$sampling_start_exp[1])
        fun_input_table$sampling_end_exp[myvar.cur_row] <-  base::as.Date(tmp_tbl_experiment$sampling_end_exp[1])
        fun_input_table$fk_method[myvar.cur_row] <-  tmp_tbl_experiment$fk_method[1]
      }
    }
  }
  rm(i, j, h)
  rm(myvar.tmp_unique_year,
     tmp_tbl_year,
     myvar.tmp_unique_colonies,
     tmp_tbl_colony,
     myvar.cur_row)
  
  fun_input_table <- dplyr::select(fun_input_table, -ID)
  base::return(fun_input_table)
}



# create matrix comparison plot -------------------------------------------

myfun.plot_matrix_avg_comparison <- function(fun_year, fun_location, fun_matrix1, fun_matrix2, fun_matrix3){
  tbl_tmp <- dplyr::filter(tbl_avg_cum_agr, year == fun_year, location_short == fun_location, matrix_short == fun_matrix1 | matrix_short == fun_matrix2 | matrix_short == fun_matrix3)
  myvar.tmp_tbl_unique_matrix <- base::unique(tbl_tmp$matrix)
  myvar.tmp_labels_matrix <- base::sort(base::unique(tbl_tmp$matrix))
  myvar.tmp_matrix_colours_viridis <- myfun.assign_viridis_to_vec(base::sort(myvar.tmp_tbl_unique_matrix))
  myvar.tmp_substances <- base::unique(tbl_tmp$substance)
  myvar.tmp_date_breaks <- base::unique(tbl_tmp$sample_date)
  myvar.tmp_dates_labels <- base::strftime(base::unique(tbl_tmp$sample_date), format = "%d.%m.")
  
  for (i in base::seq_along(myvar.tmp_substances)) {
    tbl_tmp_cur_sub <- dplyr::filter(tbl_avg_cum_agr, year == fun_year, location_short == fun_location, matrix_short == fun_matrix1 | matrix_short == fun_matrix2 | matrix_short == fun_matrix3, substance == myvar.tmp_substances[i])
    tbl_tmp_cur_sub %>%
      ggplot(mapping = aes(x = sample_date,y = concentration_avg, fill = matrix)) +
      geom_col(position = position_dodge2(preserve = "single")) +
      scale_fill_manual(values = myvar.tmp_matrix_colours_viridis,
                        labels = myvar.tmp_labels_matrix) +
      ggtitle(paste0(myvar.tmp_substances[i])) +
      theme(
        axis.text.x = element_text(
          angle = 30,
          hjust = 1,
          colour = "black",
          size = 14
        ),
        axis.text.y = element_text(size = 14, colour = "black"),
        axis.title.y = element_text(size = 16),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        plot.title = element_text(size = 20),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 13)
      ) +
      labs(fill = "Matrix") +
      xlab("") +
      ylab("Conc. [\u00b5g/kg]") +
      scale_y_continuous(expand = expansion(mult = c(0, .1))) +
      scale_x_date(breaks = myvar.tmp_date_breaks,
                   labels = myvar.tmp_dates_labels)
    ggsave(paste0(fun_location, "_matrix_mean_",myvar.tmp_substances[i],"_.jpg"),
           height = 1080,
           width = 2800,
           units = "px",
           path = paste0("./Grafik/Matrix_Comparison/Mean/", fun_year,"/", fun_location, "/"))
  }
  rm(i)
}



# create matrix comparison plot for apistrips -------------------------------------------

myfun.plot_matrix_avg_comparison_a <- function(fun_year, fun_location){
  tbl_tmp <- dplyr::filter(tbl_avg_cum_agr, year == fun_year, location_short == fun_location, matrix == "Apistrip_L2" | matrix == "Apistrip_L1")
  tbl_tmp_a <- dplyr::filter(tbl_avg_cum_agr, year == fun_year, location_short == fun_location, matrix == "Apistrip_L2")
  tbl_tmp_a_sp <- dplyr::filter(tbl_avg_cum_agr, year == fun_year, location_short == fun_location, matrix == "Apistrip_L1")
  myvar.unique_sub_a <- base::unique(tbl_tmp_a$substance)
  myva.unique_sub_a_sp <- base::unique(tbl_tmp_a_sp$substance)
  myvar.tmp_substances <- base::intersect(myvar.unique_sub_a, myva.unique_sub_a_sp)
  myvar.tmp_tbl_unique_matrix <- base::unique(tbl_tmp$matrix)
  myvar.tmp_labels_matrix <- base::sort(base::unique(tbl_tmp$matrix))
  myvar.tmp_matrix_colours_viridis <- myfun.assign_viridis_to_vec(base::sort(myvar.tmp_tbl_unique_matrix))
  myvar.tmp_date_breaks <- base::unique(tbl_tmp$sample_date)
  myvar.tmp_dates_labels <- base::strftime(base::unique(tbl_tmp$sample_date), format = "%d.%m.")
  
  for (i in base::seq_along(myvar.tmp_substances)) {
    tbl_tmp_cur_sub <- dplyr::filter(tbl_avg_cum_agr, year == fun_year, location_short == fun_location, matrix == "Apistrip_L2" | matrix == "Apistrip_L1", substance == myvar.tmp_substances[i])
    tbl_tmp_cur_sub %>%
      ggplot(mapping = aes(x = sample_date,y = concentration_avg, fill = matrix)) +
      geom_col(position = position_dodge2(preserve = "single")) +
      scale_fill_manual(values = myvar.tmp_matrix_colours_viridis,
                        labels = myvar.tmp_labels_matrix) +
      ggtitle(paste0(myvar.tmp_substances[i])) +
      theme(
        axis.text.x = element_text(
          angle = 30,
          hjust = 1,
          colour = "black",
          size = 14
        ),
        axis.text.y = element_text(size = 14, colour = "black"),
        axis.title.y = element_text(size = 16),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        plot.title = element_text(size = 20),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 13)
      ) +
      labs(fill = "Matrix") +
      xlab("") +
      ylab("Conc. [ng/Strip]") +
      scale_y_continuous(expand = expansion(mult = c(0, .1))) +
      scale_x_date(breaks = myvar.tmp_date_breaks,
                   labels = myvar.tmp_dates_labels)
    ggsave(paste0(fun_location, "_apistrip_mean_",myvar.tmp_substances[i],"_.jpg"),
           height = 1080,
           width = 2800,
           units = "px",
           path = paste0("./Grafik/Matrix_Comparison/Apistrips/Mean/", fun_year,"/", fun_location, "/"))
  }
  rm(i)
}



# export tbl_matrix_diff --------------------------------------------------


myfun.export_tbl_matrix_diff <- function(fun_year) {
  tmp_tbl <- dplyr::filter(tbl_matrix_diff, year == fun_year)
  readr::write_excel_csv(tmp_tbl ,
    base::paste0( fun_year, "_matrix_diff.csv"),
    delim = ";"
  )
}








# create_standard_substance_plot_wm -------------------------------------

myfun.create_standard_substance_plot_wm <- function(fun_year_start, fun_year_end) {
  myvar.unique_substances <- base::unique(tbl_results_wm$substance)
  
  for (i in base::seq_along(myvar.unique_substances)) {
    tmp_tbl_sub <- dplyr::filter(tbl_results_wm, substance == myvar.unique_substances[i], Year >= fun_year_start, Year <= fun_year_end)
    myvar.min_year <- base::min(tmp_tbl_sub$Year)
    myvar.max_year <- base::max(tmp_tbl_sub$Year)
    myvar.max_concentration <- base::max(tmp_tbl_sub$concentration)
    myvar.tmp_year_breaks <- base::unique(tmp_tbl_sub$Year)
    myvar.tmp_year_labels <- myvar.tmp_year_breaks
    
    tmp_tbl_sub %>%
      ggplot() +
      geom_col(mapping = aes(x = Year,y = concentration), fill = "#26828e") +
      ggtitle(paste0(myvar.unique_substances[i])) +
      theme(
        axis.text.x = element_text(
          angle = 30,
          hjust = 1,
          colour = "black",
          size = 14
        ),
        axis.text.y = element_text(size = 14, colour = "black"),
        axis.title.y = element_text(size = 16),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        plot.title = element_text(size = 20)
      ) +
      xlab("") +
      ylab("Conc. [\u00b5g/kg]") +
      scale_y_continuous(expand = expansion(mult = c(0, .1)),
                         limits = c(0, myvar.max_concentration)) +
      scale_x_continuous(breaks = myvar.tmp_year_breaks,
                         labels = myvar.tmp_year_labels)
    ggsave(paste0("Wachsmonitoring_",myvar.unique_substances[i],"_", myvar.min_year, "-", myvar.max_year,".jpg"),
           height = 2000,
           width = 4000,
           units = "px",
           path = base::paste0("./Grafik/Wachsmonitoring/Substances/"))
  }
}






# create_substance_comparison_plot_wm -------------------------------------


  myfun.create_sub_comparison_chart_wm <- function(fun_sub1, fun_sub2, fun_sub3, fun_sub4, fun_sub5, fun_year_start, fun_year_end){
    tbl_tmp <- dplyr::filter(tbl_results_wm, substance == fun_sub1 | substance == fun_sub2 | substance == fun_sub3 | substance == fun_sub4 | substance == fun_sub5, Year >= fun_year_start, Year <= fun_year_end)
    myvar.tmp_tbl_unique_sub <- base::unique(tbl_tmp$substance)
    myvar.tmp_labels_sub <- base::sort(base::unique(tbl_tmp$substance))
    myvar.tmp_sub_colours_viridis <- myfun.assign_viridis_to_vec(base::sort(myvar.tmp_tbl_unique_sub))
    myvar.tmp_year_breaks <- base::unique(tbl_tmp$Year)
    myvar.tmp_year_labels <- myvar.tmp_year_breaks
    
    tbl_tmp %>%
        ggplot(mapping = aes(x = Year,y = concentration, fill = substance)) +
        geom_col(position = position_dodge2(preserve = "single")) +
        scale_fill_manual(values = myvar.tmp_sub_colours_viridis,
                          labels = myvar.tmp_labels_sub) +
        #ggtitle(paste0(myvar.tmp_substances[i]))+
        theme(
          axis.text.x = element_text(
            angle = 30,
            hjust = 1,
            colour = "black",
            size = 14
          ),
          axis.text.y = element_text(size = 14, colour = "black"),
          axis.title.y = element_text(size = 16),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black"),
          plot.title = element_text(size = 20),
          legend.text = element_text(size = 12),
          legend.title = element_text(size = 13)
        ) +
        labs(fill = "Substance") +
        xlab("") +
        ylab("Conc. [\u00b5g/kg]") +
        scale_y_continuous(expand = expansion(mult = c(0, .1))) +
        scale_x_continuous(breaks = myvar.tmp_year_breaks,
                     labels = myvar.tmp_year_labels)
      ggsave(paste0("Wachsmonitoring_Substance_Comparison.jpg"),
             height = 2000,
             width = 4000,
             units = "px",
             path = paste0("./Grafik/Wachsmonitoring/Substance_Comparison/"))

  }
  




# experimental ------------------------------------------------------------



