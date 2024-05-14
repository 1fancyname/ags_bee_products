#17_manual_exec.r




# plot ap22-25 pollen point & smooth for all substances ch -------------------------------
#keep in mind the sample date in the filter and the span for geom_smooth

myfun.plot_trend_ch_p <- function(fun_year){
  tmp_tbl <- dplyr::filter(tbl_results_p, year == fun_year, greater_than_loq == TRUE, sample_date_start < as.Date("2023-10-01"))
  myvar.tmp_date_breaks <- base::unique(tmp_tbl$sample_date_start)
  myvar.tmp_dates_labels <- base::strftime(base::unique(tmp_tbl$sample_date_start), format = "%d.%m.")
  
  ggplot(data = tmp_tbl, mapping = aes(x = sample_date_start,y = concentration)) +
    geom_point(position = "jitter",
               size = 1) +
    geom_smooth(linewidth = 0.7,
                colour = "#35b779", 
                formula = y ~ x,
                method = "loess", 
                se = FALSE,
                span = 0.6) +
    ggtitle(paste0(fun_year)) +
    theme(
      axis.text.x = element_text(
        angle = 33,
        hjust = 1,
        colour = "black",
        size = 11
      ),
      axis.title.x = element_text(size = 16),
      axis.text.y = element_text(size = 11, colour = "black"),
      axis.title.y = element_text(size = 16),
      panel.background = element_blank(),
      axis.line = element_line(colour = "black"),
      plot.title = element_text(size = 20),
      legend.text = element_text(size = 12),
      legend.title = element_text(size = 13)
    ) +
    xlab("Sample Date") +
    ylab("log10 Conc. [\u00b5g/kg]") +
    coord_trans(x = "identity", y = "log10") +
    scale_x_date(breaks = myvar.tmp_date_breaks,
                 labels = myvar.tmp_dates_labels) +
    scale_y_continuous(breaks = c(1, 10, 100, 200, 300))
  ggsave(paste0(fun_year,".jpg"),
         height = 1080,
         width = 2300,
         units = "px",
         path = base::paste0("./Grafik/AP22-25/Pollen/Trend/"))
  
}




# plot ap22-25 beebread point & smooth for all substances ch -------------------------------


myfun.plot_trend_ch_bb <- function(fun_year){
  tmp_tbl <- dplyr::filter(tbl_results_bb, year == fun_year, greater_than_loq == TRUE)
  myvar.tmp_date_breaks <- base::unique(tmp_tbl$sample_date)
  myvar.tmp_dates_labels <- base::strftime(base::unique(tmp_tbl$sample_date), format = "%d.%m.")
  
  ggplot(data = tmp_tbl, mapping = aes(x = sample_date,y = concentration)) +
    geom_point(position = "jitter") +
    geom_smooth(colour = "#35b779", 
                formula = y ~ x,
                method = "loess", 
                se = FALSE,
                span = 0.6) +
    ggtitle(paste0(fun_year)) +
    theme(
      axis.text.x = element_text(
        angle = 33,
        hjust = 1,
        colour = "black",
        size = 11
      ),
      axis.title.x = element_text(size = 16),
      axis.text.y = element_text(size = 14, colour = "black"),
      axis.title.y = element_text(size = 16),
      panel.background = element_blank(),
      axis.line = element_line(colour = "black"),
      plot.title = element_text(size = 20),
      legend.text = element_text(size = 12),
      legend.title = element_text(size = 13)
    ) +
    xlab("Sample Date") +
    ylab("log10 Conc. [\u00b5g/kg]") +
    coord_trans(x = "identity", y = "log10") +
    scale_x_date(breaks = myvar.tmp_date_breaks,
                 labels = myvar.tmp_dates_labels)
  ggsave(paste0(fun_year,".jpg"),
         height = 1080,
         width = 2300,
         units = "px",
         path = base::paste0("./Grafik/AP22-25/Beebread/Trend/", fun_year,"/"))
  
}





# plot ap22-25 wax point & smooth for all substances ch -------------------------------


myfun.plot_trend_ch_w <- function(fun_year){
  tmp_tbl <- dplyr::filter(tbl_results_w, year == fun_year, greater_than_loq == TRUE)
  myvar.tmp_date_breaks <- base::unique(tmp_tbl$sample_date)
  myvar.tmp_dates_labels <- base::strftime(base::unique(tmp_tbl$sample_date), format = "%d.%m.")
  
  ggplot(data = tmp_tbl, mapping = aes(x = sample_date,y = concentration)) +
    geom_point(position = "jitter") +
    geom_smooth(colour = "#35b779", 
                formula = y ~ x,
                method = "loess", 
                se = FALSE,
                span = 0.6) +
    ggtitle(paste0(fun_year)) +
    theme(
      axis.text.x = element_text(
        angle = 33,
        hjust = 1,
        colour = "black",
        size = 11
      ),
      axis.title.x = element_text(size = 16),
      axis.text.y = element_text(size = 14, colour = "black"),
      axis.title.y = element_text(size = 16),
      panel.background = element_blank(),
      axis.line = element_line(colour = "black"),
      plot.title = element_text(size = 20),
      legend.text = element_text(size = 12),
      legend.title = element_text(size = 13)
    ) +
    xlab("Sample Date") +
    ylab("log10 Conc. [\u00b5g/kg]") +
    coord_trans(x = "identity", y = "log10") +
    scale_x_date(breaks = myvar.tmp_date_breaks,
                 labels = myvar.tmp_dates_labels)
  ggsave(paste0(fun_year,".jpg"),
         height = 1080,
         width = 2300,
         units = "px",
         path = base::paste0("./Grafik/AP22-25/Wax/Trend/", fun_year,"/"))
  
}





# plot ap22-25 apistrip point & smooth for all substances ch -------------------------------


myfun.plot_trend_ch_a <- function(fun_year){
  tmp_tbl <- dplyr::filter(tbl_results_a, year == fun_year, greater_than_loq == TRUE)
  myvar.tmp_date_breaks <- base::unique(tmp_tbl$sample_date)
  myvar.tmp_dates_labels <- base::strftime(base::unique(tmp_tbl$sample_date), format = "%d.%m.")
  
  ggplot(data = tmp_tbl, mapping = aes(x = sample_date,y = concentration)) +
    geom_point(position = "jitter") +
    geom_smooth(colour = "#35b779", 
                formula = y ~ x,
                method = "loess", 
                se = FALSE,
                span = 0.6) +
    ggtitle(paste0(fun_year)) +
    theme(
      axis.text.x = element_text(
        angle = 33,
        hjust = 1,
        colour = "black",
        size = 11
      ),
      axis.title.x = element_text(size = 16),
      axis.text.y = element_text(size = 14, colour = "black"),
      axis.title.y = element_text(size = 16),
      panel.background = element_blank(),
      axis.line = element_line(colour = "black"),
      plot.title = element_text(size = 20),
      legend.text = element_text(size = 12),
      legend.title = element_text(size = 13)
    ) +
    xlab("Sample Date") +
    ylab("log10 Conc. [\u00b5g/kg]") +
    coord_trans(x = "identity", y = "log10") +
    scale_x_date(breaks = myvar.tmp_date_breaks,
                 labels = myvar.tmp_dates_labels)
  ggsave(paste0(fun_year,".jpg"),
         height = 1080,
         width = 2300,
         units = "px",
         path = base::paste0("./Grafik/AP22-25/Apistrip_L2/Trend/", fun_year,"/"))
  
}




# plot ap22-25 apistrip point & smooth for all substances ch -------------------------------


myfun.plot_trend_ch_a_sp <- function(fun_year){
  tmp_tbl <- dplyr::filter(tbl_results_a_sp, year == fun_year, greater_than_loq == TRUE)
  myvar.tmp_date_breaks <- base::unique(tmp_tbl$sample_date)
  myvar.tmp_dates_labels <- base::strftime(base::unique(tmp_tbl$sample_date), format = "%d.%m.")
  
  ggplot(data = tmp_tbl, mapping = aes(x = sample_date,y = concentration)) +
    geom_point(position = "jitter") +
    geom_smooth(colour = "#35b779", 
                formula = y ~ x,
                method = "loess", 
                se = FALSE,
                span = 0.6) +
    ggtitle(paste0(fun_year)) +
    theme(
      axis.text.x = element_text(
        angle = 33,
        hjust = 1,
        colour = "black",
        size = 11
      ),
      axis.title.x = element_text(size = 16),
      axis.text.y = element_text(size = 14, colour = "black"),
      axis.title.y = element_text(size = 16),
      panel.background = element_blank(),
      axis.line = element_line(colour = "black"),
      plot.title = element_text(size = 20),
      legend.text = element_text(size = 12),
      legend.title = element_text(size = 13)
    ) +
    xlab("Sample Date") +
    ylab("log10 Conc. [\u00b5g/kg]") +
    coord_trans(x = "identity", y = "log10") +
    scale_x_date(breaks = myvar.tmp_date_breaks,
                 labels = myvar.tmp_dates_labels)
  ggsave(paste0(fun_year,".jpg"),
         height = 1080,
         width = 2300,
         units = "px",
         path = base::paste0("./Grafik/AP22-25/Apistrip_L1/Trend/", fun_year,"/"))
  
}






# ap22-25 bb trend class comparison ---------------------------------------

myfun.plot_ap22_45_trend_bb_class_ch <- function(fun_year){
  tmp_tbl <- dplyr::filter(tbl_results_bb, year == fun_year, greater_than_loq == TRUE, class != "s")
  myvar.tmp_date_breaks <- base::unique(tmp_tbl$sample_date)
  myvar.tmp_dates_labels <- base::strftime(base::unique(tmp_tbl$sample_date), format = "%d.%m.")
  myvar.tmp_unique_class <- base::unique(tmp_tbl$class)
  myvar.tmp_labels_class <- base::sort(myvar.tmp_unique_class)
  myvar.tmp_class_colours_viridis <- myfun.assign_viridis_to_vec(base::sort(myvar.tmp_unique_class))
  
  
  ggplot(data = tmp_tbl, mapping = aes(x = sample_date,y = concentration, colour = class)) +
    geom_point(position = "jitter", size = 1) +
    geom_smooth(formula = y ~ x,
                method = "loess", 
                se = FALSE,
                span = 0.6) +
    scale_colour_manual(values = myvar.tmp_class_colours_viridis,
                        labels = myvar.tmp_labels_class) +
    ggtitle(paste0(fun_year)) +
    theme(
      axis.text.x = element_text(
        angle = 33,
        hjust = 1,
        colour = "black",
        size = 11
      ),
      axis.title.x = element_text(size = 16),
      axis.text.y = element_text(size = 14, colour = "black"),
      axis.title.y = element_text(size = 16),
      panel.background = element_blank(),
      axis.line = element_line(colour = "black"),
      plot.title = element_text(size = 20),
      legend.text = element_text(size = 12),
      legend.title = element_text(size = 13)
    ) +
    xlab("Calendar Week") +
    ylab("Conc. [\u00b5g/kg]") +
    coord_trans(x = "identity", y = "log10") +
    scale_x_date(breaks = myvar.tmp_date_breaks,
                 labels = myvar.tmp_dates_labels)
  ggsave(paste0(fun_year,".jpg"),
         height = 1080,
         width = 2300,
         units = "px",
         path = base::paste0("./Grafik/AP22-25/Beebread/Trend_Class/", fun_year,"/"))
  
}





# ap22-25 p trend class comparison ---------------------------------------
#keep in mind the sample date in the filter and the span for geom_smooth

myfun.plot_ap22_45_trend_p_class_ch <- function(fun_year){
  tmp_tbl <- dplyr::filter(tbl_results_p, year == fun_year, greater_than_loq == TRUE, class != "s", sample_date_start < as.Date("2023-10-01"))
  myvar.tmp_date_breaks <- base::unique(tmp_tbl$sample_date_start)
  myvar.tmp_dates_labels <- base::strftime(base::unique(tmp_tbl$sample_date_start), format = "%d.%m.")
  myvar.tmp_unique_class <- base::unique(tmp_tbl$class)
  myvar.tmp_labels_class <- base::sort(myvar.tmp_unique_class)
  myvar.tmp_class_colours_viridis <- myfun.assign_viridis_to_vec(base::sort(myvar.tmp_unique_class))
  
  
  ggplot(data = tmp_tbl, mapping = aes(x = sample_date_start,y = concentration, colour = class)) +
    geom_point(position = "jitter", size = 1) +
    geom_smooth(linewidth = 0.7,
                formula = y ~ x,
                method = "loess", 
                se = FALSE,
                span = 0.9) +
    scale_colour_manual(values = myvar.tmp_class_colours_viridis,
                        labels = myvar.tmp_labels_class) +
    ggtitle(paste0(fun_year)) +
    theme(
      axis.text.x = element_text(
        angle = 33,
        hjust = 1,
        colour = "black",
        size = 11
      ),
      axis.title.x = element_text(size = 16),
      axis.text.y = element_text(size = 11, colour = "black"),
      axis.title.y = element_text(size = 16),
      panel.background = element_blank(),
      axis.line = element_line(colour = "black"),
      plot.title = element_text(size = 20),
      legend.text = element_text(size = 12),
      legend.title = element_text(size = 13)
    ) +
    xlab("Sample Date") +
    ylab("log10 Conc. [\u00b5g/kg]") +
    labs(colour = "Class") +
    coord_trans(x = "identity", y = "log10") +
    scale_x_date(breaks = myvar.tmp_date_breaks,
                 labels = myvar.tmp_dates_labels) +
    scale_y_continuous(breaks = c(0.1, 1, 10, 100, 200, 300))
  ggsave(paste0(fun_year,".jpg"),
         height = 1080,
         width = 2300,
         units = "px",
         path = base::paste0("./Grafik/AP22-25/Pollen/Trend_Class/"))
  
}


