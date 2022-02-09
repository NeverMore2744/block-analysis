source("common.r");

################## Active devices, same as finding B.5

dirname <- "../processed/traffic/";
ali_t <- read.table(paste0(dirname, "ali_total_active_devices.data"), 
    header = T, stringsAsFactors = F);
tc_t <- read.table(paste0(dirname, "tc_total_active_devices.data"), 
    header = T, stringsAsFactors = F);
msr_t <- read.table(paste0(dirname, "msr_total_active_devices.data"), 
    header = T, stringsAsFactors = F);

proc <- function(data_t, intv, active_col, time_col, type_name) {
  data <- data_t[c(1,2), ];
  tmp <- 0;
  cnt <- 0;
  for (i in 3:nrow(data_t)) {
    tmp <- tmp + data_t[i, active_col];
    cnt <- cnt + 1;
    if (i %% intv == 0) {
      data[nrow(data) + 1, ] <- list(data_t[i, time_col], tmp/cnt);
      tmp <- 0;
      cnt <- 0;
    }
  }
  data <- data[3:nrow(data), ];
  data$type <- type_name;

  data
}

myplot <- function(data_t, intv, days, xscale, name) {
  data1 <- proc(data_t, intv, "activeDevices", "timeInMin", "Active");;
  data2 <- proc(data_t, intv, "readActive", "timeInMin", "R-active");;
  data3 <- proc(data_t, intv, "writeActive", "timeInMin", "W-active");;
  data <- rbind(data1, data2, data3);

  data$x <- data$timeInMin / 24 / 60;
  data$y <- data[, 2];

  data <- subset(data, x <= max(xscale));
  data <- data[, c("x", "y", "type")];

  data$x <- round(data$x, digits = 4);
  data$y <- round(data$y, digits = 4);
  write.table(data, file = paste0(dirname, "b5_b6_b7_num_", name, ".data"), 
      quote = F, row.names = F, col.names = T);
}

myplot(tc_t, 20, 9, seq(0, 9, 3), "tc");
myplot(msr_t, 5, 7, seq(0, 7, 1), "msr");
myplot(ali_t, 20, 32, seq(0, 32, 8), "ali");

################ Active time & Info

proc <- function(file_name, intv, days, pf) {
  df <- read.table(file_name, header = T, stringsAsFactors = F);
  whole_period <- max(df$maxTimeInMin) - min(df$minTimeInMin) + 1;

  for (i in 1:nrow(df)) {
    if (df$activeTimeInMin[i] > days * 24 * 60) {
      df$activeTimeInMin[i] <- days * 24 * 60;
    }
    if (df$readActiveTimeInMin[i] > days * 24 * 60) {
      df$readActiveTimeInMin[i] <- days * 24 * 60;
    }
    if (df$writeActiveTimeInMin[i] > days * 24 * 60) {
      df$writeActiveTimeInMin[i] <- days * 24 * 60;
    }
  }

  data1 <- toCdfFormat(df$activeTimeInMin, 0, days * 24 * 60, 100);
  data1$type <- "Active";
  data2 <- toCdfFormat(df$readActiveTimeInMin, 0, days * 24 * 60, 100);
  data2$type <- "R-active";
  data3 <- toCdfFormat(df$writeActiveTimeInMin, 0, days * 24 * 60, 100);
  data3$type <- "W-active";

  data <- rbind(data1, data2, data3);
  data$x <- data$x / 24 / 60;
  
  write.table(data, file = paste0(dirname, "b5_b6_b7_time_", pf, ".data"), 
      quote = F, row.names = F, col.names = T);
}

proc(paste0(dirname, "tc_per_device_active_time.data"), 20, 9, "tc");
proc(paste0(dirname, "msr_per_device_active_time.data"), 5, 7, "msr");
proc(paste0(dirname, "ali_per_device_active_time.data"), 5, 32, "ali");

### Info: Finding B.7

ali_t <- read.table(paste0(dirname, "b5_b6_b7_num_ali.data"), header = T, stringsAsFactors = F);
msr_t <- read.table(paste0(dirname, "b5_b6_b7_num_msr.data"), header = T, stringsAsFactors = F);
tc_t <- read.table(paste0(dirname, "b5_b6_b7_num_tc.data"), header = T, stringsAsFactors = F);

ali_active <- subset(ali_t, type == "Active");
msr_active <- subset(msr_t, type == "Active");
tc_active <- subset(tc_t, type == "Active");

ali_read_active <- subset(ali_t, type == "R-active");
max_reduced <- max((ali_active$y - ali_read_active$y) / ali_active$y) * 100;
min_reduced <- min((ali_active$y - ali_read_active$y) / ali_active$y) * 100;
print(paste0("AliCloud: reduce: ", min_reduced, " to ", max_reduced, "%"));

msr_read_active <- subset(msr_t, type == "R-active");
max_reduced <- max((msr_active$y - msr_read_active$y) / msr_active$y) * 100;
min_reduced <- min((msr_active$y - msr_read_active$y) / msr_active$y) * 100;
print(paste0("MSRC: reduce: ", min_reduced, " to ", max_reduced, "%"));

tc_read_active <- subset(tc_t, type == "R-active");
max_reduced <- max((tc_active$y - tc_read_active$y) / tc_active$y) * 100;
min_reduced <- min((tc_active$y - tc_read_active$y) / tc_active$y) * 100;
print(paste0("Tencent: reduce: ", min_reduced, " to ", max_reduced, "%"));

proc <- function(file_name, intv, days, pf) {
  df <- read.table(file_name, header = T, stringsAsFactors = F);
  whole_period <- max(df$maxTimeInMin) - min(df$minTimeInMin) + 1;

  md <- median(df$readActiveTimeInMin);
  print(paste0(pf, ", median of read-active: ", 
        md / 24 / 60, " days (", 
        round(md / whole_period * 100.0, digits = 3), 
        " % of duration)"));
  print(max(df$readActiveTimeInMin));

  subs <- subset(df, readActiveTimeInMin > (days - 1) * 1440);
  print(paste0(pf, ", more than ", days - 1, " days: ", 
       nrow(subs) / nrow(df) * 100, " %"));

  for (i in 1:nrow(df)) {
    if (df$activeTimeInMin[i] > days * 24 * 60) {
      df$activeTimeInMin[i] <- days * 24 * 60;
    }
    if (df$readActiveTimeInMin[i] > days * 24 * 60) {
      df$readActiveTimeInMin[i] <- days * 24 * 60;
    }
    if (df$writeActiveTimeInMin[i] > days * 24 * 60) {
      df$writeActiveTimeInMin[i] <- days * 24 * 60;
    }
  }
}

proc(paste0(dirname, "tc_per_device_active_time.data"), 20, 9, "tc");
proc(paste0(dirname, "msr_per_device_active_time.data"), 5, 7, "msr");
proc(paste0(dirname, "ali_per_device_active_time.data"), 5, 31, "ali");

if (!draw_figures) {
  q()
}

library(ggplot2)
library(testit);
library(scales);

mywidth <- 3
myheight <- 1.8

yscale <- seq(0, 1, 0.2)
ylimits <- c(0, 1.15)

ali_attrs <- read.table(paste0(dirname, "b5_b6_b7_num_ali.data"), 
    header = T, stringsAsFactors = F);
msr_attrs <- read.table(paste0(dirname, "b5_b6_b7_num_msr.data"), 
    header = T, stringsAsFactors = F);
tc_attrs <- read.table(paste0(dirname, "b5_b6_b7_num_tc.data"), 
    header = T, stringsAsFactors = F);

myplot <- function(data_attrs, intv, days, volumes, yscale, xscale) {
  xlab_name <- paste0("Time (days)");
  ylab_name <- paste0("# volumes");

  xlimits <- c(0, max(xscale));
  xlabels <- xscale;

  ylimits <- c(0, max(yscale));

  data <- data_attrs;
  types <- unique(data$type);
  data$type <- factor(data$type, levels = types);

  axis.text.size <- f5_7.axis.text.size;
  legend.text.size <- f5_7.legend.text.size;
  legend.position <- ""; # c(0.5, 0.9);
  legend.direction <- "horizontal";

  linetypes <- c(1, 1, 1);
  sizes <- c(1.5, 0.5, 0.5);
  colors <- active.colors; 

  t <- ggplot(data = data, aes(x = x, y = y, colour = type, linetype = type, size = type)) + 
    geom_line(stat = "identity") +  
    coord_cartesian(xlim = xlimits, ylim = ylimits) +           
    scale_x_continuous(breaks = xscale, labels = xlabels) +
    scale_y_continuous(breaks = yscale, labels = yscale, expand = c(0.01, 0.01)) + 
    scale_colour_manual(breaks = types, labels = types, values = colors) +
    scale_linetype_manual(breaks = types, labels = types, values = linetypes) +
    scale_size_manual(breaks = types, labels = types, values = sizes) + 
    ylab(ylab_name) + xlab(xlab_name) + 
    theme_classic() +  
    theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black", size = 0.1),
        axis.text.x = element_text(angle = 0, hjust = 0.5, colour = "black", size = axis.text.size),
        axis.title.y = element_text(size = axis.text.size, hjust = 0.5),
        axis.text.y = element_text(colour = "black",size = axis.text.size),
        axis.title.x = element_text(size = axis.text.size),
        legend.title = element_blank(),
        legend.position = legend.position,
        legend.key.size = unit(0.3, "cm"),
        legend.text = element_text(size = legend.text.size),
        legend.background = element_rect(size = 5, fill = alpha(NA, 0.5)),
        legend.direction = legend.direction,
        plot.margin = unit(c(0.15,0.15,0.15,0.15), "cm"))

  t
}

simplePdf("../figures/b5_b6_b7_num_ali", mywidth, myheight, T);
print(myplot(ali_attrs, 5, 32, 1000, seq(0, 1000, 250), c(seq(0, 25, 5), 31)));
simplePdf("../figures/b5_b6_b7_num_msr", mywidth, myheight, T);
print(myplot(msr_attrs, 5, 7, 36, seq(0, 40, 10), seq(0, 7, 1)));
simplePdf("../figures/b5_b6_b7_num_tc", mywidth, myheight, T);
print(myplot(tc_attrs, 5, 9, 5000, seq(0, 5000, 1000), seq(0, 9, 1)));

################# Time analysis

ali_attr <- read.table(paste0(dirname, "b5_b6_b7_time_ali.data", header = T, stringsAsFactors = F);
msr_attr <- read.table(paste0(dirname, "b5_b6_b7_time_msr.data", header = T, stringsAsFactors = F);
tc_attr <- read.table(paste0(dirname, "b5_b6_b7_time_tc.data", header = T, stringsAsFactors = F);

myplot <- function(data_attrs, xscale, xlimits) {
  xlab_name <- paste0("Time (days)");
  ylab_name <- "Cumulative (%)";

  data <- data_attrs;

#  xlimits <- c(0, 1);
#  xscale <- seq(0, 1, 0.2);
  xlabels <- xscale; 

  ylimits <- c(0, 1.05);
  yscale <- seq(0, 1, 0.2);
  ylabels <- yscale * 100;

  types <- unique(data$type);
  data$type <- factor(data$type, levels = types);

  axis.text.size <- f5_7.axis.text.size;
  legend.text.size <- f5_7.legend.text.size;
  legend.position <- ""; #c(0.22, 0.8);
  legend.direction <- "vertical";

  linetypes <- c(1, 1, 1);
  sizes <- c(1.5, 0.5, 0.5);
  colors <- active.colors; 

  t <- ggplot(data = data, aes(x = x, y = y, colour = type, linetype = type, size = type)) + 
    geom_line(stat = "identity") +  
    coord_cartesian(xlim = xlimits, ylim = ylimits) +           
    scale_x_continuous(breaks = xscale, labels = xlabels) + 
    scale_y_continuous(breaks = yscale, labels = ylabels, expand = c(0.01, 0.01)) +
    scale_colour_manual(breaks = types, labels = types, values = colors) +
    scale_linetype_manual(breaks = types, labels = types, values = linetypes) +
    scale_size_manual(breaks = types, labels = types, values = sizes) + 
    ylab(ylab_name) + xlab(xlab_name) +
    theme_classic() +  
    theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black", size = 0.1),
        axis.text.x = element_text(angle = 0, hjust = 0.5, colour = "black", size = axis.text.size),
        axis.title.y = element_text(size = axis.text.size, hjust = 0.5),
        axis.text.y = element_text(colour = "black",size = axis.text.size),
        axis.title.x = element_text(size = axis.text.size),
        legend.title = element_blank(),
        legend.position = legend.position,
        legend.key.size = unit(0.3, "cm"),
        legend.text = element_text(size = legend.text.size),
        legend.background = element_rect(size = 5, fill = alpha(NA, 0.5)),
        legend.direction = legend.direction,
        plot.margin = unit(c(0.15,0.15,0.15,0.15), "cm"))

  t
}

simplePdf("../figures/b5_b6_b7_time_ali", mywidth, myheight, T);
print(myplot(ali_attr, c(seq(0, 25, 5), 31), c(0, 31)));
simplePdf("../figures/b5_b6_b7_time_msr", mywidth, myheight, T);
print(myplot(msr_attr, seq(0, 7, 1), c(0, 7)));
simplePdf("../figures/b5_b6_b7_time_tc", mywidth, myheight, T);
print(myplot(tc_attr, seq(0, 9, 1), c(0, 9)));
