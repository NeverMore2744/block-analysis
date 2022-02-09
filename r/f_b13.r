source("common.r");
options(scipen=999);

print("The number of total RAW/WAW/RAR/WAR requests:");
for (pf in prefices) {
  fn <- paste0("../processed/arw/", pf, "_waw_single_total_cnt.data");
  x1 <- read.table(fn, header = T, stringsAsFactors = F);
  for (tp in c("raw", "waw", "rar", "war")) {
    print(paste0(tp, " ", pf, " ", sum(as.numeric(x1[, tp])) / 1000000));
  }
}

process <- function(data, tp) {
  names(data) <- c("x", "y");
  data <- subset(data, x >= 1);
  data[nrow(data) + 1, ] <- list(0.99, 0);
  data$y <- data$y / 100;
  data$type <- tp;

  data
}

for (pf in prefices) {
  fn <- paste0("../processed/arw/", pf, "_raw_global_pct.data");
  raw <- read.table(fn, header = T, stringsAsFactors = F);
  raw_p50 <- max(raw$timeInSec[raw$pct <= 50.00001]);
  print(paste0(pf, " RAW, P50: ", 
        raw_p50, " sec, or ", 
        round(raw_p50 / 60, digits = 3), " min, or ",
        round(raw_p50 / 3600, digits = 3), " hour")); 

  fn <- paste0("../processed/arw/", pf, "_waw_global_pct.data");
  waw <- read.table(fn, header = T, stringsAsFactors = F);
  waw_p50 <- max(waw$timeInSec[waw$pct <= 50.00001]);
  print(paste0(pf, " WAW, P50: ", 
        waw_p50, " sec, or ", 
        round(waw_p50 / 60, digits = 3), " min, or ",
        round(waw_p50 / 3600, digits = 3), " hour")); 

  write.table(rbind(process(raw, "RAW"), process(waw, "WAW")), 
      file = paste0("../processed/arw/b13_", pf, ".data"),
      quote = F, row.names = F, col.names = T);
}

for (pf in prefices) {
  fn <- paste0("../processed/arw/", pf, "_raw_global_cnt.data");
  raw <- read.table(fn, header = T, stringsAsFactors = F);
  small_raw_n <- sum(as.numeric(raw[raw$timeInSec <= 60.001, "cnt"]));
  large_raw_n <- sum(as.numeric(raw[raw$timeInSec >= 15 * 60 - 0.0001, "cnt"]));
  total_raw <- sum(as.numeric(raw$cnt));

  fn <- paste0("../processed/arw/", pf, "_waw_global_cnt.data");
  waw <- read.table(fn, header = T, stringsAsFactors = F);
  small_waw_n <- sum(as.numeric(waw[waw$timeInSec <= 60.001, "cnt"]));
  large_waw_n <- sum(as.numeric(waw[waw$timeInSec >= 15 * 60 - 0.0001, "cnt"]));
  total_waw <- sum(as.numeric(waw$cnt));

  print(paste0(pf, " small raw percentage: ", 
        small_raw_n / total_raw * 100.0, " %"));
  print(paste0(pf, " large raw percentage: ", 
        large_raw_n / total_raw * 100.0, " %"));
  print(paste0(pf, " small waw percentage: ", 
        small_waw_n / total_waw * 100.0, " %"));
  print(paste0(pf, " large waw percentage: ", 
        large_waw_n / total_waw * 100.0, " %"));
}

if (!draw_figures) {
  q()
}

library(ggplot2)
library(testit);
library(scales);

mywidth <- 3
myheight <- 1.8

yscale <- seq(0, 1, 0.2)
ylimits <- c(0, 1.00)

xscale <- 10^(0:5);  # second
xlabels <- c(1, expression(10^1), expression(10^2), expression(10^3), expression(10^4), expression(10^5));
xlimits <- c(10^0, 10^5);

myplot <- function(name, flag) {
  xlab_name <- "Time (s)";
  ylab_name <- "Cumulative (%)";

  data <- read.csv(paste0("../processed/arw/b13_", name, ".data"), header = T, stringsAsFactors = F);
  types <- unique(data$type);
  types <- types[order(types)];
  types_labels <- c("RAW time", "WAW time");

  axis.text.size <- f12_13.axis.text.size;
  legend.text.size <- f12_13.legend.text.size;
  legend.position <- c(0.25, 0.91);
  legend.direction <- "vertical";

  colors <- c(color1, color2); # my_color_full[1:length(types)];
  linetypes <- rep(1, length(types));

  t <- ggplot(data = data, aes(x = x, y = y, colour = type, linetype = type)) + 
    geom_line(stat = "identity") +  
    coord_cartesian(xlim = xlimits, ylim = ylimits) +           
    scale_x_continuous(breaks = xscale, labels = xlabels, trans = 'log10', expand = c(0.03, 0.03)) + 
    scale_y_continuous(breaks = yscale, labels = yscale * 100, expand = c(0.01, 0.01)) + 
    scale_colour_manual(breaks = types, labels = types_labels, values = colors) +
    scale_linetype_manual(breaks = types, labels = types_labels, values = linetypes) +
    ylab(ylab_name) + xlab(xlab_name) + 
    theme_classic() +  
    theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black", size = 0.1),
        axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5, colour = "black", size = axis.text.size),
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

simplePdf("b13_ali", mywidth, myheight, T);
print(myplot("ali", 0));
simplePdf("b13_tc", mywidth, myheight, T);
print(myplot("tc", 1));
simplePdf("b13_msr", mywidth, myheight, T);
print(myplot("msr", 2));
