source("common.r");
options(scipen=999);

print("The number of total RAW/WAW/RAR/WAR requests:");
tmp <- 0;
for (pf in prefices) {
  fn <- paste0("../processed/arw/", pf, "_waw_single_total_cnt.data");
  x1 <- read.table(fn, header = T, stringsAsFactors = F);
  for (tp in c("raw", "waw", "rar", "war")) {
    print(paste0(tp, " ", pf, " ", sum(as.numeric(x1[, tp])) / 1000000));
    if (tp == "waw") {
      print(paste0(tp, " waw is ", 
            sum(as.numeric(x1[, tp])) / 1000000 / tmp, "x of raw"));
    } else if (tp == "war") {
      print(paste0(tp, " rar is ", 
            tmp / (sum(as.numeric(x1[, tp])) / 1000000), "x of war"));
    }
    tmp <- sum(as.numeric(x1[, tp])) / 1000000;
  }
}
q()

process <- function(data, tp) {
  names(data) <- c("x", "y");
  data <- subset(data, x >= 1);
  data[nrow(data) + 1, ] <- list(0.99, 0);
  data$y <- data$y / 100;
  data$type <- tp;

  data
}

for (pf in prefices) {
  fn <- paste0("../processed/arw/", pf, "_rar_global_pct.data");
  rar <- read.table(fn, header = T, stringsAsFactors = F);
  rar_p50 <- max(rar$timeInSec[rar$pct <= 50.00001]);
  print(paste0(pf, " RAR, P50: ", 
        rar_p50, " sec, or ", 
        round(rar_p50 / 60, digits = 3), " min, or ",
        round(rar_p50 / 3600, digits = 3), " hour")); 

  fn <- paste0("../processed/arw/", pf, "_war_global_pct.data");
  war <- read.table(fn, header = T, stringsAsFactors = F);
  war_p50 <- max(war$timeInSec[war$pct <= 50.00001]);

  print(paste0(pf, " WAR, P50: ", 
        war_p50, " sec, or ", 
        round(war_p50 / 60, digits = 3), " min, or ",
        round(war_p50 / 3600, digits = 3), " hour")); 

  write.table(rbind(process(rar, "RAR"), process(war, "WAR")), 
      file = paste0("../processed/arw/b14_", pf, ".data"),
      quote = F, row.names = F, col.names = T);
}

for (pf in prefices) {
  fn <- paste0("../processed/arw/", pf, "_rar_global_cnt.data");
  rar <- read.table(fn, header = T, stringsAsFactors = F);
  small_rar_n <- sum(as.numeric(rar[rar$timeInSec <= 60.001, "cnt"]));
  large_rar_n <- sum(as.numeric(rar[rar$timeInSec >= 15 * 60 - 0.0001, "cnt"]));
  large_rar_1hour <- sum(as.numeric(rar[rar$timeInSec >= 60 * 60 - 0.0001, "cnt"]));
  total_rar <- sum(as.numeric(rar$cnt));

  fn <- paste0("../processed/arw/", pf, "_war_global_cnt.data");
  war <- read.table(fn, header = T, stringsAsFactors = F);
  small_war_n <- sum(as.numeric(war[war$timeInSec <= 60.001, "cnt"]));
  large_war_n <- sum(as.numeric(war[war$timeInSec >= 15 * 60 - 0.0001, "cnt"]));
  large_war_1hour <- sum(as.numeric(war[war$timeInSec >= 60 * 60 - 0.0001, "cnt"]));
  total_war <- sum(as.numeric(war$cnt));

  print(paste0(pf, " small rar percentage: ", 
        small_rar_n / total_rar * 100.0, " %"));
  print(paste0(pf, " large rar percentage: ", 
        large_rar_n / total_rar * 100.0, " %"));
  print(paste0(pf, " rar >= 1h percentage: ", 
        large_rar_1hour / total_rar * 100.0, " %"));

  print(paste0(pf, " small war percentage: ", 
        small_war_n / total_war * 100.0, " %"));
  print(paste0(pf, " large war percentage: ", 
        large_war_n / total_war * 100.0, " %"));
  print(paste0(pf, " war >= 1h percentage: ", 
        large_war_1hour / total_war * 100.0, " %"));

  if (pf == "msr") {
    small_war_1sec <- sum(as.numeric(war[war$timeInSec <= 1.001, "cnt"]));
    small_rar_1sec <- sum(as.numeric(rar[rar$timeInSec <= 1.001, "cnt"]));

    print(paste0(pf, " rar <= 1s percentage: ", 
          small_rar_1sec / total_rar * 100.0, " %"));
    print(paste0(pf, " war <= 1s percentage: ", 
          small_war_1sec / total_war * 100.0, " %"));

  }
}

if (!draw_figures) {
  q()
}

library(ggplot2)
library(testit);
library(scales);

mywidth <- 3
myheight <- 1.8

xscale <- c(1/60, 1/12, 1, 60, 180, 1440, 1440*7, 1440*90) * 60;  # second
xlabels <- c("1s", "5s", "1min", "1h", "3h", "1d", "7d", "90d");
xlimits <- c(0.01, 1440, 1440*7) * 60;

#xscale <- c(1/60, 1/12, 1, 60, 180, 1440) * 60;  # second
xscale <- 10^(0:5);  # second
xlabels <- c(1, expression(10^1), expression(10^2), expression(10^3), expression(10^4), expression(10^5));
#xlimits <- c(0.01, 1440) * 60;
xlimits <- c(10^0, 10^5);

yscale <- seq(0, 1, 0.2)
ylimits <- c(0, 1.00)

myplot <- function(name, flag) {
  xlab_name <- paste0("Time (s)");

  data <- read.csv(paste0("f13_", name, ".csv"), header = T, stringsAsFactors = F);
  types <- unique(data$type);
  types <- types[order(types)];
#  types_labels <- c("Read-after-read", "Write-after-read");
  types_labels <- c("RAR time", "WAR time");

  axis.text.size <- f12_13.axis.text.size;
  legend.text.size <- f12_13.legend.text.size;
  legend.position <- c(0.26, 0.88);
  legend.direction <- "vertical";

  colors <- c(color1, color2); # my_color_full[1:length(types)];

  t <- ggplot(data = data, aes(x = x, y = y, colour = type, linetype = type)) + 
    geom_line(stat = "identity") +  
    coord_cartesian(xlim = xlimits, ylim = ylimits) +           
    scale_x_continuous(breaks = xscale, labels = xlabels, trans = 'log10', expand = c(0.03, 0.03)) + 
    scale_y_continuous(breaks = yscale, labels = yscale * 100, expand = c(0.01, 0.01)) + 
    scale_colour_manual(breaks = types, labels = types_labels, values = colors) +
    scale_linetype_manual(breaks = types, labels = types_labels, values = rep(1, length(types))) +
    ylab("Cumulative (%)") + xlab(xlab_name) +
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

simplePdf("f13_msr", mywidth, myheight, T);
print(myplot("msr", 1));
simplePdf("f13_ali", mywidth, myheight, T);
print(myplot("ali", 0));
simplePdf("f13_tc", mywidth, myheight, T);
print(myplot("tc", 0));
