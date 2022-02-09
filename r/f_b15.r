source("common.r");
options(scipen=999);

foo_print <- function(pf, label, value) {
  print(paste0(pf, " update interval ", label, ": ", 
        value, " sec, or ", 
        round(value / 60, digits = 3), " min, or ",
        round(value / 3600, digits = 3), " hour")); 
}

for (pf in prefices) {
  fn <- paste0("../processed/ud/", pf, "_total_ud_time.data");
  old <- Sys.time();
  print("before read: ");
  print(old);
  df <- read.table(fn, header = T, stringsAsFactors = F);
  old <- Sys.time()
  print("after read: ");
  print(old);

  sum_ud <- sum(as.numeric(df$cnts)); 
  sm <- 0;
  f25 <- 0;
  f50 <- 0;
  f75 <- 0;
  f90 <- 0;
  f95 <- 0;

  for (i in 1:nrow(df)) {
    sm <- sm + df$cnts[i];
    if (f50 == 0 && sm >= sum_ud * 0.5) {
      f50 <- 1;
      foo_print(pf, "P50", df$timeIn100ms[i] / 10);
    }
    if (f25 == 0 && sm >= sum_ud * 0.25) {
      f25 <- 1;
      foo_print(pf, "P25", df$timeIn100ms[i] / 10);
    }
    if (f75 == 0 && sm >= sum_ud * 0.75) {
      foo_print(pf, "P75", df$timeIn100ms[i] / 10);
      f75 <- 1;
    }
    if (f90 == 0 && sm >= sum_ud * 0.90) {
      foo_print(pf, "P90", df$timeIn100ms[i] / 10);
      f90 <- 1;
    }
    if (f95 == 0 && sm >= sum_ud * 0.95) {
      foo_print(pf, "P95", df$timeIn100ms[i] / 10);
      f95 <- 1;
    }
  }

#  xv <- seq(0.005, 1, 0.005);
#  df <- data.frame(x = xv, y = pcts);
#  write.table(df, file = paste0("../processed/ud", pf, "_cdf.data"), quote = F,
#      row.names = F, col.names = T);
}

for (pf in prefices) {
  print(pf);
  fn <- paste0("../processed/ud/", pf, "_levels.data");
  df <- subset(read.table(fn, header = T, stringsAsFactors = F), 
      type == "50th");

  print(paste0("max of 50th: ", max(df$ud) / 86400, " days"));
  print(paste0("min of 50th: ", min(df$ud), " seconds"));
}

for (pf in prefices) {
  print(pf);
  fn <- paste0("../processed/ud/", pf, "_groups.data");
  df <- subset(read.table(fn, header = T, stringsAsFactors = F), 
      type %in% c(1, 4));

  for (tp in c(1, 4)) {
    subs <- subset(df, type == tp);
    print(paste0("group ", tp, ": 50th ", median(subs$pcts)));
  }
}

if (!draw_figures) {
  q()
}

library(ggplot2)
library(testit);
library(scales);

new_percentiles <- function(vec, value_col, cnt_col, cuts) {
  vec <- vec[order(vec[, value_col]), ];
  s <- sum(as.numeric(vec[, cnt_col]));
  ts <- 0;
  index <- 1;

  ret <- c();

  for (i in 1:nrow(vec)) {
    if (index > length(cuts)) {
      break
    }
    ts <- ts + as.numeric(vec[i, cnt_col]);

    while (index <= length(cuts) & ts > s * cuts[index]) {
      ret <- c(ret, vec[i, value_col]);
      index <- index + 1;
    }
  }

  ret
}

####### 1. Total cnts

prefices <- c("ali", "tc", "msr");
filenames <- paste0("../processed/ud/", prefices, "_total_ud_time.data");

for (fn in filenames) {
  print(paste0("Reading ", fn, " ..."));
  attr <- read.table(fn, header = T, stringsAsFactors = F);
  attr$timeInSec <- attr$timeIn100ms / 10;
  print(paste0("Read ", fn, " finished"));
  pcts <- new_percentiles(attr, "timeInSec", "cnts", 
      c(0.25, 0.5, 0.75, 0.9, 0.95)) / 60;
  str <- "";
  for (pt in pcts) {
    str <- paste(str, format(round(pt, digits = 3), nsmall=3));
  }
  print(paste0("Percentiles in minutes: ", str));
}

####### 2. Levels

for (prefix in prefices) {
  fn <- paste0("../processed/ud/", prefix, "_groups.data");
  if (!file.exists(fn)) {
    next
  }
  attr <- read.table(fn, header = T, stringsAsFactors = F);
  attr <- subset(attr, type %in% c(1, 4));
  for (tp in unique(attr$type)) {
    subs <- subset(attr, type == tp);

    print(paste0("Half volumes have ", 
          format(median(subs$pcts) * 100, nsmall = 3), " % update intervals ", 
          ifelse(tp == 1, "smaller than 5 minutes", "larger than 240 minutes")));
  }
}

mywidth <- 3
myheight <- 1.8

yscale <- 10^(seq(0, 8, 2));
ylabels <- c(1, expression(10^2), expression(10^4), expression(10^6), expression(10^8));
ylimits <- 10^(c(0, 8));

myplot <- function(name) {
  xlab_name <- "Percentiles";
  ylab_name <- "Update intervals (s)";

  data <- read.table(paste0("../processed/ud/", name, "_levels.data"), header = T, stringsAsFactors = F);
  types <- unique(data$type);
  data$type <- factor(data$type, types);

  axis.text.size <- f14.axis.text.size;
  legend.text.size <- f14.legend.text.size;
  legend.position <- c(0.85, 0.35);
  legend.direction <- "vertical";

  t <- ggplot(data = data, aes(x = type, y = ud)) + 
    geom_boxplot(outlier.size = outlier.size, outlier.color = outlier.color, outlier.shape = outlier.shape) + 
    coord_cartesian(ylim = ylimits) +           
    scale_y_continuous(breaks = yscale, labels = ylabels, trans = 'log10', expand = c(0.02, 0.02)) + 
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

simplePdf("../figures/b15_ali", mywidth, myheight, T);
print(myplot("ali"));
simplePdf("../figures/b15_msr", mywidth, myheight, T);
print(myplot("msr"));
simplePdf("../figures/b15_tc", mywidth, myheight, T); 
print(myplot("tc"));

yscale <- seq(0, 1, 0.2);
ylabels <- yscale * 100;
ylimits <- c(0, 1.02);

myplot <- function(name) {
  xlab_name <- "Update interval ranges (minutes)";
  ylab_name <- "Proportion (%)";

  data <- read.table(paste0("../processed/ud/", name, "_groups.data"), 
      header = T, stringsAsFactors = F);
  types <- unique(data$type);
  data$type <- factor(data$type, types);

  axis.text.size <- f14_levels.axis.text.size;
  legend.text.size <- f14_levels.legend.text.size;
  legend.position <- c(0.85, 0.35);
  legend.direction <- "vertical";

  t <- ggplot(data = data, aes(x = type, y = pcts)) + 
    geom_boxplot(outlier.size = outlier.size, outlier.color = outlier.color, outlier.shape = outlier.shape) + 
    coord_cartesian(ylim = ylimits) +           
    scale_x_discrete(breaks = types, labels = c("<5", "5-30", "30-240", ">240")) + 
    scale_y_continuous(breaks = yscale, labels = ylabels, expand = c(0.01, 0.01)) + 
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

simplePdf("../figures/b15_ali_groups", mywidth, myheight, T);
print(myplot("ali"));
simplePdf("../figures/b15_msr_groups", mywidth, myheight, T);
print(myplot("msr"));
simplePdf("../figures/b15_tc_groups", mywidth, myheight, T); 
print(myplot("tc"));
