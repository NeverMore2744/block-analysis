source("common.r");

ali_attr <- read.table("../processed/bs/ali_attr.data", header = T, stringsAsFactors = F);
msr_attr <- read.table("../processed/bs/msr_attr.data", header = T, stringsAsFactors = F);
tc_attr <- read.table("../processed/bs/tc_attr.data", header = T, stringsAsFactors = F)

proc <- function(attr, freqs, val_col, cnt_col, cuts, type_name, traffic_type_name) {
  ret <- data.frame(matrix(ncol = 3, nrow = 0), stringsAsFactors = F);
  names(ret) <- c("log", "pct", "value");

  i2 <- 0

  for (lg in unique(freqs$log)) {
    data_t <- subset(freqs, log == lg);
    attr_t <- subset(attr, log == lg);

    sum_multiplied <- sum(as.numeric(data_t[, val_col]) * as.numeric(data_t[, cnt_col]));
    if (sum_multiplied == 0) { # Emit the zeros
      print(paste0("volume ", lg, " has no " ,traffic_type_name));
      next
    }
    traffic <- attr_t[, traffic_type_name];

    if (nrow(attr_t) == 0) {
      print(paste0("Error: attr_t of volume ", lg, " is zero. "));
      q()
    }

    tmp <- toCdfFormatUsingCutsWithCnts(data_t, val_col, cnt_col, cuts, descending = T);
    if (nrow(tmp) < 2) {
      print(paste0("Error: cdf of volume ", lg, " is too short. "));
      print(tmp);
      print(lg);
      print(data_t);
      q()
    }
    for (i in 1:nrow(tmp)) {
      ret[nrow(ret) + 1, ] <- list(lg, paste0(as.character(cuts[i] * 100)), 
          tmp$multiplied[i] / max(sum_multiplied, traffic));
    }
  }

#  if (max(ret$value) > 1) {
#    ret$value <- ret$value / max(ret$value);
#  }

   
  ret$type <- type_name;
  ret
}

foo <- function(attr, pf) {
  fn <- paste0("../processed/bs/b10_", pf, ".data");

  if (!file.exists(fn)) {
    read_freqs <- read.table(
        paste0("../processed/bs/", pf, "_readFreq.data"), 
        header = T, stringsAsFactors = F);
    write_freqs <- read.table(
        paste0("../processed/bs/", pf, "_writeFreq.data"), 
        header = T, stringsAsFactors = F);

    t1 <- proc(attr, read_freqs,  "freq", "cnt", c(0.01, 0.1), "R", "trb");
    t2 <- proc(attr, write_freqs,  "freq", "cnt", c(0.01, 0.1), "W", "twb");

    ret <- rbind(t1, t2);
    print(head(ret));
    ret$value <- ret$value * 100;
    ret$type <- paste0(ret$type, ret$pct);
    write.table(ret[ c("value", "type")], file = fn, quote = F, row.names = F, col.names = T);
  }

  ret <- read.table(fn, header = T, stringsAsFactors = F);
#  for (tp in unique(ret$type)) {
#    t <- subset(ret, type == tp);
#    p25 <- quantile(t$value, 0.25); 
#    print(paste0(pf, " ", tp, " P25: ", p25, " %"));
#  }
} 

foo(ali_attr, "ali");
foo(tc_attr, "tc");
foo(msr_attr, "msr");

ali_agg <- read.table("../processed/bs/b10_ali.data", 
    header = T, stringsAsFactors = F);
tc_agg <- read.table("../processed/bs/b10_tc.data", 
    header = T, stringsAsFactors = F);
msr_agg <- read.table("../processed/bs/b10_msr.data", 
    header = T, stringsAsFactors = F);

ali_r1 <- subset(ali_agg, type == "R1");
msr_r1 <- subset(msr_agg, type == "R1");
tc_r1 <- subset(tc_agg, type == "R1");

ali_r1_qt <- quantile(ali_r1$value, c(0.25, 0.5));
ali_r1_qt_25_75 <- quantile(ali_r1$value, c(0.25, 0.5, 0.75));
msr_r1_qt <- quantile(msr_r1$value, c(0.25, 0.5));
tc_r1_qt <- quantile(tc_r1$value, c(0.25, 0.5));
print(paste0("75% AliCloud volumes aggregates more than ", 
      ali_r1_qt[1], "% traffic in 1% read-most blocks"));
print(paste0("50% AliCloud volumes aggregates more than ", 
      ali_r1_qt[2], "% traffic in 1% read-most blocks"));
print(paste0("The number of large outliers in AliCloud R1: ", 
      nrow(subset(ali_r1, 
          value >= 2.5 * ali_r1_qt_25_75[3] - 1.5 * ali_r1_qt_25_75[1]))));

print(paste0("The minimum traffic of large outliers in AliCloud R1: ", 
      2.5 * ali_r1_qt_25_75[3] - 1.5 * ali_r1_qt_25_75[1]));
#  t <- subset(ali_attr, log );
print(paste0("The total read traffic of large outliers in AliCloud R1: "))

print(paste0("75% MSRC volumes aggregates more than ", 
      msr_r1_qt[1], "% traffic in 1% read-most blocks")); 
print(paste0("50% MSRC volumes aggregates more than ", 
      msr_r1_qt[2], "% traffic in 1% read-most blocks"));
print(paste0("75% TencentCloud volumes aggregates more than ", 
      tc_r1_qt[1], "% traffic in 1% read-most blocks")); 
print(paste0("50% TencentCloud volumes aggregates more than ", 
      tc_r1_qt[2], "% traffic in 1% read-most blocks"));

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

ali_t <- read.table("../processed/bs/b10_ali.data", 
    header = T, stringsAsFactors = F);
tc_t <- read.table("../processed/bs/b10_tc.data", 
    header = T, stringsAsFactors = F);
msr_t <- read.table("../processed/bs/b10_msr.data", 
    header = T, stringsAsFactors = F);

xscale <- seq(0, 1, 0.2);  # second
xlabels <- xscale; 
xlimits <- c(0, 1);
yscale <- seq(0, 1, 0.2) * 100; 
ylabels <- yscale; 
ylimits <- c(0, 1.02) * 100;

myplot <- function(data_t, name) {
  xlab_name <- paste0("Request type");
  ylab_name <- paste0("% of Traffic");

  print(nrow(data_t));
#  data <- subset(data_t, value > 1); 
  data <- data_t; 
  print(nrow(data));

  types <- unique(data$type);
  types <- types[order(types)];
  xlabels <- c("Read", "Write");
  types_label <- types;

  subtypes_label <- c("1%", "10%");

  data$type <- factor(data$type, levels = types);
  data$x <- xlabels[1];
  data$subtype <- subtypes_label[1];
  for (i in 1:nrow(data)) {
    if (data$type[i] == types[3] || data$type[i] == types[4]) {
      data$x[i] <- xlabels[2];
    }
    if (data$type[i] == types[2] || data$type[i] == types[4]) {
      data$subtype[i] <- subtypes_label[2];
    }
  }

  data$subtype <- factor(data$subtype, levels = subtypes_label);
  data$x <- factor(data$x, levels = xlabels);

  axis.text.size <- f9_axis.text.size;
  legend.text.size <- f9_legend.text.size;
  legend.position <- c(0.5, 0.9);
  legend.direction <- "vertical";

  t <- ggplot(data = data, aes(x = x, y = value, colour = subtype)) + 
    geom_boxplot(outlier.size = outlier.size, outlier.color = outlier.color, outlier.shape = outlier.shape) + 
    coord_cartesian(ylim = ylimits) +           
    scale_y_continuous(breaks = yscale, labels = ylabels, expand = c(0.01, 0.01)) + 
    scale_colour_manual(breaks = subtypes_label, labels = subtypes_label, values = c(color1, color2)) + 
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

simplePdf("../figures/b10_ali", mywidth, myheight, T);
print(myplot(ali_t, "ali"));
simplePdf("../figures/b10_msr", mywidth, myheight, T);
print(myplot(msr_t, "msr"));
simplePdf("../figures/b10_tc", mywidth, myheight, T);
print(myplot(tc_t, "tc"));
