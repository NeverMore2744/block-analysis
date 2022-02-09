source("common.r");

ali_attr <- read.table(paste0("../processed/bs/ali_attr.data"), header = T, 
    stringsAsFactors = F);
ali_attr$wr_ratio_in_req <- new_div(ali_attr$numWReq, ali_attr$numRReq);

tc_attr <- read.table(paste0("../processed/bs/tc_attr.data"), header = T, 
    stringsAsFactors = F);
tc_attr$wr_ratio_in_req <- new_div(tc_attr$numWReq, tc_attr$numRReq);

msr_attr <- read.table(paste0("../processed/bs/msr_attr.data"), header = T, 
    stringsAsFactors = F);
msr_attr$wr_ratio_in_req <- new_div(msr_attr$numWReq, msr_attr$numRReq);

proc <- function(data_attr, type) {
  num_write_dom <- sum(data_attr$numRReq == 0 | 
      (data_attr$numWReq / data_attr$numRReq > 1));
  num_write_larger_than_100 <- sum(data_attr$numRReq == 0 | 
      (data_attr$numWReq / data_attr$numRReq > 100));

  print(paste0(type, " overall write-read ratio ", 
        sum(as.numeric(data_attr$numWReq)) /
        sum(as.numeric(data_attr$numRReq))));
  print(paste0(type, " write-dominant volumes   ", 
        " num ", num_write_dom, " or ", 
        num_write_dom / nrow(data_attr) * 100, "(%)"));
  print(paste0(type, " volumes > 100 write-read ", 
        " num ", num_write_larger_than_100, " or ", 
        num_write_larger_than_100 / nrow(data_attr) * 100, "(%)"));
}

proc(ali_attr, "ali");
proc(tc_attr, "tc");
proc(msr_attr, "msr");

func <- function(name_col, x_cuts) {
  data1 <- toCdfFormatUsingCuts(ali_attr[, name_col], x_cuts);
  data1$type <- "AliCloud";

  data2 <- toCdfFormatUsingCuts(tc_attr[, name_col], x_cuts); 
  data2$type <- "TencentCloud";

  data3 <- toCdfFormatUsingCuts(msr_attr[, name_col], x_cuts); 
  data3$type <- "MSRC";

  data <- rbind(data1, data2, data3);

  xlimits <- c(min(data$x), max(data$x));
  xscale <- x_cuts;

  write.table(data, file = "../processed/bs/b_wr_ratio.data", quote = F, 
      row.names = F, col.names = T);
}

func("wr_ratio_in_req", 10^((-2):4));

if (!draw_figures) {
  q()
}

library(ggplot2)
library(testit);
library(scales);

mywidth <- 3
myheight <- 1.8

myplot <- function(base) {
  data <- read.table("../processed/bs/b_wr_ratio.data", header = T, 
      stringsAsFactors = F);

  xscale <- c(10^((-2):3), max(data$x)); 
  xlabels <- xscale;
  xlabels <- c(expression(10^-2), expression(paste(10^-1)),
      1, expression(10^1), expression(10^2), expression(10^3), expression(10^4));
#  xlabels[length(xlabels)] <- "10000";
  xlimits <- c(min(data$x), max(data$x));

  yscale <- seq(0, 1, 0.2);
  ylimits <- c(0, 1.05);
  ylabels <- yscale;

  axis.text.size <- traces.axis.text.size - 1;
  legend.text.size <- traces.legend.text.size - 1;
  legend.position <- c(0.25, 0.85);
  legend.direction <- "vertical";

  linetypes <- c(1, 2, 1, 2);
  colors <- my_color_full[c(1, 1, 2, 2)];
  xlab_name <- "Write-to-read ratios";
  ylab_name <- "Cumulative (%)";

  linetypes <- c(1, 1, 1);
  shapes <- c(ali.shape, tc.shape, msr.shape);
  colors <- c(ali.color, tc.color, msr.color);
  linesizes <- c(ali.linesize, tc.linesize, msr.linesize);

  types <- unique(data$type);
  data$type <- factor(data$type, levels = types);

  t <- ggplot(data = data, aes(x = x, y = y, colour = type, linetype = type, size = type)) + 
    geom_line(stat = "identity") +  
    geom_point(size = 2.5, aes(shape = type)) + 
    coord_cartesian(xlim = xlimits, ylim = ylimits) +           
    scale_x_continuous(breaks = xscale, labels = xlabels, trans = 'log10', expand = c(0.05, 0.05)) + 
    scale_y_continuous(breaks = yscale, labels = ylabels * 100, expand = c(0.01, 0.01)) + 
    scale_colour_manual(breaks = types, labels = types, values = colors) +
    scale_linetype_manual(breaks = types, labels = types, values = linetypes) +
    scale_shape_manual(breaks = types, labels = types, values = shapes) +
    scale_size_manual(breaks = types, labels = types, values = linesizes) +
    ylab(ylab_name) + xlab(xlab_name) + 
    theme_classic() +  
    theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black", size = 0.1),
        axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.25, colour = "black", size = axis.text.size),
        axis.title.y = element_text(size = axis.text.size, hjust = 0.5),
        axis.text.y = element_text(colour = "black", size = axis.text.size),
        axis.title.x = element_text(size = axis.text.size),
        legend.title = element_blank(),
        legend.position = legend.position,
        legend.key.size = unit(0.3, "cm"),
        legend.text = element_text(size = legend.text.size),
        legend.background = element_rect(size = 5, fill = alpha(NA, 0.5)),
        legend.direction = legend.direction,
        plot.margin = unit(c(0.15,0.15,0.15,0.15), "cm"));

  t
}

simplePdf("../figures/b_wr_ratio", mywidth, myheight, T);
print(myplot(0));

### Draw figures with WSS

ali_attr <- read.table("../processed/bs/ali_attr.data", 
    header = T, stringsAsFactors = F); 
msr_attr <- read.table("../processed/bs/msr_attr.data", 
    header = T, stringsAsFactors = F); 
tc_attr <- read.table("../processed/bs/tc_attr.data", 
    header = T, stringsAsFactors = F); 

xscale <- seq(0, 1, 0.1);  # second
xlabels <- xscale * 100; 
xlimits <- c(0, 1);
yscale <- seq(0, 1, 0.2)
ylimits <- c(0, 1.02);
ylabels <- yscale * 100;

myplot <- function(data_attr) {
  xlimits[1] <- 0.01;
  xlimits[2] <- 10^ceiling(log(max(c(data_attr$wss)) / 1024 / 256) / log(10));
  xscale <- 10^((-3):5);
  xlabels <- c(expression(10^-3), expression(10^-2), expression(10^-1), 1, 
      expression(10^1), expression(10^2), expression(10^3), expression(10^4), expression(10^5));

  xlab_name <- paste0("WSS (GiB)");
  ylab_name <- "Cumulative (%)";
  dataT1 <- toCdfFormatPure(data_attr$wss);
  dataT2 <- toCdfFormatPure(data_attr$urb);
  dataT3 <- toCdfFormatPure(data_attr$uwb);

  dataT1$type <- "Total";
  dataT2$type <- "Read";
  dataT3$type <- "Write";
  data <- rbind(dataT1, dataT2, dataT3);
  data$x <- data$x / 1024 / 256;
  
  for (type0 in unique(data$type)) {
    subs <- subset(data, type == type0 & x < xlimits[1]);
    if (nrow(subset(data, type == type0 & x == xlimits[1])) > 0) {
      next
    }
    data[nrow(data) + 1, ] <- list(xlimits[1], max(c(0, subs$y)), type0);
  }
  print(tail(data));
  data <- subset(data, x >= xlimits[1]);

  types <- unique(data$type);
  data$type <- factor(data$type, unique(data$type));

  linetypes <- c(1, 1, 1);
  sizes <- c(1.5, 0.5, 0.5);
  colors <- active.colors; 

  axis.text.size <- 14;
  legend.text.size <- 14;
  legend.position <- c(0.75, 0.4);
  legend.direction <- "vertical";

  xscale <- log(xscale) / log(10);
  xlimits <- log(xlimits) / log(10);
  data$x <- log(data$x) / log(10);

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
        axis.title.y = element_text(size = axis.text.size, hjust = 0.5, vjust = 0.5),
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

simplePdf("../figures/b_wss_ali", mywidth, myheight, T);
print(myplot(ali_attr));
simplePdf("../figures/b_wss_msr", mywidth, myheight, T);
print(myplot(msr_attr));
simplePdf("../figures/b_wss_tc", mywidth, myheight, T);
print(myplot(tc_attr));

#### Draw traffic

xscale <- seq(0, 1, 0.1);  # second
xlabels <- xscale * 100; 
xlimits <- c(0, 1);
yscale <- seq(0, 1, 0.2)
ylimits <- c(0, 1.02);
ylabels <- yscale * 100;

numPrinted <- 1;
myplot <- function(data_attr) {
  xlimits[1] <- 0.01;
  print(max(data_attr$twb) / 1024 / 256);
  print(max(data_attr$trb) / 1024 / 256);
  xlimits[2] <- 10^ceiling(log(max(c(data_attr$twb, data_attr$trb)) / 1024 / 256) / log(10));
  xscale <- 10^((-3):7);
  xlabels <- c(expression(10^-3), expression(10^-2), expression(10^-1), 1, 
      expression(10^1), expression(10^2), expression(10^3), expression(10^4), expression(10^5), expression(10^6), expression(10^7));

  xlab_name <- paste0("Traffic (GiB)");
  ylab_name <- "Cumulative (%)";
  dataT1 <- toCdfFormatPure(data_attr$trb);
  dataT2 <- toCdfFormatPure(data_attr$twb);

  dataT1$type <- "Read";
  dataT2$type <- "Write";
  data <- rbind(dataT1, dataT2);
  data$x <- data$x / 1024 / 256;
  
  for (type0 in unique(data$type)) {
    subs <- subset(data, type == type0 & x < xlimits[1]);
    if (nrow(subset(data, type == type0 & x == xlimits[1])) > 0) {
      next
    }
    data[nrow(data) + 1, ] <- list(xlimits[1], max(c(0, subs$y)), type0);
  }
  data <- subset(data, x >= xlimits[1]);
  print(tail(data));

  types <- unique(data$type);
  data$type <- factor(data$type, unique(data$type));

  linetypes <- c(1, 1);
  sizes <- c(0.5, 0.5);
  colors <- active.colors[2:3]; 

  axis.text.size <- 12.5;
  legend.text.size <- 13.5;
  legend.position <- c(0.85, 0.4);
  legend.direction <- "vertical";

  data$x <- log(data$x) / log(10);
  xscale <- log(xscale) / log(10);
  xlimits <- log(xlimits) / log(10);

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

simplePdf("../figures/b_traffic_ali", mywidth, myheight, T);
print(myplot(ali_attr));
simplePdf("../figures/b_traffic_msr", mywidth, myheight, T);
print(myplot(msr_attr));
simplePdf("../figures/b_traffic_tc", mywidth, myheight, T);
print(myplot(tc_attr));
