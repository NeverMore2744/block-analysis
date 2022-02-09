source("common.r");

proc <- function(pf, denominators, fn) {
  if (!file.exists(fn)) {
    data_attr <- read.table(
        paste0("../processed/mid2mid/", pf, "_per_device_per_10min_traffic.data"), 
        header = T, stringsAsFactor = F);
    rr <- rep(0, 144);
    wr <- rep(0, 144);
    rt <- rep(0, 144);
    wt <- rep(0, 144);

    for (i in 1:nrow(data_attr)) {
      timeInMin <- data_attr$timeInMin[i];
      slot_num <- (timeInMin %/% 10) %% 144 + 1;

      if (data_attr$type[i] == "RR") {
        rr[slot_num] <- rr[slot_num] + data_attr$value[i];
      } else if (data_attr$type[i] == "WR") {
        wr[slot_num] <- wr[slot_num] + data_attr$value[i];
      } else if (data_attr$type[i] == "RT") {
        rt[slot_num] <- rt[slot_num] + data_attr$value[i];
      } else if (data_attr$type[i] == "WT") {
        wt[slot_num] <- wt[slot_num] + data_attr$value[i];
      }
    }

    rr <- rr / denominators;
    wr <- wr / denominators;
    rt <- rt / denominators;
    wt <- wt / denominators;

    df <- data.frame(timeIn10min = 0:143, rr, wr, rt, wt);
    write.table(df, file = fn, quote = F, row.names = F, col.names = T)
  }

  df <- read.table(fn, header = T, stringsAsFactors = F);
  print(paste0(pf, " request day time: ", 
        sum(df$rr[37:108] + df$wr[37:108]) / 
        sum(df$rr + df$wr) * 100, " %"));
  print(paste0(pf, " traffic day time: ", 
        sum(df$rt[37:108] + df$wt[37:108]) / 
        sum(df$rt + df$wt) * 100, " %"));

  if (pf == "ali") {
    print(paste0("Ali 0:00-0:20 AM read traffic: ", 
          sum(df$rt[1:2]) / sum(df$rt) * 100, " %"));
  } else if (pf == "tc") {
    print(paste0("Tc 0:00-0:30 AM read traffic: ", 
          sum(df$rt[1:3]) / sum(df$rt) * 100, " %"));
  } else if (pf == "msr") {
    print(paste0("MSRC 1:00-2:30 AM read traffic: ", 
          sum(df$rt[7:15]) / sum(df$rt) * 100, " %"));
  }

}

proc("ali", rep(31, 144), "../processed/mid2mid/b8_ali_per_10min.data");
proc("tc", c(rep(10, 6), rep(8, 6), rep(9, 132)), 
    "../processed/mid2mid/b8_tc_per_10min.data");
proc("msr", rep(7, 144), "../processed/mid2mid/b8_msr_per_10min.data");

if (!draw_figures) {
  q()
}

library(ggplot2)
library(testit);
library(scales);
library(grid);


mywidth <- 3
myheight <- 1.8

yscale <- seq(0, 1, 0.2)
ylimits <- c(0, 1.15)

ali_attrs <- read.table("../processed/mid2mid/ali_per_10min.data", 
    header = T, stringsAsFactors = F);
tc_attrs <- read.table("../processed/mid2mid/tc_per_10min.data", 
    header = T, stringsAsFactors = F);
msr_attrs <- read.table("../processed/mid2mid/msr_per_10min.data", 
    header = T, stringsAsFactors = F);

myplot <- function(data_attr, yscale) {
  xlab_name <- paste0("Time (hour)");
  ylab_name <- paste0("# Reqs (million)");

  xscale <- seq(0, 24, 3);
  xlimits <- c(0, max(xscale));
  xlabels <- xscale;

  ylimits <- c(0, max(yscale));
  ylabels <- yscale / 1e6;

  data <- data.frame(
    x = rep(data_attr$timeIn10min / 6, 3),
    y = c(data_attr$rr + data_attr$wr, data_attr$rr, data_attr$wr),
    type = rep(c("R+W", "Read", "Write"), each = nrow(data_attr))
  ); 
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

simplePdf("../figures/f8_ali_req", mywidth, myheight, T);
print(myplot(ali_attrs, seq(0, 2e8, 5e7)))
simplePdf("../figures/f8_msr_req", mywidth, myheight, T);
print(myplot(msr_attrs, seq(0, 1e7, 2e6)))
simplePdf("../figures/f8_tc_req", mywidth, myheight, T);
print(myplot(tc_attrs, seq(0, 4e8, 1e8)))

myplot <- function(data_attr, yscale, legend = F) {
  xlab_name <- paste0("Time (hour)");
  ylab_name <- paste0("Traffic (TiB)");
  if (max(yscale) < 2^28) {
    ylab_name <- paste0("Traffic (GiB)");
  }

  xscale <- seq(0, 24, 3);
  xlimits <- c(0, max(xscale));
  xlabels <- xscale;

  ylimits <- c(0, max(yscale));
  ylabels <- yscale / 1024 / 1024 / 256;
  if (max(yscale) < 2^28) {
    ylabels <- yscale / 1024 / 256;
  }

  data_attr$rt <- as.numeric(data_attr$rt);
  data_attr$wt <- as.numeric(data_attr$wt);

  data <- data.frame(
    x = rep(data_attr$timeIn10min / 6, 3),
    y = c(data_attr$rt + data_attr$wt, data_attr$rt, data_attr$wt),
    type = rep(c("Read+Write", "Read", "Write"), each = nrow(data_attr))
  ); 
  types <- unique(data$type);
  data$type <- factor(data$type, levels = types);

  axis.text.size <- f5_7.axis.text.size;
  legend.text.size <- f5_7.legend.text.size;
  legend.position <- ""; # c(0.5, 0.9);
  if (legend) {
    legend.position <- c(0.5, 0.9);
  }
  legend.direction <- "horizontal";

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

simplePdf("../figures/f8_ali_traffic", mywidth, myheight, T);
print(myplot(ali_attrs, seq(0, 20 * 2^28, 5 * 2^28)))
simplePdf("../figures/f8_msr_traffic", mywidth, myheight, T);
print(myplot(msr_attrs, seq(0, 400 * 2^18, 100 * 2^18)))
simplePdf("../figures/f8_tc_traffic", mywidth, myheight, T);
print(myplot(tc_attrs, seq(0, 10 * 2^28, 2 * 2^28)))

simplePdf("../figures/f8_legend", mywidth, myheight, T);
t <- myplot(ali_attrs, seq(0, 50 * 2^28, 10 * 2^28), legend = T);
tmp <- ggplot_gtable(ggplot_build(t));
leg <- which(sapply(tmp$grobs, function(y) y$name) == "guide-box");
leg <- tmp$grobs[[leg]];
grid.draw(leg);
