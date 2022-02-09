source("common.r");

foo <- function(names, pfs) {
  df <- NULL;

  for (i in 1:length(names)) {
    pf <- pfs[i];
    name <- names[i];
    fn <- paste0("../processed/traffic/", pf, "_per_device_traffic.data");

    x1 <- read.table(fn, header = T, stringsAsFactors = F);
    subs <- subset(x1, type == "R");
    subs$avg <- subs$avg / 60;
    subs$peak <- subs$peak / 60;

    if (pf == "tc") { # Similar as f_b1.r
      tc_active_time <- 
        read.table("../processed/traffic/tc_per_device_active_time.data", 
            header = T, stringsAsFactors = F);

      tc_active_time$timeSpanInMin <- 
        tc_active_time$maxTimeInMin - tc_active_time$minTimeInMin + 1;

      tc_active_time$actualTimeInMin <- 
        ifelse(tc_active_time$minTimeInMin < (7*24 + 1)*60 + 2 &
            tc_active_time$maxTimeInMin > (7*24 + 2)*60 - 2, 
            tc_active_time$timeSpanInMin - 60, tc_active_time$timeSpanInMin);

      tc_active_time$tp <- 
        ifelse(tc_active_time$minTimeInMin < (7*24 + 1)*60 + 2 &
            tc_active_time$maxTimeInMin > (7*24 + 2)*60 - 2, 1, 2);

      print(paste0("  --  TencentCloud volumes across the missing 1 hour: ",
            nrow(tc_active_time[tc_active_time$tp == 1, ]), " / ", 
            nrow(tc_active_time), "  -- "));

      indices <- match(subs$log, tc_active_time$log);

      subs$timeInMin <- tc_active_time[indices, "actualTimeInMin"]; 

      subs$avg <- subs$avg * 
        tc_active_time$timeSpanInMin[indices] / 
        tc_active_time$actualTimeInMin[indices];
    }

    subs$burst <- subs$peak / subs$avg;
    cuts <- c(1, 10, 100, 1000, 10000);
    cdf <- toCdfFormatUsingCuts(subs$burst, cuts); 
    cdf$type <- name; 

    large_1000 <- nrow(subs[subs$burst >= 1000, ]) / nrow(subs);
    large_10 <- nrow(subs[subs$burst >= 10, ]) / nrow(subs);

    print(paste0(name, " > 1000 burstness: ", 
          format(round(large_1000 * 100, digits = 3), nsmall = 3), " %"));
    print(paste0(name, " < 10 burstness: ", 
          format(round((1 - large_10) * 100, digits = 3), nsmall = 3), " %"));

    df <- rbind(df, cdf);
  }

  write.table(df, file = "../processed/traffic/b2_b3_ratio.data", 
      quote = F, row.names = F, col.names = T);
}

prefices <- c("ali", "tc", "msr");
names <- c("AliCloud", "TencentCloud", "MSRC");
foo(names, prefices);

if (!draw_figures) {
  q()
}

library(ggplot2)
library(testit);
library(scales);

mywidth <- 3
myheight <- 1.8 

numPrinted <- 1;
myplot <- function(data, xlimits, xscale) {
#  xscale <- seq(0, xlimits[2], xlimits[2] %/% 4);
  xlabels <- xscale;

  yscale <- c(10^((-2):4));
  ylimits <- c(0.01, 10200);
  ylabels <- yscale;

  types <- unique(data$type);
  data$type <- factor(data$type, levels = types);

  if (xlimits[2] > 4000) {
    data <- data[c(1, 1:(nrow(data) %/% 2) * 2), ];
  }

  data_index <- data$type == "Peak";
  data_value <- data[data$type == "Peak", "y"];
  data_value <- data_value[order(-data_value)];
  data$y[data_index] <- data_value;

  colors <- c("#000000", "#000000");
  linetypes <- c(3, 1);
  sizes <- c(0.5, 0.2);

  axis.text.size <- 12.5;
  legend.text.size <- 13.5;
  legend.position <- c(0.5, 0.99);
  legend.direction <- "horizontal";

  xlab_name <- "Volumes";
  ylab_name <- "req/s";

  t <- ggplot(data = data, aes(x = x, y = y, colour = type, linetype = type, size = type)) + 
    geom_line(stat = "identity") +  
    coord_cartesian(xlim = xlimits, ylim = ylimits) +           
    scale_x_continuous(breaks = xscale, labels = xlabels, expand = c(0.01, 0.1)) + 
    scale_y_continuous(breaks = yscale, labels = ylabels, trans = 'log10', expand = c(0.01, 0.01)) + 
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

ali_attrs <- read.table("../processed/traffic/b1_ali.data", 
    header = T, stringsAsFactors = F);
simplePdf("b1_ali", mywidth, myheight, T);
print(myplot(ali_attrs, c(0, 1080), seq(0, 1000, 200)));

msr_attrs <- read.table("../processed/traffic/b1_msr.data", 
    header = T, stringsAsFactors = F);
simplePdf("b1_msr", mywidth, myheight, T);
print(myplot(msr_attrs, c(0, 40), seq(0, 40, 10)));

tc_attrs <- read.table("../processed/traffic/b1_tc.data", 
    header = T, stringsAsFactors = F);
simplePdf("b1_tc", mywidth, myheight, T);
print(myplot(tc_attrs, c(0, 5380), seq(0, 5000, 1000)));


myplot <- function(base) {
  xlimits <- c(1, 10000); 
  xscale <- c(1, 10, 100, 1000, 10000);
  xlabels <- xscale;

  yscale <- seq(0, 1, 0.2); 
  ylimits <- c(0, 1.05); 
  ylabels <- yscale * 100;

  data <- read.table("b2_b3_ratio.data", header = T, stringsAsFactors = F);
  types <- unique(data$type);
  print(types)
  data$type <- factor(data$type, levels = types);
  data <- subset(data, x %in% xscale);

  xlab_name <- "Burstiness ratios";
  ylab_name <- "Cumulative (%)";

  colors <- c(ali.color, tc.color, msr.color); 
  linetypes <- rep(1, length(types));
#  shapes <- c(6, 7);
  shapes <- c(ali.shape, tc.shape, msr.shape);

  axis.text.size <- 11;
  legend.text.size <- 11;
  legend.position <- c(0.75, 0.3);
  legend.direction <- "vertical";

  t <- ggplot(data = data, aes(x = x, y = y, colour = type, linetype = type, shape = type, size = type)) + 
    geom_line(stat = "identity") +  
    geom_point(size = 2, aes(shape = type)) + 
    coord_cartesian(xlim = xlimits, ylim = ylimits) +           
    scale_x_continuous(breaks = xscale, labels = xlabels, trans = 'log10', expand = c(0.05, 0.05)) + 
    scale_y_continuous(breaks = yscale, labels = ylabels, expand = c(0.01, 0.01)) + 
    scale_colour_manual(breaks = types, labels = types, values = colors) +
    scale_linetype_manual(breaks = types, labels = types, values = linetypes) +
    scale_size_manual(breaks = types, labels = types, values = c(ali.linesize, tc.linesize, msr.linesize)) +
    scale_shape_manual(breaks = types, labels = types, values = shapes) + 
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

mywidth <- 3
myheight <- 1.6 

simplePdf("../figures/b2_b3_ratio", mywidth, myheight, T);
print(myplot(0));
