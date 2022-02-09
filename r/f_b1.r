source("common.r");

foo <- function(fn, name, pf) {
  x1 <- read.table(fn, header = T, stringsAsFactors = F);

# The number of requests
  subs <- subset(x1, type == "R");
  subs$avg <- subs$avg / 60;
  subs$peak <- subs$peak / 60;

  if (pf == "tc") {
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

  print(paste0("Above 100 req/s in ", name, " ",  
        nrow(subs[subs$avg > 100, ]) / nrow(subs) * 100, " %"));
  print(paste0("Below 10 req/s in ", name, " ",  
        nrow(subs[subs$avg < 10, ]) / nrow(subs) * 100, " %"));
  print(paste0("Median of average intensity in ", name, " ",  
        median(subs$avg)));
  print(paste0("P90 of peak in ", name, " ", quantile(subs$peak, 0.9)));

  subs <- subs[order(-subs$avg), ];
  x_values <- rep(1:nrow(subs), 2);
  y_values <- c(subs$avg, subs$peak);
  type_values <- rep(c("Average", "Peak"), each = nrow(subs));
  
  df <- data.frame(x = x_values, y = y_values, type = type_values);

  write.table(df, file = paste0("../processed/traffic/b1_", pf, ".data"), 
      quote = F, row.names = F, col.names = T); 
}

prefices <- c("ali", "tc", "msr");
names <- c("AliCloud", "TencentCloud", "MSRC");
for (i in 1:length(prefices)) {
  pf <- prefices[i];
  nm <- names[i];
  fn <- paste0("../processed/traffic/", pf, "_per_device_traffic.data");
  foo(fn, nm, pf);
}

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
simplePdf("../figures/b1_ali", mywidth, myheight, T);
print(myplot(ali_attrs, c(0, 1080), seq(0, 1000, 200)));

tc_attrs <- read.table("../processed/traffic/b1_tc.data", 
    header = T, stringsAsFactors = F);
simplePdf("../figures/b1_tc", mywidth, myheight, T);
print(myplot(tc_attrs, c(0, 5380), seq(0, 5000, 1000)));

msr_attrs <- read.table("../processed/traffic/b1_msr.data",
    header = T, stringsAsFactors = F);
simplePdf("../figures/b1_msr", mywidth, myheight, T);
print(myplot(msr_attrs, c(0, 40), seq(0, 40, 10)));

