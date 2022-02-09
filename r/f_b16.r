source("common.r");

for (pf in prefices) {
  df <- read.table(paste0("../result/", pf, "_mr.data"), 
      header = F, stringsAsFactors = F);
  names(df) <- c("log", "trb", "twb", "ratio", "rh", "wh");

  hrs <- c();
  tps <- c();
  print(pf);

  for (rt in c(0.01, 0.1)) {
    subs <- subset(df, ratio == rt);
    subs$read_miss <- 1 - subs$rh / (subs$trb + 0.0000001);
    subs$write_miss <- 1 - subs$wh / (subs$twb + 0.0000001);
    print(paste0("read ", rt, " p25 ", 
          quantile(subs$read_miss, 0.25)));
    print(paste0("write ", rt, " p25 ", 
          quantile(subs$write_miss, 0.25)));

    hrs <- c(hrs, subs$read_miss);
    tps <- c(tps, paste0("R", subs$ratio * 100));
    hrs <- c(hrs, subs$write_miss);
    tps <- c(tps, paste0("W", subs$ratio * 100));
  }

  write.table(data.frame(num = round(hrs, digits = 5), type = tps),  
      file = paste0("../processed/mr/b16_", pf, ".data"), 
      quote = F, row.names = F, col.names = T);
}

if (!draw_figures) {
  q()
}

library(ggplot2)
library(testit);
library(scales);

mywidth <- 3
myheight <- 1.8

yscale <- seq(0, 1, 0.2); 
ylabels <- yscale * 100;
ylimits <- c(0, 1.16); 

pct90 <- function(data_vector) {
  data_vector <- data_vector[order(data_vector)];
  data_vector[floor(length(data_vector) * 0.9 + 0.9)]
}

myplot <- function(name) {
  xlab_name <- paste0("Types and Block percentages");

  orig_data <- read.table(paste0("../result/", name, "_mr.data"), header = F, stringsAsFactors = F);
  names(orig_data) <- c("log", "trb", "twb", "cachesize", "rh", "wh"); 
  print(head(orig_data));
  orig_data <- subset(orig_data, cachesize == 0.01 | cachesize == 0.1);

  log_arr <- c();
  num_arr <- c();
  type_arr <- c();
  for (lg in unique(orig_data$log)) {
    subs <- subset(orig_data, log == lg);

    log_arr <- c(log_arr, rep(subs$log, 2));
    num_arr <- c(num_arr, 
        1 - subs$rh / (subs$trb + 0.00001), 
        1 - subs$wh / (subs$twb + 0.00001));
    type_arr <- c(type_arr, 
        paste0("R", subs$cachesize * 100), paste0("W", subs$cachesize * 100));
  }

  data <- data.frame(log = log_arr, num = num_arr, type = type_arr);

  types <- c("R1", "R10", "W1", "W10");
  types <- types[order(types)];
  xlabels <- c("Read", "Write");
  subtypes_label <- c("1% WSS", "10% WSS");

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
  print(head(data));

  for (tp in types) {
    subs <- subset(data, type == tp);
    if (length(subs$num[is.nan(subs$num)]) > 0) {
      print(subs[is.nan(subs$num), ]);
      q()
    }
    print(paste0(name, " ", subs$x[1], " ", subs$subtype[1], " P25: ", 
          quantile(subs$num, 0.25))); 
  }

  axis.text.size <- f15.axis.text.size;
  legend.text.size <- f15.legend.text.size;
  legend.position <- c(0.5, 0.96);
  legend.direction <- "horizontal";

  t <- ggplot(data = data, aes(x = x, y = num, colour = subtype)) + 
    geom_boxplot(outlier.size = outlier.size, outlier.color = outlier.color, outlier.shape = outlier.shape) + 
#    stat_summary(fun.y=pct90, geom="point", shape=23, size=2, color = "#FF0000") + 
    coord_cartesian(ylim = ylimits) +           
    scale_y_continuous(breaks = yscale, labels = ylabels, expand = c(0.01, 0.01)) + 
    scale_colour_manual(breaks = subtypes_label, labels = subtypes_label, values = c(color1, color2)) + 
    ylab("Miss ratio (%)") + xlab("Request types") +
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

simplePdf("../figures/ali_mr", mywidth, myheight, T);
print(myplot("ali"));
simplePdf("../figures/msr_mr", mywidth, myheight, T);
print(myplot("msr"));
simplePdf("../figures/tc_mr", mywidth, myheight, T);
print(myplot("tc"));
