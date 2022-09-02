source("common.r");

options(scipen=999);

x1 <- read.table("../processed/traffic/ali_per_device_active_time.data", header = T, stringsAsFactors = F);
x2 <- read.table("../processed/traffic/tc_per_device_active_time.data", header = T, stringsAsFactors = F);
x3 <- read.table("../processed/traffic/msr_per_device_active_time.data", header = T, stringsAsFactors = F);

foo <- function(df) {
  df$days <- df$maxTimeInMin %/% 1440 - df$minTimeInMin %/% 1440 + 1;

  dt <- data.frame(matrix(nrow = 0, ncol = 2), stringsAsFactors = F);
  names(dt) <- c("x", "y");
  sum <- 0;
  for (i in 0:max(df$days)) {
    sum <- sum + length(df$days[df$days == i]);
    dt[nrow(dt) + 1, ] <- list(i, sum); 
  }
  dt$y <- dt$y / max(dt$y);
  dt
}

foo_attr <- function(df, attr) {
  df$days <- df$maxTimeInMin %/% 1440 - df$minTimeInMin %/% 1440 + 1;
  df$mins <- df$maxTimeInMin - df$minTimeInMin + 1;
  df <- df[df$days <= 1, ];

  print(max(df$mins)); 
  print(min(df$mins));
  print(quantile(df$mins, c(0.25, 0.5, 0.75, 0.9)));
  print(nrow(df[df$mins <= 240, ]));

  attr_short <- attr[match(df$log, attr$log), ];
  print(paste0("wss: ", sum(as.numeric(attr_short$wss)) / sum(as.numeric(attr$wss))));
  print(paste0("tab: ", sum(as.numeric(attr_short$trb + attr_short$twb)) / sum(as.numeric(attr$trb + attr$twb))));
#  print(attr_short);
}

foo_attr(x1, read.table("../processed/bs/ali_attr.data", 
      header = T, stringsAsFactors = F)); 
q()

ali <- foo(x1);
ali$type <- "AliCloud";
tc <- foo(x2);
tc$type <- "TencentCloud";
msr <- foo(x3);
msr$type <- "MSRC";

data <- rbind(ali, tc, msr);
write.table(data, file = "../processed/traffic/a3_active_days.data", quote = F, 
    row.names = F, col.names = T);

if (!draw_figures) {
  q()
}

library(ggplot2)
library(testit);
library(scales);

alpha=1;
linetype=1;

my_color_full <- c("darkblue", "darkmagenta", "dodgerblue", "coral3",
    "mediumpurple", "burlywood3", "cadetblue3", "antiquewhite2", "white",
    "#00ffff")

mywidth <- 3
myheight <- 1.8

yscale <- seq(0, 1, 0.2)
ylimits <- c(0, 1.03)

myplot <- function(name_col, xlab_name, x_cuts) {
  data <- read.table("../processed/traffic/a3_active_days.data", header = T, stringsAsFactors = F);
  types <- unique(data$type); 
  data$type <- factor(data$type, types);

  xlimits <- c(min(x_cuts), max(x_cuts));
  xscale <- x_cuts; 
  xlabels <- xscale;

  ylabels <- yscale * 100;

  axis.text.size <- traces.axis.text.size - 1;
  legend.text.size <- traces.legend.text.size - 1;
  legend.position <- c(0.65, 0.85);
  legend.direction <- "vertical";

  colors <- c(ali.color, tc.color, msr.color);
  sizes <- c(ali.linesize, tc.linesize, msr.linesize);
  shapes <- c(ali.shape, tc.shape, msr.shape);

  t <- ggplot(data = data, aes(x = x, y = y, colour = type, linetype = type, size = type)) + 
    geom_line(stat = "identity") +  
#    geom_point(size = 1.5, aes(shape = type)) + 
    coord_cartesian(xlim = xlimits, ylim = ylimits) +           
    scale_x_continuous(breaks = xscale, labels = xlabels) + 
    scale_y_continuous(breaks = yscale, labels = ylabels, expand = c(0.01, 0.01)) + 
    scale_colour_manual(breaks = types, labels = types, values = colors) +
    scale_linetype_manual(breaks = types, labels = types, values = rep(1, length(types))) +
    scale_size_manual(breaks = types, labels = types, values = sizes) +
    ylab("Cumulative (%)") + xlab(xlab_name) +
    scale_shape_manual(values = shapes, breaks = types, labels = types) + 
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
        plot.margin = unit(c(0.15,0.15,0.15,0.15), "cm"));

  t
}

simplePdf("../figures/a3_active_days", mywidth, myheight, T);
print(myplot("duration_pct", "Number of active days", c(seq(0, 25, 5), 31))); # , c(0, "", 20, "", 40, "", 60, "", 80, "", 100)));
