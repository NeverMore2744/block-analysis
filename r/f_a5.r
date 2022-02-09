source("common.r");
options(scipen=999);

ali_attr <- read.table("../processed/bs/ali_attr.data", header = T, stringsAsFactors = F); 
ali_cap <- read.table("../etc/ali_capacity.txt", header = T, stringsAsFactors = F);
ali_attr$capInGb <- ali_cap[match(ali_attr$log, ali_cap$log), "capacityInGiB"];

ali_attr$pct <- ali_attr$wss / 1024 / 256 / ali_attr$capInGb; 

data <- ali_attr;

data$type <- "40-49 GiB";
data$type[data$capInGb >= 50 & data$capInGb < 100] <- "50-99 GiB";
data$type[data$capInGb >= 100 & data$capInGb <= 199] <- "100-199 GiB";
data$type[data$capInGb >= 200] <- "200-5000 GiB";

types <- c("40-49 GiB", "50-99 GiB", "100-199 GiB", "200-5000 GiB");

for (tp in types) {
  subs <- subset(data, type == tp);
  print(paste0("Type ", tp, " volumes: ", nrow(subs), 
      " WSS-to-cap P80 ", quantile(subs$pct, 0.8) * 100, " %"));
  if (tp == types[length(types)]) {
    print(paste0("     ", tp, " P50: ", quantile(subs$pct, 0.5) * 100, " %,", 
      " WSS-to-cap > 50% ", nrow(subs[subs$pct > 0.5, ]) / nrow(subs)));
    print(paste0("     ", tp, " largest: ", subs$pct[subs$capInGb == max(subs$capInGb)] * 100, " %"));
  }
}

if (!draw_figures) {
  q()
}

mywidth <- 3
myheight <- 1.8

library(ggplot2)
library(scales);

xscale <- seq(0, 1, 0.2);
xlimits <- c(0, 1.02);
xlabels <- xscale * 100;
yscale <- seq(0, 1, 0.2)
ylimits <- c(0, 1.02);
ylabels <- yscale * 100;

numPrinted <- 1;
myplot <- function(data_attr) {
  xlab_name <- "WSS-to-capacity (%)";
  ylab_name <- "Cumulative (%)";

  data <- data_attr;

  data$type <- "40-49 GiB";
  data$type[data$capInGb >= 50 & data$capInGb < 100] <- "50-99 GiB";
  data$type[data$capInGb >= 100 & data$capInGb <= 199] <- "100-199 GiB";
  data$type[data$capInGb >= 200] <- "200-5000 GiB";
  types <- c("40-49 GiB", "50-99 GiB", "100-199 GiB", "200-5000 GiB");
  data$type <- factor(data$type, types);

  dataPlot <- NULL;
  for (type0 in types) {
    subs <- subset(data, type == type0);
    cdf <- toCdfFormatPure(subs$pct);
    cdf$type <- type0;
    dataPlot <- rbind(dataPlot, cdf);
  }

  linetypes <- c(22, 23, 24, 1);
  colors <- c("#cc0000", "#bbbb00", "#0000cc", "#910c00"); 

  axis.text.size <- 12;
  legend.text.size <- 12;
  legend.position <- c(0.72, 0.38);
  legend.direction <- "vertical";

  t <- ggplot(data = dataPlot, aes(x = x, y = y, color = type, linetype = type)) + geom_line(stat = "identity") +
    coord_cartesian(xlim = xlimits, ylim = ylimits) +           
    scale_x_continuous(breaks = xscale, labels = xlabels, expand = c(0.01, 0.01)) + 
    scale_y_continuous(breaks = yscale, labels = ylabels, expand = c(0.01, 0.01)) + 
    scale_color_manual(breaks = types, labels = types, values = colors) + 
    scale_linetype_manual(breaks = types, labels = types, values = linetypes) + 
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

simplePdf("../figures/b_ali_cap_wss", mywidth, myheight, T);
print(myplot(ali_attr));
