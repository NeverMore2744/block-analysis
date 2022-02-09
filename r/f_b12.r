source("common.r");

proc <- function(pf) {
  df <- read.table(paste0("../processed/bs/", pf, "_attr.data"), 
      header = T, stringsAsFactors = F);
  df$update_coverage <- df$uub / (df$wss + 0.00001);
  df$ut_coverage <- df$tub / (df$twb + df$trb + 0.000001);

  print(paste0("Update coverage ", pf));
  print(paste0("    mean: ", mean(df$update_coverage)));
  print(paste0("    median: ", median(df$update_coverage)));
  print(paste0("    P90: ", quantile(df$update_coverage, 0.9)));

  print(paste0("Update traffic % ", pf));
  print(paste0("    mean: ", mean(df$ut_coverage)));
  print(paste0("    median: ", median(df$ut_coverage)));
  print(paste0("    P90: ", quantile(df$ut_coverage, 0.9)));

  print(paste0("Update coverage > 65 % in ", pf, ": ",
        nrow(subset(df, update_coverage >= 0.65)) / nrow(df) * 100, " %")); 
  print("");

  df
}

ali_attr <- proc("ali");
tc_attr <- proc("tc");
msr_attr <- proc("msr");

dataT0 <- toCdfFormat(tc_attr$update_coverage, 0, 1, 200); 
dataT0$type <- "Tencent";
dataT1 <- toCdfFormat(ali_attr$update_coverage, 0, 1, 200); 
dataT1$type <- "AliCloud";
dataT2 <- toCdfFormat(msr_attr$update_coverage, 0, 1, 200); 
dataT2$type <- "MSRC";
data <- rbind(dataT1, dataT0, dataT2);
data <- subset(data, x >= 0 & x <= 1.02 & y >= 0 & y <= 1.02);
data$type <- factor(data$type, unique(data$type));

data$x <- round(data$x, digits = 4);
data$y <- round(data$y, digits = 4);
write.table(data, file = "../processed/bs/b12_coverage.data", 
    quote = F, row.names = F, col.names = T);

######### Traffic coverage
dataT0 <- toCdfFormat(tc_attr$ut_coverage, 0, 1, 200); 
dataT0$type <- "Tencent";
dataT1 <- toCdfFormat(ali_attr$ut_coverage, 0, 1, 200); 
dataT1$type <- "AliCloud";
dataT2 <- toCdfFormat(msr_attr$ut_coverage, 0, 1, 200); 
dataT2$type <- "MSRC";
data <- rbind(dataT1, dataT0, dataT2);
data <- subset(data, x >= 0 & x <= 1.02 & y >= 0 & y <= 1.02);
data$type <- factor(data$type, unique(data$type));

data$x <- round(data$x, digits = 4);
data$y <- round(data$y, digits = 4);
write.table(data, file = "../processed/bs/b12_ut_coverage.data", 
    quote = F, row.names = F, col.names = T);

if (!draw_figures) {
  q()
}

library(ggplot2)
library(testit);
library(scales);
library(grid);
mywidth <- 3 
myheight <- 1.8

xscale <- seq(0, 1, 0.2);  # second
xlabels <- xscale * 100; 
xlimits <- c(0, 1);
yscale <- seq(0, 1, 0.2)
ylimits <- c(0, 1.02)

numPrinted <- 1;
myplot <- function(base) {
  xlab_name <- "Update coverage (%)";
  ylab_name <- "Cumulative (%)";

  data <- read.table("b12_coverage.data", header = T, stringsAsFactors = F);
  types <- unique(data$type);
  data$type <- factor(data$type, unique(data$type));

  axis.text.size <- f11_axis.text.size;
  legend.text.size <- f11_legend.text.size;
  legend.position <- ""; # c(0.85, 0.4);
  legend.direction <- "vertical";

  colors <- c(ali.color, tc.color, msr.color); 
#  colors <- my_color_full[1:length(types)];
  linetypes <- rep(1, length(types));
  sizes <- c(ali.linesize, tc.linesize, msr.linesize);

  t <- ggplot(data = data, aes(x = x, y = y, colour = type, linetype = type, size = type)) + 
    geom_line(stat = "identity") +  
    coord_cartesian(xlim = xlimits, ylim = ylimits) +           
    scale_x_continuous(breaks = xscale, labels = xlabels, expand = c(0.02,0.02)) + 
    scale_y_continuous(breaks = yscale, labels = yscale * 100, expand = c(0.01, 0.01)) + 
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

simplePdf("f11_coverage", mywidth, myheight, T);
print(myplot(0));

myplot <- function(base) {
  xlab_name <- "Percentage of update traffic (%)";
  ylab_name <- "Cumulative (%)";

  data <- read.table("f11_ut_coverage.data", header = T, stringsAsFactors = F);
  types <- unique(data$type);
  data$type <- factor(data$type, unique(data$type));

  axis.text.size <- f11_axis.text.size;
  legend.text.size <- f11_legend.text.size;
  legend.position <- ""; # c(0.85, 0.4);
  if (base > 0) {
    legend.position <- c(0.5, 0.9);
  }

  legend.direction <- "horizontal";

  colors <- c(ali.color, tc.color, msr.color); 
#  colors <- my_color_full[1:length(types)];
  linetypes <- rep(1, length(types));
  sizes <- c(ali.linesize, tc.linesize, msr.linesize);

  t <- ggplot(data = data, aes(x = x, y = y, colour = type, linetype = type, size = type)) + 
    geom_line(stat = "identity") +  
    coord_cartesian(xlim = xlimits, ylim = ylimits) +           
    scale_x_continuous(breaks = xscale, labels = xlabels, expand = c(0.02,0.02)) + 
    scale_y_continuous(breaks = yscale, labels = yscale * 100, expand = c(0.01, 0.01)) + 
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

simplePdf("f11_ut_coverage", mywidth, myheight, T);
print(myplot(0));

simplePdf("f11_coverage_legend", mywidth, myheight, T);
t <- myplot(1);
tmp <- ggplot_gtable(ggplot_build(t));
leg <- which(sapply(tmp$grobs, function(y) y$name) == "guide-box");
leg <- tmp$grobs[[leg]];
grid.draw(leg);
