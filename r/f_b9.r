source("common.r");

dirname <- "../result/";

foo <- function(df) {
  names(df) <- c("log", "t1", "t2", "seq_num", "total");
  df$randomness <- 1 - (df$seq_num / (df$total + 0.00000001));
  df <- df[, c("log", "randomness")];
}

ali_rand <- foo(read.table(paste0(dirname, "ali_rand.data"), 
    header = F, stringsAsFactors = F));
tc_rand <- foo(read.table(paste0(dirname, "tc_rand.data"), 
    header = F, stringsAsFactors = F));
msr_rand <- foo(read.table(paste0(dirname, "msr_rand.data"), 
    header = F, stringsAsFactors = F));

new_df <- NULL;

foo <- function(df, name, tp) {
  print(paste0("Median of ", name, " in randomness: ", 
        median(df$randomness)));
  print(paste0("More than 50% random requests: ", 
        nrow(subset(df, randomness >= 0.5)) / nrow(df) * 100.0, 
        " %"));

  data.frame(
      log = df$log, 
      randomness = df$randomness,
      type = rep(tp, nrow(df)));
}

new_df <- rbind(new_df, foo(ali_rand, "ali", "AliCloud"));
new_df <- rbind(new_df, foo(tc_rand, "tc", "Tencent"));
new_df <- rbind(new_df, foo(msr_rand, "msr", "MSRC"));

types <- c("AliCloud", "Tencent", "MSRC");

df <- toCdfFormatWithType(new_df$randomness, new_df$type, types, 0, 1, 100);
df <- subset(df, y >= 0 & y <= 1.01 & x >= 0 & x <= 1.01);
print(subset(df, y >= 0.99999));
df$x <- round(df$x, digits = 4) * 100;
df$y <- round(df$y, digits = 4);
write.table(df, file = "../processed/randomness/b9_cdf.data", 
    quote = F, row.names = F, col.names = T);

new_df <- NULL;

foo <- function(df_rand, name, tp) {
  attr <- read.table(paste0("../processed/bs/", name, 
        "_attr.data"), header = T, stringsAsFactors = F);
  attr <- attr[order(-(attr$trb + attr$twb)), ];
  attr <- attr[1:10, ];
  df_rand <- df_rand[match(attr$log, df_rand$log), ];

  print(paste0(name, " top-10 randomness: ", 
        round(min(df_rand$randomness) * 100, digits = 4), "-",
        round(max(df_rand$randomness) * 100, digits = 4), " %, traffic ",
        round(min(attr$trb + attr$twb) / 2^28, digits = 4), "-",
        round(max(attr$trb + attr$twb) / 2^28, digits = 4), " TiB")); 

  data.frame(
      randomness = df_rand$randomness * 100,
      traffic = (attr$trb + attr$twb) / 2^28,
      type = tp);
}

new_df <- rbind(new_df, foo(ali_rand, "ali", "AliCloud"));
new_df <- rbind(new_df, foo(tc_rand, "tc", "Tencent"));
new_df <- rbind(new_df, foo(msr_rand, "msr", "MSRC"));

write.table(new_df, file = "../processed/randomness/b9_dot.data", 
    quote = F, row.names = F, col.names = T);

if (!draw_figures) {
  q()
}

library(ggplot2)
library(testit);
library(scales);

mywidth <- 3.2
myheight <- 1.8

yscale <- seq(0, 1, 0.2)
ylimits <- c(0, 1.03)

myplot <- function(base) {
  xlimits <- c(0, 1) * 100;
  xscale <- seq(0, 1, 0.2) * 100;
  xlabels <- xscale;
  ylabels <- yscale;

  data <- read.table("../processed/randomness/b9_cdf.data", 
      header = T, stringsAsFactors = F);
  types <- unique(data$type);
  data$type <- factor(data$type, levels = types);

  axis.text.size <- b9_axis.text.size;
  legend.text.size <- b9_legend.text.size;
  legend.position <- c(0.75, 0.23);
  legend.direction <- "vertical";

  xlab_name <- "Randomness ratio (%)";
  ylab_name <- "Cumulative (%)";

  colors <- c(ali.color, tc.color, msr.color); 
  sizes <- c(ali.linesize, tc.linesize, msr.linesize);

  t <- ggplot(data = data, aes(x = x, y = y, colour = type, size = type)) + 
    geom_line(stat = "identity") +  
    coord_cartesian(xlim = xlimits, ylim = ylimits) +           
    scale_x_continuous(breaks = xscale, labels = xlabels) + 
    scale_y_continuous(breaks = yscale, labels = ylabels * 100, expand = c(0.01, 0.01)) + 
    scale_colour_manual(breaks = types, labels = types, values = colors) +
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

simplePdf("../figures/b9_cdf", mywidth, myheight, T);
print(myplot(0));

myplot <- function(base) {
  xlimits <- c(10^(-1), 10^2);
  xscale <- 10^((-1):2);
  ylimits <- c(0, 1) * 100;
  yscale <- seq(0, 1, 0.2) * 100;

  axis.text.size <- b9_axis.text.size;
  legend.text.size <- b9_legend.text.size;
  
  ret2 <- read.table("../processed/randomness/b9_dot.data", 
      header = T, stringsAsFactors = F);
  types <- unique(ret2$type);
  ret2$type <- factor(ret2$type, levels = types);

  colors <- c(ali.color, tc.color, msr.color); 
  sizes <- c(ali.linesize, tc.linesize, msr.linesize);
  shapes <- c(ali.shape, tc.shape, msr.shape);

  t <- ggplot(data = ret2, aes(x = traffic, y = randomness, colour = type, shape = type)) +
      geom_point(stat = "identity", size = 2.0, stroke = 1) +
      coord_cartesian(ylim = ylimits, xlim = xlimits) +
      scale_x_continuous(breaks = xscale, labels = as.character(xscale), 
          trans = 'log10', expand = c(0.05, 0.05)) +
      scale_y_continuous(breaks = yscale, labels = as.character(yscale), 
          expand = c(0.01, 0.01)) +
      scale_shape_manual(breaks = types, labels = types, values = shapes) +
      scale_colour_manual(breaks = types, labels = types, values = colors) + 
      scale_size_manual(breaks = types, labels = types, values = sizes) + 
      ylab("Randomness ratio (%)") + xlab("Total traffic size (TiB)") +
      theme_classic() +  
      theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black", size=0.1),
        axis.text.x=element_text(angle = 0, hjust = 0.5, colour = "black", size = axis.text.size),
        axis.title.y=element_text(size = axis.text.size, hjust = 0.85),
        axis.text.y=element_text(colour = "black",size = axis.text.size),
        axis.title.x=element_text(size = axis.text.size),
        legend.title=element_blank(),
        legend.position = c(0.35, 0.8),
        legend.key.size=unit(0.3, "cm"),
        legend.text=element_text(size = legend.text.size),
        legend.background = element_blank(), #element_rect(size=5, fill=alpha(NA, 0.5)),
        legend.direction = "vertical",
        legend.spacing.y = unit(0, "mm"),
        legend.box.background = element_rect(colour = "black"),
        plot.margin = unit(c(0.15,0.15,0.15,0.15), "cm"));

  t
}

simplePdf("../figures/b9_dot", mywidth, myheight, T);
print(myplot(0));
