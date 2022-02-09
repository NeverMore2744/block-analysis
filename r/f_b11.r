source("common.r");

proc <- function(pf) {
  attr <- read.table(paste0("../processed/bs/", pf, "_attr.data"), 
      header = T, stringsAsFactors = F);
  rw <- read.table(paste0("../processed/bs/", pf, "_rw_only.data"),
      header = T, stringsAsFactors = F);

  logs <- rep(attr$log, 2);
  indices <- match(rw$log, attr$log);
  ratios <- c(rw$read_on_read_only[indices] / (attr$trb + 0.0000001), 
      rw$write_on_write_only[indices] / (attr$twb + 0.0000001));
  types <- rep(c("Reads in read-mostly", "Writes in write-mostly"), each = nrow(attr));

  print(paste0(pf, " reads in read-mostly: ", 
        sum(as.numeric(rw$read_on_read_only)) / 
        sum(as.numeric(attr$trb)) * 100, " %")); 
  print(paste0(pf, " writes in write-mostly: ", 
        sum(as.numeric(rw$write_on_write_only)) / 
        sum(as.numeric(attr$twb)) * 100, " %")); 

  if (sum(as.numeric(rw$read_on_read_only) + 
        as.numeric(rw$read_on_others)) != 
      sum(as.numeric(attr$trb))) {
    print(paste0("Warning: trb not equal")); 
    q()
  }
  if (sum(as.numeric(rw$write_on_write_only) + 
        as.numeric(rw$write_on_others)) != 
      sum(as.numeric(attr$twb))) {
    print(paste0("Warning: twb not equal")); 
    q()
  }

  ret <- data.frame(log = logs, ratio = ratios, type = types);
  print(head(ret));

  for (tp in unique(ret$type)) {
    subs <- subset(ret, type == tp);
    print(paste0("Median of ", tp, ": ", 
          median(subs$ratio) * 100, " %"));
  }

  types <- unique(types);
  print(types);
  df <- toCdfFormatWithType(ret$ratio, ret$type, types, 0, 1, 200);
  df$x <- df$x * 100;
  df$y <- df$y * 100;
  write.table(df, file = paste0("../processed/bs/b11_", pf, ".csv"), 
       quote = F, row.names = F, col.names = T); 
}

proc("ali"); 
proc("tc"); 
proc("msr");

if (!draw_figures) {
  q();
}

library(ggplot2)
library(testit);
library(scales);

mywidth <- 3 
myheight <- 1.8

ali_ratios <- read.csv("../processed/bs/b11_ali.csv", 
    header = T, stringsAsFactors = F);
msr_ratios <- read.csv("../processed/bs/b11_msr.csv", 
    header = T, stringsAsFactors = F);
tc_ratios <- read.csv("../processed/bs/b11_tc.csv", 
    header = T, stringsAsFactors = F);

xscale <- seq(0, 1, 0.2);  # second
xlabels <- xscale; 
xlimits <- c(0, 1);
yscale <- seq(0, 1, 0.2); 
ylabels <- yscale; 
ylimits <- c(0, 1);

numPrinted <- 1;
myplot <- function(data_t, name) {
  xlab_name <- paste0("% of traffic");
  ylab_name <- paste0("Cumulative (%)");

  data <- data_t;
  types <- unique(data_t$type);
  types_label <- c("Reads to read-mostly", "Writes to write-mostly");

  coef <- 100;
  data$type <- factor(data$type, levels = types);

  axis.text.size <- 13;
  legend.text.size <- 13;
  legend.position <- c(0.45, 0.9);
  legend.direction <- "vertical";

  t <- ggplot(data = data, aes(x = x, y = y, colour = type, linetype = type)) + 
    geom_line(stat = "identity") +  
    coord_cartesian(xlim = xlimits * coef, ylim = ylimits * coef) +           
    scale_x_continuous(breaks = xscale * coef, labels = xlabels * coef) +
    scale_y_continuous(breaks = yscale * coef, labels = ylabels * coef, expand = c(0.01, 0.01)) + 
    scale_colour_manual(breaks = types, labels = types_label, values = my_color_full[c(1,2)]) +
    scale_linetype_manual(breaks = types, labels = types_label, values = c(1, 2)) +
    guides(color=guide_legend(ncol=1, keywidth=1.4, keyheight=0.8),
        linetype=guide_legend(ncol=1, keywidth=1.4, keyheight=0.8)) +
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

simplePdf("../figures/b11_ali", mywidth, myheight, T);
print(myplot(ali_ratios, "ali"));
simplePdf("../figures/b11_msr", mywidth, myheight, T);
print(myplot(msr_ratios, "msr"));
simplePdf("../figures/b11_tc", mywidth, myheight, T);
print(myplot(tc_ratios, "tc"));
