source("common.r");

mywidth <- 3 
myheight <- 1.8 

cdf_file <- "../processed/reqsz/a2_req_size_cdf.data";
box_file <- "../processed/reqsz/a2_req_size_box.data";

if (!file.exists(cdf_file)) {
  ali_rs <- read.table("../processed/reqsz/ali_overall_request_size.data", 
      header = T, stringsAsFactors = F);
  ali_rs$num <- ali_rs$read_num + ali_rs$write_num;

  tc_rs <- read.table("../processed/reqsz/tc_overall_request_size.data", 
      header = T, stringsAsFactors = F);
  tc_rs$num <- tc_rs$read_num + tc_rs$write_num;

  msr_rs <- read.table("../processed/reqsz/msr_overall_request_size.data", 
      header = T, stringsAsFactors = F);
  msr_rs$num <- msr_rs$read_num + msr_rs$write_num;

  dataT1 <- toCdfFormatByFreqDirect(tc_rs, "size", "read_num", "T.R");
  dataT2 <- toCdfFormatByFreqDirect(msr_rs, "size", "read_num", "M.R"); 
  dataT5 <- toCdfFormatByFreqDirect(ali_rs, "size", "read_num", "C.R"); 
  dataT3 <- toCdfFormatByFreqDirect(tc_rs, "size", "write_num", "T.W"); 
  dataT4 <- toCdfFormatByFreqDirect(msr_rs, "size", "write_num", "M.W"); 
  dataT6 <- toCdfFormatByFreqDirect(ali_rs, "size", "write_num", "C.W"); 
  data <- rbind(dataT5, dataT6, dataT1, dataT3, dataT2, dataT4);

  print("Overall request sizes:");
  for (tp in unique(data$type)) {
    subs <- subset(data, type == tp & y >= 0.75);
    print(paste0("Type ", tp, " overall P75: ", min(subs$x) / 1024, " KiB")); 
  }
  write.table(data, file = cdf_file, quote = F, row.names = F, col.names = T, sep = ',');
}

if (!file.exists(box_file)) {
  tc_rs <- read.table("../processed/reqsz/tc_request_size.data", header = T, stringsAsFactors = F);
  tc_rs$num <- tc_rs$read_num + tc_rs$write_num;
  msr_rs <- read.table("../processed/reqsz/msr_request_size.data", header = T, stringsAsFactors = F);
  msr_rs$num <- msr_rs$read_num + msr_rs$write_num;
  ali_rs <- read.table("../processed/reqsz/ali_request_size.data", header = T, stringsAsFactors = F);
  ali_rs$num <- ali_rs$read_num + ali_rs$write_num;

  proc2 <- function(data_rs, val_col, cnt_col, cuts, type) {
    ret <- c();

    tmp <- data_rs[, val_col] %% 512;  # Check
    if (length(unique(tmp)) > 1) {
      print(paste0("Something wrong here: unique(tmp) = ", unique(tmp), 
            " type = ", type));
    }

    for (lg in unique(data_rs$log)) {
      t <- subset(data_rs, log == lg);
      mn <- new_mean(t, val_col, cnt_col); 
      if (is.nan(mn)) {
        next
      }
      ret <- c(ret, mn);
    }

    print(paste0("Type ", type, " P75: ", quantile(ret, 0.75) / 1024, " KiB"));

    dt <- toCdfFormat(ret, 0, 1024 * 1024, 2000); 
    dt$type <- type;
    dt 
  }

  dataT3 <- proc2(ali_rs, "size", "read_num", (1:19)/20, "C.R"); 
  dataT6 <- proc2(ali_rs, "size", "write_num", (1:19)/20, "C.W"); 
  dataT1 <- proc2(tc_rs, "size", "read_num", (1:19)/20, "T.R");
  dataT4 <- proc2(tc_rs, "size", "write_num", (1:19)/20, "T.W"); 
  dataT2 <- proc2(msr_rs, "size", "read_num", (1:19)/20, "M.R"); 
  dataT5 <- proc2(msr_rs, "size", "write_num", (1:19)/20, "M.W"); 
  data <- rbind(dataT1, dataT4, dataT2, dataT5, dataT3, dataT6);
  data$x <- round(data$x, digits = 3);
  data$y <- round(data$y, digits = 3);
  data <- clear_cdf_with_types(data);
  write.table(data, file = box_file, quote = F, row.names = F, col.names = T);
}

if (!draw_figures) {
  q();
}

library(ggplot2)
library(testit);
library(scales);
library(grid);

myplot <- function(base) {
  data <- read.table(cdf_file, header = T, stringsAsFactors = F); 

  types <- c("C.R", "C.W", "T.R", "T.W", "M.R", "M.W");
  type_labels <- c("AliCloud Read", "AliCloud Write", "TencentCloud Read", "TencentCloud Write", 
    "MSRC Read", "MSRC Write");
  data$type <- factor(data$type, levels = types);

  for (type0 in types) {
    data[nrow(data) + 1, ] <- list(0.1, 0, type0);
    data[nrow(data) + 1, ] <- list(1024 * 1024, 1, type0);
  }

  xscale <- c(0.1, 1, 10, 100, 1000) * 1024;  # second
  xlabels <- xscale / 1024; 
  xlimits <- c(0.1, 1000) * 1024; 

  yscale <- seq(0, 1, 0.2);
  ylabels <- yscale * 100;
  ylimits <- c(0, 1.02);

  axis.text.size <- traces.axis.text.size;
  legend.text.size <- traces.legend.text.size;
  legend.position <- ""; # c(0.72, 0.3);
  legend.direction <- "vertical";

  linetypes <- rep(c(1, 24), length(types) / 2);
  colors <- rep(c(ali.color, tc.color, msr.color), each = 2);
  sizes <- rep(c(ali.linesize, tc.linesize, msr.linesize), each = 2);
  xlab_name <- "Request size (KiB)";
  ylab_name <- "Cumulative (%)";

  t <- ggplot(data = data, aes(x = x, y = y, colour = type, linetype = type, size = type)) + 
    geom_line(stat = "identity") +  
    coord_cartesian(xlim = xlimits, ylim = ylimits) +           
    scale_x_continuous(breaks = xscale, labels = xlabels, trans = 'log10') + 
    scale_y_continuous(breaks = yscale, labels = ylabels, expand = c(0.01, 0.01)) + 
    scale_colour_manual(breaks = types, labels = type_labels, values = colors) +
    scale_linetype_manual(breaks = types, labels = type_labels, values = linetypes) +
    scale_size_manual(breaks = types, labels = type_labels, values = sizes) +
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
        plot.margin = unit(c(0.15,0.15,0.15,0.15), "cm"));

  t
}

simplePdf("../figures/a2_req_size_cdf", mywidth, myheight, T);
print(myplot(0));

myplot <- function(base, use_legend = 0) {
  data <- subset(read.table(box_file, header = T, stringsAsFactors = F), x > 0);

  types <- unique(data$type);
  types <- c("C.R", "C.W", "T.R", "T.W", "M.R", "M.W");
  type_labels <- c("AliCloud Read", "AliCloud Write", "TencentCloud Read", "TencentCloud Write", 
    "MSRC Read", "MSRC Write");

  data$type <- factor(data$type, levels = types);

  for (type0 in types) {
    data[nrow(data) + 1, ] <- list(0.1, 0, type0);
    data[nrow(data) + 1, ] <- list(1000 * 1024, 1, type0);
  }

  xscale <- c(1, 10, 100, 1000) * 1024;  # second
  xlabels <- xscale / 1024; 
  xlimits <- c(1024, 1000 * 1024); 
  yscale <- seq(0, 1, 0.2);
  ylabels <- yscale * 100;
  ylimits <- c(0, 1);

  axis.text.size <- traces.axis.text.size;
  legend.text.size <- traces.legend.text.size;
  legend.position <- c(0.5, 0.5);
  if (use_legend == 0) {
    legend.position <- "";
  }
  legend.direction <- "horizontal";

  linetypes <- rep(c(1, 24), length(types) / 2);
  colors <- rep(c(ali.color, tc.color, msr.color), each = 2);
  sizes <- rep(c(ali.linesize, tc.linesize, msr.linesize), each = 2);
  xlab_name <- "Average request size (KiB)";
  ylab_name <- "Cumulative (%)";

  print(head(data));

  t <- ggplot(data = data, aes(x = x, y = y, colour = type, linetype = type, size = type)) + 
    geom_line(stat = "identity") +  
    coord_cartesian(xlim = xlimits, ylim = ylimits) +           
    scale_x_continuous(breaks = xscale, labels = xlabels, trans = 'log10') + 
    scale_y_continuous(breaks = yscale, labels = ylabels, expand = c(0.01, 0.01)) + 
    scale_colour_manual(breaks = types, labels = type_labels, values = colors) +
    scale_linetype_manual(breaks = types, labels = type_labels, values = linetypes) +
    scale_size_manual(breaks = types, labels = type_labels, values = sizes) +
    guides(color=guide_legend(ncol=4, keywidth=1.6, keyheight=0.8),
        linetype=guide_legend(ncol=4, keywidth=1.6, keyheight=0.8),
        size=guide_legend(ncol=3, keywidth=1.6, keyheight=0.8)) +
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
        plot.margin = unit(c(0.15,0.15,0.15,0.15), "cm"));

  t
}

simplePdf("../figures/a2_req_size_box", mywidth, myheight, T);
t <- myplot(0, 0);
print(t);

t <- myplot(0, 1);
simplePdf("../figures/a2_req_size_legend", 10, 3, T);
tmp <- ggplot_gtable(ggplot_build(t));
leg <- which(sapply(tmp$grobs, function(y) y$name) == "guide-box");
leg <- tmp$grobs[[leg]];
grid.draw(leg);
