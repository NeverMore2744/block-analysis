prefices <- c("ali", "tc", "msr");
simplePdf <- function(output_name, mywidth = 4, myheight = 3, local = F) {
  str <- "../";
  if (local) {
    str <- "";
  }
  cairo_pdf(file = paste0(str, output_name, ".pdf"), family = "Times", width = mywidth, height = myheight)
}

percentile <- function(data_vector, pcts) {
  pcts <- pcts[order(pcts)];
  data_vector <- data_vector[order(data_vector)];
  ret_vector <- c();
  nr <- length(data_vector);

  for (pct in pcts) {
    ind <- pct * nr + 0.9;
    if (ind == 0) {
      ind = 1;
    }

    ret_vector <- c(ret_vector, data_vector[ind]);
  }

  ret_vector;
}

toCdfFormatPure <- function(data_vector) {
  ret <- data.frame(matrix(ncol = 2, nrow = 0), stringsAsFactors = FALSE); 
  names(ret) <- c("x", "y");
  data <- data_vector[order(data_vector)]; 
  ret[nrow(ret) + 1, ] <- list(min(data) - 1, 0);
  for (i in 1:length(data)) {
    if (i < length(data) && data[i] == data[i+1]) {
      next
    }
    ret[nrow(ret) + 1, ] <- list(data[i], i / length(data));
  }

  ret
}

# TODO Inaccurate
percentile_in_cdf <- function(data, pcts) {
  pcts <- pcts[order(pcts)];
  data <- data[order(data$x), ];
  ret_vector <- c();
  
  for (pct in pcts) {
    t <- subset(data, y <= pct);
    x <- max(t$x);
    if (is.nan(x)) {
      x <- min(data$x);
    }
    ret_vector <- c(ret_vector, x);
  }

  ret_vector
}

gglegend <- function(x){
  tmp <- ggplot_gtable(ggplot_build(x))
  leg <- which(sapply(tmp$grobs, function(y) y$name) == "guide-box")
  tmp$grobs[[leg]]
}

options(scipen=999);
my_color_full <- c("darkblue", "darkmagenta", "dodgerblue", "coral3", "mediumpurple", "burlywood3", "cadetblue3", "antiquewhite2", "white", "#00ffff")

outlier.shape <- 4;
outlier.color <- "#ff8888";
outlier.size <- 1.5;

color1 <- "#882222";
color2 <- "#66cc66";

msr.color <- "#8888ff";
tc.color <- "#66cc66";
ali.color <- "#000000";
msr.shape <- 1;
tc.shape <- 3;
ali.shape <- 2;
msr.linesize <- 0.5;
tc.linesize <- 0.7;
ali.linesize <- 0.9;

traces.axis.text.size <- 12;
traces.legend.text.size <- 12;

f5_7.axis.text.size <- 13;
f5_7.legend.text.size <- 13;

f8_axis.text.size <- 12;
f8_legend.text.size <- 12;

f9_axis.text.size <- 13;
f9_legend.text.size <- 13;

f11_axis.text.size <- 11.5;
f11_legend.text.size <- 11.5;

f12_13.axis.text.size <- 13;
f12_13.legend.text.size <- 13;

f14.axis.text.size <- 13;
f14.legend.text.size <- 13;
f14_levels.axis.text.size <- 13;
f14_levels.legend.text.size <- 13;

f15.axis.text.size <- 13;
f15.legend.text.size <- 13;

active.colors <- c("#dddd88", "mediumpurple", "#222222");
