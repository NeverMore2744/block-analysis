options(scipen=999);

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

new_mean <- function(data, val_col, cnt_col) {
  mult <- as.double(data[, val_col]) * as.double(data[, cnt_col]);
  sum_val <- sum(mult);
  sum_cnt <- sum(data[, cnt_col]);
  sum_val / sum_cnt
}

new_sd <- function(data, val_col, cnt_col, mn) {
  mult <- as.double(data[, cnt_col]) * as.double(data[, val_col] - mn)^2;
  sum_val <- sum(mult);
  sum_cnt <- sum(data[, cnt_col]);

  (sum_val / sum_cnt)^(0.5)
}

clear_cdf_with_types <- function(data) {
  ret <- data[1, c("x", "y", "type")];
  types <- unique(data$type);
  for (type0 in types) {
    t <- subset(data, type == type0); 
    prev_x <- min(t$x) - 1;
    prev_y <- 0;
    prev_printed <- F;

    t <- t[order(t$x), ];
    for (i in 1:nrow(t)) {
      if (prev_y != t$y[i]) {
        if (!prev_printed) {
          ret[nrow(ret) + 1, ] <- list(prev_x, prev_y, type0);
        }
        ret[nrow(ret) + 1, ] <- list(t$x[i], t$y[i], type0);
        prev_printed <- T;
      } else {
        prev_printed <- F;
      }
      prev_x <- t$x[i];
      prev_y <- t$y[i];
    }
  }

  ret
}

new_percentiles <- function(data, val_col, cnt_col, cuts, start = NULL, end = NULL) {
  if (is.null(start)) {
    start <- 1;
  }
  if (is.null(end)) {
    end <- nrow(data);
  }

  orders <- order(data[start:end, val_col]) + start - 1;
  data <- data[orders, ];
  sum_cnt <- sum(as.numeric(data[, cnt_col]));
  cuts <- cuts[order(cuts)];
  ret <- c();

  ind <- 1;

  tmp_cnt <- 0;

  for (i in 1:nrow(data)) {
    if (ind > length(cuts) || cuts[ind] > 1) {
      break
    }
    if (i %% 1000000 == 0) {
      print(i);
    }
    while (ind <= length(cuts) && tmp_cnt + data[i, cnt_col] >= sum_cnt * cuts[ind]) {
      ret <- c(ret, data[i, val_col]);
      if (length(ret) > 1 && ret[length(ret)] < ret[length(ret)-1]) {
        print("Error!");
        print(ret);
        q()
      }
      ind <- ind + 1;
    }
    tmp_cnt <- tmp_cnt + data[i, cnt_col];
  }

  ret
}

toCdfFormat <- function(data, start, end, num_intervals) {
  if (is.null(data) || length(data) == 0) {
    print("Error (toCdfFormat): data is empty");
  }
  data_ret <- data.frame(data);
  names(data_ret) <- c("x1");
  sorted <- data_ret[order(data_ret$x1), ];
  ret <- data.frame(matrix(ncol = 2, nrow = 0), stringsAsFactors=FALSE);
  names(ret) <- c("x", "y");
  len <- length(sorted);
## TODO: len = 0;

  interval <- (end - start) / num_intervals;

  if (2 * start - end < sorted[1]) {
    ret[nrow(ret) + 1, ] <- c(2 * start - end, 0);
  }
  if (2 * end - start >= sorted[len]) {
    ret[nrow(ret) + 1, ] <- c(2 * end - start, 1);
  }

# first phase: before "start"
  pointer <- 1;
  cumu <- 0;

  for (i in 1:len) {
    if (sorted[i] <= start - interval) {
      cumu <- cumu + 1 / len;
      pointer <- i + 1;
    } else {
      break
    }
  }
  ret[nrow(ret) + 1, ] <- c(start - interval, cumu);

# second phase: after "start"

  x <- start;
  first <- 1;

  last_x <- start - interval;
  last_cumu <- cumu;

  if (pointer <= len) {
    i <- pointer;
    while (i <= len && sorted[i] <= 2 * end - start) {
      while (i <= len && sorted[i] <= x) {
        cumu <- cumu + 1 / len;
        i <- i + 1;
      }

# ADDED
      if (cumu == last_cumu) {
        last_x <- x;
      } else {
        if (x > last_x) {
          ret[nrow(ret) + 1, ] <- c(last_x, last_cumu);
        }
        ret[nrow(ret) + 1, ] <- c(x, cumu);
        last_x <- x;
        last_cumu <- cumu;
      }
  # Ori: ret[nrow(ret) + 1, ] <- c(x, cumu);
      x <- x + interval;
    }
  }

  ret[order(ret$x), ];
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

toCdfFormatWithType <- function(data, type, types, 
    start, end, num_intervals = 0) {
  tmp_data <- data.frame(data);
  tmp_data$type <- data.frame(type);
  names(tmp_data) <- c("x", "type");

# Default: 200
  if (num_intervals == 0) num_intervals <- 200;

  ret <- data.frame(matrix(ncol = 3, nrow = 0), stringsAsFactors = FALSE); 
  names(ret) <- c("x", "y", "type");

  for (i in 1:length(types)) {
    data0 <- tmp_data[tmp_data$type == types[i], ];
    if (nrow(data0) > 0) {
      ret_rows <- toCdfFormat(data0$x, start, end, num_intervals);
      ret_rows$type <- rep(types[i], nrow(ret_rows));
      ret <- rbind(ret, ret_rows);
    }
  } 
  ret$type <- factor(ret$type, levels = types);
  ret;
}

toCdfFormatByFreqDirect <- function(data, val_col, freq_col, label, step = 0.001) {
  data <- data[order(data[, val_col]), ];
  sum0 <- sum(data[, freq_col]);

  prev_val <- min(data[, val_col]) - 1;
  cumu <- 0;

  ret <- data.frame(matrix(ncol = 2, nrow = 0), stringsAsFactors = F);
  names(ret) <- c("x", "y");

  for (i in 1:nrow(data)) {
    if (data[i, val_col] != prev_val || i == nrow(data)) {
      ret[nrow(ret) + 1, ] <- list(prev_val, cumu / sum0);
    }
    cumu <- cumu + data[i, freq_col];
    prev_val <- data[i, val_col];
  }
  ret$type <- label;

  ret
}

toCdfFormatUsingCuts <- function(data_vector, cuts) {
  ret <- data.frame(matrix(ncol = 2, nrow = 0), stringsAsFactors = FALSE); 
  names(ret) <- c("x", "y");
  data_vector <- data_vector[order(data_vector)];
  cuts <- cuts[order(cuts)];

  ind <- 1;
  for (i in 1:(length(data_vector) + 1)) {
    if (i <= length(data_vector) && data_vector[i] <= cuts[ind]) {
      next
    }
    while (i > length(data_vector) || (ind <= length(cuts) && data_vector[i] > cuts[ind])) {
      if (ind > length(cuts)) {
        break
      }
      ret[nrow(ret) + 1, ] <- list(cuts[ind], (i-1) / length(data_vector));
      ind <- ind + 1;
    }
    if (ind > length(cuts)) {
      break
    }
  }

  ret
}

toCdfFormatUsingCutsWithCnts <- function(data, val_col, cnt_col, 
    count_cuts, descending = F, upper_bound = F) {
  ret <- data.frame(matrix(ncol = 3, nrow = 0), stringsAsFactors = FALSE); 
  names(ret) <- c("x", "y", "multiplied"); # Multiplied is the cumu multiplied
  data <- data[order(data[, val_col]), ];
  data$multiplied <- data[, val_col] * data[, cnt_col];
  total_cnts <- sum(data[, cnt_col]);
  total_multiplied <- sum(data$multiplied);

  count_cuts <- ceiling(count_cuts[order(count_cuts)] * total_cnts);
  if (descending) {
    data <- data[order(-data[, val_col]), ];
  }

  ind <- 1;
  cumu_cnts <- 0;
  cumu_multiplied <- 0;
  for (i in 1:nrow(data)) {
    if (ind > length(count_cuts)) {
      break
    }

    if (data[i, cnt_col] + cumu_cnts < count_cuts[ind]) {
      cumu_cnts <- cumu_cnts + data[i, cnt_col];
      cumu_multiplied <- cumu_multiplied + data$multiplied[i];
      next
    }
    tmp_left_cnts <- data[i, cnt_col];
    while (ind <= length(count_cuts) && (tmp_left_cnts > 0 || cumu_cnts >= count_cuts[ind])) {
      if (tmp_left_cnts + cumu_cnts < count_cuts[ind]) {  # Add the left counts, turn to the next item
        cumu_cnts <- cumu_cnts + tmp_left_cnts;
        cumu_multiplied <- cumu_multiplied + tmp_left_cnts * data[i, val_col];
        break
      }

      {  # Add some counts such that the next percentile is reached
        tmp_cnts <- count_cuts[ind] - cumu_cnts; 

        ret[nrow(ret) + 1, ] <- list(data[i, val_col], count_cuts[ind] / total_cnts, cumu_multiplied + tmp_cnts * data[i, val_col]);

        cumu_multiplied <- cumu_multiplied + tmp_cnts * data[i, val_col];
        cumu_cnts <- cumu_cnts + tmp_cnts;

        tmp_left_cnts <- tmp_left_cnts - tmp_cnts;
        ind <- ind + 1;
      }
    }
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

new_div <- function(data1, data2) {
  res <- c();
  for (i in 1:length(data1)) {
    t <- 0;
    if (data1[i] > 0) {
      t <- data1[i] / data2[i];
      if (data2[i] == 0) {
        t <- 999999999;
      }
    }
    res <- c(res, t);
  }

  res
}

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

b9_axis.text.size <- 12;
b9_legend.text.size <- 12;

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

draw_figures <- F;
