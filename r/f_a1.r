x1 <- read.table("../processed/bs/ali_attr.data", header = T, stringsAsFactors = F);
x2 <- read.table("../processed/bs/tc_attr.data", header = T, stringsAsFactors = F);
x3 <- read.table("../processed/bs/msr_attr.data", header = T, stringsAsFactors = F);

x1$rwb <- x1$trb + x1$twb;
x2$rwb <- x2$trb + x2$twb;
x3$rwb <- x3$trb + x3$twb;

x1$rwr <- x1$numRReq + x1$numWReq;
x2$rwr <- x2$numRReq + x2$numWReq;
x3$rwr <- x3$numRReq + x3$numWReq;

foo <- function(data1, data2, data3) {
  print(paste0("number of volumes ", nrow(data1), "  ", nrow(data2), "  ", nrow(data3))); 
}

foo(x1, x2, x3);

foo <- function(data1, data2, data3, val_col, name, coef = 1) {
  d1 <- round(sum(as.numeric(data1[, val_col])) * coef, digits = 3);
  d2 <- round(sum(as.numeric(data2[, val_col])) * coef, digits = 3);
  d3 <- round(sum(as.numeric(data3[, val_col])) * coef, digits = 3);
  print(paste0(name, ": ", d1, "  ", d2, "  ", d3, 
        "  (Ali-MSR) ", round(d1/d3, digits = 2), 
        " times, (Tenc-Ali) ", round(d2/d1, digits = 2), " times"));
}

foo(x1, x2, x3, "numRReq", "Read Requests");
foo(x1, x2, x3, "numWReq", "Write Requests");
foo(x1, x2, x3, "trb", "Read Traffic (TiB)", coef = 1 / 1024 / 256 / 1024);
foo(x1, x2, x3, "twb", "Write Traffic (TiB)", coef = 1 / 1024 / 256 / 1024);
foo(x1, x2, x3, "tub", "Update Traffic (TiB)", coef = 1 / 1024 / 256 / 1024);
foo(x1, x2, x3, "wss", "Total WSS (TiB)", coef = 1 / 1024 / 256 / 1024);
foo(x1, x2, x3, "urb", "Read WSS (TiB)", coef = 1 / 1024 / 256 / 1024);
foo(x1, x2, x3, "uwb", "Write WSS (TiB)", coef = 1 / 1024 / 256 / 1024);
foo(x1, x2, x3, "uub", "Update WSS (TiB)", coef = 1 / 1024 / 256 / 1024);

print("");
foo(x1, x2, x3, "rwr", "Total Requests");
foo(x1, x2, x3, "rwb", "Total Traffic (TiB)", coef = 1 / 1024 / 256 / 1024);

foo <- function(data1, data2, data3, val_col, val_col2, name, coef = 1) {
  d1 <- round(sum(as.numeric(data1[, val_col])) / sum(as.numeric(data1[, val_col2])), digits = 5);
  d2 <- round(sum(as.numeric(data2[, val_col])) / sum(as.numeric(data2[, val_col2])), digits = 5);
  d3 <- round(sum(as.numeric(data3[, val_col])) / sum(as.numeric(data3[, val_col2])), digits = 5);
  print(paste0(name, ": ", d1, "  ", d2, "  ", d3));
}

print("");
foo(x1, x2, x3, "urb", "wss", "Read coverage");
foo(x1, x2, x3, "uwb", "wss", "Write coverage");
