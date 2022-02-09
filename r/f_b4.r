source("common.r");

ali_t <- read.table("../processed/b4_ali.data", header = T, stringsAsFactors = F);
msr_t <- read.table("../processed/b4_msr.data", header = T, stringsAsFactors = F);

foo <- function(data, pct0) {
  t <- subset(data, pct == pct0);
  round(percentile(t$us, 0.5), digits = 2)
}
foo2 <- function(data, pct0) {
  t <- subset(data, pct == pct0);
  round(percentile(t$us, 0.5), digits = 2)
}

print(paste0("The medians of 25th, 50th and 75th percentiles in AliCloud: ",
      foo(ali_t, "25th"), "us, ", 
      foo(ali_t, "50th"), "us, ", 
      foo(ali_t, "75th"), "us"));
print(paste0("The medians of 25th, 50th and 75th percentiles in MSRC: ",
      foo(msr_t, "25th"), "us, ", 
      foo(msr_t, "50th"), "us, ", 
      foo(msr_t, "75th"), "us"));
print(paste0("50% of 25th percentiles in AliCloud and MSRC is below: ",
      foo2(ali_t, "25th"), "us, ", 
      foo2(msr_t, "25th"), "us"));

if (!draw_figures) {
  q()
}

library(ggplot2)
library(testit);
library(scales);

mywidth <- 3
myheight <- 1.8 

yscale <- seq(0, 1, 0.2)
ylimits <- c(0, 1.02)

ali_iat <- read.table("../processed/b4_ali.data", header = T, stringsAsFactors = F);
msr_iat <- read.table("../processed/b4_msr.data", header = T, stringsAsFactors = F);

proc <- function(data_iat, name) {
  pcts <- unique(data_iat$pct);
  data_iat$pct <- factor(data_iat$pct, levels = pcts);

  yscale <- c(0.001, 1, 10^3, 10^6, 10^9, 10^12);
  ylabels <- c(expression(10^-9), expression(paste(10^-6)),
      expression(10^-3), 1, expression(10^3), expression(10^6));
  ylimits <- 10^(c(-3, 12.2));

  axis.text.size <- 12;
  legend.text.size <- 12;
  legend.position <- c(0.85, 0.35);
  legend.direction <- "vertical";

  t <- ggplot(data = data_iat, aes(x = pct, y = us)) + 
    geom_boxplot(outlier.size = outlier.size, outlier.color = outlier.color, outlier.shape = outlier.shape) + 
    coord_cartesian(ylim = ylimits) +           
    scale_y_continuous(breaks = yscale, labels = ylabels, trans = 'log10', expand = c(0.01, 0.01)) + 
    ylab("Inter-arrival times (s)") + xlab("Percentiles") +
    theme_classic() +  
    theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black", size = 0.1),
        axis.text.x = element_text(angle = 0, hjust = 0.5, colour = "black", size = axis.text.size),
        axis.title.y = element_text(size = axis.text.size, hjust = 0.6),
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

simplePdf("../figures/b4_ali", mywidth, myheight, T);
print(proc(ali_iat, "AliCloud"));
simplePdf("../figures/b4_msr", mywidth, myheight, T);
print(proc(msr_iat, "MSRC"));

##### Info: Finding B.4

