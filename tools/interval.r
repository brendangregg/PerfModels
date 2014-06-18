# interval.r	R line graph of interval measurements.
#
# USAGE: R --no-save < interval.r
#
# This time I'm putting it on github where I won't lose it.
#
# Input is a single column of measurements, taken at a known interval. The
# number of input elements, interval, and column number can be customized (see
# the N, interval, and data variables).
#
# 17-Jun-2014	Brendan Gregg	Created this.

filename <- "data.txt"
pdf("interval.pdf", w=10, h=6)
N <- 121			# max number of elements
interval <- 5			# interval
xlab <- "Time (secs)"		# x-axis label
ylab <- "Measurement"		# y-axis label
title <- "Plot of data.txt"	# plot title

input <- read.table(filename, header=FALSE, nrows=N)
data <- input$V2		# use 2nd column
xaxis <- seq(0, N * interval - interval, interval)

# type: p=points, l=lines, n=none, o=overplotted, b=both; cex=size
plot(xaxis, data, main=title, type="o", cex=0.6, xlab=xlab, ylab=ylab)

grid(col = "lightgray", lty = "dotted", lwd = par("lwd"), equilogs = TRUE)

