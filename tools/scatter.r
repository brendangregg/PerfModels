# scatter.r	R scatter plot hello world.
#
# USAGE: R --no-save < scatter.r
#
# This time I'm putting it on github where I won't lose it.
#
# Input is two columns, for time (seconds) and latency (ms).
#
# 17-Jun-2014	Brendan Gregg	Created this.

filename <- "scatter.txt"
pdf("scatter.pdf", w=10, h=5)

# max rows to use
N <- 10000

data <- read.table(filename, header=FALSE, nrows=N)
N <- length(data)

# type: p=points, l=lines, n=none, o=overplotted, b=both
plot(data, cex=0.5, xlab="Time (s)", ylab="Latency (ms)")

grid(col = "lightgray", lty = "dotted",
     lwd = par("lwd"), equilogs = TRUE)
