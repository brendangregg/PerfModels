# frequencytrail.r	Example frequency trail implementation.
#
# This implementation takes a density plot with a high resolution, and
# plots lines that are above a minimum threshold.  This hides the zero
# probability line, and provide a coarse (but probably sufficient) view
# of distribution outliers.
#
# 08-Jun-2013	Brendan Gregg	Created this.

#pdf("frequencytrailtest.pdf", w=8, h=4)

# plot a data frame as a frequency trail
plotfrequencytrail <- function(data) {
	n <- 2048			# resolution
	thr <- 0.000001			# threshold
	lwd <- 4			# line width
	den <- density(data, n=n)

	maxy <- max(den$y)
	px <- den$x[1]
	py <- den$y[1]

	# initialize x and y range
	plot(den, col=NA, fg=NA)

	for (i in 2:n) {
		if (py > thr | den$y[i] > thr) {
			lines(c(px, den$x[i]), c(py, den$y[i]), lwd=lwd)
		}
		py <- den$y[i]
		px <- den$x[i]
	}
}

# data set is a normal distribution plus outliers
data <- c(rnorm(9990, mean=1000, sd=200),
	  runif(10, min=2000, max=10000))

plotfrequencytrail(data)
