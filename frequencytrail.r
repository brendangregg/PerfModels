# frequencytrail.r	Example frequency trail implementation.
#
# This implementation takes a density plot with a high resolution, and
# removes lines that are below a minimum threshold, by setting their value
# to NA.  This hides the zero probability line, and provide a coarse (but
# probably sufficient) view of distribution outliers.
#
# 08-Jun-2013	Brendan Gregg	Created this.

pdf("frequencytrailtest.pdf", w=8, h=4)

# plot a data frame as a frequency trail
plotfrequencytrail <- function(data) {
	n <- 2048				# resolution
	lwd <- 4				# line width

	# threshold. todo: improve this calculation to be more robust.
	thr <- 1 / (sd(data) * length(data))

	den <- density(data, n=n)
	plot(den, col=NA, fg=NA)

	# replace low frequency with NA to avoid plotting
	for (i in 1:n) { if (den$y[i] < thr) { den$y[i] = NA } }

	lines(den, lwd=lwd)
}

# data set is a normal distribution plus outliers
data <- c(rnorm(9900, mean=1000, sd=100),
	  runif(10, min=2000, max=10000))

plotfrequencytrail(data)
