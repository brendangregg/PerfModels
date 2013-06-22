# da-libreal		Some Real Latency Distributions
#
# This is a library for distanalysis.r.  It requires the data files listed
# below (see read.table).
#
# Input:
#	N	number of target elements (may return a little less)
#	type	a distribution type ID (see list below)
# Output:
#	data		data set
#
# 01-Jun-2013	Brendan Gregg	Created this.

# type	description
# 500	faithful bimodal
# 501	real disk I/O latency bimodal far
# 502	real disk I/O latency bimodal far outliers

if (type == 500) {		# faithful bimodal
	outliers <- "N"
	attach(faithful); N <- length(eruptions); data <- eruptions

} else if (type == 501) {	# random disk I/O
	outliers <- "N"
	input <- read.table("out.iosnoop_randread01", header=FALSE, skip=1,
	    nrows=N, col.names=c("STIME","TIME","DELTA","DTIME","UID","PID",
	    "D","BLOCK","SIZE","COMM","PATHNAME"))
	attach(input); input <- input[DELTA < 10000, ]
	data <- input$DELTA
	N <- length(data)
	if (random) { data <- randomize(data) }

} else if (type == 502) {	# random disk I/O outliers
	outliers <- "Y"
	input <- read.table("out.iosnoop_randread01", header=FALSE, skip=1,
	    nrows=N, col.names=c("STIME","TIME","DELTA","DTIME","UID","PID",
	    "D","BLOCK","SIZE","COMM","PATHNAME"))
	attach(input);
	data <- input$DELTA
	if (random) { data <- randomize(data) }

} else if (type == 503) {	# random sync disk I/O outliers
	outliers <- "Y"
	input <- read.table("out.iosnoop_marssync01", header=FALSE, skip=1,
	    nrows=N, col.names=c("STIME","TIME","DELTA","DTIME","UID","PID",
	    "D","BLOCK","SIZE","COMM","PATHNAME"))
	attach(input);
	data <- input$DELTA
	if (random) { data <- randomize(data) }
}
