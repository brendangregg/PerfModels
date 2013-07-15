# dist.r	Distribution Analysis
#
# This analyzes data set distributions, both synthetic and actual.  It is
# especially intended for latency distributions, such as disk I/O latency,
# to aid in computer performance analysis.
#
# This uses the libraries da-common.r, da-libsynth.r, da-libreal.r.
#
# Environment variables can be set to control behavior and output: see the
# environment section below.  These are set by parent shell scripts which
# execute a series of dist.r runs to generate composite images.
#
# I doubt this is a good example of R scripting.  This includes considerable
# extra complexity for the environment and process it is used in, which won't
# be apparent from this script alone.
#
# Copyright 2013 Brendan Gregg.  All rights reserved.
#
# CDDL HEADER START
#
# The contents of this file are subject to the terms of the
# Common Development and Distribution License (the "License").
# You may not use this file except in compliance with the License.
#
# You can obtain a copy of the license at docs/cddl1.txt or
# http://opensource.org/licenses/CDDL-1.0.
# See the License for the specific language governing permissions
# and limitations under the License.
#
# When distributing Covered Code, include this CDDL HEADER in each
# file and include the License file at docs/cddl1.txt.
# If applicable, add the following below this CDDL HEADER, with the
# fields enclosed by brackets "[]" replaced with your own identifying
# information: Portions Copyright [yyyy] [name of copyright owner]
#
# CDDL HEADER END
#
# 01-Jun-2013	Brendan Gregg	Created this.

library(e1071)		# skewness, kurtosis
library(diptest)	# diptest
source("da-common.r")

# input
type <- 100		# distribution type: see da-libsynth.r and below
N <- 5000		# target elements
trim <- 0		# trim data set: 0 none, 1 sd, 2 iqr, 3 maxtrim
maxtrim <- 0		# max value for use with trim 3
random <- 1		# randomize data ordering
png <- 0		# png instead of pdf
svg <- 0		# svg instead of pdf
pngheight <- 400	# default png height
pngwidth <- 600		# default png width
pdfheight <- 4.5	# default pdf/svg height
pdfwidth <- 9	 	# default pdf/svg width
density <- 0		# draw density plot instead of histogram
denadj <- 0.4		# density adjust parameter
labels <- 1		# draw chart labels (default on)
lwidth <- 8		# density line width
trans <- 0		# transparent background
rug <- 0		# do rug plot
outfile <- "dists.pdf"	# output file
infile <- ""		# input file for dist types 600+
extra <- 0		# extra tests
weight <- 0		# density weight
statlines <- 0		# plot lines for mean, stddev
plines <- 0		# plot lines for 90th, 99th, 99.9th percentiles
symlink <- 0		# create encoded symlinks
fill <- 0		# polygon fill
numbered <- 0		# add value to right of plot
num_mvalue <- 1		# that value is mvalue
num_maxsigma <- 0	# that value is maxsigma
num_max <- 0		# that value is max
num_factor <- 1000000	# factor for max value
centermean <- 0		# center mean in plot

# labels
mtitle <- "Latency Distribution"
xtitle <- "Disk I/O latency (us)"

# environment
if ((env <- Sys.getenv("N")) != "") { N <- as.numeric(env) }
if ((env <- Sys.getenv("TYPE")) != "") { type <- as.numeric(env) }
if ((env <- Sys.getenv("TRIM")) != "") { trim <- as.numeric(env) }
if ((env <- Sys.getenv("MAXTRIM")) != "") { maxtrim <- as.numeric(env) }
if ((env <- Sys.getenv("PNG")) != "") { png <- as.numeric(env) }
if ((env <- Sys.getenv("SVG")) != "") { svg <- as.numeric(env) }
if ((env <- Sys.getenv("LABELS")) != "") { labels <- as.numeric(env) }
if ((env <- Sys.getenv("DENSITY")) != "") { density <- as.numeric(env) }
if ((env <- Sys.getenv("LWD")) != "") { lwidth <- as.numeric(env) }
if ((env <- Sys.getenv("TRANS")) != "") { trans <- as.numeric(env) }
if ((env <- Sys.getenv("RUG")) != "") { rug <- as.numeric(env) }
if ((env <- Sys.getenv("FILL")) != "") { fill<- as.numeric(env) }
if ((env <- Sys.getenv("OUTFILE")) != "") { outfile <- env }
if ((env <- Sys.getenv("INFILE")) != "") { infile <- env }
if ((env <- Sys.getenv("RANDOM")) != "") { random <- as.numeric(env) }
if ((env <- Sys.getenv("EXTRA")) != "") { extra <- as.numeric(env) }
if ((env <- Sys.getenv("SYMLINK")) != "") { symlink <- as.numeric(env) }
if ((env <- Sys.getenv("STATLINES")) != "") { statlines <- as.numeric(env) }
if ((env <- Sys.getenv("WEIGHT")) != "") { weight <- as.numeric(env) }
if ((env <- Sys.getenv("PNGWIDTH")) != "") { pngwidth <- as.numeric(env) }
if ((env <- Sys.getenv("PNGHEIGHT")) != "") { pngheight <- as.numeric(env) }
if ((env <- Sys.getenv("PDFWIDTH")) != "") { pdfwidth <- as.numeric(env) }
if ((env <- Sys.getenv("PDFHEIGHT")) != "") { pdfheight <- as.numeric(env) }

if (png) {
	if (outfile == "dists.pdf") { outfile <- "dists.png" }
	if ((pngheight < 200) & labels) { pngheight <- pngheight + 140; }
	png(outfile, pngwidth, pngheight)
} else if (svg) {
	if (outfile == "dists.pdf") { outfile <- "dists.svg" }
	svg(outfile, width=pdfwidth, height=pdfheight)
} else {
	pdf(outfile, w=pdfwidth, h=pdfheight)
}
if (!labels) {
	mtitle <- ' '; xtitle <- ' '; ytitle <- ' '
	par(bty = "n")
	if (numbered) {
		par(mai = c(0,0,0,1.5))
	} else {
		par(mai = c(0,0,0,0))
	}
} else {
	par(mgp = c(2,0.5,0))
	#par(cex = 2)
	if (numbered) {
		par(mar = c(4,3.5,3,3))
	} else {
		par(mar = c(4,3.5,3,2))
	}
}
if (trans) { par(bg = NA) }
if (density == 0) { ytitle <- "Frequency" } else { ytitle <- "Density" }

# distributions
data <- c()
source("da-libsynth.r")		# defines types 0-499
source("da-libreal.r")		# defines types 500-599

if (type == 600) {		# data is column 0 from infile
	outliers <- "?"
	input <- read.table(infile, header=FALSE, skip=1, nrows=N)
	data <- input$V1
	N <- length(data)
	if (random) { data <- randomize(data) }

} else if (type == 601) {	# data is column 1 from infile
	outliers <- "?"
	input <- read.table(infile, header=FALSE, skip=1, nrows=N)
	data <- input$V2
	N <- length(data)
	if (random) { data <- randomize(data) }

} else if (length(data) == 0) {
	printf("ERROR: distribution type %d unknown.\n", type)
	quit(save = "no")
}

# truncate negative
data <- data[data >= 0]
N <- length(data)

# pre-trimmed statistics
mean <- mean(data)
stddev <- sd(data)
mad <- mad(data)
iqr <- IQR(data)
median <- median(data)
max <- max(data)
maxsigma <- (max - mean) / stddev

# outlier trimming
if (trim == 1) {
	# +- 2 stddev
	data <- data[data <= mean + 2 * stddev]
	data <- data[data >= mean - 2 * stddev]
	N <- length(data)
} else if (trim == 2) {
	# like boxplots, keep range IQR +- 1.5 x IQR
	data <- data[data <= quantile(data, 0.75) + 1.5 * iqr]
	data <- data[data >= quantile(data, 0.25) - 1.5 * iqr]
	N <- length(data)
} else if (trim == 3) {
	data <- data[data <= maxtrim]
	N <- length(data)
}

# post trimmed
mean <- mean(data)
stddev <- sd(data)

# plot histogram
if (density == 0) {
	hist <- hist(data,
	    breaks = 100,
	    col = "gray90", 
	    main = mtitle,
	    xlab = xtitle,
	    ylab = ytitle)
	if (rug) {
		rug(data, lwd=lwidth, col="black", ticksize=0.032)
	}
	maxden <- max(hist$counts)
} else {
	# prepare density plots
	den <- density(data, adjust = denadj)
	if (weight) { den$y <- den$y * den$x }
	maxden <- max(den$y)
	if (trim == 3) {
		xlim <- c(0, maxtrim)
	} else {
		if (centermean) {
			xlim <- c(mean - (max - mean), max)
			xlim <- c(mean - 3.5 * stddev, mean + 3.5 * stddev) 
		} else {
			xlim <- c(min(den$x), max(den$x))
		}
	}

	# ylim is scaled by 1.05 so top can be cropped.
	# the lwd=8 plot can exceed 1.05 for sharp points
	ylim <- c(0, 1.05 * maxden)
}

# density plot
if (density == 1) {
	set.seed(mean + median + stddev)
	col <- "white"
	trans <- 240

	# customize color here

	# pink/magenta ++ / green/aqua	<-- node.js cost
	#col <- rgb(
	#    0,
	#    80 + sample(seq(1:100), 1),
	#    60 + sample(seq(1:65), 1),
	#    trans, maxColorValue = 255)

	# purple/violet ++ / green/brown  <-- mysql cost
	#col <- rgb(
	#    70 + sample(seq(1:65), 1),
	#    90 + sample(seq(1:100), 1),
	#    0,
	#    trans, maxColorValue = 255)

	# orange ++ / blue/turquoise	<-- disk cost
	#col <- rgb(
	#    0,
	#    80 + sample(seq(1:150), 1),
	#    255,
	#    trans, maxColorValue = 255)

	# dark blue / yellow ++
	#v1 <- 220 + sample(seq(1:35), 1)
	#v2 <- v1 - 100 - sample(seq(1:115), 1)
	#col <- rgb(v1, v1, v2, maxColorValue = 255)

	# dark yellow ++ / light blue  <-- synth yellow
	#v1 <- 255 - sample(seq(1:50), 1)
	#v2 <- sample(seq(1:65), 1)
	#col <- rgb(v2 + 5, v2 + 30, v1, trans, maxColorValue = 255)

	# magenta / light green
	#v1 <- 230 + sample(seq(1:25), 1)
	#v2 <- v1 - 60 - sample(seq(1:110), 1)
	#col <- rgb(v2, v1, v2, maxColorValue = 255)

	# green trans ++ / magenta   <-- node.js
	#v1 <- 220 + sample(seq(1:35), 1)
	#v2 <- v1 - 90 - sample(seq(1:60), 1)
	#col <- rgb(v1, v2, v1, trans, maxColorValue = 255)

	# red trans ++ / aqua   <-- disk
	#v1 <- 230 + sample(seq(1:25), 1)
	#v2 <- sample(seq(1:125), 1)
	#col <- rgb(v2, v1, v1, trans, maxColorValue = 255)

	# blue trans ++ / yellow   <-- mysql
	v1 <- 180 + sample(seq(1:55), 1)
	v2 <- v1 - 100 - sample(seq(1:80), 1)
	col <- rgb(v1, v1 - 20, v2, trans, maxColorValue = 255)

	# turquoise / pink
	#v1 <- 230 + sample(seq(1:25), 1)
	#v2 <- v1 - 60 - sample(seq(1:90), 1)
	#col <- rgb(v1, v2, v2, maxColorValue = 255)

	plot(den, main = mtitle, xlab = xtitle, ylab = ytitle,
	    lwd = lwidth, fg = NA, xlim = xlim, ylim = ylim)
	if (fill) {
		polygon(c(min(den$x), den$x, max(den$x)),
		    c(0, den$y, 0), col = col)
		plot(den, main = mtitle, xlab = xtitle, ylab = ytitle,
		    lwd = lwidth, fg = NA, xlim = xlim, ylim = ylim,
		    col = "white")
	}
	if (rug) {
		rug(data, lwd = lwidth, ticksize = 0.046, col = col,
		    xlim = xlim)
	}

# frequency trail
} else if (density == 2) {
	# walk the density values and maintain a state based on
	# y height, drawing lines or rugs when the state changes.
	plot(den, main = mtitle, xlab = xtitle, ylab = ytitle,
	    lwd = 1, col = NA, fg = NA, xlim = xlim, ylim = ylim)
	state <- 0	# 0 line, 1 rug
	bx <- den$x[1]
	by <- den$y[1]
	minden <- min(den$y)
	maxx <- max(den$x)
	threshold = 3 * maxden / pngheight

	for (i in 1:512) { 
		if (i == 512) {
			# force plot on final point
			if (state == 0) { den$y[i] = 0 }
			if (state == 1) { den$y[i] = 1.1 * threshold }
		}

		if (den$y[i] > threshold) {
			if (state == 1) {
				if (rug) {
					rdata <- data[data >= bx]
					rdata <- rdata[data < den$x[i]]
					rug(rdata, lwd=lwidth,
					    ticksize <- 0.049,
					    col="black", xlim=xlim)
				}
				state <- 0
				bx <- den$x[i]
				by <- den$y[i]
			} 

		} else {
			if (state == 0) {
				nn <- 1 + round(512 * (den$x[i] - bx) / maxx)
				sden <- density(data, adjust = denadj, n = nn,
				    from = bx, to = den$x[i], cut=0)
				if (weight) { sden$y <- sden$y * sden$x }
				if (fill) {
					polygon(c(bx, sden$x, den$x[i]),
					    c(0, sden$y, 0), col = "white")
				}
				lines(sden, main = " ", lwd = lwidth,
				    fg = NA, col = "black",
				    xlim=xlim, ylim=ylim)

				state <- 1
				bx <- den$x[i]
				by <- den$y[i]
			}
		}
	}
}

# calculate statistics
min <- min(data)
max <- max(data)
mad <- mad(data)
var <- var(data)
percentiles <- quantile(data, c(0.9, 0.99, 0.999, 0.9999, 0.99999, 0.999999))
apercentiles <- quantile(data, seq(0.01, 0.99, 0.01), names = TRUE)
quartiles <- quantile(data, c(0.25, 0.75), names = FALSE)
iqr <- IQR(data)
prange = apercentiles[55] - apercentiles[45]
median <- median(data)
skewness <- skewness(data)
kurtosis <- kurtosis(data)
diptest <- dip(data)
pstddev <- stddev * sqrt((N - 1) / N)
maxsigma <- (max - mean) / stddev
minsigma <- (mean - min) / stddev
madmax <- (max - median) / mad
madmin <- (median - min) / mad
bimodalc <- ((skewness^2) + 1) / kurtosis
bimodalcf <- ((skewness^2) + 1) /
    (kurtosis + 3 * ((N - 1)^2) / ((N - 2) * (N - 3)))
cov <- stddev / mean

# calculate madv, macdf, sacdf
madv <- 0
macdf <- 0
sacdf <- 0
stddevconn <- 0
for(i in 1:N) { 
	if (i > 1) {
		d <- abs(data[i] - data[i - 1])
		macdf <- macdf + d
		sacdf <- sacdf + d^2
	}
	if (data[i] > (mean - stddev/2) & (data[i] < (mean + stddev/2))) {
		stddevconn <- stddevconn + 1
	}
	madv <- madv + abs(data[i] - mean)
}
stddevcon <- stddevconn / N
macdf <- macdf / (N - 1)
sacdf <- sqrt(sacdf / (N - 1))
madv <- madv / N

# calculate mvalue
maxmvalue <- 0
for (a in c(2, 3, 5, 7, 10, 15, 20, 30)) {
	# try various bandwidths, starting at 2x, and keep highest mvalue
	by <- 0
	mvalue <- 0
	den <- density(data, adjust =  denadj * a)
	if (weight) { den$y <- den$y * den$x }
	maxd <- max(den$y)
	for (i in 1:length(den$x)) {
		mvalue <- mvalue + abs(den$y[i] / maxd - by)
		by <- den$y[i] / maxd
	}
	if (mvalue > maxmvalue) { maxmvalue <- mvalue }
}
mvalue <- maxmvalue

# print statistics
printf("\n%-42s %d\n", "N", N)
printf("%-42s %.2f\n", "min", min)
printf("%-42s %.2f\n", "mean", mean)
printf("%-42s %.2f\n", "median", median)
printf("%-42s %.2f\n", "max", max)
printf("%-42s %.2f\n", "max sigma", maxsigma)
printf("%-42s %.2f\n", "min sigma", minsigma)
printf("%-42s %.2f\n", "mad max", madmax)
printf("%-42s %.2f\n", "mad min", madmin)
printf("%-42s %.2f\n", "sample standard deviation", stddev)
printf("%-42s %.2f\n", "population standard deviation", pstddev)
printf("%-42s %.2f\n", "coefficient of variation", cov)
printf("%-42s %.2f\n", "variance/mean", var / mean)
printf("%-42s %.2f\n", "median absolute deviation", mad)
printf("%-42s %.2f\n", "mean absolute deviation", madv)
printf("%-42s %.2f\n", "mean absolute consecutive difference", macdf)
printf("%-42s %.2f\n", "standard absolute consecutive difference", sacdf)
printf("%-42s %.2f\n", "90th percentile", percentiles[1])
printf("%-42s %.2f\n", "99th percentile", percentiles[2])
printf("%-42s %.2f\n", "99.9th percentile", percentiles[3])
printf("%-42s %.2f\n", "99.99th percentile", percentiles[4])
printf("%-42s %.2f\n", "99.999th percentile", percentiles[5])
printf("%-42s %.2f\n", "99.9999th percentile", percentiles[6])
printf("%-42s %.2f\n", "25% quartile", quartiles[1])
printf("%-42s %.2f\n", "75% quartile", quartiles[2])
printf("%-42s %.2f\n", "inter quartile range", iqr)
printf("%-42s %.2f\n", "45%-55% percentile range", prange)
printf("%-42s %.2f\n", "skewness", skewness)
printf("%-42s %.2f\n", "kurtosis", kurtosis)
printf("%-42s %.2f\n", "bimodality coefficient", bimodalc)
printf("%-42s %.2f\n", "bimodality coefficient finite sample", bimodalcf)
printf("%-42s %.3f\n", "dip test statistic", diptest)
printf("%-42s %.3f\n", "mvalue (y difference)", mvalue)
printf("%-42s %.2f\n", "macdf/mean", macdf / mean)
printf("%-42s %.2f\n", "sacdf/mean", sacdf / mean)
printf("%-42s %.2f\n", "madv/stddev", madv / stddev)

# print table output
printf("\nHEAD type trim outliers N min mean median max minsigma maxsigma madmax iqr prange mad stddev cov skewness kurtosis bimodalcf madv macdf sacdf stddevcon diptest\n");
printf("DATA %d %d %s %d %.2f %.2f %.2f %.2f %.2f %.2f %.2f %.2f %.2f %.2f %.2f %.2f %.2f %.2f %.2f %.2f %.2f %.2f %.2f %.3f\n",
    type, trim, outliers, N, min, mean, median, max, minsigma, maxsigma, madmax, iqr, prange, mad, stddev, cov, skewness, kurtosis, bimodalcf, madv, macdf, sacdf, stddevcon, diptest);

if (extra) {
	print(shapiro.test(data))
	library(ADGofTest)
	print(ad.test(data, pnorm, 0, max(data)))
	print(ks.test(data, "pnorm", mean = mean, sd = stddev))
	print(dip(data, full = "all"))
}

if (numbered) {
	#
	# Some awful code.  Ideally we'd create a text variable with whatever
	# we want printed, then mtext() would place it right-aligned at a
	# _reasonable_ spacing to the plot.  I've never got that to work.
	# Instead, I let mtext() place it left-aligned, and achieve right-
	# alignment by padding the text variable with spaces.  Two spaces
	# for each digit, since it is variable width.
	#

	if (num_maxsigma) {
		num <- maxsigma
		# 2 dec places, up to 99; %5.2f has crooked alignment
		if (num < 10) { text <- sprintf("  %.2f", num) }
		else { text <- sprintf("%.2f", num) }
	}

	if (num_mvalue) {
		num <- mvalue
		# 2 dec places, up to 99; %5.2f has crooked alignment
		if (num < 10) { text <- sprintf("  %.2f", num) }
		else { text <- sprintf("%.2f", num) }
	}

	if (num_max) {
		num <- max / num_factor
		text <- ""; x <- round(num)
		if (x == 0) { x <- 1 }
		while (x < 1000) {
			text <- paste(text, "  ", sep = "")
			x <- x * 10
		}
		text <- paste(text, sprintf("%d", round(num)), sep = "")
	}
	if (num_max) {
		num <- max / num_factor
		text <- ""; x <- round(num)
		if (x == 0) { x <- 1 }
		while (x < 1000) {
			text <- paste(text, "  ", sep = "")
			x <- x * 10
		}
		text <- paste(text, sprintf("%d", round(num)), sep = "")
	}

	# padj = 2 for pngheight 120; 3.85 for pngheight 220; 1.5 centered
	# col = white for filled; black for trail;
	mtext(text, side = 4, las = 1, cex = 3, adj = 0.5, padj = 3.85,
	    col = "white")
}

# plot statistics
if (statlines) {
	abline(v=mean, col="black", lwd=1, lty="dashed")
	abline(v=mean + stddev, col="black", lty="dotted")
	abline(v=mean - stddev, col="black", lty="dotted")
	abline(v=percentiles[2], col="black", lty="1A")
	abline(v=mean + 6 * stddev, col="black", lty="4A")
	legend("topright",
	    c("mean", "stddev", "99th pct", expression(6 * sigma)),
	    lty=c("dashed", "dotted", "1A", "4A"),
	    lwd=1)
}

if (centermean) {
	lines(x = c(mean, mean), y = c(0, maxden),
	    col = "white", lwd = 8, lend = 1)
}

if (plines) {
	lines(x = c(percentiles[1], percentiles[1]), y = c(0, maxden / 4),
	    col = "white", lwd = 4)
	lines(x = c(percentiles[2], percentiles[2]), y = c(0, maxden / 4),
	    col = "white", lwd = 4)
	lines(x = c(percentiles[3], percentiles[3]), y = c(0, maxden / 4),
	    col = "white", lwd = 4)
}

dev.off()
printf("\n%s written.\n", outfile)

# create symlinks
if (symlink) {
	print("making symlinks...")
	inf <- basename(infile)

	# create ordered max pngs
	link <- sprintf("max_%016d_%s%d.png", round(max), inf, type)
	system(sprintf("ln -s %s %s", outfile, link))

	# create ordered maxsigma pngs
	link <- sprintf("maxsigma_%03d.%06d_%s%d.png",
	    floor(maxsigma), round(1000000 * (maxsigma %% 1)), inf, type)
	system(sprintf("ln -s %s %s", outfile, link))

	# create ordered bimodalcf pngs
	link <- sprintf("bimodalcf_%03d.%06d_%s%d.png",
	    floor(bimodalcf), round(1000000 * (bimodalcf %% 1)), inf, type)
	system(sprintf("ln -s %s %s", outfile, link))

	# create ordered diptest pngs
	link <- sprintf("diptest_%03d.%06d_%s%d.png",
	    floor(diptest), round(1000000 * (diptest %% 1)), inf, type)
	system(sprintf("ln -s %s %s", outfile, link))

	# create ordered cov pngs
	link <- sprintf("cov_%03d.%06d_%s%d%s.png",
	    floor(cov), round(1000000 * (cov %% 1)), inf, type, trim)
	system(sprintf("ln -s %s %s", outfile, link))

	# create ordered mvalue pngs
	link <- sprintf("ydiff_%03d.%06d_%s%d%s.png",
	    floor(mvalue), round(1000000 * (mvalue %% 1)), inf, type, trim)
	system(sprintf("ln -s %s %s", outfile, link))
}
