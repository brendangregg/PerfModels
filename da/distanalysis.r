# distanalysis.r	Distribution Analysis
#
# This analyzes data set distributions, both synthetic and actual.  It is
# especially intended for latency distributions, such as disk I/O latency,
# to aid in computer performance analysis.
#
# This uses the libraries da-common.r, da-libsynth.r, da-libreal.r.
#
# Environment variables can be set to control behavior and output: see the
# environment section below.  These are set by parent shell scripts which
# execute a series of distanalysis.r runs to generate composite images.
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
trim <- 0		# trim data set: 0 none, 1 outliers, 2 maxlat
maxtrim <- 0		# max value for use with trim 2
random <- 1		# randomize data ordering
png <- 0		# png instead of pdf
pngheight <- 400	# default png height
pngwidth <- 600		# default png width
pdfheight <- 4.5	# default pdf height
pdfwidth <- 9	 	# default pdf width
density <- 0		# draw density plot instead of histogram
denadj <- 0.4		# density adjust parameter
labels <- 1		# draw chart labels (default on)
lwidth <- 8		# density line width
trans <- 0		# transparent background
rug <- 0		# do rug plot
outfile <- "dists.pdf"	# output file
infile <- "out.dilt"	# input file for dist types 600+
extra <- 0		# extra tests
weight <- 0		# density weight
statlines <- 0		# plot lines for mean, stddev
symlink <- 0		# create encoded symlinks
fill <- 0		# polygon fill

# labels
mtitle <- "Latency Distribution"
xtitle <- "Disk I/O latency (us)"
ytitle <- "Frequency"

# environment
if ((env <- Sys.getenv("N")) != "") { N <- as.numeric(env) }
if ((env <- Sys.getenv("TYPE")) != "") { type <- as.numeric(env) }
if ((env <- Sys.getenv("TRIM")) != "") { trim <- as.numeric(env) }
if ((env <- Sys.getenv("MAXTRIM")) != "") { maxtrim <- as.numeric(env) }
if ((env <- Sys.getenv("PNG")) != "") { png <- as.numeric(env) }
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
if ((env <- Sys.getenv("WEIGHT")) != "") { weight <- as.numeric(env) }
if ((env <- Sys.getenv("PNGWIDTH")) != "") { pngwidth <- as.numeric(env) }
if ((env <- Sys.getenv("PNGHEIGHT")) != "") { pngheight <- as.numeric(env) }
if (png) {
	if (outfile == "dists.pdf") { outfile <- "dists.png" }
	if ((pngheight < 200) & labels) { pngheight <- pngheight + 140; }
	png(outfile, pngwidth, pngheight)
} else {
	pdf(outfile, w=pdfwidth, h=pdfheight)
}
if (!labels) {
	mtitle <- ' '; xtitle <- ' '; ytitle <- ' '
	par(mai = c(0,0,0,0)); par(bty = "n")
} else {
	par(mgp = c(2,0.5,0))
	par(mar = c(4,3.5,3,2))
}
if (trans) { par(bg = NA) }

# distributions
#
# type	description
# 600	input from infile in dilt format
# 601	input from infile in dilt10000 format
# 602	input as a single column of microseconds
# 602	input as a single column of counts
data <- c()
source("da-libsynth.r")		# defines types 0-499
source("da-libreal.r")		# defines types 500-599

if (type == 600) {		# infile dilt format
	outliers <- "?"
	input <- read.table(infile, header=FALSE, skip=1, nrows=N,
	    col.names=c("ENDTIMEus","LATENCYus","DIR","SIZEbytes","PROCESS"))
	attach(input);
	data <- input$LATENCYus
	N <- length(data)
	if (random) { data <- randomize(data) }

} else if (type == 601) {	# infile dilt10000 format
	outliers <- "?"
	input <- read.table(infile, header=FALSE, skip=1, nrows=N,
	    col.names=c("ENDTIMEus","LATENCYus","DIR","SIZEbytes","ZONENAME",
	    "PROCESS"))
	attach(input);
	data <- input$LATENCYus
	N <- length(data)
	if (random) { data <- randomize(data) }

} else if (type == 602) {	# microseconds
	outliers <- "?"
	input <- read.table(infile, header=FALSE, skip=1, nrows=N,
	    col.names=c("LATENCYus"))
	attach(input);
	data <- input$LATENCYus
	N <- length(data)
	if (random) { data <- randomize(data) }

} else if (type == 603) {	# counts
	outliers <- "?"
	input <- read.table(infile, header=FALSE, skip=1, nrows=N,
	    col.names=c("LATENCYus"))
	attach(input);
	data <- input$LATENCYus
	N <- length(data)
	if (random) { data <- randomize(data) }
	mtitle <- "Count Distribution"
	xtitle <- "Counts"

} else if (length(data) == 0) {
	printf("ERROR: type %d unknown.\n", type)
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

# outlier trimming
if (trim == 1) {
	# like boxplots, keep range IQR +- 1.5 x IQR
	data <- data[data <= quantile(data, 0.75) + 1.5 * iqr]
	data <- data[data >= quantile(data, 0.25) - 1.5 * iqr]
	N <- length(data)
} else if (trim == 2) {
	data <- data[data <= maxtrim]
	N <- length(data)
}

# plot histogram
if (density == 0) {
	hist(data,
	    breaks = 50,
	    col = "gray90", 
	    main = mtitle,
	    xlab = xtitle,
	    ylab = ytitle)
	if (rug) {
		rug(data, lwd=lwidth, col="black", ticksize=0.032)
	}
} else {
	# prepare density plots
	den <- density(data, adjust = denadj)
	if (weight) { den$y <- den$y * den$x }
	if (trim == 2) {
		xlim <- c(0, maxtrim)
	} else {
		xlim <- c(min(den$x), max(den$x))
	}

	# ylim is scaled by 1.05 so top can be cropped.
	# the lwd=8 plot can exceed 1.05 for sharp points
	ylim <- c(0, 1.05 * max(den$y))
}

# density plot
if (density == 1) {
	plot(den, main = mtitle, xlab = xtitle, ylab = ytitle,
	    lwd = lwidth, fg = NA, xlim = xlim, ylim = ylim)
	if (fill) {
		polygon(c(min(den$x), den$x, max(den$x)),
		    c(0, den$y, 0), col = "white")
	}
	if (rug) {
		rug(data, lwd = lwidth, ticksize = 0.046, col="white",
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
	maxden <- max(den$y)
	minden <- min(den$y)
	maxx <- max(den$x)
	threshold = maxden / pngheight

	for (i in 1:512) { 
		if (i == 512) {
			# force plot on final point
			if (state == 0) { den$y[i] = 0 }
			if (state == 1) { den$y[i] = 1.1 * threshold / maxden }
		}

		if (den$y[i] / maxden > 3 * threshold / maxden) {
			if (state == 1) {
				if (rug) {
					rdata <- data[data >= bx]
					rdata <- rdata[data < den$x[i]]
					rug(rdata, lwd=lwidth,
					    ticksize <- 0.046,
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
mean <- mean(data)
stddev <- sd(data)
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

# calculate ydiff
by <- 0
ydiff <- 0
# use a smoother density line
den <- density(data, adjust = 2 * denadj)
if (weight) { den$y <- den$y * den$x }
maxden <- max(den$y)
for (i in 1:512) {
	ydiff <- ydiff + abs(den$y[i] / maxden - by)
	by <- den$y[i] / maxden
}

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
printf("%-42s %.3f\n", "y difference", ydiff)
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

# plot statistics
if (statlines) {
	abline(v=mean, col="black", lwd=1, lty="dashed")
	abline(v=mean + stddev, col="black", lty="dotted")
	abline(v=mean - stddev, col="black", lty="dotted")
	legend("topright",
	    c("mean", "stddev", "99th pct"),
	    lty=c("dashed", "dotted", "dotdash"),
	    lwd=1)
}

dev.off()
printf("\n%s written.\n", outfile)

# create symlinks
if (symlink) {
print("making symlinks...")
	if (trim == 0) {
		# create ordered maxsigma pngs
		filename2 <- sprintf("maxsigma_%03d.%06d_%d.png",
		    floor(maxsigma), round(1000000 * (maxsigma %% 1)), type)
		system(sprintf("ln -s %s %s", outfile, filename2))
	} else if (trim == 1) {
		# create ordered bimodalcf pngs
		filename2 <- sprintf("bimodalcf_%03d.%06d_%d.png",
		    floor(bimodalcf), round(1000000 * (bimodalcf %% 1)), type)
		system(sprintf("ln -s %s %s", outfile, filename2))
		# create ordered diptest pngs
		filename2 <- sprintf("diptest_%03d.%06d_%d.png",
		    floor(diptest), round(1000000 * (diptest %% 1)), type)
		system(sprintf("ln -s %s %s", outfile, filename2))
	}
	# create ordered pngs
	filename2 <- sprintf("order_%03d.%06d_%d_%s.png",
	    floor(cov), round(1000000 * (cov %% 1)), type, trim)
	system(sprintf("ln -s %s %s", outfile, filename2))
	# create ydiff pngs
	filename2 <- sprintf("ydiff_%03d.%06d_%d_%s.png",
	    floor(ydiff), round(1000000 * (ydiff %% 1)), type, trim)
	system(sprintf("ln -s %s %s", outfile, filename2))
}
