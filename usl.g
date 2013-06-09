#!/usr/bin/env gnuplot
#
# gnuplot-usl		USL using gnuplot.
#
# This applies Universal Scalability Law (Dr. Neil J. Gunther) to the input
# data set.  It uses regression analysis to determine the constants.
#
# USAGE: ./gnuplot-usl
#
# See the "tunables" section for defining the input data file, and the number
# of rows to include as model input (USL insists on a minimum of six).  The
# remainder of rows are drawn as "extra" data points.  The file has the form:
#
# N	Result
# 1	2.1
# 2	4.0
# 3	5.9
# ...
#
# The row order can be rearranged to customize the model input.
#
# SEE ALSO: http://www.perfdynamics.com/Manifesto/USLscalability.html
#
# Copyright 2012 Brendan Gregg.  All rights reserved.
#
# CDDL HEADER START
#
# The contents of this file are subject to the terms of the
# Common Development and Distribution License (the "License").
# You may not use this file except in compliance with the License.
#
# You can obtain a copy of the license at usr/src/OPENSOLARIS.LICENSE
# or http://www.opensolaris.org/os/licensing.
# See the License for the specific language governing permissions
# and limitations under the License.
#
# When distributing Covered Code, include this CDDL HEADER in each
# file and include the License file at usr/src/OPENSOLARIS.LICENSE.
# If applicable, add the following below this CDDL HEADER, with the
# fields enclosed by brackets "[]" replaced with your own identifying
# information: Portions Copyright [yyyy] [name of copyright owner]
#
# CDDL HEADER END
#
# 03-May-2012	Brendan Gregg	Created this.

set terminal x11 font "arial,14"	# designed for x11 (redraws)
set autoscale

# tunables
filename = "data.txt"		# data file
inputN = 6			# rows to include as model input
scale = 1.5			# scale graph beyond data points
set grid

set xlabel "CPUs (N)"
set ylabel "Throughput"
set title "USL Scalability"
set key on right bottom
set pointsize 2

# read N1, the first value for normalizing the plot (workaround)
plot filename using 1:(N1 = $2, 0/0) every 1:1:1:0:1:0 notitle, '' using 1:($2 / N1) with linespoints

# USL
alpha = 0.01
beta = 0.001
usl(N) = N1 * N/(1 + alpha * (N - 1) + beta * N * (N - 1))

# regression analysis (non-linear least squares fitting)
fit usl(x) filename every ::1::inputN using 1:2 via alpha, beta

# plot data points
plot filename using 1:2 with points pt 6 lc rgb "#f00000" title "extra measurements",\
	filename every ::1::inputN using 1:2 with points pt 6 lc rgb "#000000" title "input for USL"
set label sprintf("a = %.4f\nb = %.4f", alpha, beta) at graph 0.5, 0.075 center
set yrange [0:GPVAL_DATA_Y_MAX * scale]
set xrange [0:GPVAL_DATA_X_MAX * scale]

# plot curves
replot usl(x) with line lc rgb "#000000" title "USL(N)"

pause -1 "Hit return to continue"
print "$0";
