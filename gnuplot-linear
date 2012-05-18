#!/usr/bin/env gnuplot
#
# gnuplot-linear	Linear scalability model using gnuplot.
#
# This applies a linear scalability model to an input data set.  It uses
# regression analysis to determine the constants.  Two linear functions
# are plotted: Linear(N), which fits the input set; and LinearN1(N), which
# uses N=1 only.
#
# USAGE: ./gnuplot-linear
#
# See the "tunables" section for defining the input data file, and the number
# of rows to include as model input.  The remainder of rows are drawn as
# "extra" data points.  The file has the form:
#
# N	Result
# 1	2.1
# 2	4.0
# 3	5.9
# ...
#
# The row order can be rearranged to customize the model input.
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
set title "Linear Scalability"
set key on right bottom
set pointsize 2

# read N1, the first value for normalizing the plot (workaround)
plot filename using 1:(N1 = $2, 0/0) every 1:1:1:0:1:0 notitle, '' using 1:($2 / N1) with linespoints

# Linear, N1 only
linearN1(N) = N1 * N

# Linear, input set
alpha = 0.9
linear(N) = N1 * alpha * N

# regression fitting
fit linear(x) filename every ::1::inputN using 1:2 via alpha

# plot data points
plot filename using 1:2 with points pt 6 lc rgb "#f00000" title "extra measurements",\
	filename every ::1::inputN using 1:2 with points pt 6 lc rgb "#000000" title "input for Linear(N)"
set label sprintf("a = %.4f", alpha) at graph 0.5, 0.075 center
set yrange [0:GPVAL_DATA_Y_MAX * scale]
set xrange [0:GPVAL_DATA_X_MAX * scale]

# plot curves
replot linear(x) with line lc rgb "#000000" title "Linear(N)"
replot linearN1(x) with line lc rgb "#a0a0a0" title "LinearN1(N)"

pause -1 "Hit return to continue"
