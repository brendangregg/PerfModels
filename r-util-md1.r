# r-util-md1.r	Queueing Theory M/D/1 mean response time vs utilization
#
# USAGE: R --save < r-util-md1.r	# generates r-util-md1.pdf
#
# See the "Tunables" section for defining the mean service time.
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
# 20-Oct-2012	Brendan Gregg	Created this.

# Tunables
svc_ms <- 1				# average disk I/O service time
pdf("r-util-md1.pdf", w=10, h=6)	# comment for interactive
util_min <- 0
util_max <- 100
ms_min <- 0
ms_max <- 10

# Plot mean response time vs utilization (M/D/1)
plot(x <- c(util_min:util_max), svc_ms * (2 - x/100) / (2 * (1 - x/100)),
    type="l", lty=1, lwd=1,
    xlim=c(util_min, util_max), ylim=c(ms_min, ms_max),
    xlab="Utilization %", ylab="Mean Response Time (ms)")

# Grids
abline(v=(seq(util_min, util_max, (util_max - util_min) / 10)),
    col="lightgray", lty="dotted")
abline(h=(seq(ms_min, ms_max, (ms_max - ms_min) / 10)),
    col="lightgray", lty="dotted")

title("Single Service Queue, Constant Service Times (M/D/1)")
