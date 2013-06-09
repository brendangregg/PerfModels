# da-libsynth.r		Synthetic Latency Distributions
#
# This is a library for distanalysis.r.
#
# This defines various synthetic distributions for modeling I/O latency.
# The distributions are composed of values that are typically between 0 and
# 10000, with a mean around 1000.  This is loosely based on storage device I/O
# latency, in units of microseconds.  You can adjust these as desired.
#
# Input:
#	N	number of target elements (may return a little less)
#	type	a distribution type ID (see list below)
# Output:
#	data		data set
#
# 01-Jun-2013	Brendan Gregg	Created this.

library(VGAM)		# rpareto

# type	description
# 0	uniform narrow
# 1	uniform wide
# 2	uniform outliers
# 100	unimodal normal narrow
# 101	unimodal normal medium
# 102	unimodal normal wide
# 103	unimodal normal with tail
# 110	unimodal normal narrow band reject
# 111	unimodal normal spike
# 112	unimodal normal fenced
# 113	unimodal normal quantized
# 120	unimodal poisson
# 121	unimodal poisson outliers
# 130	unimodal pareto narrow
# 131	unimodal pareto wide
# 140	unimodal normal outliers 1% medium
# 141	unimodal normal outliers 1% far
# 142	unimodal normal outliers 1% very far
# 143	unimodal normal outliers 2%
# 144	unimodal normal outliers 4%
# 145	unimodal normal outliers 2% clustered
# 146	unimodal normal outliers 4% close 1
# 147	unimodal normal outliers 4% close 2
# 148	unimodal normal outliers 4% close 3
# 149	unimodal normal outliers 4% close 4
# 150	unimodal normal outliers 4% close 5
# 151	unimodal normal outliers 4% close 6
# 152	unimodal normal outliers 4% close 7
# 153	unimodal normal outliers 0.5%
# 154	unimodal normal outliers 0.2%
# 155	unimodal normal outliers 0.1%
# 200	bimodal normal very close
# 201	bimodal normal close
# 202	bimodal normal medium
# 203	bimodal normal far
# 204	bimodal normal outliers 1%
# 205	bimodal normal outliers 2%
# 206	bimodal normal outliers 4%
# 210	bimodal normal major minor
# 211	bimodal normal minor major
# 212	bimodal normal major minor outliers
# 213	bimodal normal minor major outliers
# 300	trimodal normal close
# 301	trimodal normal medium
# 302	trimodal normal far
# 303	trimodal normal outliers
# 304	trimodal normal major medium minor
# 305	trimodal normal minor major minor
# 306	trimodal normal minor major medium
# 307	trimodal normal major minor medium
# 400	quadmodal normal close
# 401	quadmodal normal medium
# 402	quadmodal normal far
# 403	quadmodal normal outliers

# definitions
set.seed(type)
if (type == 0) {		# uniform narrow
	outliers <- "N"
	data <- runif(N, min=500, max=1500)

} else if (type == 1) {		# uniform wide
	outliers <- "N"
	data <- runif(N, min=0, max=3000)

} else if (type == 2) {		# uniform outliers
	outliers <- "Y"
	data <- c(runif(N * 0.99, min=500, max=1500),
	          runif(N * 0.01, min=1500, max=10000))

} else if (type == 100) {	# unimodal normal narrow
	outliers <- "N"
	data <- rnorm(N, mean=1000, sd=100)

} else if (type == 101) {	# unimodal normal medium
	outliers <- "N"
	data <- rnorm(N, mean=1000, sd=200)

} else if (type == 102) {	# unimodal normal wide
	outliers <- "N"
	data <- rnorm(N, mean=1000, sd=300)

} else if (type == 103) {	# unimodal normal with tail
	outliers <- "N"
	data <- c(rnorm(N * 0.96, mean=1000, sd=200),
	          runif(N * 0.04, min=1000, max=2250))
	data <- randomize(data)

} else if (type == 110) {	# unimodal band reject
	outliers <- "N"
	data0 <- rnorm(N, mean=1000, sd=200)
	ii <- 0
	for(i in 1:N) { 
		if (data0[i] < 770 || data0[i] > 800) {
			data[ii] <- data0[i]
			ii <- ii + 1
		}
	}
	N <- length(data)

} else if (type == 111) {	# unimodal normal spike
	outliers <- "N"
	data <- c(rnorm(N * 0.98, mean=1000, sd=200),
	          rnorm(N * 0.02, mean=750, sd=1))
	data <- randomize(data)

} else if (type == 112) {	# unimodal normal fence
	outliers <- "N"
	N <- N * 2
	data0 <- rnorm(N, mean=1000, sd=200)
	ii <- 0
	for(i in 1:N) { 
		if ((data0[i] %% 64) < 32) {
			data[ii] <- data0[i]
			ii <- ii + 1
		}
		if (ii >= 5000) { break }
	}
	N <- length(data)

} else if (type == 113) {	# unimodal normal quantized
	outliers <- "N"
	data0 <- rnorm(N, mean=1000, sd=200)
	for(i in 1:N) { 
		data[i] <- floor(data0[i] / 64) * 64
	}

} else if (type == 120) {	# unimodal poisson
	outliers <- "N"
	data <- rpois(N, lambda=1000)

} else if (type == 121) {	# unimodal poisson outliers
	outliers <- "Y"
	data <- c(rpois(N * 0.99, lambda=1000),
	          runif(N * 0.01, min=1000, max=5000))

} else if (type == 130) {	# unimodal pareto narrow
	outliers <- "N"
	data <- rpareto(N, 1000, 3)

} else if (type == 131) {	# unimodal pareto wide
	outliers <- "N"
	data <- rpareto(N, 1000, 10)

} else if (type == 140) {	# unimodal normal outliers 1% medium
	outliers <- "Y"
	data <- c(rnorm(N * 0.99, mean=1000, sd=200),
	          runif(N * 0.01, min=1000, max=5000))
	data <- randomize(data)

} else if (type == 141) {	# unimodal normal outliers 1% far
	outliers <- "Y"
	data <- c(rnorm(N * 0.99, mean=1000, sd=200),
	          runif(N * 0.01, min=1000, max=10000))
	data <- randomize(data)

} else if (type == 142) {	# unimodal normal outliers 1% very far
	outliers <- "Y"
	data <- c(rnorm(N * 0.99, mean=1000, sd=200),
	          runif(N * 0.01, min=1000, max=50000))
	data <- randomize(data)

} else if (type == 143) {	# unimodal normal outliers 2%
	outliers <- "Y"
	data <- c(rnorm(N * 0.98, mean=1000, sd=200),
	          runif(N * 0.02, min=1000, max=5000))
	data <- randomize(data)

} else if (type == 144) {	# unimodal normal outliers 4%
	outliers <- "Y"
	data <- c(rnorm(N * 0.96, mean=1000, sd=200),
	          runif(N * 0.04, min=1000, max=5000))
	data <- randomize(data)

} else if (type == 145) {	# unimodal normal outliers 2% clustered
	outliers <- "?"
	data <- c(rnorm(N * 0.98, mean=1000, sd=200),
	          rnorm(N * 0.02, mean=3000, sd=35))
	data <- randomize(data)

} else if (type == 146) {	# unimodal normal outliers 4% close 1
	outliers <- "Y"
	data <- c(rnorm(N * 0.96, mean=1000, sd=200),
	          runif(N * 0.04, min=1000, max=2700))
	data <- randomize(data)

} else if (type == 147) {	# unimodal normal outliers 4% close 2
	outliers <- "Y"
	data <- c(rnorm(N * 0.96, mean=1000, sd=200),
	          runif(N * 0.04, min=1000, max=2900))
	data <- randomize(data)

} else if (type == 148) {	# unimodal normal outliers 4% close 3
	outliers <- "Y"
	data <- c(rnorm(N * 0.96, mean=1000, sd=200),
	          runif(N * 0.04, min=1000, max=3100))
	data <- randomize(data)

} else if (type == 149) {	# unimodal normal outliers 4% close 4
	outliers <- "Y"
	data <- c(rnorm(N * 0.96, mean=1000, sd=200),
	          runif(N * 0.04, min=1000, max=3300))
	data <- randomize(data)

} else if (type == 150) {	# unimodal normal outliers 4% close 5
	outliers <- "Y"
	data <- c(rnorm(N * 0.96, mean=1000, sd=200),
	          runif(N * 0.04, min=1000, max=3500))
	data <- randomize(data)

} else if (type == 151) {	# unimodal normal outliers 4% close 6
	outliers <- "Y"
	data <- c(rnorm(N * 0.96, mean=1000, sd=200),
	          runif(N * 0.04, min=1000, max=3700))
	data <- randomize(data)

} else if (type == 152) {	# unimodal normal outliers 4% close 7
	outliers <- "Y"
	data <- c(rnorm(N * 0.96, mean=1000, sd=200),
	          runif(N * 0.04, min=1000, max=3900))
	data <- randomize(data)

} else if (type == 153) {	# unimodal normal outliers 0.5%
	outliers <- "Y"
	data <- c(rnorm(N * 0.995, mean=1000, sd=200),
	          runif(N * 0.005, min=1000, max=5000))
	data <- randomize(data)

} else if (type == 154) {	# unimodal normal outliers 0.2%
	outliers <- "Y"
	data <- c(rnorm(N * 0.998, mean=1000, sd=200),
	          runif(N * 0.002, min=1000, max=5000))
	data <- randomize(data)

} else if (type == 155) {	# unimodal normal outliers 0.1%
	outliers <- "Y"
	data <- c(rnorm(N * 0.999, mean=1000, sd=200),
	          runif(N * 0.001, min=1000, max=5000))
	data <- randomize(data)

} else if (type == 200) {	# bimodal normal very close
	outliers <- "N"
	data <- c(rnorm(N / 2, mean=850, sd=110),
	          rnorm(N / 2, mean=1150, sd=110))
	data <- randomize(data)

} else if (type == 201) {	# bimodal normal close
	outliers <- "N"
	data <- c(rnorm(N / 2, mean=825, sd=110),
	          rnorm(N / 2, mean=1175, sd=110))
	data <- randomize(data)

} else if (type == 202) {	# bimodal normal medium
	outliers <- "N"
	data <- c(rnorm(N / 2, mean=750, sd=110),
	          rnorm(N / 2, mean=1250, sd=110))
	data <- randomize(data)

} else if (type == 203) {	# bimodal normal far
	outliers <- "N"
	data <- c(rnorm(N / 2, mean=600, sd=110),
	          rnorm(N / 2, mean=1400, sd=110))
	data <- randomize(data)

} else if (type == 204) {	# bimodal normal outliers 1%
	outliers <- "Y"
	data <- c(rnorm(N * 0.495, mean=750, sd=110),
	          rnorm(N * 0.495, mean=1250, sd=110),
	          runif(N * 0.01, min=1000, max=5000))
	data <- randomize(data)

} else if (type == 205) {	# bimodal normal outliers 2%
	outliers <- "Y"
	data <- c(rnorm(N * 0.49, mean=750, sd=110),
	          rnorm(N * 0.49, mean=1250, sd=110),
	          runif(N * 0.02, min=1000, max=5000))
	data <- randomize(data)

} else if (type == 206) {	# bimodal normal outliers 4%
	outliers <- "Y"
	data <- c(rnorm(N * 0.48, mean=750, sd=110),
	          rnorm(N * 0.48, mean=1250, sd=110),
	          runif(N * 0.04, min=1000, max=5000))
	data <- randomize(data)

} else if (type == 210) {	# bimodal normal major minor
	outliers <- "N"
	data <- c(rnorm(N * 0.7, mean=750, sd=110),
	          rnorm(N * 0.3, mean=1250, sd=110))
	data <- randomize(data)

} else if (type == 211) {	# bimodal normal minor major
	outliers <- "N"
	data <- c(rnorm(N * 0.3, mean=750, sd=110),
	          rnorm(N * 0.7, mean=1250, sd=110))
	data <- randomize(data)

} else if (type == 212) {	# bimodal normal major minor outliers
	outliers <- "Y"
	data <- c(rnorm(N * 0.695, mean=750, sd=110),
	          rnorm(N * 0.295, mean=1250, sd=110),
	          runif(N * 0.01, min=1000, max=5000))
	N <- length(data)
	data <- randomize(data)

} else if (type == 213) {	# bimodal normal major minor outliers
	outliers <- "Y"
	data <- c(rnorm(N * 0.295, mean=750, sd=110),
	          rnorm(N * 0.695, mean=1250, sd=110),
	          runif(N * 0.01, min=1000, max=5000))
	N <- length(data)
	data <- randomize(data)

} else if (type == 300) {	# trimodal normal close
	outliers <- "N"
	data <- c(rnorm(N * 0.333, mean=750, sd=90),
	          rnorm(N * 0.334, mean=1000, sd=90),
	          rnorm(N * 0.333, mean=1250, sd=90))
	N <- length(data)
	data <- randomize(data)

} else if (type == 301) {	# trimodal normal medium
	outliers <- "N"
	data <- c(rnorm(N * 0.333, mean=500, sd=100),
	          rnorm(N * 0.334, mean=1000, sd=100),
	          rnorm(N * 0.333, mean=1500, sd=100))
	data <- randomize(data)

} else if (type == 302) {	# trimodal normal far
	outliers <- "N"
	data <- c(rnorm(N * 0.333, mean=500, sd=65),
	          rnorm(N * 0.334, mean=1000, sd=65),
	          rnorm(N * 0.333, mean=1500, sd=65))
	data <- randomize(data)

} else if (type == 303) {	# trimodal normal outliers
	outliers <- "Y"
	data <- c(rnorm(N * 0.333, mean=500, sd=100),
	          rnorm(N * 0.334, mean=1000, sd=100),
	          rnorm(N * 0.333, mean=1500, sd=100),
	          runif(N * 0.01, min=1000, max=5000))
	data <- randomize(data)

} else if (type == 304) {	# trimodal normal major medium minor
	outliers <- "N"
	data <- c(rnorm(N * 0.50, mean=500, sd=100),
	          rnorm(N * 0.33, mean=1000, sd=100),
	          rnorm(N * 0.17, mean=1500, sd=100))
	data <- randomize(data)

} else if (type == 305) {	# trimodal normal minor major minor
	outliers <- "N"
	data <- c(rnorm(N * 0.25, mean=500, sd=100),
	          rnorm(N * 0.50, mean=1000, sd=100),
	          rnorm(N * 0.25, mean=1500, sd=100))
	data <- randomize(data)

} else if (type == 306) {	# trimodal normal minor major medium
	outliers <- "N"
	data <- c(rnorm(N * 0.17, mean=500, sd=100),
	          rnorm(N * 0.50, mean=1000, sd=100),
	          rnorm(N * 0.33, mean=1500, sd=100))
	data <- randomize(data)

} else if (type == 307) {	# trimodal normal major minor medium
	outliers <- "N"
	data <- c(rnorm(N * 0.50, mean=500, sd=100),
	          rnorm(N * 0.17, mean=1000, sd=100),
	          rnorm(N * 0.33, mean=1500, sd=100))
	data <- randomize(data)

} else if (type == 400) {	# quad normal close
	outliers <- "N"
	data <- c(rnorm(N * 0.25, mean=700, sd=75),
	          rnorm(N * 0.25, mean=900, sd=75),
	          rnorm(N * 0.25, mean=1100, sd=75),
	          rnorm(N * 0.25, mean=1300, sd=75))
	data <- randomize(data)

} else if (type == 401) {	# quad normal medium
	outliers <- "N"
	data <- c(rnorm(N * 0.25, mean=700, sd=50),
	          rnorm(N * 0.25, mean=900, sd=50),
	          rnorm(N * 0.25, mean=1100, sd=50),
	          rnorm(N * 0.25, mean=1300, sd=50))
	data <- randomize(data)

} else if (type == 402) {	# quad normal far
	outliers <- "N"
	data <- c(rnorm(N * 0.25, mean=400, sd=60),
	          rnorm(N * 0.25, mean=800, sd=60),
	          rnorm(N * 0.25, mean=1200, sd=60),
	          rnorm(N * 0.25, mean=1600, sd=60))
	data <- randomize(data)

} else if (type == 403) {	# quad normal outliers
	outliers <- "Y"
	data <- c(rnorm(N * 0.25, mean=700, sd=50),
	          rnorm(N * 0.25, mean=900, sd=50),
	          rnorm(N * 0.25, mean=1100, sd=50),
	          rnorm(N * 0.24, mean=1300, sd=50),
	          runif(N * 0.01, min=1000, max=5000))
	data <- randomize(data)
}
