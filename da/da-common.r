# da-common.r		Common functions for distanalysis.r.
#
# 01-Jun-2013	Brendan Gregg	Created this.

# printf
printf <- function(...) cat(sprintf(...))

# randomize ordering
randomize <- function(data) {
	data0 <- data
	data <- c()
	for(i in 1:N) { 
		ii <- sample(1:length(data0), 1)
		data[i] <- data0[ii]
		data0 <- data0[c(-ii)]
	}
	return(data)
}
