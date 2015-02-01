complete <- function(directory, id = 1:332) {
	df <- data.frame()
	for (i in id) {
		zero <- if (i < 10) {
			"00"
		} else if (i < 100) {
			"0"
		} else {
			""
		}
		data <- read.csv(paste(directory, "/", zero, i, ".csv", sep = ""))
		df <- rbind(df, c(i, sum(complete.cases(data))))
	}
	colnames(df) <- c("id", "nobs")
	df
}
