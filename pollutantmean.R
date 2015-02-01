pollutantmean <- function(directory, pollutant, id = 1:332) {
	l <- numeric()
	for (i in id) {
		# formatC(aboveThreshold$id, width=3, format="d", flag="0")
		zero <- if (i < 10) {
			"00"
		} else if (i < 100) {
			"0"
		} else {
			""
		}
		data <- read.csv(paste(directory, "/", zero, i, ".csv", sep = ""))
		l <- c(l, data[, pollutant])
	}
	round(mean(l, na.rm = TRUE), digits = 3)
}
