corr <- function(directory, threshold = 0) {
	filenames = dir(directory, pattern = "*.csv")
	result <- numeric()
	for (f in filenames) {
		data <- read.csv(paste(directory, "/", f, sep = ""))
		if (sum(complete.cases(data)) > threshold) {
			dataFiltered <- data[complete.cases(data),]
			result <- c(result, cor(dataFiltered["sulfate"], dataFiltered["nitrate"]))
		}
	}
	result
}
