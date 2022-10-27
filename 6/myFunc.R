x <- rnorm(50, mean = 0, sd = 1)
k <- 10
if (!is.vector(x)) return(NULL)
xmin <- min(x)
xmax <- max(x)
sequntial <- seq(xmin, xmax, length.out = k)
chunked <- as.data.frame(sequntial)
intervals <- as.data.frame(table(cut(x, b = k), dnn = list("Interval")))
df_intervals <- cut_borders(intervals$Interval)

# veclens <- as.vector(sapply(chunked, length))
# vecmeans <- as.vector(sapply(chunked, mean))
# vecmeansdivided <- as.vector(sapply(veclens, function(item) { item / length(x)} ))

qplot(rowMeans(df_intervals), intervals$Freq / length(x), main = "My normal distribution")