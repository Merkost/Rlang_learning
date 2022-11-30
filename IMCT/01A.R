library(OneR)
library(miscTools)

help(OneR)

temp <- strsplit(readline(), " ")[[1]]
n <- as.integer(temp[1])
k <- as.integer(temp[2])

#other <- readLines()
df <- data.frame()
df_colnames <- strsplit(readline(), " ")[[1]]

exprs <- as.matrix(read.table(exprsFile, header=TRUE, sep = "\t",
                  row.names = 1,
                  as.is=TRUE))
# colnames(df) <- df_colnames
for (i in 1:n-1) {
  print(i)
  data <- matrix(nrow = 1, ncol = k + 1, strsplit(readline(), " ")[[1]])
  df <- rbind(df, data)
}


model <- OneR(data, verbose = TRUE)
summary(model)

data <- optbin(iris)
model <- OneR(data, verbose = TRUE)
summary(model)
plot(model)
prediction <- predict(model, data)
eval_model(prediction, data)
