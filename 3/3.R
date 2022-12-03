my_data <- read.table('3/my_data.txt', sep = ";", header=T)
mydata2 <- read.table('3/mydata2.txt', sep = ";", head = T)

demo <- read.table('3/demo.rd', header = T)
# demo <- load("3/demo.rd")
newdemo <- apply(demo, 2, function(x) gsub(",", ".", x))
sapply(demo, mode)

numeric_demo <- apply(newdemo, 2, function(x) sapply(x, as.numeric))
colMeans(numeric_demo)
colnames(numeric_demo)
c(
  var(numeric_demo[,1]),
  var(numeric_demo[,2]),
  var(numeric_demo[,3]),
  var(numeric_demo[,4]),
  var(numeric_demo[,5]),
)

write.table(file = '3/demo.rd', newdemo)
rm(newdemo)
newdemo <- read.table(file = '3/demo.rd', head = T)
newdemo

data()
fdeaths
trees
write.table(pressure, "3/pressure.csv", row.names=F, col.names = T, quote=FALSE)
pressure <- read.table(file = '3/pressure.csv', sep = ',')
# colVars(numeric_demo)

