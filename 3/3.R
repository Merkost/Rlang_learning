
my_data <- read.table('my_data.txt', sep = ";", header=T)
mydata2 <- read.table('mydata2.txt', sep = ";", head = T)

demo <- read.table('clipboard', header = T)
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
  var(numeric_demo[,5])
)

write.table(file = 'demo.rd', newdemo)
rm(newdemo)
newdemo <- read.table(file = 'demo.rd', head = T)
newdemo

data()
fdeaths

write.table(pressure, "pressure.csv", row.names=F, col.names = T, quote=FALSE)
deaths <- read.table(file = 'pressure.csv', sep = ',')
# colVars(numeric_demo)

