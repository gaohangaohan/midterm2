library(stringr)

names <- c("ID", "location", "continent")
for (i in 1:2540) {
  names <- c(names, paste(c("SNP", i), collapse = ""))
}
data <- read.table("hgdp.txt", header = FALSE)
names(data) <- names
key <- data
fun = function(x) {
  maina = substr(x[1],1,1)
  return (unlist(lapply(x, function(x) str_count(x, maina))))
}
key[,-1:-3] <- sapply(key[,-1:-3], fun)
write.table(key, "coded.txt", sep = "\t", row.names = FALSE, col.names = T, quote=FALSE)

