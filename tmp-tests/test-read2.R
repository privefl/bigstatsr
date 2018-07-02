# Example from http://www.chrisbilder.com/compstat/presentations/Xiaojuan/Presentation_bigmemory.pdf

mydata=matrix(c(NA),nrow=10072112,ncol=5)
set.seed(12345)
mydata[,1]=sample(c(1:17770), 10072112, replace = TRUE)
mydata[,2]=sample(c(1:480189), 10072112, replace = TRUE)
mydata[,3]=sample(c(1:5), 10072112, replace = TRUE)
mydata[,4]=sample(c(1999:2005), 10072112, replace = TRUE)
mydata[,5]=sample(c(1:12), 10072112, replace = TRUE)
write.table(mydata, file = "example.txt", sep = " ", row.names = F, col.names = F)

readLines("example.txt", n = 3)
system.time(
  test <- big_read("example.txt", sep = " ", header = FALSE, confirmed = TRUE, nlines.block = 1e6)
)

str(test)

system.time(
  x <- bigmemory::read.big.matrix(
    "example.txt", header = F, type = "integer", sep = " ",
    backingfile ="data.bin", descriptor = "data.desc",
    col.names = c("movie", "customer","rating","year", "month"), shared = TRUE)
)

all.equal(x[], test$FBM[], check.attributes = FALSE)
unlink(c("example.txt", "data.bin", "data.desc"))
