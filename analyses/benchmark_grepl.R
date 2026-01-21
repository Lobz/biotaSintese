
start <- "qywtruyqtwreuytdjhgvASNBVMNBVMNAdfsdfsdSJYGDJHGC"
end1 <- "ljhagsljhglajsfhgl"
end2 <- "jhgkjhgkjhgkjhgkjhgkjhg"

match1 <- paste0(start, end1)
match2 <- paste0(start, end2)

pattern1 <- paste0(match1, "|", match2)
pattern2 <- paste0(start, "(", end1, "|", end2, ")")

fun1 <- function(x) grepl(pattern1, x, perl = T)
fun2 <- function(x) grepl(pattern2, x, perl = T)
fun3 <- function(x) grepl(match1, x, fixed = T) | grepl(match2, x, fixed = T)

x <- replicate(100, paste0(sample(c(letters,LETTERS, start, end1, end2), 300, T), collapse=""))
y <- replicate(100, paste0(sample(c(letters,LETTERS, start, end1, end2), 300, T), collapse=""))

microbenchmark::microbenchmark(fun1(x), fun2(x), fun3(x))

fun4 <- function(x) grepl(paste0(match1, "|", match1), x, perl = T)
fun5 <- function(x) grepl(paste0(match1), x, perl = T)

microbenchmark::microbenchmark(fun4(x), fun5(x))

fun6 <- function(x,y) grepl(pattern1, x) | grepl(pattern1, y)
fun7 <- function(x,y) grepl(pattern1, paste(x,y))

microbenchmark::microbenchmark(fun6(x, y), fun7(x, y))
