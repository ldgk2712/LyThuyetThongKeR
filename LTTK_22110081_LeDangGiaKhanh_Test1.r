## Bai 1:
A <- matrix(c(1,2,3,2,2,2,3,2,3), nrow = 3, byrow = TRUE)
A 
B <- matrix(c(-3,4,-6,2,-1,2,-2,-5,4,-6,3,9,7,-2,-6), nrow = 3, byrow = TRUE)
B

# 1.1
B[,4]
c(B[1,2:3],B[1,5])

# 1.2
C <- A %*%B
C

# 1.3
A_T <- t(A)
A_T
B_T <- t(B)
B_T
BA_T <- B_T %*% A_T
# C la ma tran chuyen vi cua BA_T
# t(C) = BA_T

# 1.4
X <- solve(A) %*% t(matrix(c(1,1,1), nrow = 1))
X

##------------------------------------------------------------
## Bai 2:

# 2.1
year <- c(1970, 1971, 1972, 1973, 1974, 1975, 1976, 1977, 1978, 1979)
snow.cover <- c(6.5, 12.0, 14.9, 10.0, 10.7, 7.9, 21.9, 12.5, 14.5, 9.2)
df.snow <- data.frame(year, snow.cover)

# 2.2
plot(df.snow$year, df.snow$snow.cover)
cor(df.snow$year, df.snow$snow.cover)

# 2.3
par(mfrow = c(2,1))
hist(df.snow$snow.cover)
hist(df.snow$snow.cover, freq = FALSE)

# 2.4
boxplot(df.snow$snow.cover, horizontal = TRUE)
min(df.snow$snow.cover)
max(df.snow$snow.cover)
mean(df.snow$snow.cover)
summary(df.snow$snow.cover)

##------------------------------------------------------------

## Bai 3
# 3.1
setwd("D:/Code/R")
getwd()
data <- read.csv('data01.csv')
attach(data)
# 3.2
Index <- c()
for (i in 1:length(Age)) {
  if (Age[i] <= 60) {
    Index[i] <- 0
  } else if (Age[i] <= 70) {
    Index[i] <- 1
  } else if (Age[i] <= 80) {
    Index[i] <- 2
  } else {
    Index[i] <- 3
  }
}

# 3.3
table(Index)
pie(table(Index))
barplot(table(Index))

# 3.4
table(K, Index)
barplot(table(K, Index) , beside = TRUE, legend = rownames(table(K, Index)))

# 3.5
phuongsai <- function(x){
    n <- length(x)
    mean_x <- mean(x)
    sum((x - mean_x)^2)/(n-1)
}
phuongsai(FPSA)
var(FPSA)
# var(FPSA) = phuongsai(FPSA)

##------------------------------------------------------------

## Bai 4
phanvi <- function(X, p){
    X <- sort(X)
    n <- length(X)
    i <- p*n/100
    if (i %% 1 == 0){
        return((X[i] + X[i+1])/2)
    } else {
        return(X[round(i)])
    }
}

# 4.2
p_0 <- x
phanvi(iris$Petal.Length, p_0)
# Phan vi thu x cua Petal.Length la {value} value -> tuong ung voi p_0