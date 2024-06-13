##
## Bai kiem tra 3 - Thuc hanh Ly thuyet Thong ke
## Lop: ST3 ca 1
##
## Ho ten: Le Dang Gia Khanh - MSSV: 22110081
##
##**************************************************
## Bai 1:
# 1.1 
hist(mtcars$qsec, freq = 0, col = 'lightblue', breaks = nclass.Sturges(mtcars$qsec))
# nhan xet
# Biến qsec có phân phối lệch phải, tập trung ở khoảng 17-19 giây.

# 1.2 
ci.var <- function(x, alpha){
    n <- length(x)
    s2 <- var(x)
    CI <- c((n-1)*s2/qchisq(1-alpha/2, df = n-1), (n-1)*s2/qchisq(alpha/2, df = n-1))
    return(CI)
}
# 1.3 
ci.var(mtcars$qsec, 0.05)
ci.var(mtcars$qsec, 0.01)

##------------------------------------------------------------
## Bai 2:
# 2.1
data <- data.frame(height = c(92.5, 97.5, 102.5, 107.5, 112.5, 117.5), freq = c(10, 8, 20, 18, 29, 15))
x <- rep(data$height, data$freq)
hist(x, freq = 0, col = 'lightblue', breaks = seq(90, 120, by = 5))
# 2.2 
ktc.tyle <- function(data, data.p, alpha){
    p <- length(data.p)/length(data)
    CI <- c(p - qnorm(1-alpha/2)*sqrt(p*(1-p)/length(data)), p + qnorm(1-alpha/2)*sqrt(p*(1-p)/length(data)))
    return(CI)
}
# 2.3 
ktc.tyle(x, x[x >= 110], 0.05)
##------------------------------------------------------------
## Bai 3:
# 3.1 
log_likelihood <- function(theta){
    x <- c(0.0192, 0.4773, 0.3399, 0.4324, 0.8675, 0.2079, 0.3713, 0.2547, 0.3976, 0.2048)
    return(sum(log(theta*x^(theta-1))))
}
# 3.2 (2đ) 
# Cách 1: 
x <- c(0.0192, 0.4773, 0.3399, 0.4324, 0.8675, 0.2079, 0.3713, 0.2547, 0.3976, 0.2048)
MLE <- -length(x)/sum(log(x))
MLE
# Cách 2: 
objective <- function(theta){
    return(-log_likelihood(theta))
}
nlminb(0.5, objective)$par
##**************************************************
## Ket thuc