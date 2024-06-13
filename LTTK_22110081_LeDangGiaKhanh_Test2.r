##
## Bai kiem tra 2 - Thuc hanh Ly thuyet Thong ke
## Lop: ST3 ca Sang Thu 3
##
## Ho ten: Le Dang Gia Khanh - MSSV: 22110081
##
##**************************************************
## Bai 1:
# 1.1
twosided.interval <- function(p) {
    a <- qt((1-p)/2, 15)
    b <- qt((1+p)/2, 15)
    return(c(a, b))
}
twosided.interval(0.8)
# 1.2
right.interval <- function(p) {
    c <- qt(1-p, 15)
    return(c(c, Inf))
}
right.interval(0.9)
# 1.3
left.interval <- function(p) {
    c <- qt(1-p, 15)
    return(c(-Inf, c))
}
left.interval(0.9)

##------------------------------------------------------------

## Bai 2:
# 2.1 (1đ) Với µ = 2, σ = 2, c = 7 và d = 5, hãy mô phỏng định lý 1 theo các bước sau:
# Bước 1. Tạo vectơ ngẫu nhiên X kích thước 10000 có phân phối chuẩn N(µ; σ^2);
X <- rnorm(10000, 2, 2)
# Bước 2. Tính Y = cX + d;
Y <- 7*X + 5
# Bước 3. Vẽ histogram cho Y ;
hist(Y, freq = 0, col = 'lightblue', breaks = nclass.Sturges(Y))
# Bước 4. Vẽ thêm hàm phân phối chuẩn N(cµ + d; c2σ2) trên cùng hình để so sánh.
curve(dnorm(x, mean = 7*2 + 5, sd = sqrt((7^2)*(2^2))), add = TRUE, col = 'red', lwd = 2)
# Nhận xét kết quả.
# Kết quả cho thấy phân phối của Y tiến dần về phân phối chuẩn N(cµ + d; c^2σ^2)

# 2.2 (1.5đ) Với n = 10, µ = 2 và σ = 2 hãy mô phỏng định lý 2 theo các bước sau:
# Bước 1. Tạo hàm Z() để lấy vectơ ngẫu nhiên X kích thước n từ phân phối chuẩn
# N(µ; σ^2), sau đó tính và trả về kết quả (X¯ − µ)/(SX/√n);
Z <- function() {
    X <- rnorm(10, 2, 2)
    return((mean(X) - 2)/(sd(X)/sqrt(10)))
}
# Bước 2. Tạo hàm vecZ(m) để lặp lại hàm Z() nhiều lần (m lần);
# replicate
vecZ <- function(m) {
    return(replicate(m, Z()))
}
# Bước 3. Vẽ histogram cho vecZ(10000);
hist(vecZ(10000), freq = 0, col = 'lightblue', breaks = nclass.Sturges(vecZ(10000)))
# Bước 4. Vẽ thêm hàm phân phối Student(n − 1) trên cùng hình để so sánh.
curve(dt(x, df = 9), add = TRUE, col = 'red', lwd = 2)
# Nhận xét kết quả.
# Kết quả cho thấy phân phối của vecZ(10000) tiến dần về phân phối Student(n − 1)

##------------------------------------------------------------

## Bai 3:
# 3.1
# Phan phoi xac suat cua X la phan phoi xac suat cua binomial voi n = 50 va p = 1/5.
# 3.2
n <- 50
p <- 1/5
x <- 0:50
y <- dbinom(x, n, p)
plot(x, y, type = 'h', col = 'red')
# 3.3 
# Cách 1:
sum(dbinom(25:50, n, p))
# Cách 2:
pbinom(24, n, p, lower.tail = FALSE)

##------------------------------------------------------------

## Bai 4:
# 4.1
throw.coin <- function() {
    if (runif(1) <= 0.6) {
        return('S')
    } else {
        return('N')
    }
}
# 4.2 
sample.coin <- function(n) {
    return(replicate(n, throw.coin()))
}
# 4.3 Chạy hàm vừa viết với n là 6 chữ số cuối trong mã số sinh viên của bạn.
# Lập bảng tần suất và vẽ biểu đồ cột.
x <- sample.coin(110081)
table(x)
barplot(table(x))

##**************************************************
## Ket thuc