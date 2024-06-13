##
## Bai kiem tra 4 - Thuc hanh Ly thuyet Thong ke
## Lop: ST3 ca 1
##
## Ho ten: Le Dang Gia Khanh - MSSV: 22110081
##
##**************************************************
## Bai 1:
# 1.1
weights <- read.csv("weights.csv")
summary(weights)
hist(weights$Weight, main = "Histogram of Weights", xlab = "Weight")
boxplot(weights$Weight, main = "Boxplot of Weights", xlab = "Weight")
# Nhận xét: 
# 2. Histogram cho thấy phân phối của trọng lượng sản phẩm. Nếu phân phối lệch phải, điều đó có thể chỉ ra rằng có một số sản phẩm có trọng lượng lớn hơn nhiều so với các sản phẩm khác.
# 3. Boxplot cung cấp thông tin về phân vị và các giá trị ngoại lai. Nếu có giá trị ngoại lai, chúng sẽ được hiển thị trên boxplot dưới dạng các điểm riêng biệt.

# 1.2 (0.5đ) Viết cặp giả thuyết - đối thuyết cần kiểm định
# ## H0: mu = mu0 = 50
# ## H1: mu > mu0 = 50

# 1.3
# • Cách 1: Sử dụng hàm có sẵn trong R
t.test(weights$Weight, mu = 50, alternative = "greater", conf.level = 0.95)
# p-value = 0.1476 > 0.05 => Chấp nhận H0. Trọng lượng trung bình của sản phẩm không vượt quá 50 gram.
# • Cách 2: Tìm trị thống kê T0 và miền bác bỏ bằng công thức rồi rút ra kết luận.
n <- length(weights$Weight)
xbar <- mean(weights$Weight)
s <- sd(weights$Weight)
mu0 <- 50
alpha <- 0.05
t0 <- (xbar - mu0) / (s / sqrt(n))
t_alpha <- qt(1 - alpha, df = n - 1)
if (t0 > t_alpha) {
  cat("Kết luận: Chấp nhận H1")
} else {
  cat("Kết luận: Chấp nhận H0")
}
# 1.4 
test.twoside <- function(x, mu0, alpha) {
  n <- length(x)
  xbar <- mean(x)
  s <- sd(x)
  t0 <- (xbar - mu0) / (s / sqrt(n))
  t_alpha <- qt(1 - alpha / 2, df = n - 1)
  p_value <- 2 * pt(-abs(t0), df = n - 1)
  if (abs(t0) > t_alpha) {
    cat("Kết luận: Chấp nhận H1")
  } else {
    cat("Kết luận: Chấp nhận H0")
  }
  cat("\n")
  cat("p-value =", p_value)
}
test.twoside(weights$Weight, 51, 0.05)
# p-value = < 0.05 = alpha => Chấp nhận H1. Trọng lượng trung bình của sản phẩm không bằng 51 gram.
##------------------------------------------------------------
## Bai 2:
# 2.1
satisfaction <- read.csv("satisfaction.csv")
boxplot(Satisfaction ~ Store, data = satisfaction, main = "Boxplot of Satisfaction by Store", xlab = "Store", ylab = "Satisfaction")
# Nhận xét: Cửa hàng A có mức độ hài lòng trung bình cao hơn so với cửa hàng B.
# 2.2 
# ## H0: mu1 = mu2;
# ## H1: mu1 != mu2.
# 2.3 
var.test(Satisfaction ~ Store, data = satisfaction)
# p-value = 0.1211 > 0.05 => Chấp nhận H0. Không có sự khác biệt về mức độ hài lòng của khách hàng giữa hai cửa hàng A và B về phương sai.
t.test(Satisfaction ~ Store, data = satisfaction, var.equal = TRUE, conf.level = 0.95)
# p-value = 0.7086 > 0.05 => Chấp nhận H0. Không có sự khác biệt về mức độ hài lòng của khách hàng giữa hai cửa hàng A và B.
# 2.4
# H0: p = p0 = 0.5
# H1: p < p0 = 0.5
prop.test(table(satisfaction$Store, satisfaction$Satisfaction >= 4), correct = FALSE)
# p-value = 0.5926 > 0.05 => Chấp nhận H0. Tỉ lệ khách hàng hài lòng ở hai cửa hàng A và B không dưới 50%.
##------------------------------------------------------------
............................
##**************************************************
## Ket thuc