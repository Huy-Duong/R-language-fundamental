#Vẽ các loại biểu đồ trong R.
#Tính các đặc trưng mẫu trong R.
#Tạo một MNN từ BNN age kích thước 100
#Tính các đặc trưng mẫu là trung bình mẫu, phương sai hiệu chỉnh mẫu, độ lệch chuẩn mẫu và trung vị mẫu
#Biểu đồ quạt về tỷ trọng các khoảng của biến Age



library(ggplot2)
par(mfrow=c(2,2))
N <- 200
x <- runif(N, -4, 4)
y <- sin(x) + 0.5*rnorm(N)
ggplot(x,y, main="Scatter plot of y and x")
gghist(x, main="Histogram of x")
ggboxplot(y, main="Box plot of y")

ggplot(data = dat1, aes(x = Age)) + geom_histogram()

#Vẽ phân phối chuẩn N(156, 4,6^2)

height = seq(100,212,1)
ggplot(height, dnorm(height, 156, 4.6), type = "l", xlab="Height", ylab = "f(Height)", main ="Probability distribution of height in Vietnamese women")

height = seq(-4, 4, 0.1)
plot(height, dnorm(height, 0, 1), type="l", ylab="f(z)", xlab="z",main="Probability distribution of height in Vietnamese women")

#Vẽ hình, dùng ggsplot
pl = ggplot(data = p, aes(x= bmi, y= pcfat, fill=gender, color = gender))
pl+ geom_point()
q=ggplot(data = ccdat2,aes(x =CASH_ADVANCE_FREQUENCY,y= CASH_ADVANCE_TRX, fill =PURCHASES_TRX, color = PURCHASES_TRX))
q+ geom_point()


