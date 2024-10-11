#Ví dụ 1:
#Vẽ đám mây điểm

lr = data.frame(read.table(file.choose(), header = T))
lr1 = data.frame(lr$Age, lr$BMI, lr$Chol)

#Vẽ đám mây điểm với 2 biến
ggplot(lr1, aes(x = lr.Age, y= lr.Chol)) + geom_point()
plot(lr1$lr.Chol~lr1$lr.Age, pch = 16)

#Hệ số tương quan Pearson (Chỉ dùng trong điều kiện X và y đều có phân phối chuẩn)
cor(lr1$lr.Chol,lr1$lr.Age)

#Ta có thể không tính hệ số tương quan mà kiểm định tính tương quan thông qua hàm cor.test()
cor.test(lr1$lr.Chol, lr1$lr.Age)

#Mô hình hồi quy tuyến tính
lm(lr1$lr.Chol ~ lr1$lr.Age)

#Xem thêm thông tin của mhhq tt này
#Các giá trị có thể xem được: hệ số xác định, kiểm định của hệ số góc, phương sai ước lượng (residual standartd error)
reg = lm(lr1$lr.Chol ~ lr1$lr.Age)
summary(reg)

#fitted(reg) để tính toán giá trị hàm hồi quy ước lượng
#resid(reg) để tính toán phần dư ei cho từng cá nhân

#ta dùng hàm abline() để vẽ đường hồi quy
ggplot(lr1, aes(x = lr.Age, y= lr.Chol)) + geom_point() + geom_abline(intercept = 1.08922, slope = 0.05779, color = "REd")

#Sử dụng lệnh pairs() để vẽ các biểu đồ giá trị mhhqtt bội
pairs(lr1)

#Phân tích tương quan giữa 2 biến BMI và Chol
summary(lm(lr1$lr.Chol~lr1$lr.BMI))

#Sử dụng mhhqđb
mreg = lm(lr1$lr.Chol~lr1$lr.BMI + lr1$lr.Age)
summary(mreg)


#Cách nhập ma trận
y = c(1,2,3,4,5,6,7,8,9)
A = matrix(y, nrow=3)

#Chuyển vị
B= t(A)

#Nhân 2 ma trận
C = A%*%B

#Nghịch đảo ma trận
#Kiểm tra định thức trước
F = matrix((1:9)^2,3,3)
det(F)
solve(F)


#Bài ví dụ
vd1 = data.frame(read.table(file.choose()))
VD1 = data.frame(vd1$V2, vd1$V3, vd1$V4)
#Mô hình hồi quy tuyến tính
regvd1 = lm(VD1$vd1.V2 ~VD1$vd1.V3 + VD1$vd1.V4)
summary(regvd1)

#hoặc sử dụng công thức ma trận tính ra vector B
X = matrix(c(rep(1,25),VD1$vd1.V3,VD1$vd1.V4), ncol = 3)
Y = matrix(VD1$vd1.V2, ncol = 1)
#Tính ước lượng cho vector hệ số
A = t(X)%*%X
B = t(X)%*%Y

Beta = solve(A)%*%B


#Ví dụ 2
vd2 = data.frame(read.table(file.choose()))
regvd2 = lm(vd2$V1 ~vd2$V2+vd2$V3+vd2$V4+vd2$V5)
summary(regvd2)

X1 = matrix(c(rep(1,15),vd2$V2, vd2$V3, vd2$V4, vd2$V5), ncol = 5)
Y1 = matrix(vd2$V1, ncol = 1)
#Tính ước lượng cho vector hệ số
A1 = t(X1)%*%X1
B1 = t(X1)%*%Y1

Beta1 = solve(A1)%*%B1
Beta1
