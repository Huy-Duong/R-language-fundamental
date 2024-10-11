md2 = read.csv(file.choose(), header =TRUE)

#Mô hình hồi quy tuyến tính
regmd2 = lm(md2$price ~md2$bedrooms+ md2$bathrooms+ md2$floors)
summary(regmd2)

#hoặc sử dụng công thức ma trận tính ra vector B
Xmd2 = matrix(c(rep(1,30),md2$bedrooms,md2$bathrooms,md2$floors), ncol = 4, nrow =30)
Ymd2 = matrix(md2$price, ncol = 1)
#Tính ước lượng cho vector hệ số
Amd2 = t(Xmd2)%*% Xmd2
Bmd2 = t(Xmd2)%*% Ymd2

Betamd2 = solve(Amd2)%*%Bmd2
Betamd2