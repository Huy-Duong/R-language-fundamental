### Phân tích phương sai một nhân tố
## Ví dụ về độ Galactose

#B1: báo cho R biết có 3 tổng thể cần kiểm định với 3 số n1 = 9 , n2 = 11, n3 = 20
group = c(1,1,1,1,1,1,1,1,1, 2,2,2,2,2,2,2,2,2,2,2, 3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3)

#B2: Định nghĩa biến group là một yếu tố (factor)
group = as.factor(group)

#B3: Nhập dữ liệu cho từng mẫu
#galactose = read.table(file.choose())
galactose <- c(1343,1393,1420,1641,1897,2160,2169,2279,2890,1264,1314,1399,1605,2385,2511,2514,2767,2827,2895,3011,1809,2850,1926,2964,2283,2973,2384,3171,2447,3257,2479,3271,2495,3288,2525,3358,2541,3643,2769,3657)


#B4: Cho group và galactose vào một dataframe
### Lệnh attach() sẽ đưa dataframe của ta vào hệ thống r
gltdf = data.frame(group,galactose)
attach(gltdf)

#B5: dùng hàm lms() để phân tích phương sai rồi gán kết quả vào một biến
#Nhớ thứ tự (Các dữ liệu trước ~ nhóm nhân tố )
analysis = lm(galactose ~ group)

#B6: dùng Anova() để hiện thị bảng phân tích
### Pr(>F) chính là p value ta so sánh với 0.05 để đưa ra kết luận cho kiểm định
anova(analysis)

summary(analysis)



#Kiểm định xem các doanh thu các tuần năm 2012 của 8 cửa hàng walmart có sự khác nhau hay không?
data = read.csv(file.choose())
data$Store = as.factor(data$Store)
attach(data)
analysis1 = lm(data$Sales ~ data$Store)
anova(analysis1)

##Kết quả cho thấy có sự khác biệt về doanh thu trung bình các tuần trong cả năm 2012 của 8 cửa hàng của Walmart

data1 = read.table(file.choose(), header =T)
data1$F = as.factor(data1$F)
attach(data1)
analysis2 = lm(data1$D ~ data1$F)
anova(analysis2)
