#Tạo ra được các ước lượng, minh họa kèm bằng hình ảnh

#Viết hàm tính trung bình, độ lệch chuẩn và sai số chuẩn mẫu

desc = function(x){
  av =  mean(x)
  sd = sd(x)
  se = sd(x)/sqrt(length(x))
  c(MEAN = av, SD = sd, SE = se)
}

desc(age3)

#Kiểm định với hàm t.test
#t.test nhằm kiểm tra liệu thông số của mẫu có bằng một tham số nào đó không?
#2 loại: một mẫu và 2 mẫu (nhằm kiểm định có cùng phân phối hay có cùng bằng một tham số hay không?)

t.test(age3, mu = 52)
t.test(AGE,INsulin)

#Đây là kiểm định biến số theo cặp, sẽ khác với kiểm định 2 tổng thể
t.test(AGE,INsulin,paired = TRUE)


#Tần số
frq = table(AGE, INsulin)
margin.table(frq, 1)
margin.table(frq, 2)
##Tính phần trăm tần số theo cột/ dòng/ cả bảng
prop.table(frq,1)
prop.table(frq,2)
prop.table(frq)

#Hàm prop.test để kiểm định giả thuyết cho tỷ lệ(x,n,p)
prop.test(69,100,0.5)

##Muốn chính xác hơn dùng binom.test(x,n,p)
binom.test(69,100,0.5)


#So sánh 2 tỷ lệ sd hàm binom.test
##Ví dụ 14
fracture = c(7,14)
total = c(100,110)
prop.test(fracture, total)

##Giá trị critical value ( tới hạn) qt(1-alpha,df(n))
##Tùy vào n mà nó sẽ chuẩn hay là Student
qt(0.95,70)

#So sánh nhiều tỷ lệ
male = c(30,50,100,120)
total1 = c(101,200,201,150)
prop.test(male,total1)


##### Hàm UL cho khoảng tin cậy của tỷ lệ
######Dùng hàm/ dùng prop.test
#### Điều kiện  np và nq >= 5
##2 phía
n  =  100
p_hat  =  0.6
anpha  =  1  -  0.95 
z = qnorm(1 - anpha/2)
se  =  sqrt(p_hat*(1  -  p_hat)/n)
conf_int <- p_hat + c(-z*se, z*se)

## Khoảng tin cậy trái
n  =  100
p_hat  =  0.6
anpha  =  1  -  0.95 
z = qnorm(1 - anpha)
se  =  sqrt(p_hat*(1  -  p_hat)/n)
conf_int <- p_hat + c(-Inf,z*se)

## Khoảng tin cậy phải
n  =  100
p_hat  =  0.6
anpha  =  1  -  0.95 
z = qnorm(1 - anpha)
se  =  sqrt(p_hat*(1  -  p_hat)/n)
conf_int <- p_hat + c(-z*se,Inf)

##prop.test
prop.test(60,  100,  conf.level  =  1  -  anpha)


##### Hàm UL cho khoảng tin cậy của kỳ vọng
###Dùng hàm/ t.test
###2 phía 
n  =  length(data)
x_bar  =  mean(data)
anpha  =  1  -  0.95
se = sd(data)/sqrt(n)
####Dùng q norm
z = qnorm(1 - anpha/2)
zconf_int  =  x_bar  +  c(-z*se,  z*se)

####Dùng qt (phân phối student n<30)
t  =  qt(1  -  anpha/2,  df  =  n  -  1)
tconf_int  =  x_bar  +  c(-t*se,  t*se)

###KTC trái
n  =  length(data)
x_bar  =  mean(data)
anpha  =  1  -  0.95
se = sd(data)/sqrt(n)
####Dùng q norm
z = qnorm(1 - anpha)
zconf_int  =  x_bar  +  c(-Inf,  z*se)

####Dùng qt (phân phối student n<30)
t  =  qt(1  -  anpha,  df  =  n  -  1)
tconf_int  =  x_bar  +  c(-Inf,  t*se)

###KTC phải
n  =  16
x_bar  =  49.75
anpha  =  1  -  0.95
se = 0.5/sqrt(n)
####Dùng q norm
z = qnorm(1 - anpha)
zconf_int  =  x_bar  +  c(-z*se, Inf)

####Dùng qt (phân phối student n<30)
t  =  qt(1  -  anpha,  df  =  n  -  1)
tconf_int  =  x_bar  +  c(-t*se, Inf)


###Dùng t.test
t.test(data,  conf.level  =  1  -  anpha)