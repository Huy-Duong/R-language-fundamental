md1 = read.csv(file.choose(), header =TRUE)

#Vẽ đám mây điểm với 2 biến
ggplot(md1, aes(x = Hours.Studied, y= Performance.Index)) + geom_point()
ggplot(md1, aes(x = Previous.Scores, y= Performance.Index)) + geom_point()
ggplot(md1, aes(x = Sleep.Hours, y= Performance.Index)) + geom_point()
ggplot(md1, aes(x = Sample.Question.Papers.Practiced, y= Performance.Index)) + geom_point()


pairs(md1)



#Mô hình hồi quy tuyến tính
regmd1 = lm(md1$Performance.Index ~md1$Hours.Studied + md1$Previous.Scores+ md1$Sleep.Hours +md1$Sample.Question.Papers.Practiced)
summary(regmd1)

confint(model, level = 0.95)

#hoặc sử dụng công thức ma trận tính ra vector B
Xmd1 = matrix(c(rep(1,70),md1$Hours.Studied,md1$Previous.Scores, md1$Sleep.Hours, md1$Sample.Question.Papers.Practiced), ncol = 5)
Ymd1 = matrix(md1$Performance.Index, ncol = 1)
#Tính ước lượng cho vector hệ số
Amd1 = t(Xmd1)%*% Xmd1
Bmd1 = t(Xmd1)%*% Ymd1

Betamd1 = solve(Amd1)%*%Bmd1
Betamd1