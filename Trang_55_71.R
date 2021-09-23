#9. PHAN TICH THONG KE MO TA

#9.1 Thong ke mo ta(descriptive statistics, summary)

#Tinh gia tri trung binh
mean(x)
#Tinh phuong sai va do lech chuan
var(x)
sd(x)
#Lenh summary co the cho tat ca thong tin thong ke ve mot bien
summary(x)
#Ham tinh do lech chuan
desc = function(x){
  av = mean(x)
  sd = sd(x)
  se = sd/sqrt(length(x))
  c (MEAN=av, SD=sd, SE=se)
}
desc(x)
#Thong ke mo ta cho mot dataframe
summary(dataframe)
#Ket qua cho tung nhom Nam Nu rieng biet dung ham by
by(dataframe, sex, summary)
#Chia man anh thanh 6 cua so (2 dong, 3 cot)
op = par(mfrow=c(2, 3))
hist(c1)
hist(c2)
hist(c3)
hist(c4)
hist(c5)
hist(c6)

#9.2 Thong ke mo ta theo tung nhom

#Tinh trung binh cua mot bien so cho moi nhom nam va nu
tapply(c1, listt(sex), mean)
#Tinh trung inh cua mot bien so cho gioi tnh va sac toc
tapply(c1, list(ethnicity, sex), mean)

#9.3 Kiem dinh t (t.test)
#9.3.1 Kiem dinh t mot mau
#Ta muon biet gia tri tb 19.17 co that su khac vơi gia tri tb 30 hay khong
t.test(age, mu=30) #age la bien so can kiem dinh, mu = 30 la gia tri  gia thiet
#9.3.2 Kiem dinh t hai mau
#Ta thay phu nu co hormone igfi cao hon nam. Cau hoi la co phai su that do la
#mot khac biet co he thong hay do cac yeu to ngau nhien gay nen
t.test(igfi ~ sex)
# p_value = 0.4025 > 0.05 nen muc do khac biet giua nam va nu khong co y nghia
#thong ke
#Neu hai nhom co cung phuong sai
t.test(igfi ~ sex, var.equal=TRUE)

#9.4 Kiem dinh Wilcoxon cho hai mau (wilcox.test)
#Kiem dinh Shapiro.test de kiem tra phan phoi chuan
shapiro.test(igfi)
#Vi p_value < 0.05 nen igfi khong tuan theo phan phoi chuan
#Trong truong hop nay viec so sanh giua hai nhom co the dua vao phuong phap
#phi tham so (non-parametric) co ten kiem dinh Wilcoxon, vi kiem dinh t khong
#tuy thuoc vao phan phoi chuan
wilcox.test(igfi ~ sex)
#Tri so p = 0.682 cho thay do khac biet ve igfi cua nam va nu khong co y nghia thong ke

#9.5 Kiem dinh t cho cac bien so theo cap (paired t-test, t.test)
#Nhap du kien
before = c(180, 140, 160, 160, 220, 185, 145, 160, 160, 170)
after = c(170, 145, 145, 125, 205, 185, 150, 150, 145, 155)
bp = data.frame(before, after)
bp
#Kiem dinh t
t.test(before, after, paired = TRUE)
#Ket qua cho thay sau khi dieu tri ap suat mau giam 10.5 mmHg, va khoang tin cay
#95% la tu 2.0 mmHg den 19 mmHg, voi p = 0.029. Nhu vay chung ta co bang chung de 
#phat bieu rang muc do giam huyet ap co y nghia thong ke

#Neu chung ta phan tich sai bang thong ke cho hai nhom doc lap duoi day thi tri
#so p = 0.32 cho biet muc do giam ap suat khong co y nghia thong ke
t.test(before, after)

#9.6 Kiem dinh Wilcoxon cho cac bien so theo cap (wilcox.test)
#Thay vi dung t cho tung cap, chung ta cung co the su dung ham wilcox.test cho 
#cung muc dich
wilcox.test(before, after, paired = TRUE)
#Ket qua tren mot lan nua khang dinh rang do giam ap suat mau co y nghia thong ke 
#voi tri so p = 0.023 chang khac may so voi kiem dinh t cho tung cap

#9.7 Tan so (frequency)
#Ham table trong R co chuc nang cho ta biet ve tan so cua mot bien so mang tinh
#phan loai nhu sex va ethnicity
table(sex)
table(ethnicity)
#Mot bang thong ke 2 chieu
table(sex, ethnicity)
#Chu y: Ham table khong cung cap so phan tram
#De tinh so phan tram ta dung ham prop.table
#Tao ra mot object ten la freq de chua ket qua tan so
freq = table(sex, ethnicity)
#Kiem tra ket qua
freq
#Dung ham margin.table de xem ket qua
margin.table(freq, 1)
margin.table(freq, 2)
#Tinh phan tram bang ham prop.table
prop.table(freq, 1)
#Trong thong ke tren, prop.talbe tinh ti le sac toc cho tung gioi tinh
prop.table(freq, 2)
#Trong thong ke tren, prop.table tinh ti le gioi tinh cho tung sac toc

#9.8 Kiem dinh ti le (proportion test, prop.test, binom.test)
#Ta thay nu chiem ti le 0.39. De kiem dinh xem ti le nay co that sy khac voi
#ti le 0.5 hay khong, ta co the su dung ham prop.test(x, n, π) nhu sau
prop.test(69, 100, 0.5)
#Ket qua uoc tinh ti le nu gioi là 0.69, va khoang tin cay 95% la 0.588 den 0.776.
#Gia tri Chi binh phuong la 13.69, voi p = 0.00216. Nhu vay nghien cuu nay co ti
#le nu cao hon 50%.

#Mot cach tinh chinh xac hon kiem dinh ti le la kiem dinh nhi phan binom.test
binom.test(69, 100, 0.5)

#9.9 So sanh hai ti le (prop.test, binom.test)
#Vi du 14:
fracture = c(7, 20)
total = c (100, 110)
prop.test(fracture, total)
#Ket qua cho thay ti le gay xuong cua nhom A la 0.07 va B la 0.18.
#Phan tich tren con cho thay xac suat 95% rang do khac biet giua 2 nhom co the
#0.01 den 0.2 (1 den 20%). Voi p = 0.027, ta co the noi rang ti le gay xuong nhom
#A qua that thap hon nhom B

#9.10 So sanh nhieu ti le (prop.test, chisq.test)
table(sex, ethnicity)
female <- c( 4, 43, 22, 0)
total <- c(8, 60, 30, 2)
prop.test(female, total)
#9.10.1 Kiem dinh Chi binh ohuong (Chi squared test, chisq.test)
chisq.test(sex, ethnicity)
#Ket qua nay hoan toan giong voi ket qua tu prop.test
#9.10.2 Kiem dinh Fisher
fisher.test(sex, ethnicity)
#Tri so p tu kiem dinh Fisher la 0.1048, tuc rat gan voi so p cua Chi binh phuong.
#Cho nen, chung ta co them bang chung de khang dinh ti le gioi tinh nu giua cac
#sac toc khong khac nhau mot cach dang ke.