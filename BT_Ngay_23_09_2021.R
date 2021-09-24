ipod <- sample(c(rep(1, 250),
                 rep(2,300),
                 rep(3, 600),
                 rep(4, 800),
                 rep(5, 550),
                 rep(6, 350),
                 rep(7, 100),
                 rep(8, 25),
                 rep(9, 20),
                 rep(10, 5)))
ipod
library(ggplot2)

#Lay 25 mau 50 lan
x <- NULL
for (i in c(1:50)){
  x <- c(x,sample(ipod,25))
}
x

playList <- data.frame(x)
playList
#Ve bieu do
p <- ggplot(playList, aes(x=x)) + 
  geom_histogram()
p
# xac suat xuat hien bai 6 phut tro len
a <- table(playList)
a
xs = (a[6]+a[7]+a[8]+a[9]+a[10])/(25*50)
xs

#Lay 25 mau 500 lan
x <- NULL
for (i in c(1:500)){
  x <- c(x,sample(ipod,25))
}
x

playList <- data.frame(x)
playList
#Ve bieu do
p <- ggplot(playList, aes(x=x)) + 
  geom_histogram()
p
# xac suat xuat hien bai 6 phut tro len
a <- table(playList)
xs = (a[6]+a[7]+a[8]+a[9]+a[10])/(25*500)
xs

#Lay 30 mau 30 lan
x <- NULL
for (i in c(1:30)){
  x <- c(x,sample(ipod,30))
}
x

playList <- data.frame(x)
playList
#Ve bieu do
p <- ggplot(playList, aes(x=x)) + 
  geom_histogram()
p
#Xac suat xuat hien bai 6 phut tro len
a <- table(playList)
a[6]
xs = (a[6]+a[7]+a[8]+a[9]+a[10])/(30*30)
xs