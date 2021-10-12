library(ggplot2)
df = read.csv('FB.csv', header = TRUE)
names(df)
View(df)
attach(df)
str(df)
#Tao ham de tinh toan cac gia tri thong ke mo ta
desc <- function(x){
  av <- mean(x)
  sd <- sd(x)
  se <- sd/sqrt(length(x))
  c(MEAN=av, SD=sd, SE=se, summary(x))
}
#Open
desc(Open)
hist(Open)
Date1 <- as.Date(Date, format = '%m/%d/%Y')
ggplot(df, aes(x = Date1, y = Open)) +
  geom_line(color = "indianred3", 
            size=1 )
#Close
desc(Close)
hist(Close)
ggplot(df, aes(x = Date1, y = Close)) +
  geom_line(color = "indianred3", 
            size=1 )
#High
desc(High)
hist(High)
ggplot(df, aes(x = Date1, y = High)) +
  geom_line(color = "indianred3", 
            size=1 )
#Low
desc(Low)
hist(Low)
ggplot(df, aes(x = Date1, y = Low)) +
  geom_line(color = "indianred3", 
            size=1 )
#Volume
desc(Volume)
hist(Volume)
ggplot(df, aes(x = Date1, y = Volume)) +
  geom_line(color = "indianred3", 
            size=0.5 )
