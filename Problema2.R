#Codigo para problema 2
mis_dades <- iris
dim(mis_dades)
names(mis_dades)
mis_dades$Petal.Length
mean(mis_dades$Petal.Length)
sd(mis_dades$Petal.Length)
hist(mis_dades$Petal.Length)
x<-mis_dades$Petal.Length
y<-mis_dades$Sepal.Length
plot(x,y)
m=(sum((x-mean(x))*(y-mean(y))))/sum((x-mean(x))^2)
m
b=mean(y)-m*mean(x)
b
plot(x,m*x+b)
lm(y~x)
mod<-lm(y~x)

predict(mod,data.frame(x=1.5))
a=m*1.5+b
a
ypredict<-predict(mod,data.frame(x=x))
plot(x,y)
lines(x,m*x+b)
plot(x,y,col="red")
lines(x,m*x+b,col="blue")


Rsq<-sum((ypredict-mean(y))^2)/sum((y-mean(y))^2)
Rsq
summary(mod)
