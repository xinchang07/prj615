#EX1
#2
x <- seq(3,6,by=0.1)
prod<- exp(x)*cos(x)
plot(prod,col="blue",type = "l")
points(cos(x),col="red")
points(exp(x),col="green")

#prob 6
set.seed(50)
xVec <- sample(0:999, size = 250, replace= TRUE)
yVec <- sample (0:999, size = 250, replace= TRUE)

yVec <- yVec[-1]
xVec <- xVec[-1*length(xVec)]
dVec <- yVec -xVec

#prob 7
#a
set.seed(50)
yVec <- sample(0.999,size =250, replace = TURE)

plot(yVec,col="red")

bVec <- yVec[yVec>600]
points(bVec,col = "green")

cVec <- (yVec>600)*1
head(cVec,20)
length(cVec)
points(yVec*cVec,col = "blue")


#ex 2
#prob 4

outer(0:4,0:4,"+")

#prob 3
A<- matrix(0,nrow = 6, ncol= 6,byrow = TRUE)
row(A)
col(A)
(abs(col(A)-row(A))==1)*1




