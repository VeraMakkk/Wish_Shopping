################################################
# 2021Summer Stats 102B Group Project 
# Dataset: Sales Summer Clothes in E-commerce Wish
# Yunjing Mai    405543716
# Mengyu Zhang  105360138
# Final project submission 
################################################
data <- read.csv("WishProductSummer.csv",header = T)
dim(data) ## 1457 35 
colnames(data)
################## II. Data  #####################
X <- data[,c(2,3,4,5,7,8,10,12,14,16,18,28,33,34)] # all quantitative variables 
colnames(X) 
OutVals = boxplot(X)$out
OutVals
# check outlier for merchant_rating_count
OutVals = boxplot(data$merchant_rating_count)$out
length(OutVals) # 164 outliers 
sd(data$merchant_rating_count) # 69927.11

# check outlier for rating_count
OutVals = boxplot(data$rating_count)$out 
length(OutVals) # 165 outliers 
sd(data$rating_count) # 1928.282

# check outlier for units_sold 
OutVals = boxplot(data$units_sold)$out
length(OutVals) # 116 outliers 
summary(data$units_sold)
sd(data$units_sold) #9037.937

OutVals = boxplot(data$price)$out
length(OutVals) # 116 outliers 

X <- data[,c(4,7,10,12,14,16,18,34)]
#remove  2-price  5-units_sold  8-rating-cout   33-merchant rating count  
#        3-retail price 28-countries ship to
head(X)
colnames(X)
OutVals = boxplot(X)$out
length(OutVals) 
# With the remain variables, there are 260 outliers in total. 

### remove the rows with outliers ###
# rating 
OutVals = boxplot(X$rating)$out
which(X$rating %in% OutVals)
X = X[!(X$rating %in% OutVals),]
dim(X) 

# merchant_rating
OutVals = boxplot(X$merchant_rating)$out
length(OutVals) #34 outliers 
which(X$merchant_rating %in% OutVals)
X = X[!(X$merchant_rating %in% OutVals),]
dim(X) # 1367

#rating_five_percentage
OutVals = boxplot(X$rating_five_percentage)$out
length(OutVals) #19
which(X$rating_five_percentage %in% OutVals)
X = X[!(X$rating_five_percentage %in% OutVals),]
dim(X) # 1348

#rating_four_percentage
OutVals = boxplot(X$rating_four_percentage)$out
length(OutVals) 
which(X$rating_four_percentage %in% OutVals)
X = X[!(X$rating_four_percentage %in% OutVals),]
dim(X) 

#rating_three_percentage
OutVals = boxplot(X$rating_three_percentage)$out
length(OutVals) 
which(X$rating_three_percentage %in% OutVals)
X = X[!(X$rating_three_percentage %in% OutVals),]
dim(X) 

#rating_two_percentage
OutVals = boxplot(X$rating_two_percentage)$out
length(OutVals) 
which(X$rating_two_percentage %in% OutVals)
X = X[!(X$rating_two_percentage %in% OutVals),]
dim(X) 

#rating_one_percentage
OutVals = boxplot(X$rating_one_percentage)$out
length(OutVals) 
which(X$rating_one_percentage %in% OutVals)
X = X[!(X$rating_one_percentage %in% OutVals),]
dim(X) 

# boxplot overlook after removing outliers
OutVals = boxplot(X,angle = 45)$out ## double check the final data
dim(X) 
cleaned_X <- X 
head(cleaned_X)

################ III. Cluster Analysis (kmeans()) #####################
Xk <- cleaned_X # data for k-means clustering
set.seed(1000)
K = kmeans(Xk,centers = 2)
table(K$cluster)
K$centers
plot(X, col = K$cluster)

plot(Xk$updated_price,Xk$rating,
     col=c("red","blue")[unclass(K$cluster)],
     pch=c(23,24)[unclass(K$cluster)],
     xlab="uodated_price", ylab="rating")
legend("bottomright",c("cluster 1","cluster 2"),pch=c
       (23,24),col=c("red","blue"))

plot(Xk$updated_price,Xk$merchant_rating,
     col=c("red","blue")[unclass(K$cluster)],
     pch=c(23,24)[unclass(K$cluster)],
     xlab="updated_prie", ylab="merchant_rating")
legend("bottomright",c("cluster 1","cluster 2"),pch=c
       (23,24),col=c("red","blue"))

####### IV. Pricipal components for dimension reduction  ############
X <- cleaned_X
colnames(X)
X.cs=scale(X,scale=T); X.cs ## centered and scaled
colnames(X.cs)=c("X1cs", "X2cs", "X3cs", "X4cs", "X5cs", "X6cs", "X7cs", "X8cs")
S=var(X.cs) ## variance and correlation 
S
eigen(var(X.cs))
eigenvectors=eigen(var(X.cs))$vectors
colnames(eigenvectors)=c("V1", "V2", "V3", "V4", "V5", "V6", "V7", "V8")
eigenvalues=eigen(var(X.cs))$values
cumsum(100*eigenvalues/(sum(eigenvalues)))

PC=X.cs%*%eigenvectors
head(PC)
cor(X, PC) ##

plot(X.cs[,1],X.cs[,2],main="centered data",
     xlab="X1cs", ylab="X2cs")
abline(v=0,col = "green")
abline(h=0,col = "green")

plot(PC[,1],PC[,2],main="plot of PC", 
     xlab="PC1", ylab="PC2",ylim=c(-1.5,max(PC[,1])))
abline(v=0,col = "green")
abline(h=0,col = "green")
biplot(princomp(X.cs), main = "Biplot")

######IV. Principal components regression for prediction##################
X <-cleaned_X
set.seed(1000)
# split the data into training and testing
index <-sample(nrow(X),10,replace = F)
testing_set <- X[index,] ## testing set
training_set <- X[-index,] ##training set 
dim(training_set)

colnames(training_set)
X.cs = scale(training_set, center=T, scale=T)
y.cs = X.cs[,2] ## variable rating as dependent variable 
x.cs = X.cs[,-2] ## independent variables 
cor(x.cs) #correlation with independent variables 
Sx=var(x.cs)
EP=eigen(Sx) 
lambda=EP$values
V=EP$vectors
X.tilde=x.cs%*%V     #this is the X tilde matrix of PC
X.tilde
cor(X.tilde)
cor(x.cs, X.tilde) 
cor(y.cs,X.tilde )

## Normalization for selected PC 
x1.tilde.cs <- X.tilde[,1]/sqrt(as.numeric(crossprod(X.tilde[,1])))
x2.tilde.cs <- X.tilde[,2]/sqrt(as.numeric(crossprod(X.tilde[,2])))
x3.tilde.cs <- X.tilde[,3]/sqrt(as.numeric(crossprod(X.tilde[,3])))
x4.tilde.cs <- X.tilde[,4]/sqrt(as.numeric(crossprod(X.tilde[,4])))
x5.tilde.cs <- X.tilde[,5]/sqrt(as.numeric(crossprod(X.tilde[,5])))
x6.tilde.cs <- X.tilde[,6]/sqrt(as.numeric(crossprod(X.tilde[,6])))
x7.tilde.cs <- X.tilde[,7]/sqrt(as.numeric(crossprod(X.tilde[,7])))

## Do the regression by lm() model 
df_train <- as.data.frame(cbind(pc1 = x1.tilde.cs, pc2 = x2.tilde.cs ,pc3= x3.tilde.cs,
                                pc4 = x4.tilde.cs, pc5 = x5.tilde.cs, pc6 = x6.tilde.cs,
                                pc7 = x7.tilde.cs))
colnames(df_train)
model1=lm(y.cs~ pc1+pc2+pc3+pc5+pc6-1,data = df_train)  
summary(model1)
plot(y.cs,resid(model1),main="Residual plot/n principal component regression")
abline(h=0,col = "red")
hist(resid(model1))

## prediction with testing set 
X_test <- as.data.frame(testing_set[,c(1,3,4,5,6,7)])
head(X_test)
colnames(X_test) <- colnames(df_train)[c(1,2,3,4,5,6)]
head(X_test)
predicteds = predict(model1,X_test)
predicteds
#root mean square error 
sqrt(mean((testing_set[,2] - predicteds)^2))

#################V. Maximum Likelihood estimation ###################
hist(X[,2], prob = T, xlim = c(2, 5),ylim = c(0, 2), main = "Histogram of rating")
y = X[,2]
x = seq(0,max(X[,2]),by=0.01)
points(x,dlnorm(x, meanlog=1.5, sdlog=0.5,log=FALSE), col="red", type="o",pch=10, bg="red")
points(x,dlnorm(x, meanlog=1.5, sdlog=0.1, log=FALSE), col="yellow", type="o", pch=22, bg="yellow")
points(x,dlnorm(x, meanlog=1.3, sdlog=0.05, log=FALSE), col="green", type="o", pch=22, bg="green")
points(x,dlnorm(x, meanlog=1.4, sdlog=0.08, log=FALSE), col="purple", type="o", pch=24, bg="purple")
n=length(y)

# mle estimate for mu
y.log=log(y)
head(y.log)
mu.mle=(sum(y.log))/n
mu.mle 
# mle estimate for sigma
sigma.mle=sqrt((1/n)*sum((y.log-mu.mle)^2))
sigma.mle 

H1.1 = -n/sigma.mle^2
H1.2 = -(2/sigma.mle^3)*sum(y.log-mu.mle)
H2.1=H1.2
H2.2= (n/sigma.mle^2)-(3/sigma.mle^4)*sum((y.log-mu.mle)^2)
Hessian=matrix(c(H1.1,H2.1,H1.2,H2.2), ncol=2)
Hessian
eigen(Hessian)

##estimated information matrix
I = -Hessian
## Variance Covariance matrix
Sigma = solve(I)
Sigma
## Standard error of the estimates
sqrt(diag(Sigma))

## Confidence intervals 95%
CI_mu <- mu.mle + c(-1,1)*qnorm(0.975)*sqrt(diag(Sigma))[1]
CI_mu
CI_sigma <- sigma.mle + c(-1,1)*qnorm(0.975)*sqrt(diag(Sigma))[2]
CI_sigma

