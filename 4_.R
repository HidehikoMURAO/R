####
# 図4.1の描画および表4.1の結果の出力
####
library(glmnet)
library(ElemStatLearn)
library(makedummies)
attach(SAheart)
SAheart <- makedummies(SAheart)
x <- scale(as.matrix(SAheart[ , -10 ]))
# ロジスティック回帰モデル
glmnet_fit <- glmnet(x, chd, family="binomial")
# 解パスの描画
plot(glmnet_fit, xvar="lambda", label=TRUE, xlab="正則化パラメータの対数値", ylab="回帰係数", col="black", lwd=2.5)
cv.glmnet_fit <- cv.glmnet(x, chd, family="binomial") 
opt_lambda <- cv.glmnet_fit$lambda.min
# 交差検証法により選択された正則化パラメータに対応する回帰係数
glmnet(x, chd, lambda=opt_lambda, family="binomial")$beta

####
# 図4.2の描画および表4.2の結果の出力
####
library(glmnet)
library(AER)
library(makedummies)
data("PhDPublications")
attach(PhDPublications)
PhDPublications <- makedummies(PhDPublications, basal_level=TRUE)
x <- scale(as.matrix(PhDPublications[ , -c(1,3,4) ]))
# ポアソン回帰モデル
glmnet_fit <- glmnet(x, articles, family="poisson")
# 解パスの描画
plot(glmnet_fit, xvar="lambda", label=TRUE, xlab="正則化パラメータの対数値", ylab="回帰係数", col="black", lwd=2.5)
cv.glmnet_fit <- cv.glmnet(x, articles, family="poisson") 
opt_lambda <- cv.glmnet_fit$lambda.min
# 交差検証法により選択された正則化パラメータに対応する回帰係数
glmnet(x, articles, lambda=opt_lambda, family="poisson")$beta

####
# 図4.3の描画
####
library(glmnet)
library(ElemStatLearn)
Data <- zip.train[c(which(zip.train[,1]==2), which(zip.train[,1]==7), which(zip.train[,1]==9)), ]
x <- scale(Data[ ,-1])
y <- Data[,1]
cv.glmnet_fit <- cv.glmnet(x, y, family="multinomial") 
opt_lambda <- cv.glmnet_fit$lambda.min
# 多項ロジスティック回帰モデル
glmnet_fit <- glmnet(x, y, lambda=opt_lambda, family="multinomial")
beta2 <- abs(glmnet_fit$beta[[1]][1:256])
beta7 <- abs(glmnet_fit$beta[[2]][1:256])
beta9 <- abs(glmnet_fit$beta[[3]][1:256])
beta2[beta2!=0] <- beta7[beta7!=0] <- beta9[beta9!=0] <- 1
image_obj <- rbind(c(2,beta2), c(7,beta7), c(9,beta9))
# 画像2，7，9の判断に寄与する部分の描画
image(zip2image(image_obj, 1), col=gray(1:0))
image(zip2image(image_obj, 2), col=gray(1:0))
image(zip2image(image_obj, 3), col=gray(1:0))

####
# 図4.4の描画および表4.3の結果の出力
####
library(glmnet)
library(compound.Cox)
data("PBC")
x <- PBC[ ,-c(1,2)]
x <- scale(x)
y <- as.matrix(PBC[ ,c(1,2)])
colnames(y) <- c("time", "status")
# コックス回帰モデル
glmnet_fit <- glmnet(x, y, family="cox")
plot(glmnet_fit, xvar="lambda", label=TRUE, xlab="正則化パラメータの対数値", ylab="回帰係数", col="black", lwd=2.5)
cv.glmnet_fit <- cv.glmnet(x, y, family="cox") 
opt_lambda <- cv.glmnet_fit$lambda.min
# 交差検証法により選択された正則化パラメータに対応する回帰係数
glmnet(x, y, lambda=opt_lambda, family="cox")$beta
