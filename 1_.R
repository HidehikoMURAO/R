### Lasso
library(glmnet)

crime <- read.table("crime.txt")  # 犯罪データ読み込み
crime <- as.matrix(crime)
X <- crime[, 3:7]  # 説明変数
y <- crime[, 1]    # 目的変数
X <- scale(X)      # 説明変数を標準化
y <- y - mean(y)   # 目的変数を中心化

# Lasso推定
res <- glmnet(x=X, y=y)
# 解パス図描画
plot(res, xvar="lambda", label=TRUE, xlab="正則化パラメータの対数値", 
     ylab="回帰係数", col="black", lwd=2.5)
# 正則化パラメータの値を20と固定
res1 <- glmnet(x=X, y=y, lambda=20)  
res1$beta  # 係数の推定値
 
# CVの計算
res.cv <- cv.glmnet(x=X, y=y)
# CV値の推移をプロット
plot(res.cv, xlab="正則化パラメータの対数値", ylab="２乗誤差")
# CV値が最小となる正則化パラメータ値を出力
res.cv$lambda.min
# 1標準誤差ルールにより選択された正則化パラメータの値を出力
res.cv$lambda.1se
