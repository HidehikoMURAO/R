### CGHデータ解析
library(genlasso)
y <- read.table("cgh.txt")
y <- unlist(y)            # 目的変数

# FLSA適用
res <- fusedlasso1d(y=y)
# FLSAによる当てはめ結果
plot(res, lambda=3, pch=20, col="gray", 
     xlab="遺伝子番号", ylab="コピー数の比の対数値",
     cex.lab=1.5, cex.axis=1.5)

### Fused Lasso simulation
library(lqa)
set.seed(3)
n <- 50           # サンプルサイズ
p <- 10           # パラメータ数
beta <- rep(0,p)  # 回帰係数
beta[1:2] <- 2
beta[3:5] <- 4

X <- matrix(rnorm(n*p), n, p)       # 計画行列生成
y <- X %*% beta + rnorm(n, 0, 0.5)  # 目的変数生成

# 連結Lasso
res = lqa(as.vector(y)~X, family=gaussian(), penalty=fused.lasso(c(1,1)))

# lambda2を固定した下での解パス図
lambdarange <- exp(seq(-1, 4, length=60))  # lambdaの探索範囲
res1 <- plot.lqa (X, as.vector(y), family=gaussian(), 
                 penalty.family=fused.lasso, 
                 lambdaseq=lambdarange,
                 offset.values=c(NA, 0.2), ret.true = T)
plot(0, 0, type="n",
     xlim=range(log(lambdarange)), ylim=range(res1$beta.mat),
     xlab="正則化パラメータの対数値", ylab="回帰係数")
for(i in 1:10){
  lines(log(lambdarange), res1$beta.mat[, i])
  text(-1.1, res1$beta.mat[1,i], i, cex=0.5)
}

# lambda1を固定した下での解パス図
res2 = plot.lqa(X, as.vector(y), family=gaussian(), 
                penalty.family=fused.lasso, 
                lambdaseq=lambdarange,
                offset.values=c(5, NA), ret.true = T)
plot(0, 0, type="n",
     xlim=range(log(lambdarange)), ylim=range(res2$beta.mat),
     xlab="正則化パラメータの対数値", ylab="回帰係数")
for(i in 1:10){
  lines(log(lambdarange), res2$beta.mat[, i])
  text(-1.1, res2$beta.mat[1,i], i, cex=0.5)
}


### 一般化lasso
library(flsa)
library(jpeg)
# 画像データの読み込み
im <- readJPEG("sparse.jpg")
im <- t(apply(im, 2, rev))
# ノイズが混入したデータを生成
set.seed(0)
m <- nrow(im)
n <- ncol(im)
im.noise <- im + runif(m*n, max=0.3)

# 一般化lassoの当てはめ
fit <- flsa(im.noise)
result <- flsaGetSolution(fit, lambda2=0.1)
denoise1 <- matrix(result,m,n)

# プロットの準備
rangez <- range(im)
rangez.noise <- range(im.noise)
rangez.denoise <- range(denoise1)
n.col <- 256
col <- rgb(red=(0:n.col)/n.col, green=(0:n.col)/n.col,
           blue=(0:n.col)/n.col, names=paste("black", 0:n.col, sep = "."))

# プロット（順に元画像、ノイズ混入画像、復元画像）
image(im, col=col, zlim=rangez,
      xlab="", ylab="", bty="n", xaxt="n", yaxt="n")
image(im.noise, col=col, zlim=rangez.noise,
      xlab="", ylab="", bty="n", xaxt="n", yaxt="n")
image(denoise1, col=col, zlim=rangez.denoise,
      xlab="", ylab="", bty="n", xaxt="n", yaxt="n")


##### グループLassoの適用
library(grpreg)
data(Birthwt)     # データセット

X <- Birthwt$X    # 説明変数
y <- Birthwt$bwt  # 目的変数
group <- Birthwt$group   # グループ変数のインデックス

# グループlassoの実行
res <- grpreg(X, y, group, penalty="grLasso")

# 解パス図の作成
par(mar=c(5, 5, 1, 1))
plot(0, 0, xlim=c(-0.01,0.15), ylim=range(res$beta[-1,]), 
     type="n", xlab="正則化パラメータの値", ylab="回帰係数")
for(i in c(1:3, 9, 14:16)){
  lines(res$lambda, res$beta[i+1, ], lty=as.numeric(group)[i],
        lwd=2)
  text(-0.01, res$beta[i+1,100], i, cex=1)
}
# 解パス図は次のプログラムでも作成可能
 plot(res, lty=as.numeric(group), col=rep(1,ncol(X)), 
      xlim=c(0, 0.15), xlab="正則化パラメータの値", ylab="回帰係数")
res$beta[,10] # 回帰係数の推定値


### 階層的Lassoの適用
library(hierNet)
set.seed(0)
crime <- read.table("crime.txt")  # 犯罪データ読み込み
crime <- as.matrix(crime)
x <- crime[, 3:7]              # 説明変数
y <- as.numeric(crime[, 1])    # 目的変数
x <- scale(x)      # 説明変数を標準化
y <- y - mean(y)   # 目的変数を中心化

# 正則化パラメータを固定して強階層Lasso推定
res <- hierNet(x, y, strong=TRUE, diagonal=FALSE, lam=1000)
res # 推定値出力
# 10分割交差検証法による選択
res <- hierNet.path(x, y, strong=TRUE, diagonal=FALSE)
rescv <- hierNet.cv(res, x, y)
rescv$lamhat   # CV値が最小となる正則化パラメータ値を選択
