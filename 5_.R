dat.deca <- read.csv("decathlon.csv", header = T, row.names = 1)  # データの読み込み
cordat <- cor(dat.deca)

# グラフィカルlasso を当てはめる
library(glasso)  # グラフィカルlasso を実行するのに必要なパッケージの読み込み
library(igraph)  # グラフを描くのに必要なパッケージの読み込み
fit.glasso <- glasso(cordat, rho = 0.4)  # λ = 0.4 のときの結果
fit.glasso$wi  # 共分散逆行列の出力

# 隣接行列を作成する関数
makeadjmat <- function(mat, cordat) {
	ans <- mat
	diag(ans) <- 0
	ans <- ans != 0
	ans <- matrix(as.numeric(ans), nrow(ans), nrow(ans))
	colnames(ans) <- rownames(ans) <- colnames(cordat)
	ans
}
adjmat <- makeadjmat(fit.glasso$wi, cordat)  # 隣接行列を作成
g <- graph.adjacency(adjmat, mode = "undirected")  # グラフを描くためのオブジェクトの作成
plot(g, vertex.size = 40, vertex.shape = "rectangle", vertex.color = "#FFFF99")

# SPCA
library(elasticnet)  # elasticnet パッケージの読み込み
fit.spca.1 <- spca(cordat, K = 3, para = rep(0.1, 10), type = "Gram", lambda = 1e-06)  # K=3, lambda=1e-6, lambdaj=0.1
fit.spca.2 <- spca(cordat, K = 3, para = rep(1, 10), type = "Gram", lambda = 1e-06)  # K=3, lambda=1e-6, lambdaj=1
fit.spca.3 <- spca(cordat, K = 3, para = rep(1, 10), type = "Gram", lambda = 1)  # K=3, lambda=1, lambdaj=1
fit.spca.1
fit.spca.2
fit.spca.3  # 結果の出力
t(fit.spca.1$loadings) %*% fit.spca.1$loadings  # Z が直交しているかチェック

# スパース因子分析
library(fanc)  # fanc パッケージの読み込み
fit.fanc <- fanc(as.matrix(dat.deca), factors = 3)  # 因子数を3 に設定
plot(fit.fanc)  # plot する．Windows ユーザーはR3.4 以降が必要
plot(fit.fanc, type = "heatmap")  # heatmap でもplot できる．
out(fit.fanc, rho = 0.04, gamma = 14)  # rho=0.04, gamma=14 のときの結果を出力
select(fit.fanc, criterion = "BIC")  # BIC によって調整パラメータを選択
