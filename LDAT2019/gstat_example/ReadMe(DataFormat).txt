
時空間補間を行う計算コードとデータ
code.R  : コード

dat.csv : 観測地点のデータ
・year  : 年度(時点)
・px    : X座標
・py    : Y座標
・y     : 被説明変数(住宅地価(JPY/m2)の対数値)
・tachid: 説明変数1(立川駅までの距離:km)
・ekid  : 説明変数2(最寄駅までの距離:km)

mdat.csv: 予測地点のデータ
・year  : 年度(時点)
・px    : X座標
・py    : Y座標
・tachid: 説明変数1(立川駅までの距離:km)
・ekid  : 説明変数2(最寄駅までの距離:km)

フォーマット
・時間順(第1優先),場所順(第2優先)でデータを並べておく
・各時点の観測地点は同じでなくてはならない
・時点毎に観測地点が変化する場合は、一度でも観測値が出現する地点で
　観測地点を定義した上で観測値が存在しない時点にNA(欠損値)を入れる（dat.csv参照）
・被説明変数と説明変数の両方にNAがあってよい
