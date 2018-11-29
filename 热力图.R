###热力图(heatmap)
nba<- read.csv("http://datasets.flowingdata.com/ppg2008.csv", sep=",")
nba <- nba[order(nba$PTS),]
row.names(nba) <- nba$Name
nba <- nba[,2:20] #
nba_matrix <- data.matrix(nba)
heatmap(nba_matrix, Rowv=NA, Colv=NA, col=cm.colors(256), revC=FALSE, scale='column')
heatmap(nba_matrix, Rowv=NA, Colv=NA, col=heat.colors(256), revC=FALSE, scale="column")#换个颜色



