#######################################################
##Helper Functions 
#######################################################

#plot function of predicted probability of two class(true)
probTwoClassPlot = function(pred, obs, 
                            xlabT = "Probability of X1",
                            titleT = "Model 0"){
  library("ggplot2")
  df = data.frame(prob = pred, obs = obs)
  ggplot(df, aes(x = prob, colour = obs, fill = obs)) +
    geom_density(alpha = 0.55) +
    xlab(xlabT) +
    ggtitle(titleT)
}

#return feature pairs and their corrleations which has correlation > val 
highCor = function(mat, val){
  high_cor = which( mat > val, arr.ind = TRUE)
  high_cor = high_cor[ high_cor[,1] != high_cor[,2],]
  for(i in 1:nrow(high_cor)){
    high_cor[i, ] = sort(high_cor[i, ]) 
  }
  high_cor = high_cor[!duplicated(high_cor),]
  names = colnames(mat)
  df = cbind(high_cor, names[high_cor[,1]], names[high_cor[,2]], mat[high_cor])
  df = df[ order(df[, 5], decreasing = TRUE),]
  return(df)
}
