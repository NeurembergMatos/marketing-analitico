
library(FactoMineR)
library(factoextra)
library(ggplot2)


gerar_rate <- function(n){
  sample(seq(1, 7, .2), size = n, replace = TRUE)
}

add_arrow <- function(graph, empresa, coordenadas){
  
  xend = coordenadas[1]
  yend = coordenadas[2]
  xend_lab <- (abs(xend)/xend)*.1 + xend
  yend_lab <- (abs(yend)/yend)*.1 + yend
  cor <- '#EC7014'
  label <- empresa
  
  g <- graph +
    geom_segment(
      aes(x = 0, y = 0, xend = xend, yend = yend),
      arrow = arrow(length = unit(0.5, 'cm')),
      size = 0.9,
      colour = cor,  
    ) +
    geom_point(aes(x = xend, y = yend), color = cor) +
    annotate(
      geom = 'text', x = xend_lab, y = yend_lab, label = label, hjust = 'left', color = cor
    )
  return(g)
}


add_empresas <- function(gp, empresas, coordenadas){
  
  if (length(empresas) != length(coordenadas)) {
    stop("Os objetos `empresas` e `coordendas` possuem tamanhos diferentes!")
  }
  
  for (i in seq_along(along.with = coordenadas)) {
    gp <- add_arrow(graph = gp, empresa = empresas[[i]], coordenadas = coordenadas[[i]])
  }
  
  return(gp)
}

new_predict <- function(fit, newdata, col_empresa = 'empresas'){
  
  empresas <- newdata[[col_empresa]]
  coordenadas <- predict(fit, newdata = newdata)$coord
  coordenadas <- as.data.frame(coordenadas)
  
  repr <- split(coordenadas, empresas)
  
  
  return(
    lapply(repr, as.numeric)
  )
}
