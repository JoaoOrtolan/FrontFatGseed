Roda_Grafico_IC <- function(Dados, Nome_Intervalo, fill_color_IC, 
                            Var_Referencia,Permanece_ref, Lim_Inferior, 
                            Lim_Superior, Nome_Variavel,
                            Variavel_Meio, Nome_eixo) {

if (Lim_Superior=="3x Desvio Padrão"){
  y_sup <- Dados[Dados[,Var_Referencia] %in% Permanece_ref,Variavel_Meio] + sd(Dados[Dados[,Var_Referencia] %in% Permanece_ref,Variavel_Meio])*3
} else {y_sup <- Dados[Dados[,Var_Referencia] %in% Permanece_ref,Lim_Superior]}
  
if (Lim_Inferior=="3x Desvio Padrão"){
  y_inf <- Dados[Dados[,Var_Referencia] %in% Permanece_ref,Variavel_Meio] - sd(Dados[Dados[,Var_Referencia] %in% Permanece_ref,Variavel_Meio])*3
} else {y_inf <- Dados[Dados[,Var_Referencia] %in% Permanece_ref,Lim_Inferior]}
  
trace1 <- list(
  fill = "tonexty", 
  mode = "none", 
  name = Nome_Intervalo, 
  type = "scatter", 
  x = as.factor(c(Dados[Dados[,Var_Referencia] %in% Permanece_ref,Var_Referencia], rev(Dados[Dados[,Var_Referencia] %in% Permanece_ref,Var_Referencia]))), 
  y = c(y_sup, rev(y_inf)), 
  fillcolor = fill_color_IC
)
trace2 <- list(
  mode = "lines", 
  name = Nome_Variavel, 
  type = "scatter", 
  x = as.factor(c(Dados[Dados[,Var_Referencia] %in% Permanece_ref,Var_Referencia])), 
  y = c(Dados[Dados[,Var_Referencia] %in% Permanece_ref,Variavel_Meio]), 
  marker = list(color = "white")
)
layout <- list(
  yaxis1 = list(
    range = c(min(trace2$y), max(trace2$y)), 
    title = Nome_eixo
  )
)
p <- plot_ly()
p <- add_trace(p, fill=trace1$fill, mode=trace1$mode, name=trace1$name, type=trace1$type, x=trace1$x, y=trace1$y, fillcolor=trace1$fillcolor)
p <- add_trace(p, fill=trace2$fill, mode=trace2$mode, name=trace2$name, type=trace2$type, x=trace2$x, y=trace2$y, fillcolor=trace2$fillcolor)
p <- layout(p, yaxis1=layout$yaxis1)
return(p)
}
