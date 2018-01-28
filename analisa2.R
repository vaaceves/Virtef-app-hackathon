setwd("C:\\Users\\Arturo\\Google Drive\\github_local\\hack")
library(rjson)
getwd()


print(list.files())
x <- fromJSON(file="data\\user1.json")

transacciones <- x$transactions

montos_compra <- sapply(transacciones, function(x){
	c(x$amount,x$date)
}, simplify = T)

montos_compra <- as.data.frame(t(montos_compra))
colnames(montos_compra) <- c("monto", "fecha")
montos_compra$monto <- as.numeric(as.character(montos_compra$monto))
montos_compra$fecha <- as.Date(as.character(montos_compra$fecha), format = "%Y-%m-%d")
montos_compra$dia_semana <- weekdays(montos_compra$fecha)
montos_compra <- montos_compra[montos_compra$monto <0,]
montos_compra <- montos_compra[montos_compra$monto <0,]
montos_compra$monto <- -montos_compra$monto

library(plotly)
library(dplyr)

p_compras_x_dias <- montos_compra %>%
  group_by(dia_semana) %>%
  summarize(count = n()) %>%
  plot_ly(labels = ~paste("0", dia_semana), values = ~count) %>%
  add_pie(hole = 0.6) %>%
  layout(title = "Donut charts using Plotly",  showlegend = F,
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

print(p_compras_x_dias)
guarda_grafica_plotly(p = p_compras_x_dias, path.output = getwd(), file.carpeta = "graficas", "p_compras_x_dias")

dias_gastones <- sort(tapply(montos_compra$monto, montos_compra$dia_semana, sum), decreasing = T)
names(dias_gastones) <- paste(seq(1:length(dias_gastones)), names(dias_gastones) )

p_dias_gastalones <- plot_ly(
  x = names(dias_gastones),
  y = dias_gastones,
  name = "SF Zoo",
  type = "bar",
  mean = "Gastos en la semana",
  marker = list(color = c('rgba(222,45,38,0.8)', rep('rgba(204,204,204,1)', length(dias_gastones) - 1) 
                          )),
) %>%
layout(title = "Montos de gasto por día de la semana en el mes",
    xaxis = list(title ="Día de la semana"),
    yaxis = list(title ="¡Monto de gasto!"))
print(p_dias_gastalones)


get_business <- sapply(transacciones, function(x){
	c(x$amount,x$business$name)
}, simplify = T)

get_business <- as.data.frame(t(get_business))
colnames(get_business) <- c("monto", "negocio")
get_business$monto <- as.numeric(as.character(get_business$monto))
get_business$negocio <- as.character(get_business$negocio)
get_business <- get_business[get_business$monto <0,]
get_business$monto <- -get_business$monto
get_business_wo_empty <- get_business[get_business$negocio != "",]
get_business_wo_empty <- get_business_wo_empty[order(as.numeric(as.character(get_business_wo_empty$monto)), decreasing = T),]
get_business_wo_empty$negocio <- paste(c(1:nrow(get_business_wo_empty)), get_business_wo_empty$negocio)


p_negocio_gastos <- plot_ly(
  x = get_business_wo_empty$negocio,
  y = get_business_wo_empty$monto,
  name = "SF Zoo",
  type = "bar",
  marker = list(color = c('rgba(222,45,38,0.8)', rep('rgba(204,204,204,1)', length(dias_gastones) - 1) 
                          )),
) %>%
layout(title = "Gastos en negocios",
    xaxis = list(title ="Negocio"),
    yaxis = list(title ="¡Monto de gasto!"))
print(p_negocio_gastos)


