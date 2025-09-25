
datos = read.csv('https://www.ige.gal/igebdt/igeapi/csv/datosserc/IPI21XE/IPI21XE%26ZZS/IPI21XE%26ZZT',encoding='latin1')

datos = pivot_wider(datos,names_from = serie, values_from = dato)

View(datos)

library(plotly)

plot_ly(datos,x=~codtempo,
        y=~IPI21XE,type='scatter',line=list(color='peru',width=2), 
        mode='lines') %>% add_trace(y=~`IPI21XE&ZZS`,
        line=list(color='red',width=2))%>% 
        add_trace(y=~`IPI21XE&ZZT`,
        line=list(color='green',width=2))



datos$var1 = datos$IPI21XE/lag(datos$IPI21XE)-1
datos$var12 = datos$IPI21XE/lag(datos$IPI21XE,12)-1
View(datos)


