
##########################################################################################################################################################
# notas do servizo de contas sobre o script de selección de mostra para a enquisa de servizos socias:

## revisando o script orixinal vimos tres cousas que consideramos importantes comentar:

## 1.- Elimínanse do marco os rexistros que teñen en directorio total_asalariados=NA e non debería facerse, xa que son rexistros sen asalariados. Estes NA deben considerarse ceros.
## 2.- Codifícanse como "PF" os rexistros cuxo nif comeza coas letras E, F, J e U e deben ser codificados como "Empresa"
## 3.- No último paso, despois de seleccionar a mostra, deben seleccionarse as sustitutas, as cales tamén deben ser elixidas aleatoriamente.
##     No script actual as sustitutas non están ordenadas de modo aleatorio, cóllense tal cual están no dataframe marco (con esa orde que non é aleatoria)

## modificamos/comentamos estas liñas no propio script. Indicamos as liñas que modificamos

#############################################################################################################################################################



rm(list=ls())

#Librerias
# library(RODBC) 
# library(dplyr)
library(tidyverse)


#Otros ficheros con codigo r
source("00funciones_mod.R")

# Ahora voy a Environment y abro la carpeta marco.RData

# engadido : cargo o marco 
load("marco.RData")

marco <- marco[substring(marco$cnae,1,2) %in% 
                 c('87','88'),]


# Variables de estratificacion #
################################
# Empleo

marco$asal<-as.numeric(as.character(marco$total_asalariados))

# MARTA: substitúo os NA por ceros para que despois se asignen ao estrato adecuado

marco<-marco%>%replace_na(list(asal=0))


marco$empleo <- cut((marco$asal),breaks=c(-1,2,9,49,999999))



#marco <- filter(marco,marco$asal>0)  # MARTA: comento esta liña para non eliminar  rexistros que non tiñan asalariados

# Marta: cambio a codificación de tipodeentidad para definir como "Empresa" os rexistros que empezan por E (comunidades de bens), F(cooperativas), J (Soc. Civís) e U (UTEs)

# Tipo de entidad
#marco<-marco%>%mutate(tipoentidad=case_when((substring(marco$nif,1,1)=='A'|substring(marco$nif,1,1)=='B')~ "Empresa",
#                      (substring(marco$nif,1,1)=='P'|substring(marco$nif,1,1)=='Q'|substring(marco$nif,1,1)=='R'|substring(marco$nif,1,1)=='G'|substring(marco$nif,1,1)=='V') ~ "IPSFL",
#                      TRUE ~"PF"))



marco<-marco%>%mutate(tipoentidad=case_when((substring(marco$nif,1,1) %in% c(0:9,"X","Y","Z"))~ "PF",
                                            (substring(marco$nif,1,1)=='P'|substring(marco$nif,1,1)=='Q'|substring(marco$nif,1,1)=='R'|substring(marco$nif,1,1)=='G'|substring(marco$nif,1,1)=='V') ~ "IPSFL",
                                            TRUE ~"Empresa"))

marco$tipoentidad<-factor(marco$tipoentidad)
#marco$tipoentidad <- ordered(marco$tipoentidad,levels=c("Empresa", "IPSFL","PF"))

table(marco$tipoentidad,marco$empleo) # engadido, táboa cos tamaños poboacionais por estrato

# nota Marta: no estrato de PF de máis de 49 asalariados só hai unha unidade, isto daba problemas na función neyman() e tiven que facerlle unha pequena corrección no arquivo 00funciones.R. o script corrixido é 00funciones_mod.R

neyman2(marco$asal, list(marco$tipoentidad, marco$empleo), 
        exh=rbind(c(0,0,0,1),c(0,0,0,1),c(0,0,1,1)), r=c(0.03,0.05)) 
 
# Hacemos dos cambios en los resultados obtenidos por Neyman
# Asignamos un mínimo de 10 por estrato
# Resulta por Neyman en el estrato PF de (9,49] un tamaño muestral de 25 pero en el marco solo disponemos de 14, por lo tanto tenemos que cambiar 
# nota Marta: non entendo por qué no estrato de PF de 9 a 49, que se indica como autorrepresentado sae unha mostra superior ao total poboacional!!! Preguntar...

tam_mues <- rbind(c(10,10,16,58),c(10,10,24,55),c(20,21,14,1))
tam_mues <- data.frame(tam_mues,row.names=levels(marco$tipoentidad))
names(tam_mues) <- levels(marco$empleo)

# calculo de los Nh
#nrow(marco%>%filter(tipoentidad =='Empresa' & asal<3))
#nrow(marco%>%filter(tipoentidad =='Empresa' & (asal>=3&asal<10)))
#nrow(marco%>%filter(tipoentidad =='Empresa' & (asal>=10&asal<50)))
#nrow(marco%>%filter(tipoentidad =='Empresa' & asal>=50))
#nrow(marco%>%filter(tipoentidad =='IPSFL' & asal<3))
#nrow(marco%>%filter(tipoentidad =='IPSFL' & (asal>=3&asal<10)))
#nrow(marco%>%filter(tipoentidad =='IPSFL' & (asal>=10&asal<50)))
#nrow(marco%>%filter(tipoentidad =='IPSFL' & asal>=50))
#nrow(marco%>%filter(tipoentidad =='PF' & asal<3))
#nrow(marco%>%filter(tipoentidad =='PF' & (asal>=3&asal<10)))
#nrow(marco%>%filter(tipoentidad =='PF' & (asal>=10&asal<50)))
#nrow(marco%>%filter(tipoentidad =='PF' & asal>=50))

table(marco$tipoentidad,marco$empleo) # engadido: táboa cos tamaños poboacionais por estrato
tam_mues # engadido: tamaños mostrais







# vamos a sacar la muestra aleatoria

muestra <- c()
sustitutas <- c()
set.seed(369354128)###para desordenar las sustitutas  ## Marta: isto é para que non saian distintas unidades na mostra en distintas execucións do script (non para desordenar as sustitutas)

# engadido. nota: no estrato onde nh=1 (=Nh), a función sample non funciona, polo que o elimino e engado a posteriori ese rexistro:

tam_mues["PF","(49,1e+06]"]=0

for (i in levels(marco$tipoentidad)) {
  for (j in levels(marco$empleo)) {
   
    muestra <- c(muestra, sample(which(marco$tipoentidad == i & marco$empleo == j),tam_mues[i,j]))
  }
}

#sustitutas <- which(!1:nrow(marco) %in% muestra)  marta: comentado, as sustitutas aquí sairían coa mesma orde que estean no dataframe marco
#sustitutas=marco[row(marco)%in% sustitutas,]
#muestra=marco[row(marco)%in% muestra,]


muestra=marco[muestra,] # engadido. Este sería o dataframe que contén a mostra
# engado o rexistro autorrepresentado de PF, e máis de 49 asalariados:
# nota: no estrato onde nh=1 (=Nh), a función sample non funciona, polo que o elimino e engado a posteriori ese rexistro:
tam_mues["PF","(49,1e+06]"]=1
a<-filter(marco,tipoentidad=="PF",empleo=="(49,1e+06]")
muestra<-rbind(muestra,a)
rm(a)
dim(muestra)
table(muestra$tipoentidad,muestra$empleo)

## Marta: engadido. para seleccionar as sustitutas teño que facer o mesmo de antes co marco de sustitutas (o marco quitando as da mostra)


marco_sustitutas=marco%>%anti_join(muestra)
num_sustitutas<-table(marco$tipoentidad,marco$empleo)-tam_mues

set.seed(123456789) ## esto é para que non saian distintas sustitutas en distintas execucións do script 
for (i in levels(marco_sustitutas$tipoentidad)) {
  for (j in levels(marco_sustitutas$empleo)) {
    sustitutas <- c(sustitutas, sample(which(marco_sustitutas$tipoentidad == i & marco_sustitutas$empleo == j),num_sustitutas[i,j]))
  }
}
sustitutas=marco_sustitutas[sustitutas,]
sustitutas<-sustitutas%>%mutate(orden_sustitutas=1:nrow(sustitutas))

dim(sustitutas)


# engadido.comprobamos que a suma de sustitutas + mostra coincide co marco

total<-rbind(muestra,sustitutas[,-ncol(sustitutas)])
a1<-marco%>%anti_join(total) # todas as do marco están ou ben na mostra ou ben nas sustitutas
a2<-total%>%anti_join(marco) # todas as da mostra+ sustitutas están tamén no marco
b<-muestra%>%inner_join(sustitutas) # non hai ningunha unidade que estea á vez na mostra e nas sustitutas

# engadido: gardo o marco, a mostra e as sustitutas

save(marco,muestra,sustitutas,file="marco_mostra_sustitutas.RData")


write.table(muestra, "muestraservsoci.csv",sep='\t', dec=',', quote=F, row.names=F) # para exportar muestra
write.table(sustitutas, "sustitutasservsoci.csv",sep='\t', dec=',', quote=F, row.names=F) 
write.table(marco, "marcoservsoci.csv",sep='\t', dec=',', quote=F, row.names=F) 

#####################


