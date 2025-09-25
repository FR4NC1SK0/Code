
#Limpamos todo
rm(list=ls())
##setwd("R:/Servicios/Empresas/vacantes/muestra/2022")
#Outros ficheiros con codigo r
source("00funciones.R")


#######################
# Seleccion do marco #
#######################


load("marco.RData")


#Eliminamos as empresas sen dato de asalariados.
marco=subset(marco,total_asalariados %in% c(1:99999 ))

#Seleccionar actividades do marco (cnae)
# Descartamos as seguintes cnaes 2009 (Seccions: Divisions)
#A. Agricultura, Ganaderia y Pesca: 01,02,03
#T. Actividades de los hogares...:97,98
#U. Actividades de organizaciones y organismos extraterritoriales: 99

marco <- marco[!substring(marco$cnae,1,2) %in% 
                 c('01','02','03','97','98','99'),]
#Eliminamos B.Industrias extractivas: 05,06,07,08,09
marco <- marco[!substring(marco$cnae,1,2) %in% 
                 c('05','06','07','08','09'),]
#Eliminamos C.Industria manufacturera: 10 a 33
i<-c(10:33)
i<-as.character(i)

marco <- marco[!substring(marco$cnae,1,2) %in% 
                 i,]

#Eliminamos D.Suministro de energia electrica, gas, vapor y aire acondicionado:35

marco <- marco[!substring(marco$cnae,1,2) %in% 
                 c("35"),]
#Eliminamos E.Suministro de agua, actividades de saneamiento, gestion de residuos y descontaminacion: 36 a39
i<-c(36:39)
i<-as.character(i)

marco <- marco[!substring(marco$cnae,1,2) %in% 
                 
                 i,]
#Eliminamos F.Construccion: 41 a 43
i<-c(41:43)
i<-as.character(i)

marco <- marco[!substring(marco$cnae,1,2) %in% 
                 i,]
#Eliminamos O-Administracions publicas:84
marco <- marco[!substring(marco$cnae,1,2) %in% 
                 c("84"),]

#Eliminamos P-Educacion:85
marco <- marco[!substring(marco$cnae,1,2) %in% 
                 c("85"),]
#Eliminamos Q.Sanidade e servizos sociais:86,87 e 88
i<-c(86:88)
i<-as.character(i)

marco <- marco[!substring(marco$cnae,1,2) %in% 
                 i,]


#Gardamos como csv o noso marco

write.csv2(marco, "marco_servizos.csv")
################################
# Variables de estratificacion #
################################

# Emprego 
#Convertimos a variable asal en numerica
marco$total_asalariados<-as.numeric(as.character(marco$total_asalariados))

#Convertimos en factor cos niveis que queremos

marco$empleo <- cut((marco$total_asalariados),breaks=c(-1,9,49,99,499,999999))

#############################################
# Calculo do tamano da mostra e da afixacion#
#############################################

#Cun erro relativo fixo para o global e estratos segundo o numero de traballadores, ultimo exhaustivo
neyman(marco$total_asalariados, str=marco$empleo, exh=c(0,0,0,0,1),r=0.1)

##obtenemos el siguiente resultado:
##(-1,9]      (9,49]     (49,99]    (99,499] (499,1e+06] 
##   47         22          3          13          62

## con afijacion minima de 20 serian : 47+22+20+20+62=171 empresas 
