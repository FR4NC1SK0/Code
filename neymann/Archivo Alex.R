

library(tidyverse)


source("00funciones_mod.R")

load("marco.RData")

View(marco)

marco <- marco[as.numeric(substring(marco$cnae_2009,1,2)) %in% 
                 seq(45,99,1)]

marco
marco$asal<-as.numeric(as.character(marco$total_asalariados))
marco<-marco%>%replace_na(list(asal=0))


marco$empleo <- cut((marco$asal),breaks=c(-1,2,9,149,999999))

neyman(marco$asal, marco$empleo, 
        exh=c(0,0,0,1), r=0.05)
