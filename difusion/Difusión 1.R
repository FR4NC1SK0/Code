
View(microdatos_egresados_2020_2021_practica)

library(tidyr)
library(dplyr)
tit_code = c(2501016,2501108,2501119,4310386,4311753,5600486,5600489)

titulacions = microdatos_egresados_2020_2021_practica %>%
  filter(cod_titulacion %in% tit_code ) %>% 
  filter(periodo_estudo %in% c('periodo_t1_06','periodo_t3_06'))


titulacions

titulacions <- titulacions %>% group_by(periodo_estudo)

titulacions <- titulacions %>% mutate(act_code=case_when(
   Rel_Actividade == 'Traballando'~1,
   Rel_Actividade == 'Parado_SenTraballoAnterior'~2,
   Rel_Actividade == 'Parado_ConTraballoAnterior'~3,
   Rel_Actividade == 'Outro'~4,   
))
titulacions
library(plotly)
per_t1 = count(titulacions, Rel_Actividade) %>% filter(periodo_estudo == 'periodo_t1_06')
per_t3 = count(titulacions, Rel_Actividade) %>% filter(periodo_estudo == 'periodo_t3_06')
per_t1
plot_ly(per_t1, type='pie', labels=~Rel_Actividade,values=~n)
plot_ly(per_t3, type='pie', labels=~Rel_Actividade,values=~n)
plot_ly


titulacions <- titulacions %>% mutate(acad=case_when(
  cod_nivel_acad == 1~'Doc',
  cod_nivel_acad == 2~'Msc',
  cod_nivel_acad == 3~'Grad'))



titulacions
acad_D = titulacions %>% 
  filter(cod_nivel_acad == 1) %>%
  count(periodo_estudo)
acad_M = titulacions %>% 
  filter(cod_nivel_acad == 2) %>%
  count(periodo_estudo)
acad_G = titulacions %>% 
  group_by(periodo_estudo) %>%
  filter(cod_nivel_acad == 3) %>%
  count(periodo_estudo)

acad_G
acad_D
acad_M

acad_D_trab = titulacions %>% 
  filter(cod_nivel_acad == 1,Rel_Actividade=='Traballando') %>%
  group_by(periodo_estudo) %>% 
  summarise(ocupados=n()) %>% 
  mutate(nivel=1)
acad_M_trab = titulacions %>% 
  filter(cod_nivel_acad == 2,Rel_Actividade=='Traballando') %>%
  count(periodo_estudo)
acad_G_trab = titulacions %>% 
  group_by(periodo_estudo) %>%
  filter(cod_nivel_acad == 3,Rel_Actividade=='Traballando') %>%
  count(periodo_estudo)

acad_D_trab
acad_M_trab
acad_G_trab

df <- tibble(acad_D$n,acad_M_trab$n,acad_G_trab$n,acad_D$n,acad_M$n,acad_G$n)
df

acad_trab = titulacions %>% 
  filter(Rel_Actividade=='Traballando') %>%
  group_by(periodo_estudo,cod_nivel_acad) %>% 
  summarise(ocupados=n())

acad = titulacions %>% 
  group_by(periodo_estudo,cod_nivel_acad) %>% 
  summarise(total=n())


total = left_join(acad, acad_trab)

total$tasas = total$ocupados/total$total
total$tasas
plot_ly(total,values=~tasas,type='pie')

barplot(total$tasas)


library(tidyverse)

datos<-read.csv('https://www.ige.gal/igebdt/igeapi/csv/datos/11077/0:2021,1:2:5,2:0',encoding='latin1')




traballando <- titulacions %>% filter(Rel_Actividade=="Traballando") %>% 
  group_by(periodo_estudo)%>% filter(cod_gcot_agregado %in% c(3,4,5))

traballando_espec <- titulacions %>% group_by(periodo_estudo) %>% 
  filter(cod_gcot_agregado %in%c(1,2))
groups(traballando_espec)

datos_final <- traballando_espec %>% summarise(especializado=n())
datos_final <- datos_final %>% mutate(traballando %>% summarise(no_especializado=n()))
datos_final
datos_final$total <- datos_final$especializado+datos_final$no_especializado

tasas <- datos_final %>% transmute(tasaesp=datos_final$especializado/datos_final$total,tasanoesp=datos_final$no_especializado/datos_final$total)

tasas
datos_final

datos_final <- mutate(datos_final,tasas)


t(datos_final)


titulacions
View(titulacions)


base_cot <- titulacions %>% filter(is.na(base_cotizacion_corrixida)==F)
base_cot

base_cot <- base_cot %>% 
  group_by(periodo_estudo,cod_nivel_acad) %>% 
  summarise(base_media=mean(base_cotizacion_corrixida))

base_cot
