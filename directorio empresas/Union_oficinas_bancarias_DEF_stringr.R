### UNION DE OFICINAS BANCARIAS DO BANCO DE ESPAÑA CO DIRECTORIO DE UNIDADES LOCAIS DE GALICIA  ####

## Obxectivo: determinar as altas e baixas de oficinas bancarias en Galicia no 2022 a partir
## dos ficheiros de xaneiro de 2021 e xaneiro de 2022 do Banco de España e cruzar as baixas 
## co DIRGA para identificar os locais que se deben dar de baixa no directorio de unidades locais


## Indicamos o directorio de traballo definindo unha variable "directorio":

directorio <- "R:/Servicios/Master/Alejandro Romero Rivas/Directrorio empresas"

setwd(directorio)

# Podemos comprobar con getwd()

getwd()


## INSTALACIÓN E CARGA DOS PAQUETES QUE IMOS UTILIZAR

# "PACMAN" é un paquete de R que permite instalar e cargar paquetes:

if ("pacman" %in% rownames(installed.packages()) == FALSE) {
  install.packages("pacman")
}

pacman::p_load(readxl, tidyverse, writexl, fuzzyjoin)


## LECTURA DE DATOS

# Lemos as follas excel coas oficinas bancarias extraídas da páxina do Banco de España

bancos_2021 <-
  read_excel(path = "EstablecBancos_BcoEspaña.xlsx", sheet = "2021")
bancos_2022 <-
  read_excel(path = "EstablecBancos_BcoEspaña.xlsx", sheet = "2022")

# Lemos o libro excel que contén os locais no DIRGA con CNAE "6419"

bancos_DIRGA <-
  read_excel(path = "DIRGA_Ent_Financieras_Locais.xlsx")


# CRUCE DE DATOS DO BANCO DE ESPAÑA

# Unimos os dous data frame que conteñen as oficinas bancarias do BE, dos anos 2021 e 2022

bancos_union <-
  bancos_2021 %>% full_join(bancos_2022,
                            by = c("NIF_Entidad", "Cod_Prov", "Cod_Mun", "Domicilio"))

# Sae un aviso de que a unión de rexistros non é unívoca. Agrupamos para ver se hai duplicados
# nas combinación NIF_Entidad, Cod_Prov, Cod_Mun e Domicilio (en 2021 e 2022)

bancos_2021_dupli <-
  bancos_2021 %>% group_by (NIF_Entidad, Cod_Prov, Cod_Mun, Domicilio) %>% 
  filter(n() > 1) %>% ungroup()
bancos_2022_dupli <-
  bancos_2022 %>% group_by (NIF_Entidad, Cod_Prov, Cod_Mun, Domicilio) %>% 
  filter(n() > 1) %>% ungroup()
                                                                                    
duplicados_union <- bancos_union %>% 
  group_by (NIF_Entidad, Cod_Prov, Cod_Mun, Domicilio) %>% 
  filter(n() > 1) %>% ungroup()
                        
# A PARTIR DA UNIÓN DAS OFICINAS BANCARIAS DO BANCO DE ESPAÑA OBTEMOS AS ALTAS E AS BAIXAS
# As altas serán os locais que figuran en 2022 e non en 2021 (ID.x=NA)
# As baixas serán os locais que figuran en 2021 e non en 2022 (ID.Y=NA)

bancos_altas <- bancos_union %>% filter(is.na(ID.x))
bancos_bajas <- bancos_union %>% filter(is.na(ID.y))

# Para unha maior claridade eliminamos de bancos_bajas as columnas que rematan en .y (todos son NA)

bancos_bajas <- select(bancos_bajas,-ends_with(".y"))


# PREPARACIÓN DE TÁBOAS PARA O CRUCE DAS BAIXAS CO DIRECTORIO DE UNIDADES LOCAIS

# Agora queremos cruzar as baixas obtidas a partir dos ficheiros do Banco de España cos locais
# do directorio de empresas, para identificar os locais que debemos eliminar no DIRGA

# O cruce terá que ser por nif, provincia, concello e domicilio

# Antes de cruzar imos facer unha serie de operacións:
# 1) Na táboa de baixas, o código de concello non está en formato texto, está en
#    formato número e ademais debemos engadir ceros ata completar os 3 díxitos

# 2) Separamos o tipo de vía do resto, e convertimos o texto do domicilio a minúsculas

# 3) Creamos o campo via2 a partir de via, eliminando:
#     - en primeiro lugar unha serie de abreviaturas que gardamos nun patrón
#     - todos os caracteres que non sexan unha letra, é dicir,
#       os caracteres que cumplan a expresión regular "[^[:alpha:]]"
#
# Para as operacións de manipulación de texto utilizamos varias funcións do paquete stringr
# que forma parte de Tidyverse


patron <-
  "av\\.|avda\\.|c\\.|ca\\.|cl\\.|cr\\.|ctra\\.|pg\\.|s/n|sn\\.|s-n|rua"

bancos_bajas <- bancos_bajas %>% 
  mutate(Cod_Mun = str_pad(Cod_Mun, width = 3, pad = "0")) %>%
  mutate(tipo_via = str_sub(Domicilio, 1, 2),
              via = str_to_lower(str_sub(Domicilio, 4))) %>%
  mutate(via2 = str_replace_all(via, paste(patron, collapse = "|"), "")) %>%
  mutate(via2 = str_remove_all(via2, "[^[:alpha:]]"))


# 4) Temos que extraer o número de via en bancos_bajas, axudámonos da función str_extract
#    - Utilizamos a expresión regular que atopa unha secuencia de díxitos ("\\d+")
#    - Convertemos o campo a numérico
#    - Asignamos o valor 0 aos Nvia que son NA

bancos_bajas <- bancos_bajas %>%
  mutate(Nvia = str_extract(via, "\\d+")) %>%
  mutate(Nvia = as.numeric(Nvia)) %>%
  mutate(Nvia = if_else(is.na(Nvia), 0, Nvia))


# Facemos algo parecido en bancos_DIRGA, creando un campo via2:
# 1) Subtituímos por "" o que figura entre paréntesis, coa expresión regular: "\\(.*?\\)"
# 2) Eliminamos todos os caracteres que non sexan unha letra, expresión regular "[^[:alpha:]]"
# 3) Pasamos o texto a minúsculas
# 4) Asignamos o valor 0 aos nvia que son NA

bancos_DIRGA <- bancos_DIRGA %>%
  mutate(via2 = str_replace_all(via, "\\(.*?\\)", "")) %>%
  mutate(via2 = str_remove_all(via2, "[^[:alpha:]]")) %>%
  mutate(via2=  str_to_lower(via2)) %>%
  mutate(nvia = if_else(is.na(nvia), 0, nvia))


# Creamos en bancos_bajas unha columna enlace a partir do NIF, provincia, concello e vía

bancos_bajas <-
  bancos_bajas %>% mutate(enlace = paste(NIF_Entidad, Cod_Prov, Cod_Mun, via2, sep = "_"))


# Facemos o mesmo en bancos_DIRGA

bancos_DIRGA <-
  bancos_DIRGA %>% mutate(enlace = paste(nif, provincia, codigo_concello, via2, sep = "_"))

# FACEMOS A UNIóN DIFUSA DAS TÁBOAS COA FUNCIÓN stringdist_join DO PAQUETE fuzzyjoin
# Utilizamos o método "lv" (distancia de Levenshtein), e un máx de distancia de 4


bajas_DIRGA <- stringdist_join(
  bancos_bajas,
  bancos_DIRGA,
  by = c("enlace"),
  method = "lv",
  max_dist = 4,
  distance_col = "distancia"
)


#### OUTRA FORMA DE FACELO SEN UTILIZAR O CAMPO ENLACE, FACENDO A UNIÓN DIFUSA ENLAZANDO POLOS CAMPOS QUE NOS INTERESAN
#### OLLO, OBTÉÑENSE RESULTADOS DISTINTOS QUE ENLAZANDO POLO CAMPO ENLACE

bajas_DIRGA_sinenlace <- stringdist_join(
  bancos_bajas,
  bancos_DIRGA,
  by = c("NIF_Entidad"="nif","Cod_Prov"="provincia","Cod_Mun"="codigo_concello","via2"="via2"),
  method = "jaccard",
  max_dist = 4,
  distance_col = "distancia"
)

# COMPARACION ENTRE AS DUAS FORMAS (CON CAMPO ENLACE E SEN CAMPO ENLACE)

nrow(bajas_DIRGA)
nrow(bajas_DIRGA_sinenlace)

# IDs que aparecen solo nun dos métodos
bajas_solo_con_enlace <- anti_join(bajas_DIRGA, bajas_DIRGA_sinenlace, by = "ID.x")
bajas_solo_sin_enlace <- anti_join(bajas_DIRGA_sinenlace, bajas_DIRGA, by = "ID.x")


# Filtramos as filas onde coinciden nif, provincia, código de concello e nas que
# nvia non difira en máis de 2 unidades en ambas táboas (establecemos a 
# diferenza en dúas unidades porque ás veces un mesmo local ocupa dous números
# consecutivos na mesma rúa)
# Eses serían os rexistros que conseguimos unir

bajas_DIRGA <-
  bajas_DIRGA %>% filter(NIF_Entidad == nif,
                         Cod_Prov == provincia,
                         Cod_Mun == codigo_concello,
                         abs(Nvia - nvia) < 3)


bajas_DIRGA_sinenlace <-
  bajas_DIRGA_sinenlace %>% filter(NIF_Entidad == nif,
                         Cod_Prov == provincia,
                         Cod_Mun == codigo_concello,
                         abs(Nvia - nvia) < 3)



# Interésanos saber tamén que rexistros non se uniron (os que se deberían comprobar a man)

bajas_no_enlazan <-
  left_join(bancos_bajas, bajas_DIRGA, by = c("ID.x"))

bajas_no_enlazan <-
  bajas_no_enlazan %>% filter(is.na(nif_establecimiento))


# Selecciono as columnas que non son todo NA (dende ID.x ata enlace)

bajas_no_enlazan <- bajas_no_enlazan %>% select (ID.x:enlace)


# Estudamos se hai duplicados na unión de rexistros 

duplicados <-
  bajas_DIRGA %>% group_by(ID.x) %>% filter(n() > 1) %>% ungroup()

bajas_DIRGA <-
  bajas_DIRGA %>% group_by(ID.x) %>% filter(n() == 1) %>% ungroup()


# Solucionamos os duplicados facendo coincidir neses rexistros o nvia

bajas_DIRGA <- rbind(bajas_DIRGA, filter(duplicados, Nvia == nvia))


# Gardamos as táboas nun ficheiro Excel

write_xlsx(bajas_DIRGA, "Bajas_DIRGA_lv.xlsx")
write_xlsx(bajas_no_enlazan, "Bajas_no_enlazan_lv.xlsx")


# NORMALIZACION DO FICHEIRO DE ALTAS DE OFICINAS BANCARIAS PARA INCORPORAR AO DIRGA

bancos_altas <- select(bancos_altas,-ends_with(".x"))

patron <- str_to_upper(patron)

bancos_altas <- bancos_altas %>%
  mutate(Cod_Mun = str_pad(Cod_Mun, width = 3, pad = "0")) %>%
  mutate(tipo_via = str_sub(Domicilio, 1, 2),
         via = str_to_upper(str_sub(Domicilio, 4))) %>%
  mutate(via = str_replace_all(via, paste(patron, collapse = "|"), ""))


bancos_altas <- bancos_altas %>%
  mutate(nvia = str_extract(via, "\\d+")) %>%
  mutate(nvia = as.numeric(nvia)) 


bancos_altas <- bancos_altas %>%
  mutate(via = str_remove_all(str_trim(via), "[^[:alpha:]\\s]"))


bancos_altas <-
  bancos_altas %>% 
  select(NIF_Entidad, Cod_Prov, Cod_Mun, tipo_via, via, nvia, CP = CP.y) %>%
  mutate(Cod_Prov = as.character(Cod_Prov),
         CP = as.character(CP),
         cnae = "6419")


# Gardamos a táboa de altas nun ficheiro Excel

write_xlsx(bancos_altas, "Bancos_altas.xlsx")

