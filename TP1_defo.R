# Modelo definitivo

require("data.table")
require("rpart")
require("rpart.plot")
require("ggplot2")
require("dplyr")
getwd()

semillas <- c(888809, 888827, 888857, 888869, 888887)
dataset <- fread("./datasets/competencia1_2022.csv")

#################################################################################
#################################################################################

### Limpieza del dataset

dataset2 <- dataset
dataset2[is.na(dataset2), ] <- 0

print(sum(is.na(dataset2$Visa_fechaalta)))

dataset <- dataset2


#################################################################################
#################################################################################

#  Clases BAJAS+1 y BAJA+2 combinadas

dataset[, clase_binaria := ifelse(
  clase_ternaria == "CONTINUA",
  "noevento",
  "evento")]


#  Sólo es evento las clase BAJA+2

# dataset[, clase_binaria := ifelse(
#    clase_ternaria == "BAJA+2",
#    "evento",
#    "noevento"
#  )]



dataset[, clase_ternaria := NULL]

#################################################################################
#################################################################################

# Rankeo de variables clave


# variables para bintiar
mis_variables <- c("numero_de_cliente",
                   "ctrx_quarter",
                   "mprestamos_personales",
                   "mtarjeta_visa_consumo",
                   "mtarjeta_master_consumo",
                   "mactivos_margen",
                   "mpasivos_margen",
                   "mcaja_ahorro_dolares",
                   "mcaja_ahorro",
                   "mautoservicio",
                   "mcuenta_corriente",
                   "cliente_antiguedad",
                   "cliente_edad",
                   "mrentabilidad",
                   "mrentabilidad_annual",
                   "mcomisiones",
                   "mcuenta_corriente_adicional",
                   "mcaja_ahorro_adicional",
                   "mcuentas_saldo",
                   "mprestamos_hipotecarios",
                   "mprestamos_prendarios",
                   "mplazo_fijo_pesos",
                   "mplazo_fijo_dolares",
                   "minversion2",
                   "mpayroll",
                   "mpayroll2",
                   "mcuenta_debitos_automaticos",
                   #"mtarjeta_visa_debitos_automaticos",
                   "mpagodeservicios",
                   "mcajeros_propios_descuentos",
                   "mtarjeta_visa_descuentos",
                   "mtarjeta_master_descuentos",
                   "mcomisiones_mantenimiento",
                   "Master_mlimitecompra",
                   "Visa_mlimitecompra"
)

#   
# A todas las vamos a rankear

prefix <- "r_"
for (var in mis_variables) {
  dataset[, (paste(prefix, var, sep = "")) := ntile(get(var), 20)]
  #dtest[, (paste(prefix, var, sep = "")) := ntile(get(var), 30)]
  #dapply[, (paste(prefix, var, sep = "")) := ntile(get(var), 30)]
}



#################################################################################
#################################################################################


## Feature Engineering

#dataset[, ctrx_quarter:= NULL]

dataset[ , campo1 := as.integer( ctrx_quarter <14 & mcuentas_saldo < -1256.1 & cprestamos_personales <2 ) ]
dataset[ , campo2 := as.integer( ctrx_quarter <14 & mcuentas_saldo < -1256.1 & cprestamos_personales>=2 ) ]
# 
dataset[ , campo3 := as.integer( ctrx_quarter <14 & mcuentas_saldo>= -1256.1 & mcaja_ahorro <2601.1 ) ]
dataset[ , campo4 := as.integer( ctrx_quarter <14 & mcuentas_saldo>= -1256.1 & mcaja_ahorro>=2601.1 ) ]

dataset[ , campo5 := as.integer( ctrx_quarter>=14 & ( Visa_status>=8 | is.na(Visa_status) ) & ( Master_status>=8 | is.na(Master_status) ) ) ]
dataset[ , campo6 := as.integer( ctrx_quarter>=14 & ( Visa_status>=8 | is.na(Visa_status) ) & ( Master_status <8 & !is.na(Master_status) ) ) ]
# 
dataset[ , campo7 := as.integer( ctrx_quarter>=14 & Visa_status <8 & !is.na(Visa_status) & ctrx_quarter <38 ) ]
dataset[ , campo8 := as.integer( ctrx_quarter>=14 & Visa_status <8 & !is.na(Visa_status) & ctrx_quarter>=38 ) ]

dataset[ , campo8 := as.integer( active_quarter==0 & cdescubierto_preacordado==1 & mcomisiones>= 721.86)]
dataset[ , campo9 := as.integer( active_quarter==0 & cdescubierto_preacordado==1 & mcomisiones< 721.86 ) ]               
# 
dataset[ , campo10 := as.integer( active_quarter==1 & mcaja_ahorro < 1325.9 & mtarjeta_visa_consumo < 875.62 ) ]               
dataset[ , campo11 := as.integer( active_quarter==1 & mcaja_ahorro < 1325.9 & mtarjeta_visa_consumo > 875.62 ) ]     
#                                  
dataset[ , campo12 := as.integer( active_quarter==1 & mcaja_ahorro >= 1325.9 & mtarjeta_visa_consumo < 2000.5 ) ]               
dataset[ , campo13 := as.integer( active_quarter==1 & mcaja_ahorro >= 1325.9 & mtarjeta_visa_consumo >= 2000.5 ) ]                    
# 
dataset[ , campo14 := as.integer(campo1==0 & ctrx_quarter<28 & mcaja_ahorro<2604.3)]
dataset[ , campo15 := as.integer(campo1==0 & ctrx_quarter<28 & mcaja_ahorro>=2604.3)]
dataset[ , campo16 := as.integer(campo1==1 & cdescubierto_preacordado==1 & mpasivos_margen<8.05)]
dataset[ , campo17 := as.integer(campo1==1 & cdescubierto_preacordado==1 & mpasivos_margen>=8.05)]
 
dataset[ , campo18 := as.integer(campo1==1 & cdescubierto_preacordado==1 & campo16==1 & Master_Fvencimiento>=-713)]
dataset[ , campo19 := as.integer(campo1==1 & cdescubierto_preacordado==1 & campo16==1 & Master_Fvencimiento< -713)]
dataset[ , campo20 := as.integer(campo1==1 & cdescubierto_preacordado==1 & campo16==0 & Visa_status>=8)]
dataset[ , campo21 := as.integer(campo1==1 & cdescubierto_preacordado==1 & campo16==0 & Visa_status<8)]



#################################################################################

# variables nuevas pensando en el dominio


dataset[ , prisionero := as.integer(ccuenta_debitos_automaticos>=1 & (cpagodeservicios+ cpagomiscuentas)>=1 & ctrx_quarter>=2 & ctarjeta_debito_transacciones>= 5 & (cseguro_vida+cseguro_auto+cseguro_vivienda)>=1)]

dataset[ , tiene_saldo := as.integer( mcuentas_saldo > 0 ) ]                    
dataset[ , sueldo_sobre_lim := (Master_mlimitecompra+Visa_mlimitecompra)/(mpayroll+mpayroll2)]
dataset[ , consumo_sobre_lim := (Master_mlimitecompra+Visa_mlimitecompra) - (mtarjeta_visa_consumo+mtarjeta_master_consumo)]
dataset[ , tiene_deuda := (mprestamos_personales+mprestamos_prendarios+mprestamos_hipotecarios) > 0]
dataset[ , invierte := (minversion1_pesos + minversion2 + minversion1_dolares >0)]




#################################################################################
#################################################################################


dataset <- dataset[, -c( #"numero_de_cliente",
                         #"ctrx_quarter",
                         #"campo1",
                         #"campo14",
                         # "Visa_status",
                         # "Master_status",
                         # "r_cliente_antiguedad",
                         # "r_mrentabilidad_annual",
                         # "r_mprestamos_personales",
                         # "r_cliente_edad",
                         # "thomebanking",
                         # "Master_Fvencimiento",
                         "mprestamos_personales",
                         "mtarjeta_visa_consumo",
                         "mtarjeta_master_consumo",
                         "mactivos_margen",
                         "mpasivos_margen",
                         "mcaja_ahorro_dolares",
                         "mcaja_ahorro",
                         "mautoservicio",
                         "mcuenta_corriente",
                         "cliente_antiguedad",
                         "cliente_edad",
                         "mrentabilidad",
                         "mrentabilidad_annual",
                         "mcomisiones",
                         "mcuenta_corriente_adicional",
                         "mcaja_ahorro_adicional",
                         "mcuentas_saldo",
                         "mprestamos_hipotecarios",
                         "mprestamos_prendarios",
                         "mplazo_fijo_pesos",
                         "mplazo_fijo_dolares",
                         "minversion2",
                         "mpayroll",
                         "mpayroll2",
                         "mcuenta_debitos_automaticos",
                         #"mtarjeta_visa_debitos_automaticos",
                         "mpagodeservicios",
                         "mcajeros_propios_descuentos",
                         "mtarjeta_visa_descuentos",
                         "mtarjeta_master_descuentos",
                         "mcomisiones_mantenimiento",
                         "Master_mlimitecompra",
                         "Visa_mlimitecompra")]

#dataset[, ctrx_quarter:= NULL]



#################################################################################
#################################################################################

# separo mi conjunto de validacion

dtrain <- dataset[ foto_mes==202101 ]
dapply <- dataset[ foto_mes==202103 ]

ncol(dtrain)
ncol(dapply)

# defino unos parametros interesantes
#los hiperparametros van en una lista
#notar la forma en que esos parametros se pasan a la funcion rpart

set.seed(semillas[1])

# Particionamos de forma estratificada
in_training <- caret::createDataPartition(dtrain$clase_binaria, ######
                                          p = 0.70, list = FALSE)
dtrain1  <-  dtrain[in_training, ]
dtest   <-  dtrain[-in_training, ]

calcular_ganancia <- function(modelo, test) {
  pred_testing <- predict(modelo, test, type = "prob")
  sum(
    (pred_testing[, "evento"] >= 0.025) * ifelse(dtest$clase_binaria == "evento", #######
                                                 78000, -2000) / 0.3
  )
} 

param  <- list("cp"= -1,
               "minsplit"=  200,
               "minbucket" = 0.4,
               "maxdepth" = 5)


modelo_test <- rpart(clase_binaria ~ ., # - agregar variavkes
                data = dtrain1,
                xval = 0,
                control= param)

calcular_ganancia(modelo_test, dtest)

# print(modelo_test$variable.importance)


#################################################################################
#################################################################################


# Sobtre prueba (para Kaggle)


set.seed(semillas[1])
modelo <-  rpart::rpart(formula= "clase_binaria ~ .", #"clase_ternaria ~ ." , # "clase_binaria ~ .",
                        data= dtrain,
                        xval= 0,
                        control= param)
# Aplico el modelo a los datos dtest  pidiendo que me devuelva probabildades
prediccion  <- predict( modelo, dapply, type = "prob")
#prp(modelo, extra=101, digits=5, branch=1, type=4, varlen=0, faclen=0, tweak=1.1)
print(modelo$variable.importance)


# Ahora decido si envio el estimulo o no
# si prob( BAJA+2) > 0.025  envio  el estímulo
prob_baja2  <- prediccion[, "evento"] ########## evento #BAJA+2
Predicted   <- ifelse( prob_baja2 > 0.025, 1, 0 )  # 0.025 es la probabilidad de corte
entrega  <-  as.data.table( list( "numero_de_cliente"=dapply$numero_de_cliente, 
                                  "Predicted"=Predicted)  )
entrega[ , .N, Predicted]

#creo la carpeta donde va el experimento
#dir.create( "./exp/",  showWarnings = FALSE ) 
#dir.create( "./exp/KAGGLE/", showWarnings = FALSE )

#genero el archivo para Kaggle\n",
fwrite( entrega,
        file= "./exp/KAGGLE/para_Kaggle_031_VFinal.csv",
        sep=  "," )



# ¿Cuánto le da la ganancia en el Public Leaderboard?
# 21959.55065
# con cp = -1 y maxdepth= 17 da 22359.54246..... 8195 est.
# todo igual al anterior pero con binarizacion BAJA+2 = BAJA+1 da 17892.96719.... 15195 est.
# todo igual al mejor pero con binarizacion CONTINUA = BAJA+1 da 19639.59812.... 8714 estimulos
# todo igual pero con lo sig: minsplit1340 y minbucket600 maxdepth6... 6939 est

# 11 ganancia interna 20626667[4].... 18672 en Kaggle

# 13 22133333[1]
# 13.1 23066667[1] + arma secreta + nuevos campos sec + sueldo/limite
# 13.2 19472.93486 en kaggle

# 14 definitivo
# mejor intento en kaggle 22.4
# interno dio 25
# us´terrnaria y 0.035 de punto de corte.... 6955 estimulos


### version 18/19 las mejores

# version 20 con dataset pocas variables da 18 en kaggle
# 22 use 200 0.4 y 5
# 23 use 200 0.4 y 4

# 24 es 2000 1.4 13 con 0.07 de corte =23

# dataset[ , campo22 := as.integer(campo1==1 & cdescubierto_preacordado==0 & mcomisiones_otras<99.7 & r_cliente_antiguedad<9)]
# dataset[ , campo23 := as.integer(campo1==1 & cdescubierto_preacordado==0 & mcomisiones_otras<99.7 & r_cliente_antiguedad>=9)]
# dataset[ , campo24 := as.integer(campo1==1 & cdescubierto_preacordado==0 & mcomisiones_otras>=99.7 & r_mrentabilidad_annual<9)]
# dataset[ , campo25 := as.integer(campo1==1 & cdescubierto_preacordado==0 & mcomisiones_otras>=99.7 & r_mrentabilidad_annual>=5)]
# 
# dataset[ , campo26 := as.integer(campo1==0 & campo14==1 & Visa_status>=8 & thomebanking==0)]
# dataset[ , campo27 := as.integer(campo1==0 & campo14==1 & Visa_status>=8 & thomebanking==1)]
# dataset[ , campo28 := as.integer(campo1==0 & campo14==1 & Visa_status<8 & r_mprestamos_personales < 26)]
# dataset[ , campo29 := as.integer(campo1==0 & campo14==1 & Visa_status<8 & r_mprestamos_personales >= 26)]
# 
# dataset[ , campo30 := as.integer(campo1==0 & campo14==0 & Visa_status>=8 & r_cliente_edad>=30)]
# dataset[ , campo31 := as.integer(campo1==0 & campo14==0 & Visa_status>=8 & r_cliente_edad<30)]
# dataset[ , campo32 := as.integer(campo1==0 & campo14==0 & Visa_status<8 & ctrx_quarter < 47)]
# dataset[ , campo33 := as.integer(campo1==0 & campo14==0 & Visa_status<8 & ctrx_quarter >= 47)]


# dataset[ , campo34 := as.integer(campo1==1 & campo22==0 & cdescubierto_preacordado==0 & ccajas_transacciones == 1)]
# dataset[ , campo35 := as.integer(campo1==1 & campo22==0 & cdescubierto_preacordado==0 & ccajas_transacciones == 0 & r_mrentabilidad_annual<5)]
# dataset[ , campo36 := as.integer(campo1==1 & campo22==0 & cdescubierto_preacordado==0 & ccajas_transacciones == 0 & r_mrentabilidad_annual>=5 & Visa_fechaalta >= 4497)]
# dataset[ , campo37 := as.integer(campo1==1 & campo22==0 & cdescubierto_preacordado==0 & ccajas_transacciones == 0 & r_mrentabilidad_annual>=5 & Visa_fechaalta < 4497)]
# 
# dataset[ , campo38 := as.integer(campo1==1 & campo22==0 & cdescubierto_preacordado==1 & campo18 == 1 & Master_Fvencimiento< -682)]
# dataset[ , campo39 := as.integer(campo1==1 & campo22==0 & cdescubierto_preacordado==1 & campo18 == 1 & Master_Fvencimiento>= -682 & internet<1 )]
# dataset[ , campo40 := (campo1==1 & campo22==0 & cdescubierto_preacordado==1 & campo18 == 1 & Master_Fvencimiento>= -682 & internet>=1 )]
# 
# dataset[ , campo41 := as.integer(campo1==1 & campo22==0 & cdescubierto_preacordado==1 & campo18 == 0 & Visa_msaldodolares< -73.215)]
# dataset[ , campo42 := as.integer(campo1==1 & campo22==0 & cdescubierto_preacordado==1 & campo18 == 0 & Visa_msaldodolares>=-73.215 & campo20==1 )]
# dataset[ , campo43 := as.integer(campo1==1 & campo22==0 & cdescubierto_preacordado==1 & campo18 == 0 & Visa_msaldodolares>=-73.215 & campo20==0 )]


# dataset[ , campo44 := as.integer(campo1==0 & campo28==0 & Visa_status<8 & ctrx_quarter >= 47 & cpayroll_trx>=1)]
# dataset[ , campo45 := as.integer(campo1==0 & campo28==0 & Visa_status<8 & ctrx_quarter >= 47 & cpayroll_trx<1 & r_mtarjeta_visa_consumo>=7)]
# dataset[ , campo46 := as.integer(campo1==0 & campo28==0 & Visa_status<8 & ctrx_quarter >= 47 & cpayroll_trx<1 & r_mtarjeta_visa_consumo<7)]
# dataset[ , campo47 := as.integer(campo1==0 & campo28==0 & Visa_status<8 & ctrx_quarter < 47 & cpayroll_trx<1 & Visa_msaldototal<3658.5)]
# dataset[ , campo48 := as.integer(campo1==0 & campo28==0 & Visa_status<8 & ctrx_quarter < 47 & cpayroll_trx<1 & Visa_msaldototal>=3658.5)]


