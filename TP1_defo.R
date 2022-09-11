# Modelo definitivo

require("data.table")
require("rpart")
require("rpart.plot")
require("ggplot2")
require("dplyr")
getwd()

semillas <- c(888809, 888827, 888857, 888869, 888887)
dataset <- fread("./datasets/competencia1_2022.csv")

#Clases BAJAS+1 y BAJA+2 combinadas
dataset[, clase_binaria := ifelse(
  clase_ternaria == "CONTINUA",
  "noevento",
  "evento")]
#Sólo es evento las clase BAJA+2
# dataset[, clase_binaria := ifelse(
#    clase_ternaria == "BAJA+2",
#    "evento",
#    "noevento"
#  )]
dataset[, clase_ternaria := NULL]
#dataset[, ctrx_quarter:= NULL]

dataset[ , campo1 := as.integer( ctrx_quarter <14 & mcuentas_saldo < -1256.1 & cprestamos_personales <2 ) ]
dataset[ , campo2 := as.integer( ctrx_quarter <14 & mcuentas_saldo < -1256.1 & cprestamos_personales>=2 ) ]

dataset[ , campo3 := as.integer( ctrx_quarter <14 & mcuentas_saldo>= -1256.1 & mcaja_ahorro <2601.1 ) ]
dataset[ , campo4 := as.integer( ctrx_quarter <14 & mcuentas_saldo>= -1256.1 & mcaja_ahorro>=2601.1 ) ]

dataset[ , campo5 := as.integer( ctrx_quarter>=14 & ( Visa_status>=8 | is.na(Visa_status) ) & ( Master_status>=8 | is.na(Master_status) ) ) ]
dataset[ , campo6 := as.integer( ctrx_quarter>=14 & ( Visa_status>=8 | is.na(Visa_status) ) & ( Master_status <8 & !is.na(Master_status) ) ) ]

dataset[ , campo7 := as.integer( ctrx_quarter>=14 & Visa_status <8 & !is.na(Visa_status) & ctrx_quarter <38 ) ]
dataset[ , campo8 := as.integer( ctrx_quarter>=14 & Visa_status <8 & !is.na(Visa_status) & ctrx_quarter>=38 ) ]

dataset[ , campo8 := as.integer( active_quarter==0 & cdescubierto_preacordado==1 & mcomisiones>= 721.86)]
dataset[ , campo9 := as.integer( active_quarter==0 & cdescubierto_preacordado==1 & mcomisiones< 721.86 ) ]               

dataset[ , campo10 := as.integer( active_quarter==1 & mcaja_ahorro < 1325.9 & mtarjeta_visa_consumo < 875.62 ) ]               
dataset[ , campo11 := as.integer( active_quarter==1 & mcaja_ahorro < 1325.9 & mtarjeta_visa_consumo > 875.62 ) ]     
                                 
dataset[ , campo12 := as.integer( active_quarter==1 & mcaja_ahorro >= 1325.9 & mtarjeta_visa_consumo < 2000.5 ) ]               
dataset[ , campo13 := as.integer( active_quarter==1 & mcaja_ahorro >= 1325.9 & mtarjeta_visa_consumo >= 2000.5 ) ]                    

dataset[ , tiene_saldo := as.integer( mcuentas_saldo > 0 ) ]                    

dataset[ , sueldo_sobre_lim := (Master_mlimitecompra+Visa_mlimitecompra)/(mpayroll+mpayroll2)]
dataset[ , consumo_sobre_lim := (Master_mlimitecompra+Visa_mlimitecompra) - (mtarjeta_visa_consumo+mtarjeta_master_consumo)]
dataset[ , tiene_deuda := (mprestamos_personales+mprestamos_prendarios+mprestamos_hipotecarios) > 0]
dataset[ , invierte := (minversion1_pesos + minversion2 + minversion1_dolares >0)]


dataset[, c("ctrx_quarter", "campo1", "active_quarter", "campo3", 
             "mcuentas_saldo",
             "mcuenta_corriente", 
             "campo9",
             "mpasivos_margen",
             "ccomisiones_otras",
             "ctarjeta_master",             
             "mcaja_ahorro",
             "cproductos",
             "ctarjeta_visa",
             "campo8",
             "cliente_antiguedad",
             "mactivos_margen",
             "mcomisiones_mantenimiento",
             "mprestamos_personales",
             "tiene_saldo",
             "cprestamos_personales",
             "campo12",
             "tiene_deuda",
             "cdescubierto_preacordado",
             "mcaja_ahorro_dolares",
             "mrentabilidad",
              "internet", 
             "campo7",
            "campo4",
              "mpayroll",
              "cpayroll_trx",
              "campo2",
            "Visa_mpagospesos",
              "campo5", 
       "mcomisiones",
       "Visa_msaldopesos",
       "Visa_msaldototal", 
            "mtarjeta_visa_consumo",
       "mcomisiones_otras",
       "ccomisiones_mantenimiento", 
        "Visa_mpagominimo",
        "campo10",
        "tcuentas",
        "mextraccion_autoservicio",
        "invierte",
        "cinversion2",
        "minversion2",
        "ctarjeta_visa_descuentos",
        "numero_de_cliente",
         "cliente_vip",
        "Visa_cconsumos",
        "cextraccion_autoservicio", 
        "catm_trx",
        "matm"
)]

saldo <- dataset[, tiene_saldo]
head(saldo)

dtrain <- dataset[ foto_mes==202101 ]
dapply <- dataset[ foto_mes==202103 ]



# defino unos parametros interesantes
#los hiperparametros van en una lista
#notar la forma en que esos parametros se pasan a la funcion rpart
param  <- list("cp"= -1,
               "minsplit"=  1073,
               "minbucket" = 278,
               "maxdepth" = 5)
nrow(dtrain)

# variables para bintiar
mis_variables <- c("mprestamos_personales",
                   "mtarjeta_visa_consumo",
                   "mtarjeta_visa_consumo",
                   "mactivos_margen",
                   "mcaja_ahorro_dolares",
                   "mcaja_ahorro",
                   "mautoservicio",
                   "mcuenta_corriente",
                   "cliente_antiguedad",
                   "cliente_edad"
                   )

# A todas las vamos a rankear

prefix <- "r_"
for (var in mis_variables) {
  dtrain1[, (paste(prefix, var, sep = "")) := ntile(get(var), 20)]
  dtest[, (paste(prefix, var, sep = "")) := ntile(get(var), 20)]
  dapply[, (paste(prefix, var, sep = "")) := ntile(get(var), 20)]
}




set.seed(semillas[1])

# Particionamos de forma estratificada
in_training <- caret::createDataPartition(dtrain$clase_binaria,
                                          p = 0.70, list = FALSE)
dtrain1  <-  dtrain[in_training, ]
dtest   <-  dtrain[-in_training, ]

calcular_ganancia <- function(modelo, test) {
  pred_testing <- predict(modelo, test, type = "prob")
  sum(
    (pred_testing[, "evento"] >= 0.025) * ifelse(dtest$clase_binaria == "evento",
                                                 78000, -2000) / 0.3
  )
}


modelo_test <- rpart(clase_binaria ~ ., # - agregar variavkes
                data = dtrain1,
                xval = 0,
                cp = -1,
                minsplit = 1073,
                minbucket = 278,
                maxdepth = 5)

calcular_ganancia(modelo_test, dtest)

# print(modelo_test$variable.importance)

##################################
# Sobtre dapply

set.seed(semillas[1])
modelo <-  rpart::rpart(formula= "clase_binaria ~ .", #"clase_ternaria ~ ." , # "clase_binaria ~ .",
                        data= dtrain,
                        xval= 0,
                        control= param)
# Aplico el modelo a los datos dtest  pidiendo que me devuelva probabildades
prediccion  <- predict( modelo, dapply, type = "prob")
#prp(modelo, extra=101, digits=5, branch=1, type=4, varlen=0, faclen=0, tweak=1.1)


# ganancia <- function(probabilidades, clase) {
#   return(sum(
#     (probabilidades >= 0.025) * ifelse(clase == "evento", 78000, -2000))
#   )
# }
# print("La ganancia NORMALIZADA de nuestro modelo es:")
# print(ganancia(prediccion[, "evento"], dtest$clase_binaria) / 0.3)
# 


# Ahora decido si envio el estimulo o no
# si prob( BAJA+2) > 0.025  envio  el estímulo
prob_baja2  <- prediccion[, "evento"] #evento #BAJA+2
Predicted   <- ifelse( prob_baja2 > 0.047, 1, 0 )  # 0.025 es la probabilidad de corte
entrega  <-  as.data.table( list( "numero_de_cliente"=dapply$numero_de_cliente, 
                                  "Predicted"=Predicted)  )
entrega[ , .N, Predicted]

#creo la carpeta donde va el experimento
#dir.create( "./exp/",  showWarnings = FALSE ) 
dir.create( "./exp/KAGGLE/", showWarnings = FALSE )

#genero el archivo para Kaggle\n",
fwrite( entrega,
        file= "./exp/KAGGLE/para_Kaggle_016.csv",
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




semillas <- c(888809, 888827, 888857, 888869, 888887)

vector_ganancias  <- c()
for( semilla in semillas){
  ganancia  <- GananciaArbol( semilla, dataset, x=param, train=0.70 )
  vector_ganancias  <- c( vector_ganancias, ganancia)
}
vector_ganancias
# Calculo la media de  vector_ganancias
mean(vector_ganancias)




resultados_mis_semillas <- c()
t0 <- Sys.time()
for (s in semillas) {
  set.seed(s)
  in_training <- caret::createDataPartition(dataset[, get("clase_binaria")],
                                            p = 0.70, list = FALSE)
  dtrain  <-  dataset[in_training, ]
  dtest   <-  dataset[-in_training, ]
  
  modelo <- rpart(clase_binaria ~ .,
                  data = dtrain,
                  xval = 0,
                  cp = -1,
                  minsplit = 50,
                  minbucket = 25,
                  maxdepth = 5)
  
  pred_testing <- predict(modelo, dtest, type = "prob")
  
  gan <- ganancia(pred_testing[, "evento"], dtest$clase_binaria) / 0.3
  
  resultados_mis_semillas <- c(resultados_mis_semillas, gan)
  
}
print(Sys.time() - t0)

print(mean(resultados_mis_semillas))

print(max(resultados_mis_semillas))




GananciaArbol  <-  function( semilla, data, x, train=0.70) {
  #establezco la semilla
  set.seed(semilla)
  train_rows <- createDataPartition(dataset$clase_binaria, p= 0.70,list= FALSE)
  modelo <-  rpart::rpart(formula= "clase_binaria ~ .",
                          data= data[ train_rows],  #los datos de training
                          xval= 0,
                          control= x)
  #Aplico el modelo a los datos de testing
  prediccion  <- predict( modelo, data[ -train_rows], type = "prob")
  prob_baja2  <- prediccion[, "evento"]
  ganancia_testing <- data[ -train_rows, sum(  (prob_baja2> 0.025) * ifelse( clase_binaria=="evento", 78000, -2000) )]
  #normalizo la ganancia
  ganancia_testing_normalizada  <- ganancia_testing/0.3
  return( ganancia_testing_normalizada )
}


vector_ganancias  <- c()
for( semilla in semillas){
  ganancia  <- GananciaArbol( semilla, dataset, x=param, train=0.70 )
  vector_ganancias  <- c( vector_ganancias, ganancia)
}
vector_ganancias
# Calculo la media de  vector_ganancias
mean(vector_ganancias)





