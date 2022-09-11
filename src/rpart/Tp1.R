#Arbol elemental con libreria  rpart
#Debe tener instaladas las librerias  data.table  ,  rpart  y  rpart.plot

#cargo las librerias que necesito
require("data.table")
require("rpart")
require("rpart.plot")

#Aqui se debe poner la carpeta de la materia de SU computadora local

setwd("~/Desktop/Maestría/DM_EyF")#Establezco el Working Directory

getwd()

#cargo el dataset 
dataset  <- fread("./datasets/competencia1_2022.csv")


# t0  <- Sys.time()
# dataset <- read.csv("./datasets/competencia1_2022.csv")
# t1  <- Sys.time()
# delta  <- as.numeric(t1 - t0)  #calculo la diferencia de tiempos
# print(delta) #imprimo\
# 
# t0  <- Sys.time()
# dataset <- fread("./datasets/competencia1_2022.csv")
# t1  <- Sys.time()
# delta  <- as.numeric(  t1 - t0)  #calculo la diferencia de tiempos
# print(delta) #imprimo

nrow( dataset )
ncol( dataset )

dataset[ , .N ]

# Cuantos N hay en los 2 meses que tengo
dataset[ , .N, foto_mes]

# Nombres columnas
colnames(dataset)

# Exploración clase ternaria
dataset[  , .N, list( foto_mes, clase_ternaria) ]


# Como puedo contar los BAJA +2 #850
nrow(dataset[ clase_ternaria=="BAJA+2" ]) 
dataset[clase_ternaria=="BAJA+2", .N ] #el autentico estilo data.table
dataset[  , sum(clase_ternaria=="BAJA+2")]

# Conteo de proporcion de BAJA+2 en el dataset
dataset[ foto_mes==202101  ,  sum(clase_ternaria=="BAJA+2")/.N]

# Conteo de la proporcion de BAJA+2 en un predicado
dataset[ foto_mes==202101 & ctrx_quarter < 20  ,  sum(clase_ternaria=="BAJA+2")/.N]

# Lift del predicado  ctrx_quarter  vs el universo
dataset[ foto_mes==202101 & ctrx_quarter < 20  ,  sum(clase_ternaria=="BAJA+2")/.N]  /dataset[ foto_mes==202101  ,  sum(clase_ternaria=="BAJA+2")/.N]

# Ganancias del dataset. Agregado de la columna ganancia al dataset
## Primero le asigno a TODOS los registros el valor de  -2000, la asignacion se hace con el :=
dataset[ foto_mes==202101, ganancia := -2000]
# y finalmente a los  BAJA+2 les asigno 78000
dataset[ foto_mes==202101 & clase_ternaria=="BAJA+2", ganancia := 78000]
# Calculo la ganancia que tendria una campaña en donde envío estímulo a TODOS los clientes
dataset[ foto_mes==202101 , sum(ganancia)]
# Si le enviara estímulo a todos, se **pierden**  254 millones de pesos

# Ganancias de predicados univariados
##  Calculo la ganancia de un predicado simple, ctrx_quarter < 20
dataset[ foto_mes==202101 & ctrx_quarter < 20,  sum( ganancia )]

# Ahora  la ganancia de  *ctrx_quarter < 4 *
dataset[ foto_mes==202101 & ctrx_quarter < 4,  sum( ganancia )]

#Ahora, en forma brutal e ineficiente, busco donde esta el mejor corte de  ctrx_quarter
for(transacciones  in   0:50)
    cat(  transacciones, dataset[  foto_mes==202101 & ctrx_quarter < transacciones,  sum( ganancia )  ] )


# Ganancia predicado complejo
dataset[  foto_mes==202101 & ctrx_quarter < 20 & mpasivos_margen < 29.8 ,  sum( ganancia )  ]
# 17 M

# Visualización de la densidad de una variable versus clase_ternaria
library("ggplot2") #cargo la libreria ggplot2
campo <- "cliente_antiguedad"
ggplot(dataset[ foto_mes==202101] , aes_string(x = campo)) + geom_density(trim=TRUE, na.rm=TRUE) + facet_grid( "clase_ternaria~ .")

# los gráficos salen muy pequeños, busco la documentacion 
# https://blog.revolutionanalytics.com/2015/09/resizing-plots-in-the-r-kernel-for-jupyter-notebooks.html  y 
# agrando los graficos"

options(repr.plot.width=15, repr.plot.height=15)
ggplot(dataset[ foto_mes==202101], aes_string(x = campo)) + geom_density(trim=TRUE, na.rm=TRUE) + facet_grid( "clase_ternaria~ .")



## Construyendo un arbol
# Se construye un arbol de decisión, se ven distintas formas de pasar los parámetros y distintas formas de dibujarlo
# Se muestran funcionalidades básicas de la libreria  *data.table*"

# Primero cargo librerias de data.table y rpart. Luego busco un setting para graficos
options(repr.plot.width=25, repr.plot.height=25)  #para que los gráficos me salgan legibles

#Ahora entreno un arbol de decision
# clase_ternaria ~ .\"  
# significa predecir clase_ternaria utilizando *todo el resto* de las variables del dataset

modelo <-  rpart(formula= "clase_ternaria ~ .",
                 data= dataset[ foto_mes==202101])

# Imprimo el modelo
print(modelo)

#Esta impresión no es gráfica. No me sirve
# a pesar que no me sirve, he encontrado una piedra en el camino, 
# me está generando un arbol con un solo nodo, con solo la raiz"
# **rpart.plot** que grafica arboles generados con la libreria  *rpart*"

library("rpart.plot")
rpart.plot::prp(modelo)

#Me ha salido una impresión del arbol, que es un solo nodo, pero solo dice continua.
prp(modelo, extra=101, digits=5, branch=1, type=4, varlen=0, faclen=0, tweak=1.3)
# Ha salido solo la raiz del arbol
# Los tres numeros que muestra en el nodo con la cantidad de BAJA+1, BAJA+2 y CONTINUA, en ese orden, alfabetico
# la cantidad de CONTINUA   la está mostrando en notacion científica"

# cambio hiperparámetros del arbol para salga algo mas que un solo nodo
# El hiperparámetro cp  *complexity*   limita el split de los nodos
# El default es  cp=0.05,
# Pruebo con cp=0.0  a ver si \"se abre el arbol\

# Leo la documentación de la libreria rpart  https://cran.r-project.org/web/packages/rpart/rpart.pdf   y
# veo que existe un hiperparámetro de la funcion rpart  llamado **xval** que es para hacer cross validation, 
# que por default viene seteado en xval=10 .   
# No me interesa en este momento que haga cross validation, para evitarlo voy a poner  *xval=0*"
modelo <-  rpart(  formula= "clase_ternaria ~ .",
                   data= dataset[ foto_mes==202101],
                   xval= 0,
                   cp= 0.0)
# Grafico
prp(modelo, extra=101, digits=5, branch=1, type=4, varlen=0, faclen=0, tweak=1.3)
# sale un arbol de gran profundidad  que ni se puede visualizar

# limito profundidad
modelo <-  rpart(  formula= "clase_ternaria ~ .",
                   data= dataset[ foto_mes==202101],
                   xval= 0,
                   cp= 0.0,
                   maxdepth =2)
prp(modelo, extra=101, digits=5, branch=1, type=4, varlen=0, faclen=0, tweak=1.3)
prp(modelo, extra=101, digits=5, branch=1, type=4, varlen=0, faclen=0, tweak=1.3, cex=1.2)
# Esperaba ver un albol de profundidad 2 sin embargo, 
# por alguna misteriosa razón, se ha generado un arbol con un solo nodo
# Corto por lo sano,  y establezco  cp=-1  para que siempre se abra el arbol

modelo <-  rpart(  formula= "clase_ternaria ~ .",
                   data= dataset[ foto_mes==202101],
                   xval= 0,
                   cp= -1,
                   maxdepth =3)
prp(modelo, extra=101, digits=5, branch=1, type=4, varlen=0, faclen=0, tweak=1.3)

#Listo los objetos que estan en la memoria de R en este momento

ls()

# Me fijo cuanta memoria esta disponible

gc()

# Borro todos los elementos en memoria
# rm( list=ls())

# 4. Transformado (innecesariamente) las variables

options(repr.plot.width=20, repr.plot.height=10)

modelo <-  rpart(  formula= "clase_ternaria ~ .",
                   data= dataset[ foto_mes==202101],
                   xval= 0,
                   cp= -1,
                   maxdepth= 2 )
prp(modelo, extra=101, digits=5, branch=1, type=4, varlen=0, faclen=0, tweak=1.1, cex=1.2)
# La variable mas importante que aparece es **ctrx_quarter**

# variables colineales. agrego al dataset tres variables colineales  con ctrx_quarter

dataset[ foto_mes==202101 , ctrx_quarter_dos    :=  2*ctrx_quarter ]
dataset[ foto_mes==202101 , ctrx_quarter_tres   :=  3*ctrx_quarter ]
dataset[ foto_mes==202101 , ctrx_quarter_cuatro :=  4*ctrx_quarter ]


modelo <-  rpart(  formula= "clase_ternaria ~ ." ,
                   data= dataset[ foto_mes==202101],
                   xval= 0,
                   cp= -1,
                   maxdepth= 2 )
prp(modelo, extra=101, digits=5, branch=1, type=4, varlen=0, faclen=0, tweak=1.1)
# el arbol de decision es inmune a las colinearidad de variables

# Normalizacion de variables
# analizo ctrx_quarter

min( dataset[ foto_mes==202101 , ctrx_quarter] )
max( dataset[ foto_mes==202101, ctrx_quarter] )
boxplot(  dataset[ foto_mes==202101, ctrx_quarter])
hist( dataset[ foto_mes==202101, ctrx_quarter] )
plot( density( dataset[ foto_mes==202101, ctrx_quarter] ) )
# Normailzo  ctrx_quarter

dataset[ foto_mes==202101, ctrx_quarter_normalizado := scale(ctrx_quarter)]
plot( density( dataset[foto_mes==202101, ctrx_quarter_normalizado] ) )

# Confirmado, la variable está normalizada, ahora corremos nuevamente el arbol de decision
modelo <-  rpart(  formula= "clase_ternaria ~ ." ,
                   data= dataset[ foto_mes==202101],
                   xval= 0,
                   cp= -1,
                   maxdepth= 2 )
prp(modelo, extra=101, digits=5, branch=1, type=4, varlen=0, faclen=0, tweak=1.1, cex=1.2)
# el arbol de decision es inmune a las normalizacion de variables
# El arbol de decisión no ha cambiado. 
# Su forma es exactamente igual, las cantidades en los nodos idéntica al arbol original

# # Transformación  logaritmica
dataset <- fread("./datasets/competencia1_2022.csv")

dataset[ foto_mes==202101 , ctrx_quarter_log :=log(ctrx_quarter+1)]  #sumo el uno porque no quiero infinitos
boxplot(  dataset[foto_mes==202101 , ctrx_quarter_log])
plot( density( dataset[ foto_mes==202101, ctrx_quarter_log] ) )
# ELIMINO del dataset la variable  ctrx_quarter , para que solo juegue  ctrx_quarter_log
dataset[ , ctrx_quarter := NULL ]

modelo <-  rpart(  formula= "clase_ternaria ~ ." ,
                  data= dataset[foto_mes==202101],
                  xval= 0,
                  cp= -1,
                  maxdepth= 2 )
prp(modelo, extra=101, digits=5, branch=1, type=4, varlen=0, faclen=0, tweak=1.1, cex=1.2)

# el arbol de decision es inmune a la transformada logaritmica
# Por supuesto, el arbol original cortaba en  ctrx_quarter < 14 y 
# ahora corta en ctrx_quarter < 2.673 porque obviamente alteré esa variable, 
# pero en realidad está cortando en el mismo punto

# Ahora fabrico outliers y veo como se comporta el arbol
dataset <- fread("./datasets/competencia1_2022.csv")
boxplot(  dataset[ foto_mes==202101 , ctrx_quarter])
# cuento cuantos registros hay con ctrx_quarter > 1500
dataset[ foto_mes==202101 & ctrx_quarter > 1500, .N]
# Ahora, a esos 12 valores los transformo en outliers extremos
dataset[ foto_mes==202101 & ctrx_quarter > 1500,  ctrx_quarter := ctrx_quarter * 1000]
boxplot(  dataset[ foto_mes==202101 , ctrx_quarter])
modelo <-  rpart(  formula= "clase_ternaria ~ ." ,
                   data= dataset[foto_mes==202101],
                   xval= 0,
                   cp= -1,
                   maxdepth= 2 )
prp(modelo, extra=101, digits=5, branch=1, type=4, varlen=0, faclen=0, tweak=1.1, cex=1.2)
# el arbol de decision es inmune a los outliers




#genero el modelo,  aqui se construye el arbol
modelo  <- rpart(formula=   "clase_ternaria ~ .",  #quiero predecir clase_ternaria a partir de el resto de las variables
                 data=      dtrain,  #los datos donde voy a entrenar
                 xval=      0,
                 cp=       -0.3,   #esto significa no limitar la complejidad de los splits
                 minsplit=  0,     #minima cantidad de registros para que se haga el split
                 minbucket= 1,     #tamaño minimo de una hoja
                 maxdepth=  17 )    #profundidad maxima del arbol


#grafico el arbol
prp(modelo, extra=101, digits=5, branch=1, type=4, varlen=0, faclen=0)

# Voy a averiguar cuantos BAJA+2 hay en Kaggle

dataset <- fread("./datasets/competencia1_2022.csv")
# cuento clases
dataset[  , .N, list( foto_mes, clase_ternaria) ]
# armo un data.table con los datos del futuro
dfuturo <-  dataset[ foto_mes==202103 ]
# cuento la cantidad de lineas del dataset
nrow( dfuturo )
# Creo un vector que sean los  ids
vector_ids <- dfuturo[ , numero_de_cliente]
head( vector_ids)
# Creo un vector de todos unos, con la cantidad de registros que tiene dfuturo
vector_enviar <-  rep( 1,  nrow( dfuturo))
head( vector_enviar )
length( vector_enviar)
# creo tabla para kaggle

tabla_final  <-   as.data.table(  list("numero_de_cliente"= vector_ids,
                                       "Predicted"=         vector_enviar))
head( tabla_final)

#genero el archivo para Kaggle
#creo la carpeta donde va el experimento
dir.create("./exp/",  showWarnings = FALSE )
dir.create("./exp/ZH2016/", showWarnings = FALSE )
#genero el archivo para Kaggle
fwrite(tabla_final,
        file= "./exp/ZH2016/todos_unos.csv",
        sep=  "," )

# Se que se cumplen estas dos cosas:
# POS + NEG = 162900
# 78000\\*POS - 2000\\*NEG =-249001570
# Es un sistema de dos ecuaciones con dos incógnitas, 
# hay multiples formas de resolverlo, la primera sería despejar POS
# pero utilizamos esta página  https://keisan.casio.com/exec/system/14013211335541

# Al correrlo obtenemos que  POS = 960 , o sea **las BAJA+2  de 202103  son  960

# Con lo cual es de esperar mayores ganancias de los modelos predictivos en 202103  que en 202101,  
# va a irnos mejor en el Public Leaderboard que en nuestros modelos

# zero to hero 7
dataset <- fread("./datasets/competencia1_2022.csv")

dtrain <- dataset[ foto_mes==202101 ]
dapply <- dataset[ foto_mes==202103 ]

modelo <-  rpart::rpart(formula= "clase_ternaria ~ ." ,
                        data= dtrain,
                        xval= 0,
                        cp= -1,
                        maxdepth= 2 )

# Cargo el dataset a donde voy a aplicar el modelo
# Aplico el modelo a los datos dapply  pidiendo que me devuelva probabildades

prediccion  <- predict( modelo, dapply, type = "prob")
head( prediccion )
# Prediccion es una matriz; me interesa la columna \"BAJA+2\"  
# que es la probabilidad que modelo asigna a cada registro de dapply

prob_baja2  <- prediccion[, "BAJA+2"]
head( prob_baja2)
# prob_baja2 es el vector de probabilidades

length( prob_baja2 )
nrow( dapply )
# Ahora decido si envio el estimulo o no
# si prob( BAJA+2) > 0.025  envio  el estímulo
Predicted   <- ifelse( prob_baja2 > 0.025, 1, 0 )
head( Predicted)
hist( Predicted)
# creo una tabla con  lo que voy a enviar a Kaggle
entrega  <-  as.data.table( list( "numero_de_cliente"=dapply$numero_de_cliente, 
                                  "Predicted"=Predicted)  )
head( entrega)
entrega[ , .N, Predicted]

#genero el archivo para Kaggle\n",
#creo la carpeta donde va el experimento\n",
dir.create( "./exp/",  showWarnings = FALSE )
dir.create( "./exp/KA003/", showWarnings = FALSE )
fwrite( entrega,
        file= "./exp/KA003/para_Kaggle_0107.csv",
        sep=  "," )

# otro modelo para kaggle

dataset <- fread("./datasets/competencia1_2022.csv")

# dataset[, clase_binaria := ifelse(
#                                   clase_ternaria == "CONTINUA",
#                                   "noevento",
#                                   "evento"
#                                     )]
# 
# dataset[, clase_ternaria := NULL]
dtrain <- dataset[ foto_mes==202101 ]
dapply <- dataset[ foto_mes==202103 ]



# defino unos parametros interesantes
#los hiperparametros van en una lista
#notar la forma en que esos parametros se pasan a la funcion rpart
param  <- list("cp"= -1,
               "minsplit"=  900,
               "minbucket"= 440,
               "maxdepth"= 17)
nrow(dtrain)

modelo <-  rpart::rpart(formula= "clase_binaria ~ ." ,
                        data= dtrain,
                        xval= 0,
                        control= param)
# Aplico el modelo a los datos dapply  pidiendo que me devuelva probabildades
prediccion  <- predict( modelo, dapply, type = "prob")

prob_baja2  <- prediccion[, "evento"]

# Ahora decido si envio el estimulo o no
# si prob( BAJA+2) > 0.025  envio  el estímulo

Predicted   <- ifelse( prob_baja2 > 0.025, 1, 0 )  # 0.025 es la probabilidad de corte

entrega  <-  as.data.table( list( "numero_de_cliente"=dapply$numero_de_cliente, 
                                  "Predicted"=Predicted)  )

#creo la carpeta donde va el experimento
dir.create( "./exp/",  showWarnings = FALSE ) 
dir.create( "./exp/KAGGLE/", showWarnings = FALSE )

#genero el archivo para Kaggle\n",
fwrite( entrega,
        file= "./exp/KAGGLE/para_Kaggle_007.csv",
        sep=  "," )

entrega[ , .N, Predicted]

# ¿Cuánto le da la ganancia en el Public Leaderboard?
# 21959.55065
# con cp = -1 y maxdepth= 17 da 22359.54246
# todo igual al anterior pero con binarizacion BAJA+2 = BAJA+1 da 17892.96719
# todo igual al mejor pero con binarizacion CONTINUA = BAJA+1 da 19639.59812.... 8714 estimulos






################
# solo usando BAJA+1

dataset <- fread("./datasets/competencia1_2022.csv")

dtrain <- dataset[ foto_mes==202101]
dapply <- dataset[ foto_mes==202103 ]

# dtrain[, clase_binaria := ifelse(
#                                   clase_ternaria == "BAJA+2",
#                                   "evento",
#                                   "noevento"
#                                     )]


nrow( dapply )
nrow( dtrain )

param  <- list("cp"= -1,
               "minsplit"=  900,
               "minbucket"= 440,
               "maxdepth"= 17)

modelo <-  rpart::rpart(formula= "clase_ternaria ~ ." ,
                        data= dtrain,
                        xval= 0,
                        control= param)

# Aplico el modelo a los datos dapply  pidiendo que me devuelva probabildades
prediccion  <- predict( modelo, dapply, type = "prob")

prob_baja2  <- prediccion[, "BAJA+2"]
Predicted   <- ifelse( prob_baja2 > 0.52, 1, 0 )  # 0.025 es la probabilidad de corte\n",

entrega  <-  as.data.table( list( "numero_de_cliente"=dapply$numero_de_cliente, 
                                  "Predicted"=Predicted)  )

#creo la carpeta donde va el experimento
dir.create( "./exp/",  showWarnings = FALSE ) 
dir.create( "./exp/KA005/", showWarnings = FALSE )

#genero el archivo para Kaggle
fwrite( entrega,
        file= "./exp/KA005/para_Kaggle_005.csv",
        sep=  "," )


entrega[ , .N, Predicted]
               









################################
dataset <- fread("./datasets/competencia1_2022.csv")


dtrain  <- dataset[ foto_mes==202101 ]  #defino donde voy a entrenar
dapply  <- dataset[ foto_mes==202103 ]  #defino donde voy a aplicar el modelo

#aplico el modelo a los datos nuevos
prediccion  <- predict( object= modelo,
                        newdata= dapply,
                        type = "prob")

#prediccion es una matriz con TRES columnas, llamadas "BAJA+1", "BAJA+2"  y "CONTINUA"
#cada columna es el vector de probabilidades 

#agrego a dapply una columna nueva que es la probabilidad de BAJA+2
dapply[ , prob_baja2 := prediccion[, "BAJA+2"] ]

#solo le envio estimulo a los registros con probabilidad de BAJA+2 mayor  a  1/40
dapply[ , Predicted := as.numeric( prob_baja2 > 1/40 ) ]

#genero el archivo para Kaggle
#primero creo la carpeta donde va el experimento
dir.create( "./exp/" )
dir.create( "./exp/KA2001" )

fwrite( dapply[ , list(numero_de_cliente, Predicted) ], #solo los campos para Kaggle
        file= "./exp/KA2001/K101_001.csv",
        sep=  "," )


############################# 109

# El objetivo es hacer una division del dataset en training/testing que sea estratificada en la clase
dataset <- fread("./datasets/competencia1_2022.csv")

denero <- dataset[ foto_mes==202101 ]

# la funcion  **createDataPartition** devolverá un vector de posiciones, 
# las que cumplen con la particion indicada

#p= 0.5* significa que queremos el 50% de los registros\n",
    
#  dataset$clase_ternaria*   es el vector con la clase ternaria, que es por donde se estratificará
# install.packages("lattice")
library("caret")
set.seed(1) # con el set.seed me aseguro la misma partición siempre
train_rows <- createDataPartition(denero$clase_ternaria, p= 0.50, list= FALSE)
# veamos que ha quedado en  los primeros registros del vector **train_rows**
head(train_rows) 
# Aqui ya vemos que la particion tiene un suconjunto de los registros
# ahora creamos los datasets de training y testing

dtrain <- denero[ train_rows]
# -train_rows  significa el complemento  (no confundir con numeros negativos)
dtest <- denero[ -train_rows]
# Compruebo la division

nrow( dtrain)
nrow( dtest )
nrow( dtrain) + nrow(dtest)
nrow( denero)

# Compruebo que la particion es estratificada

denero[  , .N, clase_ternaria]
dtrain[  , .N, clase_ternaria]
dtest[  , .N, clase_ternaria]
# La division se ha construido en forma perfectamente estratificada !


param  <- list("cp"= -1,
               "minsplit"=  900,
               "minbucket"= 440,
               "maxdepth"= 17)

modelo <-  rpart::rpart(formula= "clase_ternaria ~ ." ,
                        data= dtrain,
                        xval= 0,
                        control= param)

# Aplico el modelo a los datos dtest  pidiendo que me devuelva probabildades
prediccion  <- predict( modelo, dtest, type = "prob")

prob_baja2  <- prediccion[, "BAJA+2"]
Predicted   <- ifelse( prob_baja2 > 0.025, 1, 0 )  # 0.025 es la probabilidad de corte

entrega  <-  as.data.table( list( "numero_de_cliente"=dtest$numero_de_cliente, 
                                  "Predicted"=Predicted)  )

entrega[ , .N, Predicted]

#########################

# Experimentos con azar Replicables

# El objetivo es calcular la ganancia en testing de un arbol de decision


dataset <- fread("./datasets/competencia1_2022.csv")   #cargo el dataset #cargo el dataset
dataset <- dataset[ foto_mes==202101 ]  # me quedo solo con el periodo 202101"
#creo training con 70%  y testing con 30%\n",
set.seed(888809) # c(888809, 888827, 888857, 888869, 888887)
train_rows <- createDataPartition(dataset$clase_ternaria, p= 0.70,list= FALSE)
dtrain  <- dataset[ train_rows]
dtest  <-  dataset[ -train_rows]

# Entreno el modelo en los datos de training

param  <- list("cp"= -0.5,
              "minsplit"=  900,
              "minbucket"= 440,
              "maxdepth"= 5)

# Entreno el modelo en los datos de training         
#genero el modelo
modelo <-  rpart::rpart(formula= "clase_ternaria ~ ." ,
                        data= dtrain,
                        xval= 0,
                        control= param)

#aplico el modelo a los datos de testing
prediccion  <- predict( modelo, dtest, type = "prob")

# Calculo la ganancia del modelo en los datos de testing
# a partir de la prediccion, calculo la probabilidad de BAJA+2 
# de cada registro de testing  **dtest**
prob_baja2  <- prediccion[, "BAJA+2"]

#Para el cálculo de la ganancia del modelo aplicado a testing debo tener en cuenta lo siguiente
# Solo envio estimulo a los registros que el modelo asigno una probabilidad mayor a 0.025
# Si no envio estimulo, no gano ni pierdo, es decir la ganacia es cero\n",
# Si el registro al que envio el estímulo es un **BAJA+2**  entonces la ganancia es de +78000
# Si el registro al que envio el estímulo es un **BAJA+1**  entonces la ganancia es de  -2000
# Si el registro al que envio el estímulo es un **CONTINUA** entonces la ganancia es de  -2000"

## A la condición que solo envio a los registros con probabilidad mayor a 1/60 
# la resuelvo con la condicion lógica  (prob_baja2> 0.025)
# Esta condicion termina siendo un 0 o un
# vale 1 si y solo si se cumple la condicion, es decir  si prob_baja2 es mayor a 0.025
# La ganancia de +78000 o -2000 la resuelvo de esta forma ifelse( clase_ternaria==\"BAJA+2\", 78000, -2000)
#Finalmente,  que se cumplan ambas condiciones al mismo tiempo, en este caso, es multiplicarlas, o sea\n",
# (prob_baja2> 0.025) * ifelse(clase_ternaria == "BAJA+2", 78000, -2000)
# La condicion anterior toma los siguientes valores"
#    prob(BAJA+2) | clase_ternaria | ganancia |
#    --- | :---: | ---: |
#    menor o igual a 0.025 | BAJA+1 | 0 |
#    menor o igual a 0.025 | CONTINUA | 0 |
#    menor o igual a 0.025 | BAJA+2 | 0 |
#    mayor a 0.025 | BAJA+1 | -2000 |
#    mayor a 0.025 | CONTINUA | -2000 |
#    mayor a 0.025 | BAJA+2 | 78000

# Finalmente hago el calculo en data.table
ganancia_testing <- dtest[ , sum(  (prob_baja2>0.025) * ifelse( clase_ternaria=="BAJA+2", 78000, -2000) )]
ganancia_testing
# Es importante que la ganancia esta calculada sobre los datos de testing, que en este caso son apenas el 30%
# Si quiero extrapolar a todo el dataset, debo hacer el **cociente** de esa ganancia por 0.30   ( coloquialmente, debo dividir por 0.30)

ganancia_testing_normalizada  <-  ganancia_testing/0.3

#######################

# Funcion de Ganancia de Arbol training/testing

# El objetivo es poner el notebook anterior dentro de una funcion, e invocarla con distintas semillas, 
# para maravillarnos ante la dispersion de las ganancias

GananciaArbol  <-  function( semilla, data, x, train=0.70) {
# establezco la semilla
          set.seed(semilla)
          train_rows <- createDataPartition(dataset$clase_ternaria, p= 0.70,list= FALSE)
          modelo <-  rpart::rpart(formula= "clase_ternaria ~ ." ,
                                  data= data[ train_rows],  #los datos de training
                                  xval= 0,
                                  control= x)
# Aplico el modelo a los datos de testing 
          prediccion  <- predict( modelo, data[ -train_rows], type = "prob")
          prob_baja2  <- prediccion[, "BAJA+2"]
          ganancia_testing <- data[ -train_rows, sum(  (prob_baja2>0.025) * ifelse( clase_ternaria=="BAJA+2", 78000, -2000) )]
          return( ganancia_testing)
}

# Vale la pena notar que dentro de la funcion no se han creado  dtrain y dtest,  sino que directamente se utilizan:
# data[ train_rows]  como training
# data[ -train_rows]  como testing

# Aqui empieza el programa

dataset <- fread("./datasets/competencia1_2022.csv")   #cargo el dataset #cargo el dataset
dataset <- dataset[ foto_mes==202101 ]  # me quedo solo con el periodo 202101
param  <- list("cp"= -0.5,
               "minsplit"=  900,
               "minbucket"= 440,
               "maxdepth"= 5)

# Ahora hago algunas llamadas a la funcion

GananciaArbol(888809, dataset, x=param)/0.3 # 22160000
# semillas 888809, 888827, 888857, 888869, 888887)
GananciaArbol(888827, dataset, x=param)/0.3 # 19986667
GananciaArbol(888857, dataset, x=param)/0.3 # 23633333
GananciaArbol(888869, dataset, x=param)/0.3 # 20353333
GananciaArbol(888887, dataset, x=param)/0.3 # 16540000

# Esta dispersión es **NOTABLE**  ya que el algoritmo es el mismo, con los mismos parámetros.
# Lo único que cambia es que datos se utilizan para entrenar y testear, pero siempre es 70% / 30%
# Es más notable aún que se ha tenido cuidado que la partición sea estratificada segun el campo **clase_ternaria**, 
# lo que apriori uno supondria que va a generar particiones muy homogéneas.

#################################

# 13  Acumulando resultados en un vector'

# Este elemental notebook muestra desde cero como acumular resultados en un vector, 
# será un primer paso para luego escribir scripts en donde guardemos los resultados de nuestros procesos 
# en vectores  o en un dataset si es necesario

# Partimos primero de un vector, al que queremos agregarle al final un elemento nuevo
vector  <- c( 1, 2, 3, 4, 5)
# agregar un elemento al final, a  **vector** , es tan simple como
vector <- c( vector,  6 )

# Ahora definimos una funcion para ejemplificar
potencia_invertida  <-  function( x ){
    return(  1/2^x )
}

#creo el vector vacio donde voy a acumular
resultados <-  c()
#Lo siguiente es la instruccion  **for**
# Lo razón mas importante de existir de las computadoras es poder computar en donde se pueda repitir algo *muchas veces*
# Cuando está fija la cantidad de veces que se va a repetir algo, se utiliza la instruccion **for**

for( x in  1:4){
    y  <- potencia_invertida(x)
    resultados  <- c( resultados, y)
    #cat(resultados,"")
}

# Se observa como va creciendo el vector, agregando a cada paso el nuevo resultado

sum(resultados)

#################################

# El objetivo es implementar con un for loop la estimacion montecarlo, que promedia las ganancias

GananciaArbol  <-  function( semilla, data, x, train=0.70) {
      #establezco la semilla
      set.seed(semilla)
      train_rows <- createDataPartition(dataset$clase_ternaria, p= 0.70,list= FALSE)
      
      modelo <-  rpart::rpart(formula= "clase_ternaria ~ ." ,
                              data= data[ train_rows],  #los datos de training
                              xval= 0,
                              control= x)
      #Aplico el modelo a los datos de testing
      prediccion  <- predict( modelo, data[ -train_rows], type = "prob")
      prob_baja2  <- prediccion[, "BAJA+2"]
      ganancia_testing <- data[ -train_rows, sum(  (prob_baja2> 0.025) * ifelse( clase_ternaria=="BAJA+2", 78000, -2000) )]
      #normalizo la ganancia
      ganancia_testing_normalizada  <- ganancia_testing/0.3
      return( ganancia_testing_normalizada )
}

dataset <- fread("./datasets/competencia1_2022.csv")   #cargo el dataset #cargo el dataset
dataset <- dataset[ foto_mes==202101 ]  # me quedo solo con el periodo 202101

param  <- list("cp"= -0.5,
               "minsplit"=  900,
               "minbucket"= 440,
               "maxdepth"= 5)

semillas <- c(888809, 888827, 888857, 888869, 888887)
vector_ganancias  <- c()
for( semilla in semillas){
          ganancia  <- GananciaArbol( semilla, dataset, x=param, train=0.70 )
          vector_ganancias  <- c( vector_ganancias, ganancia)
}
# Calculo la media de  vector_ganancias
mean(vector_ganancias)


######################### 

# .15 Montecarlo Estimation, Arbol
## En una funcion
dataset <- fread("./datasets/competencia1_2022.csv")   #cargo el dataset #cargo el dataset\n",
dataset <- dataset[ foto_mes==202101 ]

semillas <- c(888809, 888827, 888857, 888869, 888887)
  
GananciaArbol  <-  function( semilla, data, x, train=0.70) {
  #establezco la semilla
  set.seed(semilla)
  train_rows <- createDataPartition(dataset$clase_ternaria, p= 0.70,list= FALSE)
  
  modelo <-  rpart::rpart(formula= "clase_ternaria ~ ." ,
                          data= data[ train_rows],  #los datos de training
                          xval= 0,
                          control= x)
  #Aplico el modelo a los datos de testing
  prediccion  <- predict( modelo, data[ -train_rows], type = "prob")
  prob_baja2  <- prediccion[, "BAJA+2"]
  ganancia_testing <- data[ -train_rows, sum(  (prob_baja2> 0.025) * ifelse( clase_ternaria=="BAJA+2", 78000, -2000) )]
  #normalizo la ganancia
  ganancia_testing_normalizada  <- ganancia_testing/0.3
  return( ganancia_testing_normalizada )
}

ArbolMontecarlo <- function( semillas, data, x, train=0.70){
        vector_ganancias <- c()  #vector donde voy a ir acumulando las ganancias
        for( semilla in semillas){
          ganancia  <- GananciaArbol( semilla, dataset, x=x, train=0.70 )
          vector_ganancias  <-  c( vector_ganancias, ganancia)
}
        return( mean( vector_ganancias))
}       
param  <- list("cp"= -0.5,
               "minsplit"=  900,
               "minbucket"= 440,
               "maxdepth"= 5)
# llamo a la fucion  ArbolMontecarlo con las 5 semillas de semillas
ganancia_montecarlo1  <- ArbolMontecarlo(semillas, dataset, x= param, train= 0.70 )
#el SEGUNDO set de hiperparametros  
param2  <- list("cp"= -0.5,
               "minsplit"=  1340,
               "minbucket"= 600,
               "maxdepth"= 6)
ganancia_montecarlo2  <- ArbolMontecarlo(semillas, dataset, x= param2, train= 0.70 )


# Segun este experimento, el primer juego de hiperparametros es mejor que el primero
# ¿Se cumplirá esto subiendo a Kaggle?
# ¿Qué sucede si en Kaggle funciona bastante mejor el segundo set de hiperparámetros?"

ganancia_montecarlo1 #20534667
ganancia_montecarlo2 # 19062667

##################################

# El objetivo es mostrar como funciona la 
# Optimizacion Bayesiana en el caso mas simple : una funcion univariada

# El uso del método de Optimización Bayesiana para la optimización de hiperparámetros 
# en modelos predictivos tiene menos de una década

#Entender los fundamentos de la O.B. requieren de una sólida formación matemática y **no** son el objetivo de esta asignatura.
#Para quienes deseen aventurarse a los detalles técnicos:
#* El paper original de la libreria mlrMBO (año 2018 )   https://arxiv.org/pdf/1703.03373.pdf\n",
#* El método de Kriging , tal cual se deriva del uso original, en este pequeño libro de 106 páginas 
#\"Basic Steps in Geostatistics: The Variogram and Kriging\" https://www.pdfdrive.com/basic-steps-in-geostatistics-the-variogram-and-kriging-e187336318.html"
#Ejemplo de optimizacion bayesiana, univariada

rm( list=ls() )  #remove all objects\n",
gc()             #garbage collection\n

dataset <- fread("./datasets/competencia1_2022.csv")


# Defino la funcion a optimizar, un polinomio de grado 4
func_univariada01  <- function( x ){
    y  <- -2 * (x+13) * (x-3) * (x-7) * (x-19)
    return( y )
}
# Grafico la funcion

intervalo  <- seq(-15,21,0.1)
plot(intervalo, func_univariada01(intervalo))
#La funcion tiene dos máximos, uno de ellos es el global
# Optimizacion Bayesiana. ¿Qué tan rápido encontrará la Optmización Bayesiana el máximo global?
# Defino las caracteristicas de la optimizacion

#fn    es el nombre de la funcion
#minimize   por defecto es TRUE,  pero en este caso le asigno FALSE, ya que deseo maximizar  el valor
#par.set  indica cuales son los hiperparmetros de la funcion, en este caso hay una sola variable que llamé  x
#makeNumericParam indica que ese hiperparámetro es un numero real, una variable continua  ( no es ni un entero ni una categoria )

require("DiceKriging")
require("mlrMBO")


obj.fun  <- makeSingleObjectiveFunction(
                  fn=       func_univariada01,
                  minimize= FALSE,   #estoy Maximizando la ganancia
                  par.set=  makeParamSet(  makeNumericParam( "x", lower= -100, upper=  100) )
)

# ahora defino la funcion proxy, la que se construye internamente intentando emular la realidad
#* **cl**  es la clase de learner,  \"reg.km\" indica el método de kriging \"**reg**ression **k**riging **m**ethodd\"
#"* **predict.type**  es el tipo de prediccion que deseo me devuelva, \"se\" significa que espero dos valores  media y standard error
#"* **covtype**    es la funcion de covarianza que se va a utilizar, cual es la covarianza de dos mediciones como fucion de la distancia 
# entre los puntos donde fueron tomadas las mediciones, fue inventada por Bertil Matérn

fproxy  <- makeLearner( cl= "regr.km",
                           predict.type= "se", 
                           covtype= "matern3_2" )


# ultima definicion,  especificar la optimizacion bayesiana\n",
# **crit**   indica el criterio con el que se completan los valores iniciales \"no inteligentes\"
# **iters**  indica la cantidad de iteraciones inteligentes que hará la Optimizacion Bayesiana, 
# las que son adicionales a las primeras cuatro de este caso.*  

ctrl  <- makeMBOControl()
ctrl  <- setMBOControlInfill( ctrl, crit= makeMBOInfillCritEI())
ctrl  <- setMBOControlTermination( ctrl, iters= 25 )
 
# finalmente , lanzo la Optimizacion Bayesiana

# **fun**  es la especificacion de la funcion que deseo optimizar, si maximizo o minimizo, cuales son las variables de la misma
# **learner**  especifica cual es la función proxy interna que va a utilizar la Optimziación Bayesiana
# **control**  indica la la forma en que se harán las iteraciones

run  <- mbo( 
          fun = obj.fun, 
          learner= fproxy, 
          control= ctrl)

tb_resultados  <- as.data.table( run$opt.path )
tb_resultados[ which.max( tb_resultados$y ) ]

#La gran pregunta es :  la Optimización Bayesiana, 
# ¿se focaliza luego de muchas iteraciones solo en el entorno del máximo que está cerca de x= -8 ?


###########################

# El objetivo es mostrar como funciona la Optimizacion Bayesiana 
# en el caso DOS variables y una superficie irregular

# carga data.table y las 2 del punto anterior


require("plotly")

# funcion a optimizar 
func_volcano  <- function( x ){
                z  <- volcano[ x$b, x$a ]
                return( z )
}

# Grafico el volcan

p <- plot_ly(z = volcano, type = "surface")
p

# La funcion tiene varios maximos locales
#  **fn**    es el nombre de la funcion
# *minimize**   por defecto es TRUE,  pero en este caso le asigno FALSE, ya que deseo maximizar  el valor
# *par.set**  indica cuales son los hiperparmetros de la funcion, en este caso hay una sola variable que llamé  x
# *makeIntegerParam** indica que ese hiperparámetro es un numero entero ( que NO es un numero real continuo )"


configureMlr( show.learner.output = FALSE)
obj.fun2  <- makeSingleObjectiveFunction(
              fn=       func_volcano,
              minimize= FALSE,   #estoy Maximizando la ganancia
              has.simple.signature = FALSE,  #porque tengo DOS dimensiones\n",
              par.set=  makeParamSet(  makeIntegerParam( "a", lower= 1, upper=  61),
                                       makeIntegerParam( "b", lower= 1, upper=  87))
)
                                             
# ahora defino la funcion proxy, la que se construye internamente intentando emular la realidad

fproxy2  <- makeLearner( cl= "regr.km",
                        predict.type= "se",
                        covtype= "matern3_2" )
#especifico opt bayesiana

ctrl  <- makeMBOControl()
ctrl  <- setMBOControlInfill( ctrl, crit= makeMBOInfillCritEI())
ctrl  <- setMBOControlTermination( ctrl, iters= 20 )

# opt bayesiana
run  <- mbo( fun= obj.fun2,
             learner= fproxy2, 
             control= ctrl )

tb_resultados  <- as.data.table( run$opt.path )
tb_resultados[ which.max( tb_resultados$y ) ]










  