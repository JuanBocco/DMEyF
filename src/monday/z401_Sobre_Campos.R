##
## Sobre Campos
##
## ---------------------------
## Step 1: Cargando los datos y las librerías
## ---------------------------
##
## Genius is one percent inspiration and 99 percent perspiration
## --- ~~Thomas Edison~~ Kate Sanborn

# Limpiamos el entorno
rm(list = ls())
gc(verbose = FALSE)

# Librerías necesarias
require("data.table")
require("rpart")
require("ggplot2")
require("dplyr")

# Poner la carpeta de la materia de SU computadora local
setwd("/Users/angus/Desktop/Maestria/DM_EyF")
# Poner sus semillas
semillas <- c(888809, 888827, 888857, 888869, 888887)

# Cargamos el dataset
dataset <- fread("./datasets/competencia1_2022.csv")

# Nos quedamos solo con el 202101
dataset <- dataset[foto_mes == 202101]

# Creamos una clase binaria
dataset[, clase_binaria := ifelse(
                            clase_ternaria == "BAJA+2",
                                "evento",
                                "noevento"
                            )]
# Borramos el target viejo
dataset[, clase_ternaria := NULL]

set.seed(semillas[4])

# Particionamos de forma estratificada
in_training <- caret::createDataPartition(dataset$clase_binaria,
                     p = 0.70, list = FALSE)
dtrain  <-  dataset[in_training, ]
dtest   <-  dataset[-in_training, ]

calcular_ganancia <- function(modelo, test) {
    pred_testing <- predict(modelo, test, type = "prob")
    sum(
        (pred_testing[, "evento"] >= 0.025) * ifelse(test$clase_binaria == "evento",
                                         78000, -2000) / 0.3
    )
}

## ---------------------------
## Step 2: Importancia de variables
## ---------------------------

# Antes de empezar vamos a ver la importancia de variables
modelo <- rpart(clase_binaria ~ .,
                data = dtrain,
                xval = 0,
                cp = -1,
                minsplit = 110,
                minbucket = 440,
                maxdepth = 5)

calcular_ganancia(modelo, dtest)

print(modelo$variable.importance)

# numero de cliente es un id pero en la practica marca la antiguedad

# es una variable clave porque sino la pongo, se aprende los clientes de memoria

## Preguntas
## - ¿Cuáles son las variables más importantes para el modelo?
## - ¿Cómo calcula RPART la importancia de una variable?
## - ¿Es la única forma de calcular la importancia de una variable?

## ---------------------------
## Step 2: Datos nulos
## ---------------------------

# En el summary del modelo buscamos un corte donde la primera variable
# tenga missing.

summary(modelo)

## Preguntas
## - ¿Cómo operó con la variable nula?
## - ¿Hace falta imputar las variables para que el árbol abra?

# Cuando se dan los nulos? Errores de sistema, o a veces me sirve que sea nulo. Como en una encuesta
# que pregunte cantidad de hijos pongas 0 y te diga cuantos años tiene

# Redes neuronales y vecinos cercanos (knn) no perrmiten datos faltantes

# La regresión logistica no tolera datos faltantes

# arboles pueden tolerar

# como funciona el rpart para las variables missing?

# usa variables subrogadas, variables con estructura de coerte similar a otra variable. Tengo un arbol
# no puedo escribir la variable, aca tenemos primary split y su corresponfiente subrrogada
# Cada una de las subrogadas responde a la variable primaria en el mismo orden
# los datos faltantes usan la variable subrrogada para decidir si van a la izquierda o a la derecha (pensarlo como 2 ramas de un arbol)
# la smiilitud es determinada por el corte, es decir, separa y creea una variable que explique la separacion
# lo hace para mantener la separacion en la rama

## - ¿Hace falta imputar las variables para que el árbol abra?
# no es imprescindible 


## ---------------------------
## Step 3: Datos nulos - Metiendo mano
## ---------------------------

# Numero de nulos en variable Visa_fechaalta
print(sum(is.na(dtrain$Visa_fechaalta)))


# Imputamos los nulos de nuestra variable con ceros
dtrain[, Visa_fechaalta_2 := ifelse(is.na(Visa_fechaalta), 
            0,
            Visa_fechaalta)] 

print(sum(is.na(dtrain$Visa_fechaalta2)))

# mejora con 0 y con -1

# Chequeamos el número de nulos de la nueva variable
print(sum(is.na(dtrain$Visa_fechaalta_2)))

# Comparamos las estadísticas de ambas variables
summary(dtrain$Visa_fechaalta)
summary(dtrain$Visa_fechaalta_2)

# Hacemos un modelo sin la variable vieja
modelo2 <- rpart(clase_binaria ~ . - Visa_fechaalta,
                data = dtrain,
                xval = 0,
                cp = -1,
                minsplit = 20,
                minbucket = 10,
                maxdepth = 5)



print(modelo2$variable.importance)

# Para calcular la ganancia hay que agregar la variable a test
dtest[, Visa_fechaalta_2 := ifelse(is.na(Visa_fechaalta), 
            0,
            Visa_fechaalta)] 

calcular_ganancia(modelo2, dtest)

## Preguntas
## - ¿Desde el punto de vista de la importancia de variable, después que se 
##   imputo, pasó a ser más o menos importante?

# problema de media es que es susceptible a los outliers

## ---------------------------
## Step 4: Datos nulos - Metiendo mano, una vez más
## ---------------------------

mean_Visa_fechaalta <- mean(dtrain$Visa_fechaalta, na.rm = T)
# Imputamos los nulos de nuestra variable con la media
dtrain[, Visa_fechaalta_3 := ifelse(is.na(Visa_fechaalta), 
            mean_Visa_fechaalta,
            Visa_fechaalta)] 

dtest[, Visa_fechaalta_3 := ifelse(is.na(Visa_fechaalta), 
            mean_Visa_fechaalta,
            Visa_fechaalta)] 

# Hacemos un modelo sin la variable vieja
modelo3 <- rpart(clase_binaria ~ . - Visa_fechaalta - Visa_fechaalta_2,
                data = dtrain,
                xval = 0,
                cp = -1,
                minsplit = 20,
                minbucket = 10,
                maxdepth = 5)

print(modelo3$variable.importance)
calcular_ganancia(modelo3, dtest)

## Preguntas
## - ¿Son muchos los casos nulos? Es bajo en terminos relativos, 5 mil del 100k (70% de 160k)

## - En mi caso aparenta una mejora, con más casos cree que esa mejora se
##   mantendría 
## - ¿Existe otro valor mejor que la media para imputar?

# reemplazarlo por el valor optimo ? 
# para saberlo hay que hacer un experimento:
# para cada conjunto de train, para cada semilla, armar su media, imputarla en test, sacar el modelo, su ganancia y distribucion
# y ver como funciona, luego obtengo un parametro de ajuste

## ---------------------------
## Step 5: Datos nulos - Midiendo bien
## ---------------------------

## Actividad para medir bien la influencia de la media en de esa variable, 
## escriba una función de experimento que refleje la transformación  

experimento <- function() {
    gan <- c()
    for (s in semillas) {
        set.seed(s)
        in_training <- caret::createDataPartition(dataset$clase_ternaria, p = 0.70,
            list = FALSE)
        train  <-  dataset[in_training, ]
        test   <-  dataset[-in_training, ]
        
        mean_Visa_fechaalta <- mean(train$Visa_fechaalta, na.rm = T)
        # Imputamos los nulos de nuestra variable con la media
        train[, Visa_fechaalta_3 := ifelse(is.na(Visa_fechaalta), 
                                            mean_Visa_fechaalta,
                                            Visa_fechaalta)] 
        
        test[, Visa_fechaalta_3 := ifelse(is.na(Visa_fechaalta), 
                                           mean_Visa_fechaalta,
                                           Visa_fechaalta)] 

        r <- rpart(clase_ternaria ~ .,
                    data = train,
                    xval = 0,
                    cp = -1,
                    minsplit = 20,
                    minbucket = 10,
                    maxdepth = 5)

        gan <- c(gan, calcular_ganancia(r, test))
    }
    mean(gan)
}

experimento()

# saaco media de train y lo imputo a trsin y test y luego lo hago para cada semilla

# Veamos la 
## Preguntas
## - ¿Qué sucede si una transformación que depende del dataset no se aplica de
##   esta manera?
## - A como funciona el rpart ¿Qué decisión toma sobre esta variable?

## ---------------------------
## Step 6: Correlaciones
## ---------------------------

# cuando son perfectamente correrlacionadas, no se puede sacar el determinante de las matrices por ser linealmente dependientes
# scando errores, es casi imposible tener una variable perfectamente correlacionada
# matrices malcondicionadas me dan numeros raros, no son modelos que soportan bien la correlacion entre variables

# arboles: 2 variables correlacionadas, va a elegir ese comportamiento, es como doble data
# al abrir una variable, la otra pierde sentido, no se ven afectados por la correlacion


# Veamos la correlación entre las dos variables previas construidas
cor(dtrain$Visa_fechaalta_2,dtrain$Visa_fechaalta_3)

# correlacion casi perfecta ya que solo difieren en el valor de los nulos

# Varios modelos en los que entren dos variables muy correlacionadas se 
# romperían. Veamos que pasa con los árboles

modelo4 <- rpart(clase_binaria ~ . ,
                data = dtrain,
                xval = 0,
                cp = -1,
                minsplit = 20,
                minbucket = 10,
                maxdepth = 5)
calcular_ganancia(modelo4, dtest)

# me abre la variable con esa ganancia


## Preguntas
## - ¿Por qué no empeora el modelo cuándo metemos variables correlacionadas?


## ---------------------------
## Step 5: Outliers
## ---------------------------

## afectan fuertemente a las regresiones
## GLM som todas funciones lineales generalizadas

# para arboles no deberia afectar demasiado es por ello que los arboles trabajan tan bien
# ya que la mayoria de las variables cuentan von outliers


# Veamos el boxplot de una variable muy importante según nuestro árbol
ggplot(dtrain, aes(x=ctrx_quarter)) + geom_boxplot()

# Vemos la distribución de los deciles
quantile(dtrain$ctrx_quarter, probs = c(0,0.5, 0.75, 0.9, 0.95, 0.99, 1))

## Preguntas
## - ¿Qué tan frecuentes considera estas dispersiones en los datasets?

## ---------------------------
## Step 6: Outliers - Luchando 
## ---------------------------
log(9.5 + 1)

# No importa el espacio, la distancia de los puntos, importa el orden. La regresion logistica se la banca




## Preguntas
## - Mirando los puntos de corte de los dos modelos ¿Existe una relación
##   matermática entre ellos?
## - ¿Es útil una transformación monótona en los árboles de decisión?

## ---------------------------
## Step 7: Outliers - Una más y no jodemos más 
## ---------------------------

dtrain[, ctrx_quarter_2 := ntile(ctrx_quarter, 10)]
dtest[, ctrx_quarter_2 := ntile(ctrx_quarter, 10)]

modelo_cq_4 <- rpart(clase_binaria ~ . - ctrx_quarter - ctrx_quarter_2 - Visa_fechaalta_2 - Visa_fechaalta_3,
                    data = dtrain,
                    xval = 0,
                    cp = -1,
                    minsplit = 20,
                    minbucket = 10,
                    maxdepth = 5)

calcular_ganancia(modelo_cq_4, dtest)

# rankeo, decilar (bins), cortarlo en 10
# al primer decil le pones 10
# el arbol corta mas rapido porque tiene menos cortes, predice mejor generalmente

# drifting \\ rankear ayuda al datadrifting

# inflacion, variables relacionadas como salario. La persona que esta mas arriba de todas 

# las variables mas importantes requieren ser revisadas con este criterio
# en algunos casos no tiene sentido, como cantidad de prestramos hipotecarios
# entonces tenes que transformar la variable


## Actividad: Para mi semilla, esta estrategia es buena, hacer un experimento
## donde no quede al azar este resultado.

## ---------------------------
## Step 8: Un poco de R, como procesar multiples variables con una técnica 
## ---------------------------

# Transformación masiva

# Supongamos que tenemos una lista de variables a las que queremos transformar
mis_variables <- c("ctrx_quarter",
                    "mprestamos_personales",
                    "mcuentas_saldo",
                    "mactivos_margen",
                    "mcaja_ahorro",
                    "mcuenta_corriente")

# A todas las vamos a rankear

prefix <- "r_"
for (var in mis_variables) {
    dtrain[, (paste(prefix, var, sep = "")) := ntile(get(var), 20)]
    dtest[, (paste(prefix, var, sep = "")) := ntile(get(var), 20)]
}

dtrain

## ---------------------------
## Step 9: Un + poco de R, seleccionar las variables para modelar 
## ---------------------------

# Las mejores más la variables rankeadas
mis_variables_2 <- c("r_ctrx_quarter",
                    "active_quarter",
                    "r_mprestamos_personales",
                    "cprestamos_personales",
                    "r_mactivos_margen",
                    "r_mcuentas_saldo",
                    "ccomisiones_otras",
                    "r_mcuenta_corriente",
                    "cdescubierto_preacordado") 

campos <- paste(mis_variables_2, collapse = " + ")
formula <- paste0( "clase_binaria ~ ", campos )

modelo5 <- rpart(formula,
                    data = dtrain,
                    xval = 0,
                    cp = -1,
                    minsplit = 20,
                    minbucket = 10,
                    maxdepth = 5)

print(modelo5$variable.importance)
calcular_ganancia(modelo5, dtest)

# probar quitandole la variable mas influyente

## ---------------------------
## Step 10: Embeddings (Caseros)
## ---------------------------

# hay un espacio de dimension n
# por ejemplo PCA busca reducir el spacio
# T-SNE JMAP # scikit-learn embeddings 
# sistema de reduccion de dimensiones para visualizaciones

# embedding busca transformar un espacio

# Hagamos interactuar algunas variables para ver si conseguimos alguna mejor
nuevas <- c()
for (var1 in mis_variables_2) {
    for (var2 in mis_variables_2) {
        if (var1 != var2) {
            nueva <- paste(var1, var2, sep = "___")
            dtrain[, (nueva) := get(var1) * get(var2)]
            dtest[, (nueva) := get(var1) * get(var2)]
            nuevas <- c(nuevas, nueva)
        }
    }
}

# esto genera una nueva variable a partir de otra variable, facil, casero


mis_variables_3 <- c(nuevas, mis_variables_2) 

campos2 <- paste(mis_variables_3, collapse = " + ")
formula2 <- paste0( "clase_binaria ~ ", campos2 )

modelo6 <- rpart(formula2,
                    data = dtrain,
                    xval = 0,
                    cp = -1,
                    minsplit = 20,
                    minbucket = 10,
                    maxdepth = 5)

print(modelo6$variable.importance)

# Importante: Que una modelo tenga otras variables importantes no implicar que
# sea mejor, ni peor. Eso se debe evaluar con los experimentos

##
## TAREA: Multiples experimentos. Un script por cada uno que debe incluir:
## - Feature engineering correctamente aplicado
## - Opt Bayesiana para el dataset que se incluya nuevas variables
## - Scorear en los datos de marzo y subir a kaggle el score.

## Feature engineering tips

# preguntarle a la gente que conoce del dominio

# universo de tarjetas de credito, ejemplo de features
# cuantas tarjetas tiene
# tiene al menos 1 
# en que estado estan las tarjetas
# que saldo tienen, consumos limites, moneda


#ver limites ratio contra sueldo. 
# limites contra consumo
# es indistinto usar suma u otra cosa


# despues de elegir columnas, hacerle la optimizacion bayesiana

# hacer las nuevas varriables con la parte de marzo, ajustar con la opt bayesiana
# al ppio usar pocas variables, 50 deberia ser una hora
# armar un script con esos parametros, generar features sobre marzo
# aplicar aal train de enero y modelar








# 