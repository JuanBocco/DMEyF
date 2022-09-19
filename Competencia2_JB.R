# Modelo definitivo

require("data.table")
require("rpart")
require("rpart.plot")
require("ggplot2")
require("dplyr")
require("ranger")
require("randomForest")
require("lightgbm")


# Limpiamos el entorno
rm(list = ls())
gc(verbose = FALSE)

# Poner sus semillas
semillas <- c(888809, 888827, 888857, 888869, 888887)

# Cargamos los datasets y nos quedamos solo con 202101 y 202103
dataset <- fread("./datasets/competencia2_2022.csv.gz")
#enero <- dataset[foto_mes == 202101]
marzo <- dataset[foto_mes == 202103]
dapply <- dataset[foto_mes == 202105]

# dd <- dataset[foto_mes==202101 | foto_mes==202102 | foto_mes==202103, ]
# 
# numericas_dd <- dd[,sapply(dd,is.numeric),with=FALSE]
# 
# column_names <- list(colnames(numericas_dd))
# 
# prueba <- numericas_dd[, .(mean(mcuentas_saldo)), by = foto_mes]
# 
# for (i in column_names){
#   ans <- numericas_dd[, lapply(colMeans(numericas_dd[,i], na.rm = TRUE), by=.(foto_mes))]
#   
# }
# 
# ans <- numericas_dd[, colMeans(numericas_dd, na.rm = TRUE), by=.(foto_mes)]
# 
# ans




## ans <- dataset[foto_mes == "202101" & ]

### es importante 
# Importante que la clase sea factor
marzo[, clase_binaria := factor(ifelse(
  clase_ternaria == "BAJA+2",
  "evento",
  "noevento"
))]

marzo$clase_ternaria <- NULL
set.seed(semillas[5])

in_training <- caret::createDataPartition(marzo$clase_binaria,
                                          p = 0.70, list = FALSE)

dtrain  <-  marzo[in_training, ]
dtest   <-  marzo[-in_training, ]

# no soporta missing values. Entonces los imputamos
# ranger no soporta, como lo hacen otras librerías, los missing values, los imputa con la mediana
dtrain <-  na.roughfix(dtrain)
dtest <-  na.roughfix(dtest)

# Cantidad de variables que abren por cada hoja
n_variables <- round(sqrt(dim(dtrain)[2] - 1))



# necesitamos que sean profundos pero no entrenan con todos los datos
# criterio random forest para abrir nodo > no usa todas las variebles, elige solo algunas
# si tiene 100 variables, le hace la raiz cuadrada y toma la decision en base a 10

# rf tiene 2 parametros importantes, la cantidad de variables y la cantidad de arboles, 
# hay que tener cuidado con la alta correlación en las variables. debido a la decision de cada nodo
# siempre sacar variables se hace en post de la eficiencia mas que en torno a mejorar el resultado del modelo

#


t0 <- Sys.time()
modelo_rf_1 <- ranger(clase_binaria ~ ., data = dtrain,
                      probability = TRUE,
                      num.trees = 100,
                      min.node.size=10,
                      mtry = n_variables,
                      splitrule = "gini",
                      sample.fraction = 0.66,
                      importance = "impurity",
                      verbose = TRUE)
t1 <- Sys.time()
as.numeric(t1 - t0, units = "secs")

# corta por gini


## ---------------------------
## Step 3: Midiendo el primero RF
## ---------------------------

pred_train <- predict(modelo_rf_1, dtrain)
pred_test <- predict(modelo_rf_1, dtest)

# Ganancia en dtrain
print(sum((pred_train$predictions[, "evento"] >= 0.025) * ifelse(
  dtrain$clase_binaria == "evento",
  78000, -2000) / 0.7))
# Ganancia en dtest
print(sum((pred_test$predictions[, "evento"] >= 0.025) * ifelse(
  dtest$clase_binaria == "evento",
  78000, -2000) / 0.3))

## Preguntas
## - ¿Qué paso en `train`?
## - ¿Se veía esa diferencia en los árboles?

## ---------------------------
## Step 4: Importancia de variables
## ---------------------------

importancia <- as.data.table(modelo_rf_1$variable.importance,
                             keep.rownames = TRUE)
colnames(importancia) <- c("variable", "importancia")
setorder(importancia, -importancia)
importancia


#### las variables con importancia cero pueden ser importantes en un modelo futuro
# sobre todo si tengo muchos arboles

## Preguntas
## - ¿Qué significa que una variable sea más importante que otra?
## - ¿Qué significa que una variable tenga 0 importancia?
## - ¿Con el **RF** es suficiente como para descartarlas?
## - ¿Qué una variable tenga algo de importancia es suficiente como para
## - entender que da valor?

## ---------------------------
## Step 5: Un experimento con pollitos
## ---------------------------

dtrain$pollito <- runif(nrow(dtrain))

modelo_rf_2 <- ranger(clase_binaria1 ~ ., data = dtrain,
                      probability = TRUE,
                      num.trees = 150,
                      min.node.size = 10, # <---------
                      mtry = n_variables,
                      splitrule = "gini",
                      importance = "impurity",
                      verbose = TRUE)

importancia2 <- as.data.table(modelo_rf_2$variable.importance,
                              keep.rownames = TRUE)
colnames(importancia2) <- c("variable", "importancia")
setorder(importancia2, -importancia)
importancia2
which(importancia2$variable == "pollito")


# porque ? no se entiende bien, seguro que overfitea, los nodos de bajo tamaño tambien se aplica
# como las impportancias son greedy, solo cuentan los exitos, aparece arriba



## Active learning o a llorar a la iglesia.

## ---------------------------
## Step 5.1: Hablando de los Extra Trees
## ---------------------------

modelo_rf_3 <- ranger(clase_binaria1 ~ ., data = dtrain,
                      probability = TRUE,
                      num.trees = 150,
                      min.node.size = 1000, # <---------
                      mtry = n_variables,
                      splitrule = "extratrees", # <---------
                      num.random.splits = 10, # <---------
                      importance = "impurity",
                      verbose = TRUE)

importancia3 <- as.data.table(modelo_rf_3$variable.importance,
                              keep.rownames = TRUE)
colnames(importancia3) <- c("variable", "importancia")
setorder(importancia3, -importancia)
importancia3
which(importancia3$variable == "pollito")



### hacenmas random los RF
# se toma un % de la variables (raiz cuadrada) y luego arma un corte de manera estricta
# esta gente decide hacerlo de manera aleatoria


## ---------------------------
## Step 6: Boosting, la navaja suiza de los modelos - Conceptos

# agarra un arbol. ve como predice las clases, si una clase da 0.85, agarra y mira el residuo y entonces 
# busca pasarle al proximo arbol el dato y que lo corrija.

## ---------------------------

# Estos se construyen de forma serial.
# Primero se parte de un modelo (que puede ser un valor constante) y se
# complementa con un modelo que busca mejorar al anterior.

# Hay dos algoritmos muy conocidos de este tipo:

# **Adaboost**: Que cada nuevo modelo va mejorando a los anteriores poniendo un
# peso mayor en los casos donde la clasificación es incorrecta

# **Gradient Boosting**: Que cada nuevo modelo va mejorando los anteriores,
# tratando de corregir los residuos, buscando estos últimos con el gradiente
# de una función de perdida.

# Este último se empezó a hacer muy popular por la excelente pieza de tecnología
# que es su implementación **xgboost**, superado luego por el LightGBM.

## ---------------------------
## Step 7: LightGBM
## ---------------------------

# Cargamos todo para tener un código limpio
dataset <- fread("./datasets/competencia2_2022.csv.gz")


dataset2 <- dataset
dataset2[is.na(dataset2), ] <- 0

print(sum(is.na(dataset2$Visa_fechaalta)))

dataset <- dataset2



###########################################################################
###########################################################################

enero <- dataset[foto_mes == 202101]
marzo <- dataset[foto_mes == 202103]
mayo <- dataset[foto_mes == 202105]


rm(dataset)

clase_binaria <- ifelse(marzo$clase_ternaria == "BAJA+2", 1, 0)
marzo$clase_ternaria <- NULL

###########################################################################
###########################################################################



dtrain  <- lgb.Dataset(data = data.matrix(marzo), label = clase_binaria)

ganancia_lgb <- function(probs, datos) {
  return(list("name" = "ganancia",
              "value" =  sum( (probs > 0.025) *
                                ifelse(getinfo(datos, "label") == 1, 78000, -2000)) / 0.2,
              "higher_better" = TRUE))
}

set.seed(semillas[1])
# LightGBM, al igual que XGB traen su implementación del CValidation
# Los parámetros los iremos viendo en profundidad la siguiente clase.
model_lgbm_cv <- lgb.cv(data = dtrain,
                        eval = ganancia_lgb,
                        stratified = TRUE,
                        nfold = 5,
                        param = list(objective = "binary",
                                     max_bin = 15,
                                     min_data_in_leaf = 4000,
                                     learning_rate = 0.05
                        )
)

# Mejor iteración
model_lgbm_cv$best_iter

# Ganancia de la mejor iteración
unlist(model_lgbm_cv$record_evals$valid$ganancia$eval)[model_lgbm_cv$best_iter]

# Una vez que elegimos los parámetros tenemos que entrenar con todos.
model_lgm <- lightgbm(data = dtrain,
                      nrounds = model_lgbm_cv$best_iter, # <--- OJO! Double Descent alert
                      params = list(objective = "binary",
                                    max_bin = 15,
                                    min_data_in_leaf = 4000,
                                    learning_rate = 0.05),
                      verbose = -1)

# También tiene su importancia de variables
lgb.importance(model_lgm, percentage = TRUE)


## ---------------------------
## Step 8: En Mayo
## ---------------------------

pred <- predict(model_lgm, data.matrix(mayo[, 1:154]))


# Sobre 100 LB
leaderboad <- data.table()
set.seed(semillas[1])
for (i in 1:100) {
  split <- caret::createDataPartition(marzo$clase_ternaria,
                                      p = 0.70, list = FALSE)
  privado <- sum((marzo$pred[split] > 0.025) *
                   ifelse(marzo$clase_ternaria[split] == "BAJA+2", 78000, -2000)) / 0.7
  publico <- sum((marzo$pred[-split] > 0.025) *
                   ifelse(marzo$clase_ternaria[-split] == "BAJA+2", 78000, -2000)) / 0.3
  leaderboad <- rbindlist(list(leaderboad,
                               data.table(privado = privado, publico = publico)))
}

# Comparar con la salida del árbol
summary(leaderboad)


#prob_baja2  <- pred[, "evento"] ########## evento #BAJA+2
Predicted   <- ifelse( pred > 0.025, 1, 0 )  # 0.025 es la probabilidad de corte
entrega  <-  as.data.table( list( "numero_de_cliente"=mayo$numero_de_cliente, 
                                  "Predicted"=Predicted)  )
entrega[ , .N, Predicted]

dir.create( "./exp/KAGGLE2/", showWarnings = FALSE )

fwrite( entrega,
        file= "./exp/KAGGLE2/comp2_002.csv",
        sep=  "," )

