
require("lubridate")
require("lhs")
require("DiceKriging")
require("mlrMBO") 


##########################################################################################
# Ahora hacer lo mismo pero con todo el dataset: Hay que modificar:

ganancia <- function(probabilidades, clase) {		
  return(sum(		  
    (probabilidades >= 0.025) * ifelse(clase == "evento", 78000, -2000)) ######
  )		  
}


# Una función auxiliar para los experimentos
experimento_rpart2 <- function(ds, semillas, cp = -1, ms = 20, mb = 1, md = 10) {
  gan <- c()
  for (s in semillas) {
    set.seed(s)
    in_training <- caret::createDataPartition(ds$clase_binaria, p = 0.70,######
                                              list = FALSE)
    train  <-  ds[in_training, ]
    test   <-  ds[-in_training, ]
    #train_sample <- tomar_muestra(train)
    r <- modelo_rpart2(train, test, 
                       cp = cp, ms = ms, mb = mb, md = md)
    gan <- c(gan, r)
  }
  mean(gan)
}


# Armamos una función para modelar con el fin de simplificar el código futuro
modelo_rpart2 <- function(train, test, cp =  0, ms = 20, mb = 1, md = 10) {
  modelo <- rpart(clase_binaria ~ ., data = train, ######
                  xval = 0,
                  cp = cp,
                  minsplit = ms,
                  minbucket = mb,
                  maxdepth = md)
  
  test_prediccion <- predict(modelo, test, type = "prob")
  ganancia(test_prediccion[, "evento"], test$clase_binaria)#######
}

############################################################
#Ahora corremos 11 cambiado con ganancia

set.seed(semillas[1])
obj_fun_md_ms_mb <- function(x) {
  experimento_rpart2(dtrain, semillas
                     , md = x$maxdepth
                     , ms = x$minsplit
                     , mb = floor(x$minbucket * x$minsp) )
}

obj_fun <- makeSingleObjectiveFunction(
  minimize = FALSE,
  fn = obj_fun_md_ms_mb,
  par.set = makeParamSet(
    makeIntegerParam("maxdepth",  lower = 5L, upper = 6L),
    makeIntegerParam("minsplit",  lower = 50L, upper = 150L),
    makeNumericParam("minbucket", lower = 150L, upper = 300L)
    
    # makeNumericParam <- para parámetros continuos
  ),
  noisy = TRUE,
  has.simple.signature = FALSE
)

ctrl <- makeMBOControl()
ctrl <- setMBOControlTermination(ctrl, iters = 10L) #Aumentó a 30 porque sí, para aumentar el espacio de búsqueda. Luego veremos cómo hacerlo
ctrl <- setMBOControlInfill(
  ctrl,
  crit = makeMBOInfillCritEI(),
  opt = "focussearch",
  # sacar parámetro opt.focussearch.points en próximas ejecuciones
  opt.focussearch.points = 20
)

lrn <- makeMBOLearner(ctrl, obj_fun)

surr_km <- makeLearner("regr.km", predict.type = "se", covtype = "matern3_2")

run_md_ms <- mbo(obj_fun, learner = surr_km, control = ctrl, )
print(run_md_ms)