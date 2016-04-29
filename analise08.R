# -------------------------------------------------------------------------------------
# Thomio Watanabe
# Ciência de dados
# Abril 2016
# USP ICMC

# KDD - Knowledge Discovery in Database
# Etapas do KDD
# 1 - Seleção
# 2 - Pre-processamento e limpeza
# 3 - Transformação
# 4 - Mineração de dados
# 5 - Avaliação
# -------------------------------------------------------------------------------------
# Subfunctions
# -------------------------------------------------------------------------------------
# Obtido de https://www.kaggle.com/wiki/LogarithmicLoss
# Modificação: nrow -> length
# MultiLogLoss <- function(act, pred)
#    {
#      eps = 1e-15;
#      nr <- nrow(pred)
#      pred = matrix(sapply( pred, function(x) max(eps,x)), nrow = nr)      
#      pred = matrix(sapply( pred, function(x) min(1-eps,x)), nrow = nr)
#      ll = sum(act*log(pred) + (1-act)*log(1-pred))
#      ll = ll * -1/(nrow(act))      
#      return(ll);
#    }

MultiLogLoss <- function(act, pred)
{
    eps = 1e-15;
    nr <- length(pred)
    pred = matrix(sapply( pred, function(x) max(eps,x)), nrow = nr)
    pred = matrix(sapply( pred, function(x) min(1-eps,x)), nrow = nr)
    ll = sum(act*log(pred) + (1-act)*log(1-pred))
    ll = ll * -1/(length(act))
    return(ll);
}

min_function <- function(par,data){
    lower_threshold <- par[1]
    upper_threshold <- par[2]
    lower_values <- which(prediction <= lower_threshold)
    upper_values <- which(prediction >= upper_threshold)
    min_test <- prediction
    min_test[lower_values] <- 0.0
    min_test[upper_values] <- 1.0
    return (MultiLogLoss(data,min_test))
}

setThreshold <- function(th,prediction){
    neg <- which( prediction < th  )
    prediction[neg] <- 0
    pos <- which( prediction >= th  )
    prediction[pos] <- 1
    return (prediction)
}
# -------------------------------------------------------------------------------------

# Carrega a tabela de treinamento
table_name <- 'test_sampled.csv'
cat( paste("Carregando tabela de treino: ",table_name) )
test_table <- read.csv(table_name)


# Divide a tabela de treinamento em duas
total_samples <- length(test_table$target)
samples <- 1:total_samples
num_samples <- round( 0.5 * total_samples )
test_samples <- sample( samples, size = num_samples )

test_table_01 <- test_table[ test_samples, ]
test_table_02 <- test_table[ -test_samples, ]


# Carrega modelo do randomForest
rf <- readRDS("model_randomForest1000Trees.rds")   # Erro -> 0.167809
# rf <- readRDS("model_randomForest3000Trees.rds")    # Erro -> 0.167793

library(randomForest)
prediction_01 <- predict(rf, test_table_01)
prediction_02 <- predict(rf, test_table_02)


# Calcula o log loss das duas tabelas e mostra o resultado
log_loss_01 <- MultiLogLoss(test_table_01$target,prediction_01)
cat( paste("Log loss tabela de test 01: ",log_loss_01, '\n') )     # Erro -> 

log_loss_02 <- MultiLogLoss(test_table_02$target,prediction_02)
cat( paste("Log loss tabela de test 02: ",log_loss_02, '\n') )     # Erro -> 


# Análise utilizando um threshold
th <- 0.5
cat( paste("Set target threshold to ", th, '\n') )
biased_prediction_01 <- setThreshold(th, prediction_01)
biased_prediction_02 <- setThreshold(th, prediction_02)

log_loss_01 <- MultiLogLoss(test_table_01$target, biased_prediction_01)
cat( paste("Log loss tabela de test biased 01: ", log_loss_01, '\n') )     # Erro -> 8.09661750737087

log_loss_02 <- MultiLogLoss(test_table_02$target, biased_prediction_02)
cat( paste("Log loss tabela de test biased 02: ", log_loss_02, '\n') )     # Erro -> 8.20383177379695


# Calculate the confusion matrix
library(caret)
confusionMatrix(biased_prediction_01, test_table_01$target)
confusionMatrix(biased_prediction_02, test_table_02$target)

# RESULT: th = 0.5
#          Reference
#Prediction     0     1
#         0   932   692
#         1  3990 14359
#                                          
#               Accuracy : 0.7656          
#                 95% CI : (0.7596, 0.7714)
#    No Information Rate : 0.7536          
#    P-Value [Acc > NIR] : 3.856e-05       
#                                          
#                  Kappa : 0.1851          
# Mcnemar's Test P-Value : < 2.2e-16       
#                                          
#            Sensitivity : 0.18935         
#            Specificity : 0.95402         
#         Pos Pred Value : 0.57389         
#         Neg Pred Value : 0.78255         
#             Prevalence : 0.24643         
#         Detection Rate : 0.04666         
#   Detection Prevalence : 0.08131         
#      Balanced Accuracy : 0.57169         
#                                          
#       'Positive' Class : 0               
#                                          
#Confusion Matrix and Statistics

#          Reference
#Prediction     0     1
#         0   938   771
#         1  3973 14291
#                                          
#               Accuracy : 0.7625          
#                 95% CI : (0.7565, 0.7684)
#    No Information Rate : 0.7541          
#    P-Value [Acc > NIR] : 0.003024        
#                                          
#                  Kappa : 0.1792          
# Mcnemar's Test P-Value : < 2.2e-16       
#                                          
#            Sensitivity : 0.19100         
#            Specificity : 0.94881         
#         Pos Pred Value : 0.54886         
#         Neg Pred Value : 0.78247         
#             Prevalence : 0.24588         
#         Detection Rate : 0.04696         
#   Detection Prevalence : 0.08557         
#      Balanced Accuracy : 0.56991         
#                                          
#       'Positive' Class : 0  




# Análise utilizando um threshold
th <- 0.43
cat( paste("Set target threshold to ", th, '\n') )
biased_prediction_01 <- setThreshold(th, prediction_01)
biased_prediction_02 <- setThreshold(th, prediction_02)

log_loss_01 <- MultiLogLoss(test_table_01$target, biased_prediction_01)
cat( paste("Log loss tabela de test biased 01: ", log_loss_01, '\n') )     # Erro ->  8.09663207971676

log_loss_02 <- MultiLogLoss(test_table_02$target, biased_prediction_02)
cat( paste("Log loss tabela de test biased 02: ", log_loss_02, '\n') )     # Erro ->  8.14159174530331


# Calculate the confusion matrix
confusionMatrix(biased_prediction_01, test_table_01$target)
confusionMatrix(biased_prediction_02, test_table_02$target)


# RESULT: th = 0.43
#          Reference
#Prediction     0     1
#         0   568   328
#         1  4354 14723
#                                          
#               Accuracy : 0.7656          
#                 95% CI : (0.7596, 0.7714)
#    No Information Rate : 0.7536          
#    P-Value [Acc > NIR] : 3.856e-05       
#                                          
#                  Kappa : 0.1292          
# Mcnemar's Test P-Value : < 2.2e-16       
#                                          
#            Sensitivity : 0.11540         
#            Specificity : 0.97821         
#         Pos Pred Value : 0.63393         
#         Neg Pred Value : 0.77177         
#             Prevalence : 0.24643         
#         Detection Rate : 0.02844         
#   Detection Prevalence : 0.04486         
#      Balanced Accuracy : 0.54680         
#                                          
#       'Positive' Class : 0               
#                                          
#Confusion Matrix and Statistics

#          Reference
#Prediction     0     1
#         0   593   390
#         1  4318 14672
#                                          
#               Accuracy : 0.7643          
#                 95% CI : (0.7583, 0.7702)
#    No Information Rate : 0.7541          
#    P-Value [Acc > NIR] : 0.0004161       
#                                          
#                  Kappa : 0.1299          
# Mcnemar's Test P-Value : < 2.2e-16       
#                                          
#            Sensitivity : 0.12075         
#            Specificity : 0.97411         
#         Pos Pred Value : 0.60326         
#         Neg Pred Value : 0.77262         
#             Prevalence : 0.24588         
#         Detection Rate : 0.02969         
#   Detection Prevalence : 0.04922         
#      Balanced Accuracy : 0.54743         
#                                          
#       'Positive' Class : 0





# -------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------
