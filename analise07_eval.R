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
# -------------------------------------------------------------------------------------

# Carrega a tabela de treinamento
table_name <- 'train_sampled.csv'
paste("Carregando tabela de treino: ",table_name)
table <- read.csv(table_name)


rf <- readRDS("model_randomForest1000Trees.rds")      # Erro -> 0.1678097
# rf <- readRDS("model_randomForest3000Trees.rds")    # Erro -> 0.167793

library(randomForest)
prediction <- predict(rf, table)

MultiLogLoss(table$target,prediction)

# -------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------
