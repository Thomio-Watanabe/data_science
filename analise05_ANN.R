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
# Treinamento 
# -------------------------------------------------------------------------------------

# Carrega as informações
table_name <- 'train_sampled.csv'
paste("Carregando tabela de trainamento: ",table_name)
# table = read.csv(table_name, nrows = 10000 )
table = read.csv(table_name)
# -------------------------------------------------------------------------------------


# Deleta atributo v56 -> Random forest não resolve atributos categóricos com muitos níveis
table$v56 <- NULL





# -------------------------------------------------------------------------------------
# Deleta a variável que contém a tabela
rm(table)
# -------------------------------------------------------------------------------------
