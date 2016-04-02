# Thomio Watanabe
# Ciência de dados

# Carrega as informações
table_name <- 'train.csv'
paste("Carregando tabela de trainamento: ",table_name)
table = read.csv(table_name)

# Número de linhas 
paste("Número de linhas da tabela: ",nrow(table))

# Número de colunas
paste("Número de colunas: ",ncol(table))

# Mostra nome dos atributos
paste("Nome dos atributos: ")
head(table,0)

# Deleta a variável que contém a tabela
rm(table)
