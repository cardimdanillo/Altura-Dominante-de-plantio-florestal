## Esse script serve exclusivamente para transformar os dados da planilha da aula de Manejo Florestal
## em dados de inventário em formato csv.

##pacotes

#####

if(!require(dplyr)) install.packages("dplyr")
library(dplyr)

if(!require(readxl)) install.packages("readxl")
library(readxl)

#####

# Carregar dados tabelados no EXCEL
#####
# Obtém o diretório do script atual
#script_dir <- dirname(rstudioapi::getSourceEditorContext()$path)

# Define o diretório de trabalho para o diretório do script
#setwd(script_dir)


dados_inv <- read_xlsx('SitioPrognoseRegulação_23_1.xlsx',
                       sheet = "Natalia Barros",
                       #range = cell_limits(c(2,1)),
                       col_names = TRUE,
                       skip = 1
)


##excluir colunas vazias e com o nome da esp?cie
dados_inv <- dados_inv[, colSums(is.na(dados_inv)) < 20]

#dados_inv <- dados_inv[,-c(5,6,11,16)]

glimpse(dados_inv)


#Estimar alturas = 0 com modelo de Curtis e Trorey

# Criar Data Frames e tratamento dos dados
##### 


i <- 2019 # Primeiro ano
j <- 1   # Começando com a primeira coluna do DataFrame

# Lista para armazenar os DataFrames criados
lista_inventarios <- list()

# Loop para criar um DataFrame para cada ano
for (i in 2019:2022) {
  
  # Criar um nome para o DataFrame usando o ano
  file_name <- paste0("inv_", i)
  
  # Selecionar as colunas para o DataFrame atual
  cols_selecionadas <- j:(j + 3)
  
  # Criar um novo DataFrame com as colunas selecionadas e uma coluna de ano repetida
  novo_dataframe <- dados_inv[, c(cols_selecionadas)]
  
  # Renomear as colunas
  colnames(novo_dataframe) <- c("parcela_id", "arvore_id", "CAP", "altura")
  
  novo_dataframe <- na.omit(novo_dataframe)

    # Adicionar coluna de Ano
    novo_dataframe$Ano <- i
  
  # Adicionar o novo DataFrame à lista
  lista_inventarios[[file_name]] <- novo_dataframe
  
  j <- j + 4 # Avançar para as próximas colunas
}

#Combinar a lista lista_inventarios em um único dataframe
dados_inventarios_combinados <- do.call(rbind, lista_inventarios)

# Exportar dados combinados como um arquivo CSV
write.csv(dados_inventarios_combinados, file = "dados_inventarios_combinados.csv", row.names = FALSE)


