#####
'"
Este script tem como objetivo o calculo da altura dominante de parcelas 
amostrais de um plantio florestal
Ele leva em consideração erros inerentes a aquisição de dados como a falta de 
algumas alturas. Para tanto, as alturas faltantes foram estimadas através da 
modelos de altura alimentados com dados das alturas que haviam.
Por fim o resultado são as alturas dominantes por parcela e idade.

O script carrega funções carregadas no mesmo diretório. Elas ajudam a deixar 
este script mais organizado é de mais fácil compreensão.

Os dados usados neste script foi o disponibilizado no curso de Engenharia 
Florestal da Universidade de Brasília na cadeira de Manejo Florestal.

Repositório no Github: https://github.com/cardimdanillo/Altura-Dominante-de-plantio-florestal
-----------------------------------------------------------------------------
Este script foi desnvolvido por: 
Danillo Cardim
cardim.danillo@outlook.com

"'

#####
# Pacotes
#####
if(!require(dplyr)) install.packages(dplyr)
library(dplyr)

if(!require(tidyverse)) install.packages("tidyverse")
library(tidyverse)
#####

#####
#importar dados de inventários em formato CSV.
#####

csv_file <- "dados_inventarios_combinados.csv"
if (!file.exists(csv_file)) {
  stop("O arquivo CSV não foi encontrado no diretório.")
}
dados <- read.csv(csv_file, header = TRUE, sep = ",")

glimpse(dados)
#####

# Calculo do DAP em centímetros
dados$DAP <- dados$CAP/pi

# Excluir dados com erros (DAP < 5 e Altura < 8)

dados <- dados %>%
  filter(DAP>= 5) %>%
  filter(altura >= 8 | altura == 0)


# Funções externas e verificação de existência

external_functions <- c("modelo_trorey_function.R", "modelo_curtis_function.R",
                        "AlturaZero_substitui_function.R", "hdom_assmann_function.R",
                        "graficos_output_function.R")

for (func_file in external_functions) {
  if (!file.exists(func_file)) {
    stop(paste("O arquivo", func_file, "não foi encontrado no diretório."))
  }
  source(func_file)
}

# Substituir dados de altura com altura = 0.
#####
# Modelo de Trorey

output_trorey <- modelo_trorey(dados)

# Modelo de Curtis

output_curtis <- modelo_curtis(dados)

# Substituir alturas

dados <- AlturaZero_substitui()

#####

# Calcular altura dominante pelo princípio de Assmann (1970)

altura_dominante <- hdom_assmann()

altura_dominante$hdom <- round(altura_dominante$hdom, 3)

# Exportar tabela com alturas dominantes médias por parcela e idade.

write.table(altura_dominante, file = "hdom_output.csv", sep = ",",
            row.names = FALSE, col.names = TRUE, quote = FALSE)

# Função que gera gráfico que resumem os dados dos inventários e as alturas dominantes.

graficos_output()


