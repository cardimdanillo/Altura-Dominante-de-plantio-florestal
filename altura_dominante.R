
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
dados <- read.csv("dados_inventarios_combinados.csv", header = TRUE, sep = ",")

glimpse(dados)
#####

# Calculo do DAP em centímetros
dados$DAP <- dados$CAP/pi

# Excluir dados com erros (DAP < 5 e Altura < 8)

dados <- dados %>%
  filter(DAP>= 5) %>%
  filter(altura >= 8 | altura == 0)


# Substituir dados de altura com altura = 0.

# Modelo de Trorey

source("modelo_trorey_function.R")      #Carregar script com função de regressão
output_trorey <- modelo_trorey(dados)

# Modelo de Curtis

source("modelo_curtis_function.R")
output_curtis <- modelo_curtis(dados)

# Substituir alturas

source("AlturaZero_substitui_function.R")
dados <- AlturaZero_substitui()

# Calcular altura dominante pelo princípio de Assmann (1970)

source("hdom_assmann_function.R")
altura_domiante <- hdom_assmann()

write.table(altura_domiante, file = "hdom.txt", sep = "\t",
            row.names = TRUE, col.names = NA, quote = FALSE)









