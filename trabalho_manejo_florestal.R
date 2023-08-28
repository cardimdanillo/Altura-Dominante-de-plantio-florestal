
##pacotes

#####
if(!require(xlsx)) install.packages("xlsx")
library(xlsx)

if(!require(dplyr)) install.packages("dplyr")
library(dplyr)

if(!require(tidyverse)) install.packages("tidyverse")
library(tidyverse)

library(readxl)
#####

# Carregar dados tabelados no EXCEL
#####

#local <- "C:/Users/e1415051/OneDrive - unb.br/Manejo Florestal/Trabalho/R"
local <- "D:/OneDrive - unb.br/Manejo Florestal/Trabalho/R"

setwd(local)



#dados_inv <- read_excel('dados_inventarios.xlsx',header = TRUE, sheetName = "dados", startRow = 2)

#tamanho das parcelas (tpar) em metros quadrados.

tpar <- 600

i <- 2019
#Natalia Barros

dados_inv <- read_xlsx('SitioPrognoseRegulação_23_1.xlsx',
                      sheet = "Natalia Barros",
                      #range = cell_limits(c(2,1)),
                      col_names = TRUE,
                      skip = 1
                      )

# dados_inv2 <- read_xlsx('dados_inventarios.xlsx',
#                         sheet = "dados",
#                        #range = cell_limits(c(2,1)),
#                        col_names = TRUE,
#                        skip = 1
#                        )
# 
# glimpse(dados_inv2)

##excluir colunas vazias e com o nome da esp?cie
dados_inv <- dados_inv[,-c(5,6,11,16)]

glimpse(dados_inv)
                       

#Estimar alturas = 0 com modelo de Curtis e Trorey

# Criar Data Frames e tratamento dos dados
##### 


i <- 2019 #primeiro ano
j <- 0

# Criar uma Data Frame para cada ano.

for (i in 2019:2022) {
  
  file_name <- paste0("inv_",i)
  #file_name_hdom <- paste0("ht",i)

  
  assign(file_name,data.frame(dados_inv[j+1],dados_inv[j+2],dados_inv[j+3],dados_inv[j+4]))
  #assign(file_name_hdom,data.frame(dados_inv[j+1],dados_inv[j+2],dados_inv[j+3],dados_inv[j+4]))
  j <- j + 4

}

# Fun??o DAP: Renomeia colunas e calcula o DAP com a circunfer?ncia (CAP)

DAP <- function(inv1){
  
  colnames(inv1) <- c("parcela","arv","CAP","ALT")
  
  inv1$DAP <- inv1$CAP/pi
  
  return(inv1)
  
}

inv_2019 <- DAP(inv_2019)
inv_2020 <- DAP(inv_2020)
inv_2021 <- DAP(inv_2021)
inv_2022 <- DAP(inv_2022)

DAP_ALT <- function(inv1){
  
  inv1 <- na.omit(inv1)
  
  inv1 <- inv1 %>%
    filter(inv1$ALT>8 | inv1$ALT==0)
    
  inv1 <- inv1 %>%
    filter(inv1$DAP>5)
  
  return(inv1)
  
} 

inv_2019 <- DAP_ALT(inv_2019)
inv_2020 <- DAP_ALT(inv_2020)
inv_2021 <- DAP_ALT(inv_2021)
inv_2022 <- DAP_ALT(inv_2022)

invlist <- list()
invlist[[1]] <- (inv_2019)
invlist[[2]] <- (inv_2020)
invlist[[3]] <- (inv_2021)
invlist[[4]] <- (inv_2022)




ret_zero <- function(inv1){
  
  inv1 <- na.omit(inv1)
  
  inv1 <- inv1 %>%
    filter(inv1$ALT != 0)
  
  return(inv1)
  
}    #Fun??o para filtrar alturas > 8 e DAP > 5 e manter as alturas = 0

inv_2019 <- ret_zero(inv_2019)
inv_2020 <- ret_zero(inv_2020)
inv_2021 <- ret_zero(inv_2021)
inv_2022 <- ret_zero(inv_2022)



#####

# Aplicar modelos de regress?o para as alturas que est?o com zero.
#####

  #Modelo de Trorey
  
  trorey <- function(alt,dap,ano){
    
    #if(!require(rstatix)) install.packages("rstatix")
    #library(rstatix)

  
  tro <- data.frame(alt,dap,dap^2)
  colnames(tro) <- c("ALT","DAP","DAP2")

  
  #Modelo
  
  modt <- lm(ALT ~ DAP + DAP2, tro)
  
  #anova <- anova(modt)

  
  fit1 <- (modt$fitted.values)
  
  res1 <- tro$ALT-fit1
  
  res_rel <- 100*res1/tro$ALT
  
  NSQR1 = sum(res1^2)
  
  QMR1 = NSQR1/modt$df.residual
  
  Syx1 <- sqrt(QMR1)
  
  Syx_rel1 <- 100*Syx1/mean(tro$ALT)
  
  coef <- as.data.frame(modt$coefficients)
  colnames(coef) <- c("coeficientes")
  #cat("Erro padr?o relativo Syx (%)",Syx_rel1)
  
  df <- data.frame()
  df <- data.frame(ajustado = c(fit1),residuo = c(res_rel))
  
  
  
  a <- ggplot(df,aes(ajustado,residuo)) +
    geom_point() +
    labs(x = "altura ajustada (m)", y = "res?duo (%)",title = paste(ano," Trorey")) +
    theme_classic()
  
  output <- list("Trorey",Syx_rel1,coef,fit1,res1,res_rel,a)
  names(output) <- c("modelo","syx relativo","coeficiente","h ajustado",
                     "residuo","res?duo relativo","grafico")
  
  return(output)
  
  }
  
  #Modelo de Curtis
  
  curtis <- function(ALT,DAP,ano){

    if(!require(SciViews)) install.packages("SciViews")
    library(SciViews)
    

  curt <- data.frame(ln(ALT),1/DAP)
  colnames(curt) <- c("LNALT","DAP")
  
  ALT <- as.numeric(ALT)
  
  #glimpse(curt)
  
  
  #Modelo
  
  modc <- lm(LNALT ~ DAP, curt)
  
  #par(mfrow=c(2,2))
  
  #plot(modc)
  
  #par(mfrow=c(1,1))
  
  
  anova <- anova(modc)

  
  fmeyer = exp(0.5*anova$`Mean Sq`[[2]])
  
  fit2 <- exp(modc$fitted.values)*fmeyer
  
  res2 <- ALT-fit2
  
  res_rel <- 100*res2/ALT
  
  NSQR2 = sum(res2^2)
  
  QMR2 = NSQR2/modc$df.residual
  
  Syx2 <- sqrt(QMR2)
  
  Syx_rel2 <- 100*Syx2/mean(ALT)
  
  coef <- as.data.frame(modc$coefficients)
  colnames(coef) <- c("coeficientes")
  
  df <- data.frame()
  df <- data.frame(ajustado = c(fit2),residuo = c(res_rel))
  
  
  
  a <- ggplot(df,aes(ajustado,residuo)) +
    geom_point() +
    labs(x = "altura ajustada (m)", y = "res?duo (%)",title = paste(ano," Curtis")) +
    theme_classic()

  
  output <- list("curtis",Syx_rel2,coef,fit2,fmeyer,res2,res_rel,a)
  names(output) <- c("modelo","syx relativo","coeficiente",
                     "h ajustado","fator de meyer","residuo",
                     "residuo relativo","grafico")
  
  return(output)
  
  }  
  
  
  #Regress?o altura 2019
  
  fit_tro_19 <- trorey(inv_2019$ALT,inv_2019$DAP,2019)
  
  fit_curtis_19 <- curtis(inv_2019$ALT,inv_2019$DAP,2019)
  
  
  #Regress?o altura 2020
  
  fit_tro_20 <- trorey(inv_2020$ALT,inv_2020$DAP,2020)
  
  fit_curtis_20 <- curtis(inv_2020$ALT,inv_2020$DAP,2020)

  #Regress?o altura 2021
  
  fit_tro_21 <- trorey(inv_2021$ALT,inv_2021$DAP,2021)
  
  fit_curtis_21 <- curtis(inv_2021$ALT,inv_2021$DAP,2021)
  
  #Regress?o altura 2022
  
  fit_tro_22 <- trorey(inv_2022$ALT,inv_2022$DAP,2022)
  
  fit_curtis_22 <- curtis(inv_2022$ALT,inv_2022$DAP,2022)
#####

# Aplicar melhor modelo ?s alturas  
#####

# Carregar dados dos invent?rios guardados na lista (invlist)
i=1
 for(i in 1:4){
   j=0
   if (i==1)  {j <- 2019}
   if (i==2)  {j <- 2020}
   if (i==3)  {j <- 2021}
   if (i==4)  {j <- 2022}
   
   file_name <- paste0("tab",j)
   
   assign(file_name,as.data.frame(invlist[i]))
   print(i)
   print(j)
   print(file_name)
   
 } #criar arquivos tab 2019, 2020, 2021, 2022

i=1

#colnames(tab2022) <- c("Parcela","Arv","CAP","ALT","DAP")

subht <-function(tab,trorey,curtis){
  
  tab <- na.omit(tab)
  
  #colnames(tab) <- c("Parcela","Arv","CAP","ALT","DAP")


  for(i in 1:length(tab$ALT)){


    if (as.numeric(trorey[2])<as.numeric(curtis[2])) {
    #return(j)

      if(tab$ALT[i] == 0){

        coef <- as.data.frame(trorey[3])
        tab$ht_est[i]= coef[[1,1]]+coef[[2,1]]*tab$DAP[i]+
          (coef[[3,1]]*(tab$DAP[i]^2))

      } else{

          tab$ht_est[i] <- tab$ALT[i]

          }


    }else{
      if(tab$ALT[i]==0){
        coef <- as.data.frame(curtis[3])
        fm <- as.numeric(curtis[5])
        tab$ht_est[i]=exp(coef[[1,1]]+coef[[2,1]]*(1/tab$DAP[i]))

        }else{tab$ht_est[i] <- tab$ALT[i]}
    }

  }

  
  return(tab)
  
  
} # fun??o para substituir h = 0 por h ajustado



tab19 <- subht(tab2019,fit_tro_19,fit_curtis_19)

tab20 <- subht(tab2020,fit_tro_20,fit_curtis_20)

tab21 <- subht(tab2021,fit_tro_21,fit_curtis_21)
  
tab22 <- subht(tab2022,fit_tro_22,fit_curtis_22)


#####
#M?dia das alturas das 100 ?rvores com maior DAP.

# tamanho da parcela = tpar m?      np ?rvores com maiores DAPs
#               1 ha = 10000 m?     100 ?rvores com maiores DAP

# Calcular alturas dominantes
#####

ranking <- function(inv){
  
  inv <- arrange(inv,parcela,desc(DAP))
  
  inv <- inv %>%
    group_by(parcela) %>%
    mutate(rank = rank(-DAP,ties.method='first'))
  
}

codigo <- function (inv){
  
  inv$codigo <- paste("#",inv$parcela, inv$arv, sep = "-")
  
  return(inv)
  
}

tab22 <- ranking(tab22)

tab22 <- codigo(tab22)

tab21 <- codigo(tab21)

tab20 <- codigo(tab20)

tab19 <- codigo(tab19)


DAPMaiores <- function(inv){
  
  np <- 100*tpar/10000
  
  inv <- inv %>%
    group_by(parcela) %>%
    filter(rank <= 6)
  
  return(inv)
  
}

tab22 <- DAPMaiores(tab22)

arvdom <- function(inv,invref){
  
  inv <- inner_join(inv,invref %>% select(codigo))
  
  return(inv)
  
}

tab19 <- arvdom(tab19,tab22)
tab20 <- arvdom(tab20,tab22)
tab21 <- arvdom(tab21,tab22)


hdom <- function(inv){

  
  inv <- inv %>%
    group_by(parcela) %>%
    summarise(hdom = mean(ht_est))
  
  
  return(inv)
  
}


tab19 <- hdom(tab19)

tab20 <- hdom(tab20)

tab21 <- hdom(tab21)

tab22 <- hdom(tab22)


tab19$idade  = 3
tab20$idade  = 4
tab21$idade  = 5
tab22$idade  = 6


#####

# Gr?ficos de res?duo dos modelos de altura

# if(!require(ggplot2)) install.packages("ggplot2")
# library(ggplot2)

if(!require(ggpubr)) install.packages("ggpubr")
library(ggpubr)

ggarrange(fit_tro_19[[7]],fit_curtis_19[[8]],
          fit_tro_20[[7]],fit_curtis_20[[8]],
          fit_tro_21[[7]],fit_curtis_21[[8]],
          fit_tro_22[[7]],fit_curtis_22[[8]], ncol = 2, nrow =  4)

AltDom <- data.frame()

AltDom <- data.frame("Parcela" = c(1:30), "2019" = c(tab19$hdom),"2020" = c(tab20$hdom), 
                     "2021" = c(tab21$hdom), "2022" = c(tab22$hdom))

colnames(AltDom) <- c("Parcela", "2019", "2020", "2021", "2022") 

AltDom <- format(round(AltDom, 3), nsmall = 3)

AltDom$Parcela <- as.integer(AltDom$Parcela)

write.table(AltDom, file = "AltDom_JulioCesar.txt", sep = "\t",
            row.names = TRUE, col.names = NA, quote = FALSE)


hdom <- bind_rows(tab19,tab20,tab21,tab22)
write.table(hdom, file = "hdom_site_JulioCesar.txt", sep = "\t",
            row.names = TRUE, col.names = NA, quote = FALSE)

write.table(hdom, file = "hdom_site_JulioCesar.xlsx", sep = "\t",
            row.names = TRUE, col.names = NA, quote = FALSE)


modelos <- data.frame("Curtis 2019" = c(fit_curtis_19[3]),
                      "Curtis 2020" = c(fit_curtis_20[3]),
                      "Curtis 2021" = c(fit_curtis_21[3]),
                      "Curtis 2022" = c(fit_curtis_22[3]))

colnames(modelos) <- c("2019", "2020", "2021", "2022")
row.names(modelos) <- c("int", "DAP")

modelos <- format(round(modelos, 4), nsmall = 4)

write.table(modelos, file = "CoefAlt_Curtis_JulioCesar.txt", sep = "\t\t",
            row.names = TRUE, col.names = NA, quote = FALSE)

modelos2 <- data.frame("Trorey 2019" = c(fit_tro_19[3]),
                      "Trorey 2020" = c(fit_tro_20[3]),
                      "Trorey 2021" = c(fit_tro_21[3]),
                      "Trorey 2022" = c(fit_tro_22[3]))

colnames(modelos2) <- c("2019", "2020", "2021", "2022")
row.names(modelos2) <- c("int", "DAP", "DAP?")

modelos2 <- format(round(modelos2, 4), nsmall = 4)

write.table(modelos2, file = "CoefAlt_Trorey_JulioCesar.txt", sep = "\t\t",
            row.names = TRUE, col.names = NA, quote = FALSE)
