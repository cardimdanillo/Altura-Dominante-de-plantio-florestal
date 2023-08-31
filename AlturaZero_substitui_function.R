
AlturaZero_substitui <- function(){

  anos <- unique(dados$Ano)
  
  for (ano in anos) {
  
    syx_curtis <- output_curtis[[as.character(ano)]]$erro_padrao_rel
    syx_trorey <- output_trorey[[as.character(ano)]]$erro_padrao_rel
    
    if (syx_curtis < syx_trorey) {
      
      modelo_selecionado <- "curtis"
      coeficientes <- output_curtis[[as.character(ano)]]$coeficientes
      fmeyer = output_curtis[[as.character(ano)]]$fmeyer
      
    } else {
      
      modelo_selecionado <- "trorey"
      coeficientes <- output_trorey[[as.character(ano)]]$coeficientes
      
    }
    
    dados_ano <- dados[dados$Ano == ano, ]
    
    #dados_ano$altura[dados_ano$altura == 0] <- exp(coeficientes$coeficientes[1] + coeficientes$coeficientes[2]/dados_ano$DAP[dados_ano$altura == 0])
    
    
    if (modelo_selecionado == "curtis") {
      dados_ano$altura[dados_ano$altura == 0] <- fmeyer*(exp(coeficientes$coeficientes[1] + coeficientes$coeficientes[2]/dados_ano$DAP[dados_ano$altura == 0]))
      
    } else {
      dados_ano$altura[dados_ano$altura == 0] <- coeficientes$coeficientes[1] + coeficientes$coeficientes[2] * dados_ano$DAP[dados_ano$altura == 0] +  coeficientes$coeficientes[3] * (dados_ano$DAP[dados_ano$altura == 0])^2
    }
  
    dados[dados$Ano == ano, ] <- dados_ano
  }
  
return(dados)

}
