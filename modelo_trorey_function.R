# função que retorna o ajuste das alturas segundo modelo de Trorey

modelo_trorey <- function(dados){
  
  output_list <- list()
  anos <- unique(dados$Ano)
  
  for (ano in anos) {
    
    dados_ano_szero <- dados %>%
      filter(Ano == ano) %>%
      filter(altura != 0)
    
    
    df <- data.frame((dados_ano_szero$altura),dados_ano_szero$DAP,(dados_ano_szero$DAP^2))
    colnames(df) <- c("altura","DAP","DAP2")
    
    
    mod_trorey <- lm(altura ~ DAP + DAP2, df)
    
    
    residuo_relativo <- 100*mod_trorey$residuals/dados_ano_szero$altura
    
    NSQR = sum(mod_trorey$residuals^2)  #Nova soma do Quadrado Médio do Resíduo 
    
    QMR = NSQR/mod_trorey$df.residual   #Quadrado médio do resíduo
    
    Syx <- sqrt(QMR)                    #Erro padrão da estimativa
    
    Syx_relativo <- 100*Syx/mean(dados_ano_szero$altura)
    
    coeficientes <- as.data.frame(mod_trorey$coefficients)
    colnames(coeficientes) <- c("coeficientes")
    
    df <- data.frame()
    df <- data.frame(ajustado = c(mod_trorey$fitted.values),residuo = c(residuo_relativo))
    
    plot_residuo <- ggplot(df,aes(ajustado,residuo)) +
      geom_point() +
      labs(x = "altura ajustada (m)", y = "resíduo (%)",title = paste(ano," Trorey")) +
      theme_classic()
    
    
    output_list[[as.character(ano)]] <- list(
      
      nome_modelo = "Trorey", 
      erro_padrao = Syx,
      erro_padrao_rel = Syx_relativo,
      coeficientes = coeficientes,
      fitted = mod_trorey$fitted.values,
      residuos = mod_trorey$residuals,
      residuos_rel = residuo_relativo,
      grafico = plot_residuo
      
    )
  }
  return(output_list)
}