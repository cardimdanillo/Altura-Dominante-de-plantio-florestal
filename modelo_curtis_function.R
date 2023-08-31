
modelo_curtis <- function(dados){
  
  output_list <- list()
  anos <- unique(dados$Ano)
  
  for (ano in anos) {
    
    dados_ano_szero <- dados %>%
      filter(Ano == ano) %>%
      filter(altura != 0)
    
    df <- data.frame(log(dados_ano_szero$altura),1/dados_ano_szero$DAP)
    colnames(df) <- c("lnAlt","inv_DAP")
    
    modelo <- lm( lnAlt ~ inv_DAP, df)
    
    qmr <- sum((modelo$residuals)^2) / modelo$df.residual
    
    fmeyer = exp(0.5*qmr)
    
    modelo$fitted.values <- fmeyer*(exp(modelo$fitted.values))
    
    modelo$residuals <- dados_ano_szero$altura - modelo$fitted.values
    
    residuo_relativo <- 100*modelo$residuals/dados_ano_szero$altura
    
    NSQR = sum(modelo$residuals^2)  #Nova soma do Quadrado Médio do Resíduo 
    
    QMR = NSQR/modelo$df.residual   #Quadrado médio do resíduo
    
    Syx <- sqrt(QMR)                    #Erro padrão da estimativa
    
    Syx_relativo <- 100*Syx/mean(dados_ano_szero$altura)
    
    coeficientes <- as.data.frame(modelo$coefficients)
    colnames(coeficientes) <- c("coeficientes")
    
    df <- data.frame()
    df <- data.frame(ajustado = c(modelo$fitted.values),residuo = c(residuo_relativo))
    
    plot_residuo <- ggplot(df,aes(ajustado,residuo)) +
      geom_point() +
      labs(x = "altura ajustada (m)", y = "resíduo (%)",title = paste(ano," Curtis")) +
      theme_classic()
    
    
    output_list[[as.character(ano)]] <- list(
      
      nome_modelo = "Curtis", 
      erro_padrao = Syx,
      erro_padrao_rel = Syx_relativo,
      coeficientes = coeficientes,
      fmeyer = fmeyer,
      fitted = modelo$fitted.values,
      residuos = modelo$residuals,
      residuos_rel = residuo_relativo,
      grafico = plot_residuo
      
    )
  }
  return(output_list)
}
