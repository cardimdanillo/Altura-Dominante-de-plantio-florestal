# função que retorna o ajuste das alturas segundo modelo de Trorey

modelo_trorey <- function(dados){
  
  if(!require(ggplot2)) install.packages(ggplot2)
  library(ggplot2)
  
  output_list <- list()
  anos <- unique(dados$Ano)
  
  for (ano in anos) {
    
    dados_ano_szero <- dados %>%
      filter(Ano == ano) %>%
      filter(altura != 0)
    
    
    df <- data.frame((dados_ano_szero$altura),dados_ano_szero$DAP,(dados_ano_szero$DAP^2))
    colnames(df) <- c("altura","DAP","DAP2")
    
    
    modelo <- lm(altura ~ DAP + DAP2, df)
    
    
    residuo_relativo <- 100*modelo$residuals/dados_ano_szero$altura
    
    NSQR = sum(modelo$residuals^2)  #Nova soma do Quadrado Médio do Resíduo 
    
    QMR = NSQR/modelo$df.residual   #Quadrado médio do resíduo
    
    Syx <- sqrt(QMR)                    #Erro padrão da estimativa
    
    Syx_relativo <- 100*Syx/mean(dados_ano_szero$altura)
    
    coeficientes <- as.data.frame(modelo$coefficients)
    colnames(coeficientes) <- c("coeficientes")
    
    # Cálculo do R²
    SSY <- sum((dados_ano_szero$altura - mean(dados_ano_szero$altura))^2)
    SSR <- SSY - NSQR
    R2 <- SSR / SSY
    
    # Cálculo do R² ajustado
    n <- length(dados_ano_szero$altura)
    p <- length(coeficientes$coeficientes)
    R2_adjusted <- 1 - (NSQR / (n - p - 1)) / (SSY / (n - 1))
    
    df <- data.frame()
    df <- data.frame(ajustado = c(modelo$fitted.values),residuo = c(residuo_relativo))

    
    # Adicionando as informações ao título do gráfico
    title_text <- paste(ano," Trorey - R² ajustado =", round(R2_adjusted, 3))
    
    plot_residuo <- ggplot(df,aes(ajustado,residuo)) +
      geom_point() +
      labs(x = "altura ajustada (m)", y = "resíduo (%)", title = title_text) +
      theme_classic()
    
    
    output_list[[as.character(ano)]] <- list(
      
      nome_modelo = "Trorey", 
      erro_padrao = Syx,
      erro_padrao_rel = Syx_relativo,
      coeficientes = coeficientes,
      R2 = R2,                # Adicionando o R²
      R2_ajustado = R2_adjusted,  # Adicionando o R² ajustado
      fitted = modelo$fitted.values,
      residuos = modelo$residuals,
      residuos_rel = residuo_relativo,
      grafico = plot_residuo
      
    )
  }
  return(output_list)
}