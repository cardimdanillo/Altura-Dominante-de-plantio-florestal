"'
Função que retorna gráficos que resumem os dados dos inventários e as alturas dominantes

'"

graficos_output <- function(){
    
  # Gráfico boxplot das alturas por idade dos inventários
  gra_dados_alt_box  <- ggplot(dados,aes(factor (Ano), altura)) + 
    geom_boxplot() +
    labs(
      title = "Boxplots das alturas por Idade",
      x = "Idade (anos)",
      y = "altura (m)")
  plot(gra_dados_alt_box)
  ggsave("gra_dados_alt_box.png",plot = gra_dados_alt_box, width = 10, height = 6, dpi = 300)
  
  # Gráfico boxplot dos DAP's por idade dos inventários
  gra_dados_dap_box <- ggplot(dados,aes(factor (Ano), DAP)) + 
    geom_boxplot() +
    labs(
      title = "Boxplots dos DAP's por Idade",
      x = "Idade (anos)",
      y = "DAP (cm)")
  plot(gra_dados_dap_box)
  
  ggsave("gra_dados_dap_box.png",plot = gra_dados_dap_box, width = 10, height = 6, dpi = 300)
  
  # Gráfico resumo com alturas dominantes por parcela
  
  
  gra_hdom_parcelas_col <- ggplot(altura_dominante, aes(x = factor(parcela_id), y = hdom, fill = factor(idade))) +
    geom_col(position = "dodge") +
    labs(title = "Resumo das alturas dominantes ao longo do tempo levando em conta a localização", x = "Parcela", y = "Altura Dominante", fill = "idade") +
    theme_minimal()
  plot(gra_hdom_parcelas_col)
  
  ggsave("gra_hdom_parcelas_col.png", plot = gra_hdom_parcelas_col, width = 10, height = 6, dpi = 300)
  
  # Graficos das alturas dominantes pela idade.
  
  gra_hdom_point <- ggplot(altura_dominante,aes(idade,hdom)) + 
    geom_point() + 
    ggtitle("Gráfico de dispersão das Alturas Dominantes") +
    labs(x = "idade (anos)", y = "hdom (m)")
  plot(gra_hdom_point)
  
  ggsave("gra_hdom_point.png",plot = gra_hdom_point, width = 10, height = 6, dpi = 300)
  
  
  gra_hdom_box <- ggplot(altura_dominante,aes(factor (idade), hdom)) + 
    geom_boxplot() +
    labs(
      title = "Boxplots das Alturas Dominantes por Idade",
      x = "Idade (anos)",
      y = "Altura Dominante (m)")
  plot(gra_hdom_box)
  
  ggsave("gra_hdom_box.png",plot = gra_hdom_box, width = 10, height = 6, dpi = 300)

}
