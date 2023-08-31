
hdom_assmann <- function(){
  
  anos <- unique(dados$Ano)
  ultimo_ano <- max(unique(dados$Ano))
  
  ano_plantio <- 2016
  area_parcelas <- 600
  numero_de_arvores <- area_parcelas/100
  
  # Selecionar dados apenas do último inventário.
  dados_rank <- dados[dados$Ano==ultimo_ano,]
  
  # Rankiar árvores com os maiores DAP's de cada parcela.
  dados_rank <- dados_rank %>%
    arrange(parcela_id,desc(DAP)) %>% 
    group_by(parcela_id) %>%
    mutate(rank_dap = rank(-DAP,ties.method='first')) %>% 
    # mutate(rank_dap = row_number()) %>%
    arrange(parcela_id, rank_dap) %>%
    filter(rank_dap <= numero_de_arvores)
  
  #Selecionar árvores com maiores DAP's em todos os anos.
  arvores_selecionadas <- dados %>%
    semi_join(dados_rank, by = c("parcela_id", "arvore_id"))
  
  # Adicionar dados de idade em cada árvore com base no ano do plantio
  arvores_selecionadas$idade <- arvores_selecionadas$Ano - ano_plantio
  
  hdom_parcela_idade <- arvores_selecionadas %>% 
    #select(parcela_id,idade) %>% 
    group_by(parcela_id,idade) %>% 
    summarize(hdom = mean(altura), .groups = 'drop')

  return(hdom_parcela_idade)
}
