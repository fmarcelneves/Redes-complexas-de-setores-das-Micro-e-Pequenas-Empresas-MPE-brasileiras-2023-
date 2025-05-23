library(dplyr)
library(ggplot2)
library(sf)
library(arrow)
library(ggrepel)
library(stringr)
library(lubridate)
library(geobr)

# Função para formatar rótulos com porcentagem e número formatado
format_labels <- function(n, total) {
  percent <- round(n / total * 100, 1)
  paste0(format(percent, decimal.mark = ","), "% (", 
         format(n, big.mark = ".", decimal.mark = ","), ")")
}



# Função para mapear UF para Região
map_uf_to_region <- function(uf) {
  regions <- list(
    Norte = c('AC', 'AP', 'AM', 'PA', 'RO', 'RR', 'TO'),
    Nordeste = c('AL', 'BA', 'CE', 'MA', 'PB', 'PE', 'PI', 'RN', 'SE'),
    Centro_Oeste = c('GO', 'MT', 'MS', 'DF'),
    Sudeste = c('ES', 'MG', 'RJ', 'SP'),
    Sul = c('PR', 'RS', 'SC')
  )
  for (region in names(regions)) {
    if (uf %in% regions[[region]]) {
      return(region)
    }
  }
  return(NA)
}

# Função para adicionar zero à esquerda em CNAE
pad_cnae <- function(cnae) {
  ifelse(nchar(cnae) == 6, paste0("0", cnae), cnae)
}

# Carregar dados de CNAE
df_cnae_setor <- read_csv('Dados/setor_atividade.csv', col_types = cols(CNAE = col_character()))
df_cnae_setor <- df_cnae_setor %>%
  mutate(CNAE = pad_cnae(CNAE))

# Carregar dados de empresas ativas (arquivo Parquet)
df_abertas <- read_parquet('Dados/df_ativas_ultimo_trimestre_2023.parquet')

# Filtrar MPE
df_abertas <- df_abertas %>%
  filter(Porte %in% c('ME', 'EPP'))

# Carregar os dados geográficos dos estados
estados <- read_state(year = 2020) # Ou o ano que preferir


# Função personalizada para formatar valores em milhares com vírgula como separador decimal e adicionar "K"
format_thousands <- function(x) {
  paste0(format(round(x / 1e3, 1), big.mark = ".", decimal.mark = ",", nsmall = 1), "K")
}

# Calcular a contagem de empresas por porte
contagem_portes <- df_abertas %>%
  count(Porte)

# Calcular a soma total de 'n' para calcular percentagens
contagem_portes <- contagem_portes %>%
  mutate(percent = n / sum(n) * 100)

# Adicionar labels formatados usando milhares
contagem_portes <- contagem_portes %>%
  mutate(labels = paste0(format(round(percent, 1), decimal.mark = ","), "% (", format_thousands(n), ")"))

# Reordenar os fatores da coluna 'Porte'
contagem_portes <- contagem_portes %>%
  mutate(Porte = factor(Porte, levels = c("ME", "EPP", "DEMAIS")))

# Definir cores específicas para cada porte
cores_portes <- c("ME" = "#1b9e77", "EPP" = "#d95f02")

# Gráfico de Donut
ggplot(contagem_portes, aes(x = 2, y = n, fill = Porte)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  xlim(0.5, 2.5) +
  geom_text(aes(label = labels), position = position_stack(vjust = 0.5), size = 6.5) +
  scale_fill_manual(values = cores_portes) +
  theme_void() +
  theme(legend.position = "right",
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16, face = "bold"),
        plot.title = element_text(size = 16, face = "bold"))

# Gráfico de Barras Lado a Lado
ggplot(contagem_portes, aes(x = Porte, y = n, fill = Porte)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = labels), vjust = -0.5, size = 6) +
  scale_fill_manual(values = cores_portes) +
  labs(fill = "Porte", y = "Número de empresas", x = "Porte") +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal() +
theme(axis.text.x = element_text(size = 15, hjust = 1),
      axis.text.y = element_text(size = 15, hjust = 1),
      axis.title.x = element_text(size = 18),
      axis.title.y = element_text(size = 18),
      plot.title = element_text(size = 12, face = "bold"),
      plot.subtitle = element_text(size = 10),
      legend.title = element_text(size = 16, face = "bold"),
      text = element_text(size = 15),
      legend.position = "none")

# Merge dos dados de CNAE com os dados das empresas
df_abertas <- df_abertas %>%
  left_join(df_cnae_setor, by = c("CNAE_Principal" = "CNAE"))

# Criar a variável Região a partir da variável UF
df_abertas <- df_abertas %>%
  mutate(Região = sapply(UF, map_uf_to_region))

# Preparação dos dados por Setor para ME, EPP, e Total (ME + EPP)
setor_count_porte <- df_abertas %>%
  group_by(Porte, Setor) %>%
  summarize(n = n(), .groups = 'drop') %>%
  mutate(percent = n / sum(n) * 100) %>%
  mutate(labels = paste0(format(round(percent, 1), decimal.mark = ","), "% (", format(n, big.mark = ".", decimal.mark = ","), ")"))

# Dados para o total (ME + EPP)
setor_count_total <- df_abertas %>%
  group_by(Setor) %>%
  summarize(n = n(), .groups = 'drop') %>%
  mutate(
    percent = n / sum(n) * 100,
    labels = paste0(
      trimws(format(round(percent, 1), nsmall = 1, decimal.mark = ",")), # Remove espaços
      "% (",
      trimws(format(n, big.mark = ".", decimal.mark = ",")), # Remove espaços
      ")"
    )
  )

# Preparar os dados de CNAE para ME, EPP e Total (ME + EPP)
top10_cnaes_porte <- df_abertas %>%
  group_by(Porte, CNAE_Principal) %>%
  count() %>%
  top_n(10, wt = n) %>%
  left_join(df_cnae_setor, by = c("CNAE_Principal" = "CNAE")) %>%
  mutate(labels = paste0(format(round(n / sum(n) * 100, 1), decimal.mark = ","), "% (", format(n, big.mark = ".", decimal.mark = ","), ")"))

top10_cnaes_total <- df_abertas %>%
  count(CNAE_Principal) %>%
  top_n(10, wt = n) %>%
  left_join(df_cnae_setor, by = c("CNAE_Principal" = "CNAE")) %>%
  mutate(labels = paste0(format(round(n / sum(n) * 100, 1), decimal.mark = ","), "% (", format(n, big.mark = ".", decimal.mark = ","), ")"))

# Função para criar gráficos por setor separados por porte com limites de porcentagem ajustáveis
grafico_setor_porte <- function(df, porte, y_max_percent = 50) {
  df_porte <- df %>% filter(Porte == porte)
  
  ggplot(df_porte, aes(x = reorder(Setor, -percent), y = percent, fill = Setor)) +
    geom_bar(stat = "identity", width = 0.9, alpha = 0.8) +
    geom_text(aes(label = labels), vjust = -0.5, size = 3.5) +
    labs(x = "Setor", y = "Percentagem de Empresas (%)") +
    scale_y_continuous(labels = function(x) paste0(format(x, decimal.mark = ","), "%"), limits = c(0, y_max_percent), breaks = seq(0, y_max_percent, by = 10)) +
    scale_fill_manual(values = c("Servicos" = "#FC4E2A", "Comercio" = "#005b96", "Industria" = "#7570b3", 
                                 "Construcao Civil" = "#e7298a", "Agropecuaria" = "#66a61e")) +
    theme_minimal() +
    theme(axis.text.x = element_text(size = 15, angle = 45, hjust = 1),
          axis.text.y = element_text(size = 15, hjust = 1),
          axis.title.x = element_text(size = 18),
          axis.title.y = element_text(size = 18),
          plot.title = element_text(size = 12, face = "bold"),
          plot.subtitle = element_text(size = 10),
          legend.title = element_text(size = 16, face = "bold"),
          text = element_text(size = 15),
          legend.position = "none")
}



# Função para criar gráficos de CNAE separados por porte com limites de contagem ajustáveis
grafico_cnae_porte <- function(df, porte, y_max_count = 10000) {
  df_porte <- df %>% filter(Porte == porte)
  
  ggplot(df_porte, aes(x = reorder(Description, n), y = n)) +
    geom_bar(stat = "identity", fill = "skyblue", width = 0.9, alpha = 0.8) +
    geom_text(aes(label = labels), hjust = -0.1, size = 3.5) +
    labs(x = "Descrição do CNAE", y = "Número de Empresas") +
    scale_y_continuous(limits = c(0, y_max_count)) +
    theme_minimal() +
    theme(axis.text.x = element_text(size = 15, hjust = 1),
          axis.text.y = element_text(size = 15),
          axis.title.x = element_text(size = 15),
          axis.title.y = element_text(size = 15),
          plot.title = element_text(size = 22, face = "bold"),
          plot.subtitle = element_text(size = 20),
          text = element_text(size = 25)) +
    coord_flip()
}

# Ajustar o nome da região 'Centro_Oeste' para 'Centro Oeste'
df_abertas <- df_abertas %>%
  mutate(Região = ifelse(Região == "Centro_Oeste", "Centro Oeste", Região))

# Função para criar o mapa de proporção de empresas por UF ou Região, com exibição opcional do número de empresas
mapa_proporcao_empresas <- function(df, estados, titulo = NULL, paleta = NULL, subtitulo = NULL, exibir_num_empresas = FALSE) {
  if (is.null(paleta)) {
    paleta <- c("#d4e157", "#aed581", "#8bc34a", "#689f38", "#33691e")
  }
  
  estados$name_state <- gsub("Amazônas", "Amazonas", estados$name_state)
  estados$name_state <- gsub("Rio Grande Do Sul", "Rio Grande do Sul", estados$name_state)
  estados$name_state <- gsub("Rio De Janeiro", "Rio de Janeiro", estados$name_state)
  estados$name_state <- gsub("Rio Grande Do Norte", "Rio Grande do Norte", estados$name_state)
  estados$name_state <- gsub("Mato Grosso Do Sul", "Mato Grosso do Sul", estados$name_state)
  
  
  # Preparação dos dados geográficos
  dados_mapa <- merge(estados, df, by.x = "abbrev_state", by.y = "UF")
  estados_centroids <- estados %>%
    st_transform(crs = st_crs(4326)) %>%
    st_centroid() %>%
    st_coordinates() %>%
    as.data.frame()
  estados_centroids <- cbind(estados$abbrev_state, estados_centroids[,1:2])
  colnames(estados_centroids) <- c("abbrev_state", "X", "Y")
  dados_mapa <- merge(dados_mapa, estados_centroids, by = "abbrev_state")
  
  # Definir rótulo com ou sem o número de empresas
  if (exibir_num_empresas) {
    label <- paste0(dados_mapa$abbrev_state, "\n", format(round(dados_mapa$proporcao_nacional, 2), big.mark = ".", decimal.mark = ",", nsmall = 2), "%\n", dados_mapa$total_empresas, " Empresas")
  } else {
    label <- paste0(dados_mapa$abbrev_state, "\n", format(round(dados_mapa$proporcao_nacional, 2), big.mark = ".", decimal.mark = ",", nsmall = 2), "%")
  }
  
  # Criação do mapa
  ggplot(data = dados_mapa) +
    geom_sf(aes(fill = proporcao_nacional), color = "white", size = 0.25) +
    geom_label_repel(aes(x = X, y = Y, label = label),
                     size = 2.8, box.padding = 0.25, label.padding = 0.18, point.padding = 0, max.overlaps = Inf) +
    scale_fill_gradient(low = paleta[1], high = paleta[5]) +
    labs(title = titulo,
         subtitle = subtitulo,
         fill = "Proporção Nacional (%):") +
    theme_void() +
    theme(legend.position = "bottom", 
          plot.title = element_text(hjust = 0.5), 
          plot.subtitle = element_text(hjust = 0.5),
          legend.title = element_text(size = 10),
          legend.text = element_text(size = 10),
          text = element_text(size = 10))
}

# Função para criar o mapa das regiões, com exibição opcional do número de empresas
mapa_proporcao_empresas_regioes <- function(df, regioes, titulo = NULL, paleta = NULL, subtitulo = NULL, exibir_num_empresas = FALSE) {
  if (is.null(paleta)) {
    paleta <- c("#d4e157", "#aed581", "#8bc34a", "#689f38", "#33691e")
  }
  
  # Preparação dos dados geográficos
  dados_mapa <- merge(regioes, df, by.x = "name_region", by.y = "Região")
  regioes_centroids <- regioes %>%
    st_transform(crs = st_crs(4326)) %>%
    st_centroid() %>%
    st_coordinates() %>%
    as.data.frame()
  regioes_centroids <- cbind(regioes$name_region, regioes_centroids[,1:2])
  colnames(regioes_centroids) <- c("name_region", "X", "Y")
  dados_mapa <- merge(dados_mapa, regioes_centroids, by = "name_region")
  
  # Definir rótulo com ou sem o número de empresas
  if (exibir_num_empresas) {
    label <- paste0(dados_mapa$name_region, "\n", format(round(dados_mapa$proporcao_nacional, 2), big.mark = ".", decimal.mark = ",", nsmall = 2), "%\n", dados_mapa$total_empresas, " Empresas")
  } else {
    label <- paste0(dados_mapa$name_region, "\n", format(round(dados_mapa$proporcao_nacional, 2), big.mark = ".", decimal.mark = ",", nsmall = 2), "%")
  }
  
  # Criação do mapa
  ggplot(data = dados_mapa) +
    geom_sf(aes(fill = proporcao_nacional), color = "white", size = 0.25) +
    geom_label_repel(aes(x = X, y = Y, label = label),
                     size = 2.8, box.padding = 0.25, label.padding = 0.18, point.padding = 0, max.overlaps = Inf) +
    scale_fill_gradient(low = paleta[1], high = paleta[5]) +
    labs(title = titulo,
         subtitle = subtitulo,
         fill = "Proporção Nacional (%):") +
    theme_void() +
    theme(legend.position = "bottom", 
          plot.title = element_text(hjust = 0.5), 
          plot.subtitle = element_text(hjust = 0.5),
          legend.title = element_text(size = 14),
          legend.text = element_text(size = 14),
          text = element_text(size = 14))
}

# Função para gerar mapas por tipo de porte (ME, EPP ou Total), com opção de exibir o número de empresas
gerar_mapas_por_porte <- function(df_abertas, tipo_mapa, titulo = NULL, estados = NULL, regioes = NULL, exibir_num_empresas = FALSE, porte = "Total") {
  if (porte == "ME") {
    # Mapas para ME
    if (!is.null(estados)) {
      mapa_proporcao_empresas(df_abertas %>%
                                filter(Porte == "ME") %>%
                                group_by(UF) %>%
                                summarize(total_empresas = n()) %>%
                                mutate(proporcao_nacional = total_empresas / sum(total_empresas) * 100),
                              estados, titulo = paste0(titulo, " - ME"), exibir_num_empresas = exibir_num_empresas)
    }
    if (!is.null(regioes)) {
      mapa_proporcao_empresas_regioes(df_abertas %>%
                                        filter(Porte == "ME") %>%
                                        group_by(Região) %>%
                                        summarize(total_empresas = n()) %>%
                                        mutate(proporcao_nacional = total_empresas / sum(total_empresas) * 100),
                                      regioes, titulo = paste0(titulo, " - ME"), exibir_num_empresas = exibir_num_empresas)
    }
  }
  
  if (porte == "EPP") {
    # Mapas para EPP
    if (!is.null(estados)) {
      mapa_proporcao_empresas(df_abertas %>%
                                filter(Porte == "EPP") %>%
                                group_by(UF) %>%
                                summarize(total_empresas = n()) %>%
                                mutate(proporcao_nacional = total_empresas / sum(total_empresas) * 100),
                              estados, titulo = paste0(titulo, " - EPP"), exibir_num_empresas = exibir_num_empresas)
    }
    if (!is.null(regioes)) {
      mapa_proporcao_empresas_regioes(df_abertas %>%
                                        filter(Porte == "EPP") %>%
                                        group_by(Região) %>%
                                        summarize(total_empresas = n()) %>%
                                        mutate(proporcao_nacional = total_empresas / sum(total_empresas) * 100),
                                      regioes, titulo = paste0(titulo, " - EPP"), exibir_num_empresas = exibir_num_empresas)
    }
  }
  
  if (porte == "Total") {
    # Mapas para Total (ME + EPP)
    if (!is.null(estados)) {
      mapa_proporcao_empresas(df_abertas %>%
                                group_by(UF) %>%
                                summarize(total_empresas = n()) %>%
                                mutate(proporcao_nacional = total_empresas / sum(total_empresas) * 100),
                              estados, titulo = paste0(titulo, " - Total (ME + EPP)"), exibir_num_empresas = exibir_num_empresas)
    }
    if (!is.null(regioes)) {
      mapa_proporcao_empresas_regioes(df_abertas %>%
                                        group_by(Região) %>%
                                        summarize(total_empresas = n()) %>%
                                        mutate(proporcao_nacional = total_empresas / sum(total_empresas) * 100),
                                      regioes, titulo = paste0(titulo, exibir_num_empresas = exibir_num_empresas))
    }
  }
}

# Carregar os dados geográficos das regiões
regioes <- read_region(year = 2020)  # Ou o ano que preferir

# Gerar mapas por UF para os diferentes portes com exibição do número de empresas
gerar_mapas_por_porte(df_abertas, tipo_mapa = "UF", titulo = "Proporção de Empresas por UF", estados = estados, exibir_num_empresas = TRUE, porte = "ME")
gerar_mapas_por_porte(df_abertas, tipo_mapa = "UF", titulo = "Proporção de Empresas por UF", estados = estados, exibir_num_empresas = TRUE, porte = "EPP")
gerar_mapas_por_porte(df_abertas, tipo_mapa = "UF", titulo = "Proporção de Empresas por UF", estados = estados, exibir_num_empresas = TRUE, porte = "Total")
# Gerar mapas por Região para os diferentes portes com exibição do número de empresas
gerar_mapas_por_porte(df_abertas, tipo_mapa = "Região", titulo = "Proporção de Empresas por Região", regioes = regioes, exibir_num_empresas = TRUE, porte = "ME")
gerar_mapas_por_porte(df_abertas, tipo_mapa = "Região", titulo = "Proporção de Empresas por Região", regioes = regioes, exibir_num_empresas = TRUE, porte = "EPP")
gerar_mapas_por_porte(df_abertas, tipo_mapa = "Região", regioes = regioes, porte = "Total")

mapa_total <- gerar_mapas_por_porte(df_abertas, tipo_mapa = "UF", titulo = "Proporção de Empresas por UF", estados = estados, exibir_num_empresas = TRUE, porte = "Total")

# Gráfico Total de Setores (ME + EPP)
ggplot(setor_count_total, aes(x = reorder(Setor, -percent), y = percent, fill = Setor)) +
  geom_bar(stat = "identity", width = 0.9, alpha = 0.8) +
  geom_text(aes(label = labels), vjust = -0.5, size = 6) +
  labs(x = "Setor", y = "Percentagem de Empresas (%)") +
  scale_y_continuous(labels = function(x) paste0(format(x, decimal.mark = ","), "%"), limits = c(0, 70), breaks = seq(0, 70, by = 10)) +
  scale_fill_manual(values = c("Servicos" = "#FC4E2A", "Comercio" = "#005b96", "Industria" = "#7570b3", 
                               "Construcao Civil" = "#e7298a", "Agropecuaria" = "#66a61e")) +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 15, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 15, hjust = 1),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        plot.title = element_text(size = 12, face = "bold"),
        plot.subtitle = element_text(size = 10),
        legend.title = element_text(size = 16, face = "bold"),
        text = element_text(size = 15),
        legend.position = "none")

# Filtrar os 10 maiores CNAEs para cada porte (ME e EPP)
top10_cnaes_porte <- df_abertas %>%
  group_by(Porte, CNAE_Principal, Descricao_CNAE_Principal) %>%
  summarise(n = n(), .groups = 'drop') %>%
  arrange(Porte, desc(n)) %>%
  group_by(Porte) %>%
  slice_max(n, n = 10) %>%
  mutate(labels = paste0(format(round(n / sum(n) * 100, 1), decimal.mark = ","), "% (", format(n, big.mark = ".", decimal.mark = ","), ")"))

# Filtrar os 10 maiores CNAEs para o Total (ME + EPP)
top10_cnaes_total <- df_abertas %>%
  group_by(CNAE_Principal, Descricao_CNAE_Principal) %>%
  summarise(n = n(), .groups = 'drop') %>%
  arrange(desc(n)) %>%
  slice_max(n, n = 10) %>%
  mutate(labels = paste0(format(round(n / sum(n) * 100, 1), decimal.mark = ","), "% (", format(n, big.mark = ".", decimal.mark = ","), ")"))

# Função para criar gráficos de CNAE separados por porte com limites de contagem ajustáveis
grafico_cnae_porte <- function(df, porte, y_max_count = 10000) {
  df_porte <- df %>% filter(Porte == porte)
  
  ggplot(df_porte, aes(x = reorder(Descricao_CNAE_Principal, n), y = n)) +
    geom_bar(stat = "identity", fill = "skyblue", width = 0.9, alpha = 0.8) +
    geom_text(aes(label = labels), hjust = -0.1, size = 3.5) +
    labs(x = "Descrição do CNAE", y = "Número de Empresas") +
    scale_y_continuous(limits = c(0, y_max_count)) +
    theme_minimal() +
    theme(axis.text.x = element_text(size = 10, hjust = 1),
          axis.text.y = element_text(size = 10),
          axis.title.x = element_text(size = 19),
          axis.title.y = element_text(size = 21),
          plot.title = element_text(size = 22, face = "bold"),
          plot.subtitle = element_text(size = 20),
          text = element_text(size = 20)) +
    coord_flip()
}

# Função para criar o gráfico de CNAE para o Total
grafico_cnae_total <- function(df, y_max_count = 10000) {
  ggplot(df, aes(x = reorder(Descricao_CNAE_Principal, n), y = n)) +
    geom_bar(stat = "identity", fill = "skyblue", width = 0.9, alpha = 0.8) +
    geom_text(aes(label = labels), hjust = -0.1, size = 5) +
    labs(x = "Descrição do CNAE", y = "Número de Empresas") +
    scale_y_continuous(limits = c(0, y_max_count)) +
    theme_minimal() +
    theme(axis.text.x = element_text(size = 14, hjust = 1),
          axis.text.y = element_text(size = 14),
          axis.title.x = element_text(size = 16),
          axis.title.y = element_text(size = 16),
          plot.title = element_text(size = 22, face = "bold"),
          plot.subtitle = element_text(size = 15),
          text = element_text(size = 15)) +
    coord_flip()
}


# Renomear as descrições dos CNAEs específicos
top10_cnaes_total <- top10_cnaes_total %>%
  mutate(Descricao_CNAE_Principal = ifelse(Descricao_CNAE_Principal  == "Comércio varejista de mercadorias em geral, com predominância de produtos alimentícios - minimercados, mercearias e armazéns", 
                              "Minimercados, mercearias e armazéns", 
                              Descricao_CNAE_Principal )) %>%
  mutate(Descricao_CNAE_Principal  = ifelse(Descricao_CNAE_Principal == "Atividades de consultoria em gestão empresarial, exceto consultoria técnica específica", 
                              "Atividades de consultoria em gestão empresarial*", 
                              Descricao_CNAE_Principal)) %>%
  mutate(Descricao_CNAE_Principal = ifelse(Descricao_CNAE_Principal == "Atividades de consultoria em gestão empresarial, exceto consultoria técnica específica", 
                              "Atividades de consultoria em gestão empresarial*", 
                              Descricao_CNAE_Principal)) %>%
  mutate(Descricao_CNAE_Principal = ifelse(Descricao_CNAE_Principal == "Preparação de documentos e serviços especializados de apoio administrativo não especificados anteriormente", 
                                           "Preparação de documentos e serviços especializados*", 
                                           Descricao_CNAE_Principal))


"Preparação de documentos e serviços especializados de apoio administrativo não especificados anteriormente"

# Criar gráficos de CNAE para ME, EPP e Total
grafico_cnae_porte(top10_cnaes_porte, "ME", y_max_count = 12000)
grafico_cnae_porte(top10_cnaes_porte, "EPP", y_max_count = 5000)
grafico_cnae_total(top10_cnaes_total, y_max_count = 10000)

# 1. Gráfico das 10 divisões mais frequentes do CNAE
top10_divisoes <- df_abertas %>%
  group_by(CNAE_Divisão, Divisão_Descrição) %>%
  summarise(n = n(), .groups = 'drop') %>%
  arrange(desc(n)) %>%
  slice_max(n, n = 10) %>%
  mutate(labels = format_labels(n, sum(n)))

# Corrigir as descrições, deixando apenas a primeira letra maiúscula
top10_divisoes <- top10_divisoes %>%
  mutate(Divisão_Descrição = str_to_sentence(Divisão_Descrição, locale = "pt"))

# Exibir o resultado
print(top10_divisoes)

# Renomear as descrições dos CNAEs específicos
top10_divisoes <- top10_divisoes %>%
  mutate(Divisão_Descrição = ifelse(Divisão_Descrição  == "Serviços de escritório, de apoio administrativo e outros serviços prestados principalmente às\r\nEmpresas", 
                                    "Serviços de escritório e de apoio administrativo*", 
                                    Divisão_Descrição ))

# Gráfico de divisões do CNAE com limites ajustados
grafico_divisoes <- ggplot(top10_divisoes, aes(x = reorder(Divisão_Descrição, n), y = n)) +
  geom_bar(stat = "identity", fill = "darkorange", width = 0.9, alpha = 0.8) +
  geom_text(aes(label = labels), hjust = -0.1, size = 4) +
  labs(x = "Divisão do CNAE", y = "Número de Empresas") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 14, hjust = 1),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16)
  ) +
  coord_flip() +
  ylim(0, 40000) # Ajuste dos limites do eixo Y

# Exibir o gráfico de divisões do CNAE
print(grafico_divisoes)


# 2. Gráfico dos 10 RGIs mais frequentes
top10_rgis <- df_abertas %>%
  group_by(COD_RGI, NOME_RGI) %>%
  summarise(n = n(), .groups = 'drop') %>%
  arrange(desc(n)) %>%
  slice_max(n, n = 10) %>%
  mutate(labels = format_labels(n, sum(n)))

grafico_rgi <- ggplot(top10_rgis, aes(x = reorder(NOME_RGI, n), y = n)) +
  geom_bar(stat = "identity", fill = "skyblue", width = 0.9, alpha = 0.8) +
  geom_text(aes(label = labels), hjust = -0.1, size = 3.5) +
  labs(x = "Nome do RGI", y = "Número de Empresas", 
       title = "Top 10 RGIs por Número de Empresas") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 10, hjust = 1),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 19),
        axis.title.y = element_text(size = 21),
        plot.title = element_text(size = 22, face = "bold")) +
  coord_flip()

# Exibir o gráfico de RGIs
print(grafico_rgi)