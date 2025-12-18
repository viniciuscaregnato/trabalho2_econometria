library(bigrquery)
library(basedosdados)
library(dplyr)

# Defina o seu projeto no Google Cloud
set_billing_id("crucial-pagoda-476121-p4")

# Para carregar o dado direto no R
query <- "
SELECT
    dados.ano as ano,
    dados.id_municipio AS id_municipio,
    diretorio_id_municipio.nome AS id_municipio_nome,
    dados.sigla_uf AS sigla_uf,
    diretorio_sigla_uf.nome AS sigla_uf_nome,
    dados.natureza_juridica AS natureza_juridica,
    diretorio_natureza_juridica.descricao AS natureza_juridica_descricao,
    dados.populacao_atendida_agua as populacao_atendida_agua,
    dados.populacao_atentida_esgoto as populacao_atentida_esgoto,
    dados.populacao_urbana as populacao_urbana,
    dados.volume_agua_produzido as volume_agua_produzido,
    dados.volume_agua_faturado as volume_agua_faturado,
    dados.consumo_eletrico_sistemas_agua as consumo_eletrico_sistemas_agua,
    dados.consumo_eletrico_sistemas_esgoto as consumo_eletrico_sistemas_esgoto,
    dados.volume_esgoto_tratado as volume_esgoto_tratado,
    dados.volume_esgoto_faturado as volume_esgoto_faturado,
    dados.receita_operacional_direta as receita_operacional_direta,
    dados.receita_operacional_direta_agua as receita_operacional_direta_agua,
    dados.receita_operacional_direta_esgoto as receita_operacional_direta_esgoto,
    dados.arrecadacao_total as arrecadacao_total,
    dados.despesa_energia as despesa_energia,
    dados.investimento_agua_prestador as investimento_agua_prestador,
    dados.investimento_esgoto_prestador as investimento_esgoto_prestador,
    dados.investimento_total_prestador as investimento_total_prestador,
    dados.investimento_agua_municipio as investimento_agua_municipio,
    dados.investimento_esgoto_municipio as investimento_esgoto_municipio,
    dados.investimento_total_municipio as investimento_total_municipio,
    dados.investimento_agua_estado as investimento_agua_estado,
    dados.investimento_esgoto_estado as investimento_esgoto_estado,
    dados.investimento_total_estado as investimento_total_estado
FROM `basedosdados.br_mdr_snis.prestador_agua_esgoto` AS dados
LEFT JOIN (SELECT DISTINCT id_municipio,nome  FROM `basedosdados.br_bd_diretorios_brasil.municipio`) AS diretorio_id_municipio
    ON dados.id_municipio = diretorio_id_municipio.id_municipio
LEFT JOIN (SELECT DISTINCT sigla,nome  FROM `basedosdados.br_bd_diretorios_brasil.uf`) AS diretorio_sigla_uf
    ON dados.sigla_uf = diretorio_sigla_uf.sigla
LEFT JOIN (SELECT DISTINCT id_natureza_juridica,descricao  FROM `basedosdados.br_bd_diretorios_brasil.natureza_juridica`) AS diretorio_natureza_juridica
    ON dados.natureza_juridica = diretorio_natureza_juridica.id_natureza_juridica
"

raw_data <- read_sql(query, billing_project_id = get_billing_id())
#write.csv(raw_data, file = "raw_data.csv", row.names = FALSE)

# limpando os dados:
# por estado:
dados_filtrados <- raw_data %>% filter(sigla_uf %in% c("BA", "PI", "MA"))

# limpando colunas:
dados_filtrados$natureza_juridica_descricao <- NULL
dados_filtrados$sigla_uf_nome <- NULL

#corrigindo os tipos
dados_filtrados <- dados_filtrados %>%
  mutate(
    id_municipio = as.character(id_municipio),
    sigla_uf = as.factor(sigla_uf),
    natureza_juridica = as.factor(natureza_juridica)
  )


#write.csv(dados_filtrados, file = "dados_filtrados.csv", row.names = FALSE)


#dados para o app:
df_app <- dados_filtrados[,!(names(dados_filtrados) %in% "id_municipio")] 
df_def <- data.frame(
  var_name = names(df_app),
  var_def = names(df_app),
  type = c("ts_id", "cs_id", rep("factor",2), rep("numeric",23))
)

write.csv(df_app, file ="app/df_app.csv", row.names = FALSE)
write.csv(df_def, file ="app/df_def.csv", row.names = FALSE)


#dados analise de AG
#dados do app

df_ag <- dados_filtrados %>%
  filter(!is.na(populacao_atendida_agua))

cols_para_na <- c(
  "populacao_atentida_esgoto",
  "consumo_eletrico_sistemas_esgoto",
  "volume_esgoto_tratado",
  "volume_esgoto_faturado",
  "receita_operacional_direta_esgoto",
  "investimento_esgoto_prestador",
  "investimento_esgoto_municipio",
  "investimento_esgoto_estado"
)

# transforma essas colunas em NA (se existirem)
df_ag <- df_ag %>%
  mutate(across(any_of(cols_para_na), ~ NA))

df_ag$id_municipio <- NULL

write.csv(df_ag, file = "app/df_ag.csv", row.names = FALSE)

df_ag <- df_ag %>%
  filter(ano > 2010)
write.csv(df_ag, file = "painel/df_ag.csv", row.names = FALSE)

df_def_ag <- data.frame(
  var_name = names(df_ag),
  var_def = names(df_ag),
  type = c("ts_id", "cs_id", rep("factor",2), rep("numeric",23))
)

write.csv(df_def_ag, file = "app/df_def_ag.csv", row.names = FALSE)

#dados analise de ES

df_es <- dados_filtrados %>%
  filter(!is.na(populacao_atentida_esgoto))



cols_para_na <- c(
  "populacao_atendida_agua",
  "volume_agua_produzido",
  "volume_agua_faturado",
  "consumo_eletrico_sistemas_agua",
  "receita_operacional_direta_agua",
  "investimento_agua_prestador",
  "investimento_agua_municipio",
  "investimento_agua_estado"
)

# transforma essas colunas em NA (se existirem)
# dados para o app es
df_es <- df_es %>%
  mutate(across(any_of(cols_para_na), ~ NA))

df_es$id_municipio <- NULL


write.csv(df_es, file = "app/df_es.csv", row.names = FALSE)

df_es <- df_es %>%
  filter(ano > 2010)

write.csv(df_es, file = "painel/df_es.csv", row.names = FALSE)


df_def_es <- data.frame(
  var_name = names(df_es),
  var_def = names(df_es),
  type = c("ts_id", "cs_id", rep("factor",2), rep("numeric",23))
)

write.csv(df_def_es, file = "app/df_def_es.csv", row.names = FALSE)
