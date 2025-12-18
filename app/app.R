library(ExPanDaR)

df <- read.csv("df_app.csv")
df_def <- read.csv("df_def.csv")

df_ag <- read.csv("df_ag.csv")
df_def_ag <- read.csv("df_def_ag.csv")

df_es <- read.csv("df_es.csv")
df_def_es <- read.csv("df_def_es.csv")

ExPanD(
  df = list(df, df_ag, df_es),
  df_def = list(df_def, df_def_ag, df_def_es),
  title = "Análise do Painel dos Municípios de BA, PI e MA",
  abstract = "App interativo SNIS",
  components = c(by_group_violin_graph = FALSE),
  export_nb_option = TRUE
)
