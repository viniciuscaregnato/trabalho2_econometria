library(plm)
library(glmnet)
library(dplyr)
library(lmtest)
library(sandwich)

painel_ag <- read.csv("df_ag.csv")

#filtrando por ano
df_ag <- df_ag %>%
  filter(ano > 2010)

# remover duplicatas id-ano
painel_ag <- painel_ag %>%
  add_count(id_municipio_nome, ano) %>%
  filter(n == 1) %>%
  select(-n)


#######################################################
# aplicando LASSO sobre variaveis de receita e despesa#
#######################################################


# model frame 
mf <- model.frame(
  populacao_atendida_agua ~
    populacao_urbana +
    volume_agua_produzido +
    volume_agua_faturado +
    consumo_eletrico_sistemas_agua +
    receita_operacional_direta +
    receita_operacional_direta_agua +
    arrecadacao_total +
    despesa_energia +
    investimento_agua_prestador +
    investimento_total_prestador +
    investimento_agua_municipio +
    investimento_total_municipio +
    investimento_agua_estado +
    investimento_total_estado,
  data = painel_ag,
  na.action = na.omit
)

# separar y e X
y <- mf$populacao_atendida_agua
X <- model.matrix(~ populacao_urbana +
                    volume_agua_faturado +
                    volume_agua_produzido +
                    consumo_eletrico_sistemas_agua +
                    receita_operacional_direta +
                    receita_operacional_direta_agua +
                    arrecadacao_total +
                    despesa_energia +
                    investimento_agua_prestador +
                    investimento_total_prestador +
                    investimento_agua_municipio +
                    investimento_total_municipio +
                    investimento_agua_estado +
                    investimento_total_estado,
                  data = mf
)[, -1]

# LASSO
lasso_model <- cv.glmnet(X, y, alpha = 1)

coef_lasso <- coef(lasso_model, s = "lambda.1se")

vars_selecionadas <- rownames(coef_lasso)[
  as.numeric(coef_lasso) != 0
]

# remover intercepto
vars_selecionadas <- setdiff(vars_selecionadas, "(Intercept)")


#####################
# organizando painel#
#####################


painel_ag$natureza_juridica <- as.factor(painel_ag$natureza_juridica)

pdata <- pdata.frame(painel_ag, index = c("id_municipio_nome", "ano"))

#write.csv(pdata, file = "painelfinal_ag.csv", row.names = FALSE)



################################
# Rodando a regressao de painel#
###############################

# formula
form <- populacao_atendida_agua ~
  populacao_urbana +
  volume_agua_produzido +
  volume_agua_faturado +
  consumo_eletrico_sistemas_agua +
  receita_operacional_direta_agua +
  arrecadacao_total
  

fe <- plm(form, data = pdata, model = "within")
re <- plm(form, data = pdata, model = "random")

phtest(fe, re)

####################
# testando residuos#
####################

# autocorrelação
pwartest(fe)

# heterocedasticidade
bptest(fe)


# dependencia seccional
pcdtest(fe, test = "cd")


#################################
#Driscoll–Kraay (robusto a tudo)#
#################################


vcov_dk <- vcovSCC(fe, type = "HC1")
coeftest(fe, vcov = vcov_dk)

###################
# Estrutura final #
###################

pdata <- read.csv("painelfinal_ag.csv")

pdata <- pdata.frame(pdata, index = c("id_municipio_nome", "ano"))

form <- populacao_atendida_agua ~
  populacao_urbana +
  volume_agua_produzido            

mod_fe_final <- plm(form, data = pdata, model = "within")

summary(mod_fe_final)
