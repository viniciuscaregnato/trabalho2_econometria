library(plm)
library(glmnet)
library(dplyr)
library(lmtest)
library(sandwich)

painel_es <- read.csv("painel_es.csv")


# remover duplicatas id-ano
painel_es <- painel_es %>%
  add_count(id_municipio, ano) %>%
  filter(n == 1) %>%
  select(-n)


#######################################################
# aplicando LASSO sobre variaveis de receita e despesa#
#######################################################


# model frame 
mf <- model.frame(
  populacao_atentida_esgoto ~
    populacao_urbana +
    volume_esgoto_tratado +
    volume_esgoto_faturado +
    consumo_eletrico_sistemas_esgoto +
    receita_operacional_direta +
    receita_operacional_direta_esgoto +
    arrecadacao_total +
    despesa_energia +
    investimento_esgoto_prestador +
    investimento_total_prestador +
    investimento_esgoto_municipio +
    investimento_total_municipio +
    investimento_esgoto_estado +
    investimento_total_estado,
  data = painel_es,
  na.action = na.omit
)

# separar y e X
y <- mf$populacao_atentida_esgoto
X <- model.matrix(~ populacao_urbana +
                    volume_esgoto_faturado +
                    volume_esgoto_tratado +
                    consumo_eletrico_sistemas_esgoto +
                    receita_operacional_direta +
                    receita_operacional_direta_esgoto +
                    arrecadacao_total +
                    despesa_energia +
                    investimento_esgoto_prestador +
                    investimento_total_prestador +
                    investimento_esgoto_municipio +
                    investimento_total_municipio +
                    investimento_esgoto_estado +
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

# Limpando as observações que se repetem no mesmo ano
painel_es <- painel_es %>%
  add_count(id_municipio, ano) %>%
  filter(n == 1) %>%
  select(-n)

painel_es$natureza_juridica <- as.factor(painel_es$natureza_juridica)

pdata <- pdata.frame(painel_es, index = c("id_municipio", "ano"))

#write.csv(pdata, file = "painelfinal_es.csv", row.names = FALSE)



################################
# Rodando a regressao de painel#
###############################

# formula
form <- populacao_atentida_esgoto ~
  populacao_urbana +
  volume_esgoto_faturado +
  volume_esgoto_tratado +
  receita_operacional_direta_esgoto +
  arrecadacao_total +
  investimento_esgoto_municipio +
  investimento_total_municipio
  

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


###################################################
#Erros-padrao robustos clusterizados por municipio#
###################################################

coeftest(
  fe,
  vcov = vcovHC(fe, type = "HC1", cluster = "group")
)


###################
# Estrutura final #
###################

pdata <- read.csv("painelfinal_es.csv")

pdata <- pdata.frame(pdata, index = c("id_municipio", "ano"))

form <- populacao_atentida_esgoto ~
  populacao_urbana +
  volume_esgoto_tratado +
  volume_esgoto_faturado +
  investimento_esgoto_municipio +
  investimento_total_municipio
  
  
mod_fe_final <- plm(form, data = pdata, model = "within")

summary(mod_fe_final)


