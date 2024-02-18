# Carregar o pacote base (se já não estiver carregado)
#####
library(base)
library(rstudioapi)
library(readxl)
library(ggplot2)
library(tidyr)
library(dplyr)
library(writexl)
library(gridExtra)
# Obter o diretório do arquivo atual
ROOT <- rstudioapi::getActiveDocumentContext()$path
ROOT <- setwd(dirname(ROOT))
ROOT
#####
dom = read_excel("../Od_Domic_2012.xlsx")
ind = read_excel("../OD_Individuos_2012.xlsx")
via = read_excel("../OD_Viagens_2012.xlsx")
colnames(via)[15] <- "desc_campo"
colnames(via)[17] <- "desc_campo2"
dom_sta_luzia = dom[dom$nm_cidad == 'SANTA LUZIA',]
ind_sta_luzia = ind[ind$nm_cidad == 'SANTA LUZIA',]
via_sta_luzia = via[via$munic_origem == 'SANTA LUZIA' & 
                      via$munic_destino == 'SANTA LUZIA',]
#####
produc <- aggregate(fator_ex ~ desc_campo, data = via_sta_luzia, FUN = sum)
atrac <- aggregate(fator_ex ~ desc_campo2, data = via_sta_luzia, FUN = sum)
produc <- data_frame(BAIRRO = produc$desc_campo,
                     PRODUCAO = produc$fator_ex)
atrac <- data_frame(BAIRRO = atrac$desc_campo,
                     ATRACAO = atrac$fator_ex)
#pop
pop <- aggregate(fator_exp_pop_ah ~ desc_campo, data = ind_sta_luzia, FUN = sum)
pop <- data.frame(BAIRRO = pop$desc_campo,
                 POPULACAO = pop$fator_exp_pop_ah)
#renda
produc_renda <- pivot_wider(
  data = ind_sta_luzia,
  id_cols = desc_campo,
  names_from = ds_renda,
  values_from = fator_exp_pop_ah,
  values_fn = sum,
  values_fill = 0
  )
col_renda <- colnames(produc_renda)
col_renda
produc_renda <- produc_renda[, -c(4,6,7,8,9,10)]
produc_renda$Total <- rowSums(produc_renda[,-1])
renda_ate3 = data.frame(BAIRRO = produc_renda$desc_campo,RENDA = produc_renda$Total)
#auto
auto <- pivot_wider(
  data = dom_sta_luzia,
  id_cols = ds_campo,
  names_from = qtd_auto,
  values_from = fator_exp_domic_ah,
  values_fn = sum,
  values_fill = 0
)
BAIRRO = auto$ds_campo
auto_ = auto[,-1]
for (i in colnames(auto_)) {
  fator <- as.numeric(i)
  auto_[,i] <- auto_[,i] * fator
}
auto_$Total = data.frame(rowSums(auto_))
produc_auto = data.frame(BAIRRO = BAIRRO,AUTO = auto_$Total$rowSums.auto_.)
####
produc_modelo = data.frame(BAIRRO = BAIRRO,
                           POPULACAO = pop$fator_exp_pop_ah, 
                           RENDA = renda_ate3$produc_renda.Total,
                           AUTO = produc_auto$AUTO)
produc_modelo <- merge(produc, pop, by= 'BAIRRO', all = TRUE) %>%
  merge(produc_auto, by='BAIRRO', all = TRUE) %>%
  merge(renda_ate3, by='BAIRRO', all = TRUE)
replace(x = produc_modelo, list = is.na(produc_modelo), values = 0)
####
#coletivo
coletivo = pivot_wider(
  data = via_sta_luzia,
  id_cols = desc_campo2,
  names_from = modos_ag,
  values_from = fator_ex,
  values_fn = sum,
  values_fill = 0
)
colunas_coletivo <- data.frame(colnames(coletivo))
colunas_coletivo
coletivo <- coletivo[,-c(2,3,5)]
coletivo <- data.frame(BAIRRO = coletivo$desc_campo2,
                      COLETIVO = coletivo$coletivo)
####
#trabalho
trabalho = pivot_wider(
  data = via_sta_luzia,
  id_cols = desc_campo2,
  names_from = motivo_d,
  values_from = fator_ex,
  values_fn = sum,
  values_fill = 0
)
colunas_trabalho = data.frame(colnames(trabalho))
colunas_trabalho
trabalho <- trabalho[,c(1,4,12, 15)]
trabalho$Total <- rowSums(trabalho[,-1])
trabalho <- data.frame(BAIRRO = trabalho$desc_campo2,
                       TRABALHO = trabalho$Total)
####
atrac_modelo <- merge(atrac, pop, id = 'BAIRRO', all = TRUE) %>%
  merge(trabalho, id = 'BAIRRO', all = TRUE) %>%
  merge(coletivo, id = 'BAIRRO', all = TRUE)
replace(x = atrac_modelo, list = is.na(atrac_modelo), values = 0)
####
modelo_atr = lm(formula=ATRACAO ~ POPULACAO + COLETIVO + TRABALHO,
                data = atrac_modelo,na.action = na.omit)
summary(modelo_atr)
modelo_pro = lm(formula= PRODUCAO ~ POPULACAO + RENDA + AUTO,
                data = produc_modelo, na.action = na.omit)
summary(modelo_pro)
####
write.table(atrac_modelo, 
            file="modelo_atr.csv", sep=';')
write.table(produc_modelo,
            file="modelo_pro.csv", sep=';')
write_xlsx(produc_modelo, "modelo_pro.xls")
write_xlsx(atrac_modelo, "modelo_atr.xls")
####
atr_pop <- ggplot(modelo_atr, aes(x = POPULACAO, y = ATRACAO)) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE, color = "grey")+
  labs(title = "ATRAÇÃO x POPULAÇÃO", x = "População", y = "Atração")+
  theme(panel.grid = element_blank())

atr_col <- ggplot(modelo_atr, aes(x = COLETIVO, y = ATRACAO)) +
    geom_point() +
    geom_smooth(method = lm, se = FALSE, color = "grey")+
    labs(title = "ATRAÇÃO x COLETIVO", x = "Coletivo", y = "Atração")+
    theme(panel.grid = element_blank())

atr_tra <- ggplot(modelo_atr, aes(x = TRABALHO, y = ATRACAO)) +
    geom_point() +
    geom_smooth(method = lm, se = FALSE, color = "grey")+
    labs(title = "ATRAÇÃO x TRABALHO", x = "Trabalho", y = "Atração")+
    theme(panel.grid = element_blank())
grid.arrange(atr_pop, atr_col, atr_tra, nrow =3)
####
pro_pop <- ggplot(modelo_pro, aes(x = POPULACAO, y = PRODUCAO)) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE, color = "grey")+
  labs(title = "PRODUÇÃO x POPULAÇÃO", x = "População", y = "Produção")+
  theme(panel.grid = element_blank())

pro_auto <- ggplot(modelo_pro, aes(x = AUTO, y = PRODUCAO)) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE, color = "grey")+
  labs(title = "PRODUÇÃO x AUTOMÓVEL", x = "Automóvel", y = "Produção")+
  theme(panel.grid = element_blank())

pro_renda <- ggplot(modelo_pro, aes(x = RENDA, y = PRODUCAO)) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE, color = "grey")+
  labs(title = "PRODUÇÃO x RENDA", x = "Renda até 3 Salários Mínimos", y = "Produção")+
  theme(panel.grid = element_blank())
grid.arrange(pro_pop, pro_auto, pro_renda, nrow =3)
