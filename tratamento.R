library(tidyverse)

## VACINACAO

# BRASIL

Vacinas_BR = read.csv("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/vaccinations/vaccinations.csv") %>% 
  filter(location == "Brazil") %>% select(date, total_vacinas = total_vaccinations,
                                          dose_1 = people_vaccinated,
                                          dose_2 = people_fully_vaccinated,
                                          dose_reforco = total_boosters,
                                          dia_vacinas = daily_vaccinations)

Vacinas_BR$dose_2[1:19] = 0
Vacinas_BR$dose_reforco[1:228] = 0
Vacinas_BR$dia_vacinas[1] = 112

F_NA = function(x){
  
  while(sum(is.na(x)) > 0){
    NUM = which(is.na(x))
    x[NUM] = x[NUM-1]
  }
  
  return(x)
  
}

Vacinas_BR %>% mutate(total_vacinas = F_NA(total_vacinas), dose_1 = F_NA(dose_1),
                      dose_2 = F_NA(dose_2), dose_reforco = F_NA(dose_reforco)) %>%
  filter(date <= "2022-12-31") %>% write.csv("Vacinacao_Brasil.csv", row.names = F)

# BAHIA

read.csv("https://ftp.sei.ba.gov.br/covid19/WesleyCota.csv", sep = ";") %>% 
  select(state, dose_1 = vaccinated, vaccinated_second, vaccinated_single,
         reforco = vaccinated_third) %>% filter(state == "BA") %>% 
  mutate(dose_2 = vaccinated_second + vaccinated_single) %>%
  select(-vaccinated_second, -vaccinated_single) %>% 
  write.csv("Vacinacao_Bahia.csv", row.names = F)

## CASOS, OBITOS e RECUPERADOS - https://covid.saude.gov.br/

setwd(paste0(getwd(), "/dados originais"))

COVID = rbind(read.csv("COVID_2020_2022 (1).csv", sep = ";"), 
              read.csv("COVID_2020_2022 (2).csv", sep = ";"),
              read.csv("COVID_2020_2022 (3).csv", sep = ";"),
              read.csv("COVID_2020_2022 (4).csv", sep = ";"),
              read.csv("COVID_2020_2022 (5).csv", sep = ";"),
              read.csv("COVID_2020_2022 (6).csv", sep = ";"))

# BRASIL

### DADOS TEMPORAIS

COVID %>% filter(regiao == "Brasil") %>%
  select(data, casos = casosAcumulado, n_casos = casosNovos, 
         obitos = obitosAcumulado, n_obitos = obitosNovos) %>% 
  filter(data <= "2022-12-31") %>% write.csv("COVID_Brasil.csv", row.names = F)

### DADOS TOTAIS POR ESTADO

DATA = COVID %>% group_by(data) %>% summarise(casos = sum(casosNovos)) %>%
  select(data) %>% tail(1) %>% data.frame()

COVID %>% filter(estado != "", municipio == "", data == DATA$data, is.na(codmun)) %>% 
  select(data, regiao, estado, coduf, pop = populacaoTCU2019, casos = casosAcumulado,
         obitos = obitosAcumulado) %>% 
  mutate(casos_100k = casos*100000/pop, obitos_100k = obitos*100000/pop) %>% 
  filter(data <= "2022-12-31") %>% write.csv("COVID_BR_Est.csv", row.names = F)

# BAHIA

### DADOS TEMPORAIS

COVID %>% filter(estado == "BA") %>% group_by(estado, data) %>% 
  summarise(casos = sum(casosAcumulado), n_casos = sum(casosNovos), 
            obitos = sum(obitosAcumulado), n_obitos = sum(obitosNovos)) %>%
  data.frame() %>% select(-estado) %>% filter(data <= "2022-12-31") %>% 
  write.csv("COVID_BA.csv", row.names = F)

### DADOS TOTAIS POR MUNICIPIO

DATA = COVID %>% group_by(data) %>% summarise(casos = sum(casosNovos)) %>%
  select(data) %>% tail(1) %>% data.frame()

COVID %>% filter(estado == "BA", municipio != "", data == DATA$data) %>%
  select(data, municipio, codmun, pop = populacaoTCU2019, casos = casosAcumulado,
         obitos = obitosAcumulado) %>% 
  mutate(casos_100k = casos*100000/pop, obitos_100k = obitos*100000/pop) %>% 
  write.csv("COVID_BA_Mun.csv", row.names = F)
