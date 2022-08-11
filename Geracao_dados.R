library(tidyverse)

## VACINACAO BRASIL

Vacinas_BR = read.csv("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/vaccinations/vaccinations.csv") %>% 
  filter(location == "Brazil") %>% select(date, total_vaccinations, people_vaccinated,
                                          people_fully_vaccinated, total_boosters,
                                          daily_vaccinations)

Vacinas_BR$people_fully_vaccinated[1:19] = 0
Vacinas_BR$total_boosters[1:228] = 0
Vacinas_BR$daily_vaccinations[1] = 112

F_NA = function(x){
  
  while(sum(is.na(x)) > 0){
    NUM = which(is.na(x))
    x[NUM] = x[NUM-1]
  }
  
  return(x)
  
}

Vacinas_BR %>% mutate(total_vaccinations = F_NA(total_vaccinations),
                      people_vaccinated = F_NA(people_vaccinated),
                      people_fully_vaccinated = F_NA(people_fully_vaccinated),
                      total_boosters = F_NA(total_boosters)) %>%
  write.csv("Vacinacao_Brasil.csv", row.names = F)

## CASOS, OBITOS e RECUPERADOS

COVID = rbind(read.csv("COVID_2020_2022_AGOSTO (1).csv", sep = ";"), 
              read.csv("COVID_2020_2022_AGOSTO (2).csv", sep = ";"),
              read.csv("COVID_2020_2022_AGOSTO (3).csv", sep = ";"),
              read.csv("COVID_2020_2022_AGOSTO (4).csv", sep = ";"),
              read.csv("COVID_2020_2022_AGOSTO (5).csv", sep = ";"),
              read.csv("COVID_2020_2022_AGOSTO (6).csv", sep = ";"))

# BRASIL

COVID %>% filter(regiao == "Brasil") %>%
  select(data, casos = casosAcumulado, n_casos = casosNovos, 
         obitos = obitosAcumulado, n_obitos = obitosNovos) %>% 
  write.csv("COVID_Brasil.csv", row.names = F)

# BAHIA

COVID %>% filter(estado == "BA") %>% group_by(estado, data) %>% 
  summarise(casos = sum(casosAcumulado), n_casos = sum(casosNovos), 
            obitos = sum(obitosAcumulado), n_obitos = sum(obitosNovos)) %>%
  data.frame() %>% select(-estado) %>% write.csv("COVID_BA.csv", row.names = F)


