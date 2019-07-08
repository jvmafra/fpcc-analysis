library(RSQLite)
library(ggplot2)
library(tidyverse)
library(ggthemes)

read_sqlite <- function(filename){
  sqlite.driver <- dbDriver("SQLite")
  db <- dbConnect(sqlite.driver,
                  dbname = filename)
  
  results <- dbReadTable(db, "query_result")
  return (results)
}

# Lê os arquivos utilizados na simulação
trace <- read.csv("/local/mafra/workspace/community-based-analytics-simulator/input_trace/trace_utfpr.csv", sep = ",", header = FALSE)
colnames(trace) <- c("type", "origin", "dest", "timestamp", "cardNum", "duration", "start_time", "id")
result.own.data <- read_sqlite("/local/mafra/workspace/community-based-analytics-simulator/results/result-own-data.sqlite3")
result.presence.data <- read_sqlite("/local/mafra/workspace/community-based-analytics-simulator/results/result-presence-data-1h-v2.sqlite3")
result.all.data <- read_sqlite("/local/mafra/workspace/community-based-analytics-simulator/results/result-all-data.sqlite3")


# Define função que calcula a proporção de requisições atendidas por usuário
propByUser <- function(trace, result, scenario){
  dep <- trace %>% filter(type == "DEPARTURE")
  queries.with.user <- merge(result, dep, by = "id")
  avail.table <- queries.with.user %>% dplyr::group_by(cardNum) %>% 
    dplyr::summarise(avail = sum(used_information > 0)/n(), total = n()) %>%
    dplyr::mutate(scenario = scenario)
  return (avail.table)  
}

# Obtém a proporção de requisições atendidas por usuário em cada cenário
prop.own.data <- propByUser(trace, result.own.data, "own-data")
prop.presence.data <- propByUser(trace, result.presence.data, "presence-based")
prop.all.data <- propByUser(trace, result.all.data, "all-data")

prop.all.scenarios <- do.call(rbind, list(prop.own.data,
                                          prop.presence.data,
                                          prop.all.data))

prop.all.scenarios$scenario = factor(prop.all.scenarios$scenario, levels = c("all-data", "presence-based","own-data"))


# Histograma dos 3 cenários - Visão geral dos dados
ggplot(prop.all.scenarios, aes(x = avail * 100)) +
  geom_histogram(bins = 15, fill = "salmon") +
  xlab("Proporção de requisições atendidas (%)") +
  ylab("Frequência") +
  facet_grid(~scenario) +
  theme_calc() 

# Densidade
ggplot(prop.all.scenarios, aes(x = avail * 100)) +
  geom_density(fill = "salmon") +
  xlab("Proporção de requisições atendidas dos usuários (%)") +
  ylab("Density") +
  facet_grid(~scenario) +
  theme_calc() 

# Função de distribuição de probabilidade
ggplot(prop.all.scenarios, aes(x = avail * 100, group = scenario, color = scenario)) +
  stat_ecdf(geom = "step", pad = FALSE) +
  labs(x = "Proporção de requisições atendidas do usuário (%)", 
       y = "P(x <= X)",
       color = "Política") +
  theme_calc() 


# Diferença entre as proporções médias dos cenários

df <- data.frame(scenario = character(),
                 conf.low = double(),
                 conf.high = double())



# Intervalo de confiança para diferença da proporção média entre all-data e own-data
all.own <- rbind(prop.all.data, prop.own.data)
test.a <- t.test(avail ~ scenario, data=all.own, conf.level=.99)
conf.low <- test.a$conf.int[1]
conf.high <- test.a$conf.int[2]
all.versus.own <- data.frame(scenario = "all-data/own-data",
                             conf.low = conf.low,
                             conf.high = conf.high)
df <- rbind(df, all.versus.own)

# Intervalo de confiança para diferença da proporção média entre presence-data e own-data
presence.own <- rbind(prop.presence.data, prop.own.data)
presence.own$scenario = factor(presence.own$scenario, levels = c("presence-based","own-data"))
test.c <- t.test(avail ~ scenario, data=presence.own, conf.level=.99)
conf.low <- test.c$conf.int[1]
conf.high <- test.c$conf.int[2]
presence.versus.own <- data.frame(scenario = "presence-based/own-data",
                                  conf.low = conf.low,
                                  conf.high = conf.high)
df <- rbind(df, presence.versus.own)

# Intervalo de confiança para diferença da proporção média entre all-data e presence-data
all.presence <- rbind(prop.all.data, prop.presence.data)
test.b <- t.test(avail ~ scenario, data=all.presence, conf.level=.99)
conf.low <- test.b$conf.int[1]
conf.high <- test.b$conf.int[2]
all.versus.presence <- data.frame(scenario = "all-data/presence-based",
                             conf.low = conf.low,
                             conf.high = conf.high)
df <- rbind(df, all.versus.presence)

ggplot(df, aes(x = scenario, color = "salmon")) +
  geom_errorbar(aes(ymax = conf.high*100, ymin = conf.low*100)) +
  labs(x = "Políticas comparadas duas a duas",
       y = "Diferença entre as proporções médias (%)") +
  theme_calc() +
  guides(color=FALSE)



# Define função que calcula o ganho de informação por usuário
valid.queries <- result.own.data %>% filter(used_information > 0)
valid.queries.id <- valid.queries$id

getGainPerUser <- function(trace, result, valid.queries.id, scenario){
  
  dep <- trace %>% filter(type == "DEPARTURE")
  queries.with.user <- merge(result, dep, by = "id")
  
  filtered.queries <- queries.with.user %>% filter(id %in% valid.queries.id)
  merged <- merge(filtered.queries, result.own.data, by = "id")
  
  gain.per.queries <- merged %>%
    dplyr::mutate(gain = (used_information.x - used_information.y)/used_information.y)
  
  gain.per.user <- gain.per.queries %>% 
    dplyr::group_by(cardNum) %>% 
    dplyr::summarise(median_gain = median(gain)) %>%
    dplyr::mutate(scenario = scenario)
  
  return (gain.per.user)
}


# Obtendo o ganho dos dois cenários em comparação com o all-data
gain.per.user.presence.scenario <- getGainPerUser(trace, result.presence.data, valid.queries.id, "presence-based")
gain.per.user.all.data.scenario <- getGainPerUser(trace, result.all.data, valid.queries.id, "all-data")

gain.binded <- rbind(gain.per.user.presence.scenario, gain.per.user.all.data.scenario)


# Densidade
ggplot(gain.binded, aes(x = median_gain * 100)) +
  geom_density(fill = "salmon") +
  xlab("Ganho de informação em comparação com a política own-data (%) em escala de log") +
  ylab("Density") +
  facet_grid(~scenario) +
  scale_x_log10() +
  theme_calc() 

# FDP
ggplot(gain.binded, aes(x = median_gain * 100, group = scenario, color = scenario)) +
  stat_ecdf(geom = "step", pad = FALSE) +
  labs(x = "Ganho de informação em comparação com a política own-data (%)", 
       y = "P(x <= X)",
       color = "Política") +
  scale_x_log10() +
  theme_calc()


# 

df.gain <- data.frame(scenario = character(),
                 conf.low = double(),
                 conf.high = double())



# Intervalo de confiança para diferença do ganho médio de informação entre all-data e presence-data 
#(Ganho esse em relação ao own-data)
all.presence.gain <- rbind(gain.per.user.all.data.scenario, gain.per.user.presence.scenario)
test <- t.test(median_gain ~ scenario, data=all.presence.gain, conf.level=.99)
conf.low <- test$conf.int[1]
conf.low
conf.high <- test$conf.int[2]
conf.high
all.versus.presence.gain <- data.frame(scenario = "all_data - own_data",
                             conf.low = conf.low,
                             conf.high = conf.high)

df.gain <- rbind(df.gain, all.versus.presence.gain)


ggplot(df.gain, aes(x = scenario, color = "salmon")) +
  geom_errorbar(aes(ymax = conf.high*100, ymin = conf.low*100)) +
  labs(x = "X",
       y = "Diferença entre a média dos ganhos(%)") +
  theme_calc() +
  guides(color=FALSE)



