

library(tidyverse)
library(ggplot2)
library(scales)


### Oppgave 1




url1 <- "https://raw.githubusercontent.com/uit-sok-2013-h23/Data_Arbeidskrav_1/main/datasett1.csv"

data1 <- read.csv(url1)


## 1.1

tabell1 <- data1 %>% 
  mutate(openness = (EXP + IMP) / gdp)
head(tabell1)

tabell1 %>% filter(LOCATION == "NOR")


## 1.2

tabell2 <- tabell1 %>% filter(TIME == 2020) %>% 
  mutate(bnp_pi = gdp / pop)
head(tabell2)

plot1 <- ggplot(tabell2, aes(x = bnp_pi, y = openness)) +
  geom_point() +
  geom_text(aes(label = LOCATION), size = 3, nudge_y = 0.13) +
  labs(x = "BNP per innbygger", y = "Openness") +
  scale_x_continuous(labels = number) +
  geom_smooth(method = lm, formula = y ~ x + I(x^2)) +
  theme_bw()
plot1


## 1.3

plot2 <- ggplot(tabell2, aes(x = log(bnp_pi), y = openness)) +
  geom_point() +
  geom_text(aes(label = LOCATION), size = 3, nudge_y = 0.13) +
  labs(x = "Log av BNP per innbygger", y = "Openness") +
  scale_x_continuous(labels = number) +
  geom_smooth(method = lm, formula = y ~ x + I(x^2)) +
  theme_bw()
plot2



### Oppgave 2



url2 <- "https://raw.githubusercontent.com/uit-sok-2013-h23/Data_Arbeidskrav_1/main/datasett2.csv"

data2 <- read.csv(url2)


## 2.1

plot3 <- ggplot(data2, aes(x = simil_index, y = overlap)) +
  geom_point() +
  geom_text(aes(label = pcode), size = 3, nudge_y = 0.02) +
  labs(x = "Similarity index", y = "Handelsoverlapp") +
  scale_x_continuous(labels = number) +
  geom_smooth(method = lm, formula = y ~ x + I(x^2)) +
  theme_bw()
plot3


## 2.2

countries <- c("NOR", "DNK", "FIN", "SWE")

countryfilter <- data2[data2$pcode %in% countries,]


plot4 <- ggplot(data2, aes(x = simil_index, y = overlap)) +
  geom_point() +
  geom_text(data = countryfilter, aes(label = pcode), size = 3, hjust = 0, vjust = 1) +
  labs(x = "Similarity index", y = "Handelsoverlapp") +
  scale_x_continuous(labels = number) +
  geom_smooth(method = lm, formula = y ~ x + I(x^2)) +
  theme_bw()
plot4






