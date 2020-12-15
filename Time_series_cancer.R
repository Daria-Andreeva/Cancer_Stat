library(readr)
library(tibble)
library(tsibble)
library(dplyr)
library(tidyr)
library(feasts)
library(dtwclust)
library(ggrepel)
library(plotly)
library(prophet)

#### Данные
cancer<-read.csv("all_data.csv") #все данные прочитаем
d<-read.csv("mortality_female.csv")
data_18 <- read_csv("Bioinf/statistics_r/cancer/data/data.csv")

#### Форматирование
mort <- cancer %>% filter(Type=="mortality")
incid <- all_data %>% filter(Type == 'incidence')

mort$Age<-paste(mort$Sex, mort$Age)# потому что лень создавать отдельную переменную 
incid$Age<-paste(incid$Sex, incid$Age)

cryptos <- as_tsibble(mort[, -c(1, 2)], key = c(Age) , index = Year)
cryptos_in <- as_tsibble(incid[, -c(1, 2)], key = c(Age) , index = Year)

glimpse(cryptos, width = 60)
glimpse(cryptos_in, width = 60)

#### потому что у этих групп нет смертей
cryptos<-cryptos %>% filter(Age!="male age04"& Age!="male age1014"&Age!= "male age59"&Age!="male age1519"&Age!="fem age04"& Age!="fem age1014"&Age!= "fem age59"&Age!="fem age1519")
cryptos_in<-cryptos_in %>% filter(Age!="male age04"& Age!="male age1014"&Age!= "male age59"&Age!="male age1519"&Age!="fem age04"& Age!="fem age1014"&Age!= "fem age59"&Age!="fem age1519")

cryptos %>% autoplot()+scale_x_continuous(n.breaks = 21)
cryptos_in %>% autoplot()+scale_x_continuous(n.breaks = 21)

group<-c(rep("pink", 294), rep("blue", 294))

#### Разложение рада на компоненты
cryptos %>% autoplot()+scale_x_continuous(n.breaks = 21)
group<-c(rep("pink", 294), rep("blue", 294))
col_group<-rep(group, 3)
cryptos %>%
  group_by_key() %>% 
  model(STL(Amount ~ trend())) %>%
  components() %>% 
  autoplot() + theme_minimal() +
  theme(legend.position = "none", legend.direction = "vertical")

cryptos_in %>% autoplot()+scale_x_continuous(n.breaks = 21)
group<-c(rep("pink", 294), rep("blue", 294))
col_group<-rep(group, 3)
cryptos_in %>%
  group_by_key() %>% 
  model(STL(Amount ~ trend())) %>%
  components() %>% 
  autoplot() + theme_minimal() +
  theme(legend.position = "none", legend.direction = "vertical")

#### Структура остатков. Интересный паттерн. Мб стоит вывести его в сезонность
p  <-cryptos %>%
  group_by_key() %>% 
  model(STL(Amount ~ trend())) %>%
  components()

ggplot(p, aes(x = Year, y = remainder, colour = group, group = Age))+geom_line()

inc <- cryptos_in %>%
  group_by_key() %>% 
  model(STL(Amount ~ trend())) %>%
  components()

ggplot(inc, aes(x = Year, y = remainder, colour = group, group = Age))+geom_line()

#### Вычленение признаков и анализ главных компонент
all_crypto_features <- cryptos %>% 
  features(., Amount, feature_set(pkgs = "feasts"))

all_crypto_features_inc <- cryptos_in %>% 
  features(., Amount, feature_set(pkgs = "feasts"))
# Функция для нахождения признаков c нулевой дисперсией:
zero_var_cols <- function(dat) {
  checks <- lapply(dat, function(x) length(unique(x)))
  keep <- which(!checks > 1)
  unlist(keep) %>% names(.)
}

set.seed(1984)
pca <- all_crypto_features %>% na.omit() %>% 
  dplyr::select(-c(Age, zero_var_cols(.))) %>%
  prcomp(scale = TRUE)

pca_inc <- all_crypto_features_inc %>% na.omit() %>% 
  dplyr::select(-c(Age, zero_var_cols(.))) %>%
  prcomp(scale = TRUE)

summary(pca)
summary(pca_inc)

pc <- all_crypto_features %>%  na.omit() %>% 
  dplyr::select(Age) %>% 
  bind_cols(., as_tibble(pca$x))
pca_inc <- all_crypto_features_inc %>%  na.omit() %>% 
  dplyr::select(Age) %>% 
  bind_cols(., as_tibble(pca_inc$x))

pc %>% 
  ggplot(., aes(PC1, PC2, label = Age)) +
  geom_point() +
  geom_text_repel(force = 10, segment.color = "gray60") +
  theme_minimal()   

plot_ly(pc, x = ~PC1, y = ~PC2, z = ~PC3)

pca_inc %>% 
  ggplot(., aes(PC1, PC2, label = Age)) +
  geom_point() +
  geom_text_repel(force = 10, segment.color = "gray60") +
  theme_minimal()   

plot_ly(pca_inc, x = ~PC1, y = ~PC2, z = ~PC3)

#### Анализ автокорреляции в рядах. Она есть в некоторых на лагах 1 и 2 в основном
cryptos %>% filter(Age == "male age85") %>% gg_lag(geom = "point", period = "year", lags = c(1:12)) + theme_minimal()

cryptos_in %>% filter(Age == "male age85") %>% gg_lag(geom = "point", period = "year", lags = c(1:12)) + theme_minimal()

cryptos %>% 
  group_by_key(Age) %>% 
  ACF() %>% 
  autoplot() + theme_minimal()

cryptos_in %>% 
  group_by_key(Age) %>% 
  ACF() %>% 
  autoplot() + theme_minimal()
#### Кластерный анализ
cryptos_list <- cryptos %>% 
  mutate(Amount = log(Amount+0.0001)) %>% 
  pivot_wider(., names_from = Age, values_from = Amount) %>% 
  arrange(Year) %>% select(-Year) %>% 
  as.list()

cryptos_list_inc <- cryptos_in %>% 
  mutate(Amount = log(Amount+0.0001)) %>% 
  pivot_wider(., names_from = Age, values_from = Amount) %>% 
  arrange(Year) %>% select(-Year) %>% 
  as.list()

hc_4_ward <- tsclust(
  cryptos_list[-29],
  k = 3,                 # запрашиваемое число кластеров
  type = "hierarchical", # тип кластеризации
  distance = "dtw",      # мера расстояния
  seed = 42,
  control = 
    hierarchical_control(method = "average"), # метод агломерации
  args = 
    tsclust_args(dist = list(window.size = 7)) # размер окна Сакэ-Чиба
) 

hc_4_ward_i <- tsclust(
  cryptos_list_inc[-29],
  k = 3,                 # запрашиваемое число кластеров
  type = "hierarchical", # тип кластеризации
  distance = "dtw",      # мера расстояния
  seed = 42,
  control = 
    hierarchical_control(method = "average"), # метод агломерации
  args = 
    tsclust_args(dist = list(window.size = 7)) # размер окна Сакэ-Чиба
) 

hc_4_ward   
hc_4_ward@cluster

hc_4_ward_i
hc_4_ward_i@cluster

par(mar = c(0, 4, 2, 2))
plot(hc_4_ward, xlab = "", sub = "", main = "")   
plot(hc_4_ward_i, xlab = "", sub = "", main = "")   
#### Попытка моделирования не одного ряда, а обощенных данных по кластеру (ну раз они похожи)
clust_name<- names(hc_4_ward@cluster)[hc_4_ward@cluster == 3]
clust_name_inc<- names(hc_4_ward_i@cluster)[hc_4_ward_i@cluster == 2]
str(hc_4_ward_i@cluster)


bitcoin_train <-cryptos[cryptos$Age%in%clust_name, ]%>% index_by(Year) %>% summarise(y = sum(Amount)/18) %>% 
  mutate(ds = c('1993-01-01', '1994-01-01', '1995-01-01', '1996-01-01', 
                '1997-01-01', '1998-01-01', '1999-01-01', '2000-01-01', 
                '2001-01-01', '2002-01-01', '2003-01-01', '2004-01-01', 
                '2005-01-01', '2006-01-01', '2007-01-01', '2008-01-01', 
                '2009-01-01', '2010-01-01', '2011-01-01', '2012-01-01', 
                '2013-01-01')) %>% 
  as.data.frame() %>% 
  select(ds, y)

bitcoin_train_inc <-cryptos_in[cryptos_in$Age%in%clust_name_inc, ]%>% index_by(Year) %>% summarise(y = sum(Amount)/22) %>% 
  mutate(ds = c('1993-01-01', '1994-01-01', '1995-01-01', '1996-01-01', 
                '1997-01-01', '1998-01-01', '1999-01-01', '2000-01-01', 
                '2001-01-01', '2002-01-01', '2003-01-01', '2004-01-01', 
                '2005-01-01', '2006-01-01', '2007-01-01', '2008-01-01', 
                '2009-01-01', '2010-01-01', '2011-01-01', '2012-01-01', 
                '2013-01-01', '2014-01-01', '2015-01-01', '2016-01-01', 
                '2017-01-01', '2018-01-01')) %>% 
  as.data.frame() %>% 
  select(ds, y)


bitcoin_train %>% 
  ggplot(., aes(x = ds, y = y)) +
  geom_line() 

bitcoin_train_inc %>% 
  ggplot(., aes(x = ds, y = y)) +
  geom_line()

ggplot(bitcoin_train, aes(x = ds, y = y)) +
  geom_line() +
  geom_point()
ggplot(bitcoin_train_inc, aes(x = ds, y = y)) +
  geom_line() +
  geom_point()

M0 <- prophet(bitcoin_train)   
M0_i <- prophet(bitcoin_train_inc)

future_df <- make_future_dataframe(M0, periods = 10, freq = "year")
forecast_M0 <- predict(M0, future_df)   

future_df_i <- make_future_dataframe(M0_i, periods = 10, freq = "year")
forecast_M0_i <- predict(M0_i, future_df_i) 

forecast_M0 %>% 
  dplyr::select(yhat, yhat_lower, yhat_upper) %>% head()   
forecast_M0_i %>% 
  dplyr::select(yhat, yhat_lower, yhat_upper) %>% head() 

plot(M0, forecast_M0, xlab = 'Calendar period', ylab = 'Absolute number of esophageal cancer death')  +
  theme_minimal()   
plot(M0_i, forecast_M0_i, xlab = 'Календарный период', ylab = 'Абсолютное число случаев смерти\n вследствие рака пищевода')  +
  theme_minimal() 

library(Rcan)
csu_ageSpecific(data_18, var_age='Age',
                var_cases='Incidence',
                var_py='Population')

asr <- csu_asr(data_18,
        var_age='Age',
        var_cases='Incidence',
        var_py='Population',
        group_by=c('Year', 'Sex'),
        last_age=18,
        pop_base='SEGI')

csu_time_trend(asr, var_trend='Incidence',
               var_year='Year',
               group_by='Sex')

csu_time_trend(general_pmi, var_trend='Incidence',
               var_year='Year',)
