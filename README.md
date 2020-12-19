# ESOPHAGEAL CANCER INCIDENCE AND MORTALITY TRENDS IN RUSSIA 1993-2018
First semestr project (Institute of Bioinformatics)

Esophageal cancer (EC) is the eighth most common cancer worldwide and the sixth leading cause of cancer related mortality1. The aim of this study is to analyze changes in EC incidence and mortality trends using data from the Russian State Cancer Registry.

## Goal: 
to assess trends in the incidence and survival of esophageal cancer among the population of Russia from 1993 to 2018.


### Project objectives:
1. Average annual age-standardized mortality (ASMR) and incidence (ASIR)
2. Changes in incidence and mortality trends
3. Time series analysis
4. Predictions for 10 years

Data was obtained from National Cancer Register (http://www.cancer-reg.ru/).
  
### Methods
  All analyses were performed in R (version 3.6.3, 2020-02-29)), using packages:
  ‘tidyr‘ (version 1.1.2) - data merging, statistical summaries
  ‘Epi‘ (version 2.0)  - statistic analysis and visualization, statistical summaries
  ‘Rcan’ (version 1.3.82) - statistic analysis and visualization, statistical summaries
  ‘tsibble‘ (version 0.9.3) - modelling
  'feast' (version 0.1.6) - statistical summaries, modelling
  'dtwclust' (version 5.5.6) - modelling
  ‘prophet’ (version 0.6.1) - visualization
  'plotly' (version 4.9.2.2) - visualization
  'ggplot2' (version 3.3.2) - visualization
  
### Summary
This study is the first broad assessment of esophageal cancer incidence and mortality patterns and trends from the State Cancer Registry of Russia. The results revealed substantial changes in trends of esophageal cancer in Russia during the past decades. 
The increasing risk of EC can partly be explained by 
      ~ changes in the current Siewert classification
      ~ continuing trends of population aging and growth. 
These results should be used as an aid to setting cancer control priorities in Russia, including the need for the implementation of effective and cost-effect screening and prevention programs, as well the planning of future cancer services based on an allocation of finite resources to ensure their operationalization.


### File guide
#### Data:
1. all_data.csv - data from cancer-register (data has been revised and presented in a convenient form)
2. data.csv - data presented for Rcan package 
3. ASR_1.csv - data from WHO website, standardized population data in the World
4. 23110000100030200003_Численность_постоянного_населения_-_мужчин_по_возрасту_на_1_января.xlsx
23110000100030200004_Численность_постоянного_населения_-_женщин_по_возрасту_на_1_января.xlsx - data from Federal State Statistic Service, standardized population data in Russia


#### Analysis:
1. base_stat.Rmd and .html - ASMR and ASIR, changes in incidence and mortality trends.
2. glm.Rmd - trying to build glmodel, a pattern in residuals - is not good model.
3. Time_series_cancer - time series analysis,good model, predictions for 10 years.


