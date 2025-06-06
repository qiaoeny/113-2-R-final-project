##讀取csv資料

library(tidyverse)
離婚分析 <- read_csv("離婚.csv")

##查看資料

glimpse(離婚分析)

##重新命名資料欄位

離婚分析 <- 離婚分析 |>
  rename(
    地區 = `離婚對數...1`,
    離婚對數 = `離婚對數...2`
  )

##列出地區不重複的值

unique(離婚分析$地區)

##將地區及年分分開以便分析

library(tidyverse)

離婚分析_clean <- 離婚分析 |>
  separate(地區, into = c("年月", "地區"), sep = "/ ") |> 
  mutate(年月 = str_trim(年月),
         地區 = str_trim(地區))

glimpse(離婚分析_clean)

##將區域別總計過濾

離婚分析_clean <- 離婚分析_clean |>
  filter(地區 != "區域別總計")



library(dplyr)
library(ggplot2)
library(stringr)
library(forcats)

# 計算每個地區的平均離婚對數（排除總計與合併月份資料）
平均離婚對數 <- 離婚分析_clean |>
  filter(!str_detect(年月, "1~10月"), 地區 != "區域別總計") |>
  group_by(地區) |>
  summarise(平均離婚對數 = mean(離婚對數, na.rm = TRUE)) |>
  ungroup() |>
  mutate(地區 = fct_reorder(地區, 平均離婚對數))

# 繪圖
ggplot(平均離婚對數, aes(x = 地區, y = 平均離婚對數)) +
  geom_col(fill = "lightcoral") +
  coord_flip() +
  labs(
    title = "109年各縣市平均離婚對數比較",
    x = "地區",
    y = "平均離婚對數（每月）"
  ) +
  theme_minimal()

summarise(總離婚對數 = sum(離婚對數, na.rm = TRUE))
