test_ono
#sampleです
#変更しました道端
library(tidyverse)

str(iris)
d <- iris
x <- list(m = matrix(1:10, nrow = 2), v = 1:100, df = iris)

x[['df']][["Petal.Length"]]

tashizan <- function(a, b){
  if ((class(a) == "numeric") == FALSE | (class(b) == "numeric") == FALSE){
    stop("数値を入力してください")
  }
  a+b
}

tashizan

x$df$Petal.Length
getwd()
setwd("~/Box Sync/R_lecture/RStudio入門/Sample")
getwd()

for (i in 1:10) {
  print(i)
}

system.time(
  dat <- read.csv("https://raw.githubusercontent.com/ymattu/SampleData/master/csv/Sales.csv")
)

str(dat)

dat <- read.csv("https://raw.githubusercontent.com/ymattu/SampleData/master/csv/Sales.csv", stringsAsFactors = F)

str(dat)


dat2 <- read_csv("https://raw.githubusercontent.com/ymattu/SampleData/master/csv/Sales.csv")
class(dat2)

dat2 <- read_csv("https://raw.githubusercontent.com/ymattu/SampleData/master/csv/Sales.csv",
                 col_types = list(col_character(), col_character(), col_datetime()))

dat2 <- read_csv("https://raw.githubusercontent.com/ymattu/SampleData/master/csv/Sales.csv",
                 col_types = 'ccT')

product <- read_csv("https://raw.githubusercontent.com/ymattu/SampleData/master/csv/Products.csv")
head(product)

product_cp932 <- read_csv("https://raw.githubusercontent.com/ymattu/SampleData/master/csv/Products_cp932.csv")
head(product_cp932)

guess_encoding("https://raw.githubusercontent.com/ymattu/SampleData/master/csv/Products.csv")
guess_encoding("https://raw.githubusercontent.com/ymattu/SampleData/master/csv/Products_cp932.csv")

product_enc <- read_csv("https://raw.githubusercontent.com/ymattu/SampleData/master/csv/Products_cp932.csv",
                        locale = locale(encoding = "cp932"))
head(product_enc)

vignette("dplyr")

scores_messy <- data.frame(
  名前=c("生徒A", "生徒B"),
  算数=c("100","100"),
  国語=c("80","100"),
  理科=c("60","100"),
  社会=c("40","20"),
  stringsAsFactors = F
)

scores_messy

library(tidyverse)
gather(scores_messy,
       key="subject", value = "score",
       算数,国語,理科,社会)

scores_tidy <- gather(scores_messy,
                      key="subject", value = "score",
                      算数,国語,理科,社会)

spread(scores_tidy,
       subject, score)

d <- mpg
d[,1]

mpg %>% 
  select(manufacturer, model, displ, year, cyl) %>% 
  filter(manufacturer == "audi") %>% 
  mutate(century = ceiling(year/100))

mpg %>% 
  arrange(cty)

mpg %>% 
  arrange(-cty)

mpg %>% 
  arrange(desc(cty, hwy))

mpg %>% 
  arrange(desc(manufacturer))

mpg %>% 
  select(model, trans)

mpg %>% 
  select(manufacturer:year)

mpg %>% 
  select(-manufacturer, -year)

mpg %>% 
  select(MODEL = model)

mpg %>% 
  rename(MODEL = model, TRANS = trans)

mpg %>% 
  select(matches(".*c.*"))

mpg %>% 
  mutate(cty_6 = if_else(cyl >= 6,"6_or_more", "less_than_6"))

mpg %>% 
  transmute(cty_6 = if_else(cyl >= 6,"6_or_more", "less_than_6"))

mpg %>% 
  summarise(displ_max = max(displ))

mpg %>% 
  group_by(class) %>% 
  summarize(mean_cty = mean(cty))

mpg %>% 
  group_by(year,class) %>% 
  summarize(max_displ=max(displ))

mpg_grouped <- mpg %>% 
  group_by(manufacturer, year)

mpg %>% 
  transmute(displ_rank = rank(displ, ties.method = "max"))

mpg_grouped %>% 
  transmute(displ_rank=rank(displ, ties.method = "max"))

mpg_grouped %>% 
  filter(n() > 20)

mpg_grouped %>% 
  summarise(displ_max = max(displ))

ungroup(mpg_grouped)
mpg_grouped

mpg %>%
  group_by(manufacturer, year)

lag(1:10)
lag(1:10,3)

uriage <- tibble(
  day = c(1,1,2,2,3,3,4,4),
  store = c("a","b","a","b","a","b","a","b"),
  sales = c(100,500,200,500,400,500,800,500)
)

uriage %>% 
  group_by(store) %>% 
  mutate(sales_diff=sales-lag(sales))

uriage %>% 
  group_by(store) %>% 
  mutate(sales_diff=sales-lead(sales))

uriage %>% 
  group_by(store) %>% 
  mutate(sales_mean = mean(sales),
         sales_err = sales-sales_mean)

mpg %>% mutate(new_col = cyl)
mpg %>% select(new_col = cyl)

set.seed(1)
d <- tibble(
  id=1:10,
  test1 = runif(10, max = 100),
  test2 = runif(10, max = 100),
  test3 = runif(10, max = 100),
  test4 = runif(10, max = 100)
)

d %>% 
  mutate(test1 = round(test1),
         test2 = round(test2),
         test3 = round(test3),
         test4 = round(test4)
  )

d_tidy <- d %>% 
  gather(key = "test", value = "value", test1:test4)
d_tidy

d_tidy %>% 
  mutate(value = round(value))

d_tidy %>% 
  group_by(test) %>%
  summarise(value_avg = mean(value))

d_tidy %>% 
  mutate(value = round(value)) %>%
  spread(key = test, value = value)

d %>% 
  mutate_all(round)

mpg %>%
  mutate_if(is.numeric, round)

d %>% 
  mutate_at(c("test1", "test2", "test3", "test4"), round)

d %>% 
  mutate_at(vars(-id), round)

uriage
tenko <- tibble(
  day = c(1,2,3,4),
  rained = c(F,F,T,F)
)

uriage %>% 
  inner_join(tenko, by = "day")

tenko2 <- tibble(
  DAY = c(1,2,3,4),
  rained = c(F,F,T,F)
)

uriage %>% 
  inner_join(tenko2, by = c("day" = "DAY"))

tenko3 <- tibble(
  DAY = c(1,1,2,2,3),
  store = c("a", "b","a", "b", "b"),
  rained = c(F,F,T,F,T)
)

uriage %>% 
  inner_join(tenko3,by=c("day"="DAY", "store"))

res <- uriage %>% 
  left_join(tenko3, by=c("day"="DAY", "store"))

res %>% 
  mutate(rained=replace_na(rained, FALSE))
