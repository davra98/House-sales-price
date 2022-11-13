####Import packages and data####

library(tidyr)
library(dplyr)
library(tidyverse)
library(tidymodels)
library(doParallel)
all_cores <- parallel::detectCores(logical = FALSE)
registerDoParallel(cores = all_cores)

PATH <- "https://raw.githubusercontent.com/aldosolari/DM/master/docs/HomePrices/"
Train = read_csv2(paste0(PATH,"home_prices_train.csv"))
Test =  read_csv2(paste0(PATH,"home_prices_test.csv"))

####Data preparation####

Train[,c(3,4,7:10,15)] <- sapply(Train[,c(3,4,7:10,15)], as.character)
Test[,c(2,3,6:9,14)] <- sapply(Test[,c(2,3,6:9,14)], as.character)

Train$date_sold<-as.Date(Train$date_sold)
Test$date_sold<-as.Date(Test$date_sold)


Train<-separate(Train, "date_sold", c("Year", "Month", "Day"), sep = "-")
Test<-separate(Test, "date_sold", c("Year", "Month", "Day"), sep = "-")


Train <- Train %>% mutate_if(is.character,as.factor)
Test <- Test %>% mutate_if(is.character,as.factor)

Train$d_year=Train$year_renovated - Train$yr_built
Test$d_year=Test$year_renovated - Test$yr_built

year=c(seq(0,100,20),max(Train$d_year))
group=c(1:6)

for (i in c(1:6)){
  Train$d_year<-ifelse(Train$d_year<=year[i+1] & Train$d_year>year[i],group[i],Train$d_year)
  Test$d_year<-ifelse(Test$d_year<=year[i+1] & Test$d_year>year[i],group[i],Test$d_year)
}

Train$d_year<-as.factor(Train$d_year)
Test$d_year<-as.factor(Test$d_year)

####Data pre-processing####

levels(Train$d_year)[2:5]='1'
levels(Train$d_year)[3:4]='2'
levels(Test$d_year)[2:5]='1'
levels(Test$d_year)[3:4]='2'

which(Train$bedrooms==33)
Train<-Train[-12659,]
Train$bedrooms<-as.factor(as.character(Train$bedrooms)) ##cos? rimuvo un livello vuoto


levels(Train$bedrooms)[1:2]='<=1'
levels(Train$bedrooms)[c(2:3,9:11)]='>7'

levels(Test$bedrooms)[1:2]='<=1'
levels(Test$bedrooms)[c(2,8:10)]='>7'

levels(Train$bathrooms)[1:3]<-'<1'
levels(Train$bathrooms)[2:3]<-'1'
levels(Train$bathrooms)[16:26]<-'5'

levels(Test$bathrooms)[1:3]<-'<1'
levels(Test$bathrooms)[2:3]<-'1'
levels(Test$bathrooms)[16:22]<-'5'

levels(Train$floors)[5:6]='>=3'
levels(Test$floors)[5:6]='>=3'

####Data modeling####

continuos_train<-select_if(Train, is.numeric)

mod <- lm(price ~ ., data=continuos_train)
cooksd <- cooks.distance(mod)

Train<-Train[cooksd< 0.035,]

rec_poly<-recipe(price ~  Month + Day + price + bedrooms + bathrooms + sqft_living + 
                   sqft_lot + sqft_above+ floors + waterfront + view + condition+ year_renovated + zip_code + lattitude + 
                   longitude + nn_sqft_living + nn_sqft_lot + d_year,
                 data = Train) %>%
  step_poly(sqft_above,sqft_living,sqft_lot,lattitude,longitude,nn_sqft_living,nn_sqft_lot,degree=2)

rec_interact<-rec_poly %>%
  step_dummy(all_nominal_predictors()) %>% 
  step_interact( ~ sqft_lot_poly_1:starts_with("zip_code"))%>%
  step_interact( ~ sqft_lot_poly_1:starts_with("view")) %>%
  step_interact( ~ lattitude_poly_1:starts_with("zip_code"))%>%
  step_interact( ~ lattitude_poly_1:starts_with("floors")) %>%
  step_interact( ~ longitude_poly_1:starts_with("zip_code"))%>%
  step_interact( ~ longitude_poly_1:starts_with("floors")) %>%
  step_interact( ~ longitude_poly_1:starts_with("waterfront")) %>%
  step_interact( ~ nn_sqft_lot_poly_1:starts_with("d_year"))

rec_interact_sl<- rec_interact %>% 
  step_ns(lattitude_poly_1,longitude_poly_1,sqft_lot_poly_1, deg_free = 20)

lm_model <- linear_reg() %>% set_engine("lm")

lm_wflow <- 
  workflow() %>% 
  add_model(lm_model) %>% 
  add_recipe(rec_interact_sl)

lm_fit <- fit(lm_wflow, Train)

xgboost_model <- 
  parsnip::boost_tree(
    mode = "regression",
    trees = 1000,
    min_n = 10,
    tree_depth = 10,
    learn_rate = 0.02,
  ) %>%
  set_engine("xgboost", objective = "reg:squarederror")

xgboost_wf <- 
  workflows::workflow() %>%
  add_model(xgboost_model) %>% 
  add_formula(price ~ .)

xgb_fit <- fit(xgboost_wf, Train)

lm_test_pred <- bind_cols(
  predict(lm_fit, new_data = Test))

xgb_test_pred<-bind_cols(
  predict(xgb_fit, new_data = Test))

####Final model####

ensemble<-(lm_test_pred$.pred*0.25+xgb_test_pred$.pred*0.75)
