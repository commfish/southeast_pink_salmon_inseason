# inseason forecasting model
# load libraries
devtools::install_github("ben-williams/FNGr")
library(tidyverse)
library(broom)
library(FNGr)
library(lubridate)
library(ggpmisc)
theme_set(theme_sleek())

# sex ratio deviation from mean
sex_dev <- function(x){
 x %>% 
    group_by(week) %>%
    mutate(sex_ratio = ifelse(!is.na(sex_ratio), sex_ratio - mean(sex_ratio, na.rm = T), 0)) %>% 
    group_by(year) %>% 
    mutate(sex_dev = cumsum(sex_ratio)) 
}

# negative log likelihood models

nll <- function(par, data){
  int = par[1]
  slope = par[2]
  sigma = par[3]
  catch = data$catch
  tot_catch = data$tot_catch
  
  
  fit = data$catch * slope + int
  like <- dnorm(data$tot_catch, mean = fit, sd = sigma)
  ll <- log(like)
  deviance <- -2 * sum(ll)
  deviance
}

nll_sex <- function(par, data){
  int = par[1]
  slope = par[2]
  sex = par[3]
  sigma = par[4]
  catch = data$catch
  tot_catch = data$tot_catch
  
  
  fit = (data$catch * slope + int) * (1 + sex * data$sex_dev)
  like <- dnorm(data$tot_catch, mean = fit, sd = sigma)
  ll <- log(like)
  deviance <- -2 * sum(ll)
  deviance
}

# estimate models
f_est <- function(data){
data %>% 
  group_by(week) %>% 
  nest() %>% 
  mutate(fit = purrr::map(data, ~optim(par = c(5, 1, 5), 
                                         fn = nll, 
                                         hessian = T, 
                                         data = .)),
         fit_sex = purrr::map(data, ~optim(par = c(5, 1, 5, 5), 
                                          fn = nll_sex, 
                                          hessian = T, 
                                          data = .)))
  }

f_params <- function(fits, sex = FALSE){
  
  if(sex){
    
    fits %>%
      unnest(fit_sex %>% 
               map(tidy)) %>% 
      group_by(week) %>% 
      mutate(., term = c('int', 'slope', 'sex', 'sigma')) %>% 
      ungroup() %>% 
      dplyr::select(-parameter)
    
  } else {
    
  fits %>%
    unnest(fit %>% 
             map(tidy)) %>% 
    group_by(week) %>% 
    mutate(., term = c('int', 'slope', 'sigma')) %>% 
    ungroup() %>% 
    dplyr::select(-parameter) 
  }
}

f_pred <- function(data, params, sex = FALSE){
  
  if(sex){
    params %>% 
      spread(term, value) %>% 
      mutate(int = ifelse(int<0, 0, int)) %>% 
      left_join(data) %>% 
      group_by(week) %>% 
      mutate(pred = (int + slope * catch) * (1 + sex * sex_dev), 
             resid = tot_catch - pred,
             log_resid = log(tot_catch) - log(pred)) 
    
  } else {
    
  params %>% 
    spread(term, value) %>% 
    mutate(int = ifelse(int<0, 0, int)) %>% 
    left_join(data) %>% 
    group_by(week) %>% 
    mutate(pred = (int + slope * catch), 
           resid = tot_catch - pred,
           log_resid = log(tot_catch) - log(pred)) 
  }
}

model_select <- function(preds, preds_sex){
  
  preds %>% 
    group_by(week) %>% 
    mutate(forecast = mean(int)+mean(slope)*catch) %>% 
    mutate(model = "int_cum") -> model1
  
  preds_sex %>% 
    group_by(week) %>% 
    mutate(forecast = (mean(int)+mean(slope)*catch)*(1 + mean(sex) * sex_dev),  
          model = "int_cum_sex") -> model2
  
  x<- rbind(model1,model2)
  
    x %>% 
      filter (year == year_forecast)-> forecast
  
 
  preds %>% 
    group_by(week) %>% 
    summarise(lssq = log(sum(resid^2)),
              n = n(),
              k = 2) %>% 
      mutate(AIC = n * (lssq - log(n)) + 2 * k,
             AICc = AIC + (2 * k * (k + 1)) / (n - k - 1),
             model = "int_cum") %>% 
      bind_rows(
      preds_sex %>% 
      group_by(week) %>% 
      summarise(lssq = log(sum(resid^2)),
                n = n(),
                k = 3) %>% 
      mutate(AIC = n * (lssq - log(n)) + 2 * k,
             AICc = AIC + (2 * k * (k + 1)) / (n - k - 1),
             model = "int_cum_sex")) %>% 
    group_by(week) %>% 
    mutate(delta_AICc = AICc - min(AICc),
           exp_delta = exp(-delta_AICc / 2),
           AICc_weight = exp_delta / sum(exp_delta)) %>% 
    arrange(week, model) %>% 
    left_join(forecast) %>% 
    mutate(m1 = case_when(model=="int_cum" ~ forecast * AICc_weight),
           m2 = case_when(model=="int_cum_sex" ~ forecast * AICc_weight)) %>% 
    group_by(week) %>% 
    mutate(model_avg = sum(m1,m2, na.rm = T)) %>% 
    dplyr::select(-m1, -m2) %>%
    filter (year == year_forecast)
  
} 

tickr_length <- data.frame(SW = 26:40)
axisb <- tickr(tickr_length, SW, 1)
chart<- function(data) {
    ggplot(data %>%
           mutate(year = factor(year, ordered = TRUE, 
                               levels = c("2019", "10_year_average"),
                               labels = c("2019", "10 year average"))),      
           
           aes(week, catch), group=year, shape=year) +  geom_line(aes(linetype=year)) +
    geom_point(aes(shape=year), size=5) + scale_shape_manual(values=c(1,16)) + scale_linetype_manual(values=c(1,1)) +
    scale_x_continuous(breaks = axisb$breaks, labels = axisb$labels) +
    xlab("Statistical Week") + theme(legend.position=c(.2,.9), legend.title=element_blank(),
                                     legend.text=element_text(size=16), plot.title = element_text(size=18, hjust=0.5),
                                     axis.title.x = element_text(size=16), axis.title.y = element_text(size=16),
                                     legend.key.size = unit(1.5, 'lines'), axis.text=element_text(size=16)) +
    
    ylab("Millions of Pink Salmon")
}


model_fig <- function(data) {
  formula <- y ~ x
  data %>% 
    group_split(week) %>%
    map_df(~{fit = lm(tot_catch ~ catch, data = .)
    
    data.frame(., ci = predict(fit, ., interval = 'confidence'),
               pi = predict(fit, ., interval = 'prediction'))
    }) %>% 
    ggplot(aes(catch, tot_catch)) + 
    geom_point(aes(y = tot_catch), alpha = 0.5) +
    facet_wrap(~week, dir = "v", ncol = 3, scales = "free") +
    geom_line(aes(catch, ci.fit)) +
    geom_ribbon(aes(ymin = (ci.lwr), ymax = (ci.upr)), alpha = 0.4) +
    geom_ribbon(aes(ymin = (pi.lwr), ymax = (pi.upr)), alpha = 0.2) +
    stat_poly_eq(formula = formula ,  
                 eq.with.lhs = "italic(hat(y))~`=`~",
                 aes(label = paste(..eq.label.., ..adj.rr.label.., sep = "*plain(\",\")~")),
                 parse = TRUE, size = 3, vjust=1) +
    expand_limits(x = 0, y = 0) +
    xlab("Cumulative Catch (millions)") +
    ylab("Total Catch (millions)")
}

model_fig2 <- function(data) {
  g1 <- subset(data, year == year_forecast)
  formula <- y ~ x
  data %>% 
    group_split(week) %>%
    map_df(~{fit = lm(tot_catch ~ catch, data = .)
    
    data.frame(., ci = predict(fit, ., interval = 'confidence'),
               pi = predict(fit, ., interval = 'prediction'))
    }) %>% 
    ggplot(data=., aes(catch, tot_catch)) + geom_point(alpha=1/5) +
    geom_point(data=g1,aes(catch, tot_catch), colour="red") +
    geom_text_repel(data=g1, label=year_forecast, size=3) +
    facet_wrap(~week, dir = "v", ncol = 3, scales = "free") +
    geom_line(aes(catch, ci.fit)) +
    geom_ribbon(aes(ymin = (ci.lwr), ymax = (ci.upr)), alpha = 0.4) +
    geom_ribbon(aes(ymin = (pi.lwr), ymax = (pi.upr)), alpha = 0.2) +
    stat_poly_eq(formula = formula ,  
                 eq.with.lhs = "italic(hat(y))~`=`~",
                 aes(label = paste(..eq.label.., ..adj.rr.label.., sep = "*plain(\",\")~")),
                 parse = TRUE, size = 3, vjust=1) +
    expand_limits(x = 0, y = 0) +
    xlab("Cumulative Catch (millions)") +
    ylab("Total Catch (millions)")
}


model_fig3 <- function(data) {
  g1 <- subset(data, year == year_forecast)
  formula <- y ~ x1*(1+x2)
  data %>% 
    group_split(week) %>%
    map_df(~{fit = lm(tot_catch ~ catch*(1+sex_dev), data = .)
    
    data.frame(., ci = predict(fit, ., interval = 'confidence'),
               pi = predict(fit, ., interval = 'prediction'))
    }) %>% 
    ggplot(data=., aes(catch, tot_catch)) + geom_point(alpha=1/5) + 
    geom_point(data=g1, colour="red") +
    geom_text_repel(data=g1, label=year_forecast, size=3) +
    facet_wrap(~week, dir = "v", ncol = 3, scales = "free") +
    geom_line(aes(catch, ci.fit)) +
    geom_ribbon(aes(ymin = (ci.lwr), ymax = (ci.upr)), alpha = 0.4) +
    geom_ribbon(aes(ymin = (pi.lwr), ymax = (pi.upr)), alpha = 0.2) +
    stat_poly_eq(formula = formula ,  
                 eq.with.lhs = "italic(hat(y))~`=`~",
                 aes(label = paste(..eq.label.., ..adj.rr.label.., sep = "*plain(\",\")~")),
                 parse = TRUE, size = 3, vjust=1) +
    expand_limits(x = 0, y = 0) +
    xlab("Cumulative Catch (millions)") +
    ylab("Total Catch (millions)")
}
