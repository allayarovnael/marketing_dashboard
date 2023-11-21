rm(list=ls())

# graphs and html-formatted tables:
library(highcharter)
library(stargazer)

# Color palette (theme simple):
COLORS = c("#d35400", "#2980b9", "#2ecc71", "#f1c40f", "#2c3e50", "#7f8c8d")

# tidy workflow:
library(dplyr)
library(tidyr)
library(purrr)
library(forecast)
library(lubridate)

# FMCG Demo ---------------------------------------------------------------------------

df_skeleton <- readRDS('data/modelling_data.rds')

MA_columns <- names(select(df_skeleton, starts_with("TV"), starts_with("Print")))
AV_columns <- names(select(df_skeleton, Sales))
UV_columns <- names(select(df_skeleton, -date, -Sales))
EXP_columns <- names(select(df_skeleton, -date))


# # -------------------------------------------------------------------------------------------
#
#                                         Collection of highcharts
#
# # -------------------------------------------------------------------------------------------

# Impacts -------------------------------------------------------------------------------------
impactsBarChart <- function(model){
  hc <- hchart(impact(model), "bar",  
               hcaes(x=variable, y=round(impact, 2)), 
               dataLabels=list(enabled=TRUE)
               #, colorByPoint=TRUE
               ) %>%
    hc_title(text = "Contribution of each factor summed up") %>%
    hc_subtitle(text = "impact %") %>%
    hc_yAxis(title = list(text = "")) %>%
    hc_xAxis(title = list(text = "")) %>%
    hc_exporting(enabled = TRUE) %>%
    hc_add_theme(hc_theme_smpl())
  
  return(hc)
}

# real vs. estimated --------------------------------------------------------------------------
ModelFitChart <- function(df, model){
  
  df_fitted <- data.frame(
    Week = df$date,
    Estimated = model$fitted.values,
    Real = model$fitted.values + model$residuals) 

  hc <- highchart(type="stock") %>%
    hc_xAxis(title=list(text="Date"),type="datetime",dateTimeLabelFormats=list(day='%d of %b')) %>%
    hc_add_series(data = df_fitted, type="line", hcaes(Week, Real), name="Real") %>%
    hc_add_series(data = df_fitted, type="line", hcaes(Week, Estimated), name="Estimated") %>%
    hc_xAxis(title = "") %>%
    hc_yAxis(title = "") %>%
    hc_exporting(enabled = TRUE) %>%
    hc_add_theme(hc_theme_smpl())
  
  return(hc)
}

# Plot original media performance values as column chart and transformed version as line chart
OriginalTransformedChart <- function(df){
  # df have columns: date, original, transformed
  hc <- highchart() %>% 
    hc_xAxis(type="datetime", dateTimeLabelFormats=list(day='%d of %b')) %>%
    hc_yAxis_multiples(
      list(title = list(text = "Original")),
      list(title = list(text = "Transformed"), opposite = TRUE)) %>% 
    hc_exporting(enabled = TRUE) %>%
    hc_add_series(data=df, type="column", hcaes(date, original), name="Original", yAxis = 0) %>%
    hc_add_series(data=df, type="line", hcaes(date, transformed), name="Transformed", yAxis = 1) %>%
    hc_title(text = "Original vs. transformed") %>%
    hc_add_theme(hc_theme_smpl())
  
  return(hc)
}

ExplorationChart <- function(df_graph, Var_1, Var_2, type_1, type_2, timeline){

  df <- data.frame(date=df_graph[,'date'], 
                   var_one=df_graph[,Var_1], 
                   var_two=df_graph[,Var_2]) %>%
    filter(date >= timeline[1] & date <= timeline[2])
  
  hc <- highchart(type = "stock") %>% 
    hc_xAxis(title=list(text="Date"),type="datetime",dateTimeLabelFormats=list(day='%d of %b')) %>%
    hc_yAxis_multiples(
      list(title = list(text = Var_1)),
      list(title = list(text = Var_2), opposite = FALSE)
    ) %>% 
    hc_exporting(enabled = TRUE) %>%
    hc_add_series(data=df, type=type_1, hcaes(date, var_one), name=Var_1, yAxis = 0) %>%
    hc_add_series(data=df, type=type_2, hcaes(date, var_two), name=Var_2, yAxis = 1) %>%
    hc_title(text = paste(Var_1, " vs. ", Var_2)) %>%
    hc_subtitle(text = paste("Zeitraum:", "von", min(df$date), "bis", max(df$date))) %>%
    hc_add_theme(hc_theme_smpl()) 

  
  return(hc)
}

# Following chart represents decomposition of sales into each factor

DecompositionChart <- function(model){
  
  df_model <- as_tibble(model$model[,-1]) %>%
    mutate(`(Intercept)` = 1) %>%
    select(`(Intercept)`, everything())
  
  result <- map2_dfc(df_model, model$coefficients, `*`)
  
  series <- map2(result, names(result), ~ list(name = .y, data=.x))
  names(series) <- NULL
  
  hc <- highchart() %>% 
    hc_add_theme(hc_theme_smpl()) %>%
    hc_chart(type = "area") %>% 
    hc_plotOptions(area = list(
      stacking = "normal",
      lineColor = "#ffffff",
      lineWidth = 1,
      marker = list(lineColor = "#ffffff"))) %>% 
    hc_exporting(enabled = TRUE) %>%
    hc_tooltip(shared=TRUE) %>%
    hc_title(text = "Contribution of each factor over modelling time period") %>%
    hc_subtitle(text = "impact abs.") %>%
    hc_add_series_list(series)
    
  return(hc)
}


# R? ------------------------------------------------------------------------------------------
rsq <- function(x, y){cor(x, y) ^ 2}

# MAPE ----------------------------------------------------------------------------------------
mape <- function(model){
  mean(abs(model$residuals/(model$residuals+model$fitted.values)))
}

# Impacts -------------------------------------------------------------------------------------
impact <- function(model){

  coeffs <- model$coefficients
  fitVal <- sum(model$fitted.values)
  
  wirkung_0 <- model$model %>%
    select(names(coeffs)[-1]) %>%     # exclude intercept
    map_df(~ .x - min(.x))            # set variables on zero level
  
  impact <- seq(0,0,length.out=length(coeffs))

  for (i in 2:(length(model$coefficients))) {
    impact[i] <- round(100 * coeffs[i] * sum(wirkung_0[, names(coeffs)[i]]) / fitVal,2)
  }
  
  impact[1] <- 100 - sum(impact[-1])
  names(impact) <- names(coeffs)

  as.data.frame(impact) %>%
    tibble::rownames_to_column('variable')
}


# Depot ---------------------------------------------------------------------------------------
depotFunction <- function(v, depot){
  newCol <- v
  for(i in 2:length(newCol)){
    newCol[i] <- newCol[i]+ newCol[i-1]*depot/100
  }
  return(newCol)
}


# AdHyp ---------------------------------------------------------------------------------------
Adstock_Hyperbel <- function(v, adstock, hyperbel, divisor=1000){
  v_t <- depotFunction(v, adstock)                                                                   # adstock
  v_t <- (v_t * (100 - adstock)/100)/divisor                                                         # justierung + standartisierung
  if (hyperbel != 0){
    v_t <- (v_t)/(hyperbel + v_t)                                                                    # hyperbel 
  }
  return(v_t)
}

# Generate Adstock X Hyperbel Grid ------------------------------------------------------------
AdHypCombinations <- function(df, v, adstockRange=c(0,99), adstockStep=1,
                              hyperbelRange=c(0,1), hyperbelStep=0.1,
                              divisor=3, append=FALSE){
  
  adstockParams  <- seq(adstockRange[1], adstockRange[2], by = adstockStep)
  hyperbelParams <- seq(hyperbelRange[1], hyperbelRange[2], by = hyperbelStep)
  
  adhypGrid <- expand.grid(adstock=adstockParams, hyperbel=hyperbelParams)
  
  df_extended <- data.frame(df)
 
  for (i in 1:nrow(adhypGrid)){
    
    v_new <- Adstock_Hyperbel(df[,v], adstock=adhypGrid$adstock[i], 
                              hyperbel=adhypGrid$hyperbel[i], 
                              divisor=divisor)
    
    df_extended <- data.frame(df_extended, v_new)
    
    names(df_extended)[ncol(df_extended)] <- paste0(v,'_SAG',adhypGrid$adstock[i], 'xH',adhypGrid$hyperbel[i])
  }
  
  if(append == FALSE){
    df_extended <- df_extended %>%
      select(contains(paste0(v,"_SAG"))) 
  }
  
  return(df_extended)
}

# Grid Optimization of Linear Model ------------------------------------------------------------

# following function takes an existing linear model and df for testing
# -> selection of the best variable from df according to max R2

lmGridOptimization <- function(modell, df, p=0.1){

  model <- modell
  test_df <- df
  data_extended <- data.frame(model$model, test_df)

  num_vars <- model$rank + 1    # number of variables in model_update
  stats <- data.frame(variable=names(test_df), p_value=0, r_squared=0, coef=0)
  
  # iterate over the columns of test_df:
  for (i in seq_along(test_df)){      
    data_extended$new_var <- test_df[,i]   # variable for test
    model_upd <- update(model, .~. + new_var, data = data_extended)   
    #----------------------------------------------------------------------------- # extract:
    stats[i, 'p_value']   <- summary(model_upd)$coefficients[num_vars,'Pr(>|t|)']  # p-value
    stats[i, 'r_squared'] <- summary(model_upd)$r.squared                          # R2
    stats[i, 'coef']      <- summary(model_upd)$coefficients[num_vars,'Estimate']  # coefficient
  }
  
  stats_sign <- stats %>%
    filter(p_value <= p & coef > 0)                             # select significant variables only
  
  if (nrow(stats_sign)==0){
    
    return('No significant improvement')
    
  } else {
    
    stats_best_r_sq <- stats_sign %>%
      arrange(desc(r_squared)) %>%
      map_if(is.factor, as.character) %>% 
      as_tibble() %>%
      head(1) %>%
      pull(variable)
    
    return(stats_best_r_sq)
    
  }
  
}

#model_lm <- lm(
#  Absatz ~ Saisonality + Feiertag
#  , data = df_skeleton
#)

#tv_test <- AdHypCombinations(
#  df_skeleton, "TV"
#)

#lmGridOptimization(model_lm, tv_test)

#head(df_skeleton)



# Marginal utility curve ----------------------------------------------------------------------
plotSaturation <- function(v, media_df, adstock, hyperbel, divisor, cut_off=0){

  media_col    <- pull(media_df, v)
  media_cutoff <- ifelse(media_col >= cut_off, 1, 0)
  budget_vars  <- seq(from=0.25, to=4, by=0.25)
  budget_vars  <- set_names(as.list(budget_vars), as.character(budget_vars))
  
  budget_df <- budget_vars %>%
    map_df(~ .x * media_col) %>%
    map_df(~ Adstock_Hyperbel(.x, adstock, hyperbel, divisor)) %>%
    map_df(~ .x * media_cutoff / sum(media_cutoff)) %>%
    colSums() 
  
  budget_df <- budget_df %>%
    map_df(~ .x / max(budget_df)*100)
  
  # time unit based on campaign timeline: 
  timeUnitCampaign <- budget_vars %>% 
    map_df(~ .x * media_col) %>%
    map_df(~ .x * media_cutoff) %>%
    colSums() / sum(media_cutoff)
  
  df <- tibble(GRP = round(timeUnitCampaign,2), 
               impact = round(as.vector(t(budget_df[1,])),2))
  
  hc <- hchart(df, "line", hcaes(x = GRP, y = impact)) %>% 
    hc_yAxis(min = 0, max = 100) %>%
    hc_exporting(enabled = TRUE) %>%
    hc_add_theme(hc_theme_smpl()) %>%
    hc_title(text = "Saturation curve") %>%
    hc_subtitle(text = "Media impact per week (indexed)")  
  
  return(hc)
}


# Media: original vs transformed --------------------------------------------------------------
plotMediaTransform <- function(v, df, adstock, hyperbel, divisor){
  
  df <- data.frame(date = pull(df, date), original = pull(df, v))
  
  df$transformed <- Adstock_Hyperbel(pull(df, original), adstock, hyperbel, divisor)
  
  hc <- highchart() %>% 
    hc_xAxis(categories = df$date, type = "datetime") %>%
    hc_yAxis_multiples(
      list(lineWidth = 3),
      list(showLastLabel = FALSE, opposite = TRUE)
    ) %>% 
    hc_add_series(type="column", data = df$original) %>%
    hc_add_series(type="line", data = df$transformed, yAxis = 1) %>%
    hc_tooltip(shared=TRUE) %>%
    hc_add_theme(hc_theme_smpl())
  
  return(hc)
}


# Download lm report --------------------------------------------------------------------------

export_model <- function(lm, file){
  
  require(dplyr)
  require(purrr)
  require(broom)
  require(openxlsx)
  
  effects <- lm$model %>%
    mutate(`(Intercept)` = 1) %>% 
    select(names(lm$coefficients))
  
  effects_mult <- as.data.frame(t(t(effects)*lm$coefficients)) %>%  # mutiply each column by respective coefficient
    mutate(real = lm$fitted.values + lm$residuals,
           fitted = lm$fitted.values) 
  
  wb <- createWorkbook(); addWorksheet(wb, "data")
  
  writeData(wb,"data", broom::tidy(lm), startCol=2, startRow=1, rowNames=FALSE)
  writeData(wb,"data", broom::glance(lm), startCol=3+ncol(broom::tidy(lm)), startRow=1, rowNames=FALSE)
  writeData(wb,"data", effects, startCol=2, startRow = nrow(broom::tidy(lm))+3, rowNames=FALSE)
  writeData(wb,"data", effects_mult, startCol=ncol(effects)+4, startRow = nrow(broom::tidy(lm))+3, rowNames=FALSE)
  
  saveWorkbook(wb, file, overwrite = TRUE)
}

