## ----setup---------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE,
                      warning = FALSE,
                      message = FALSE,
                      fig.path = "res-emotion-figs/",
                      fig.width = 12,
                      fig.height = 8)

library(nlme)
library(tidyverse)
library(broom.mixed)
library(knitr)
library(kableExtra)

res_q <- read_csv("/Users/Ben/Dropbox/PhD/Research/Support Self Reg/analyses/clean files/res_q_clean.csv")

emotion <- read_csv("/Users/Ben/Dropbox/PhD/Research/Support Self Reg/analyses/clean files/res_emo_clean.csv")

emotion <- emotion %>% 
  mutate(d_reap_v_lneg = case_when(
    type == "reap" ~ 1,
    type == "lneg" ~ 0,
    TRUE ~ as.numeric(NA)
  ),
  d_lneg_v_lneu = case_when(
    type == "lneg" ~ 1,
    type == "lneu" ~ 0
  )
  )

res <- full_join(emotion,
                 select(res_q,
                        id,
                        condition,
                        rejfa_friend,
                        rejfa_partner_fight,
                        ladder,
                        socialdes),
                 by = "id")

# kable style
print_kable <- function(table) {
  kable(table) %>% 
    kable_styling(full_width = FALSE)
}

## ----autocor-------------------------------------------------------------
emo_fit00 <- lme(rating ~ 1,
                random = ~ 1|id,
                data = emotion,
                na.action = na.omit)

# autocorr_search <- map(1:20,
#     ~ lme(rating ~ 1,
#           random = ~ 1|id,
#           correlation = corARMA(
#             p = .x,
#             form = ~ trialnumber | id),
#           data = emotion,
#           na.action = na.omit))
# 
# write_rds(autocorr_search,
#           "/Users/Ben/Dropbox/PhD/Research/Support Self Reg/analyses/autocorr_search.rds")

autocorr_search <- read_rds("/Users/Ben/Dropbox/PhD/Research/Support Self Reg/analyses/autocorr_search.rds")

autocorr_search <- append(list(emo_fit00), autocorr_search)

# from https://stackoverflow.com/a/43436211/
ar_lrt <- eval(
  parse(
    text = paste(
      "anova(",
      paste("autocorr_search[[",
            1:length(autocorr_search),
            "]]",
            sep = "",
            collapse = ",")
      ,")")
    )
  ) %>% 
  as_tibble() %>% 
  mutate(call = (1:n()) -1) %>% 
  rename(ar = call) %>% 
  mutate(p.adj = p.adjust(`p-value`, method = "fdr"))

print_kable(ar_lrt)

## ----autocorr_acf--------------------------------------------------------
# acf plots

# can't just use ACF on residuals because
# this doesn't keep track of missing values
# lme removes missing values
# need to add the missing values back to the residuals vector
# and tell the acf function to account for them in the ordering
# per Zhur 2009 p 147
# plot(ACF(emo_fit11, resType = "normalized"),
#      alpha = .05)

e1 <- residuals(autocorr_search[[1]],
               resType = "normalized")

# length(e1)
# length(emotion$rating)

enonmiss <- !is.na(res$rating)

efull1 <- vector(length = length(res$rating))
efull1 <- NA
efull1[enonmiss] <- e1

acf(efull1, na.action = na.pass)
pacf(efull1, na.action = na.pass)


e12 <- residuals(autocorr_search[[13]],
               resType = "normalized")
efull12 <- vector(length = length(res$rating))
efull12 <- NA
efull12[enonmiss] <- e12

acf(efull12, na.action = na.pass)
pacf(efull12, na.action = na.pass)


plot(ACF(autocorr_search[[5]],
         resType = "normalized",
         form = ~ trialnumber | id),
     alpha = .05)
pacf(residuals(autocorr_search[[1]],
          resType = "normalized"))

## ----aim3----------------------------------------------------------------
emofit10 <- lme(rating ~ d_reap_v_lneg +
                   rejfa_friend + # control vars
                   rejfa_partner_fight +
                   ladder +
                   socialdes,
                random = ~ 1|id, # random int only
                data = res,
                na.action = na.omit)
summary(emofit10)

emofit11 <- update(emofit10,
                random = ~ d_reap_v_lneg|id) # random slope
summary(emofit11)

anova(emofit10, emofit11)

## ----aim3_arsearch-------------------------------------------------------
# autocorr_search1 <- map(1:20,
#                        ~ lme(rating ~ d_reap_v_lneg +
#                                rejfa_friend + # control vars
#                                rejfa_partner_fight +
#                                ladder +
#                                socialdes,
#                              random = ~ d_reap_v_lneg |id,
#                              correlation = corARMA(
#                                p = .x,
#                                form = ~ trialnumber | id),
#                              data = res,
#                              na.action = na.omit))
# write_rds(autocorr_search1,
#           "/Users/Ben/Dropbox/PhD/Research/Support Self Reg/analyses/autocorr_search1.rds")

autocorr_search1 <- read_rds("/Users/Ben/Dropbox/PhD/Research/Support Self Reg/analyses/autocorr_search1.rds")

autocorr_search1 <- append(list(emofit11), autocorr_search1)

ar_lrt1 <- eval(
  parse(
    text = paste(
      "anova(",
      paste("autocorr_search1[[",
            1:length(autocorr_search1),
            "]]",
            sep = "",
            collapse = ",")
      ,")")
    )
  ) %>% 
  as_tibble() %>% 
  mutate(call = (1:n()) -1) %>% 
  rename(ar = call) %>% 
  mutate(p.adj = p.adjust(`p-value`, method = "fdr"))

print_kable(ar_lrt1)

## ----aim3_ar-------------------------------------------------------------
emo_fit11_ar_tidied <- map_df(autocorr_search1, tidy) %>% 
  filter(term == "d_reap_v_lneg") %>% 
  mutate(ar = 0:(n()-1)) %>% 
  select(ar, everything(), -effect, -group)

print_kable(emo_fit11_ar_tidied)

acf_plots <- map(c(1:3, 9),
    ~ autocorr_search1[[.x]] %>%              
    ACF(resType = "normalized",
         form = ~ trialnumber | id) %>% 
     plot(alpha = .05, main = paste("AR", as.character(.x - 1)))
)

acf_plots

## ----aim1----------------------------------------------------------------
emofit20 <- lme(rating ~ d_lneg_v_lneu * condition +
                   rejfa_friend + # control vars
                   rejfa_partner_fight +
                   ladder +
                   socialdes,
                random = ~ 1|id, # random int only
                data = res,
                na.action = na.omit)
summary(emofit20)

emofit21 <- update(emofit20,
                random = ~ d_lneg_v_lneu|id) # random slope
summary(emofit21)

anova(emofit20, emofit21)

## ----aim1_arsearch-------------------------------------------------------
# autocorr_search2 <- map(1:20,
#                        ~ lme(rating ~ d_lneg_v_lneu * condition +
#                                rejfa_friend + # control vars
#                                rejfa_partner_fight +
#                                ladder +
#                                socialdes,
#                              random = ~ d_lneg_v_lneu|id,
#                              correlation = corARMA(
#                                p = .x,
#                                form = ~ trialnumber|id),
#                              data = res,
#                              na.action = na.omit))
# write_rds(autocorr_search2,
#           "/Users/Ben/Dropbox/PhD/Research/Support Self Reg/analyses/autocorr_search2.rds")

autocorr_search2 <- read_rds("/Users/Ben/Dropbox/PhD/Research/Support Self Reg/analyses/autocorr_search2.rds")

autocorr_search2 <- append(list(emofit21), autocorr_search2)

ar_lrt2 <- eval(
  parse(
    text = paste(
      "anova(",
      paste("autocorr_search1[[",
            1:length(autocorr_search1),
            "]]",
            sep = "",
            collapse = ",")
      ,")")
    )
  ) %>% 
  as_tibble() %>% 
  mutate(call = (1:n()) -1) %>% 
  rename(ar = call) %>% 
  mutate(p.adj = p.adjust(`p-value`, method = "fdr"))

print_kable(ar_lrt2)

## ----aim1_ar-------------------------------------------------------------
emo_fit21_ar_tidied <- tibble(ar = 0:(length(autocorr_search2) - 1),
                              autocorr = map(autocorr_search2, tidy)) %>%
  unnest(autocorr) %>% 
  filter(term %in% c(
    "d_lneg_v_lneu",
    "condition",
    "d_lneg_v_lneu:condition"
    )) %>% 
  select(-effect, -group)

print_kable(emo_fit21_ar_tidied)

acf_plots <- map(c(1:3, 9),
    ~ autocorr_search1[[.x]] %>%              
    ACF(resType = "normalized",
         form = ~ trialnumber | id) %>% 
     plot(alpha = .05, main = paste("AR", as.character(.x - 1)))
)

acf_plots

## ----aim2----------------------------------------------------------------
emofit30 <- lme(rating ~ d_reap_v_lneg * condition +
                   rejfa_friend + # control vars
                   rejfa_partner_fight +
                   ladder +
                   socialdes,
                random = ~ 1|id, # random int only
                data = res,
                na.action = na.omit)
summary(emofit30)

emofit31 <- update(emofit30,
                random = ~ d_reap_v_lneg|id) # random slope
summary(emofit31)

anova(emofit30, emofit31)

## ----aim2 ar search------------------------------------------------------
# autocorr_search3 <- map(1:20,
#                        ~ lme(rating ~ d_reap_v_lneg * condition +
#                                rejfa_friend + # control vars
#                                rejfa_partner_fight +
#                                ladder +
#                                socialdes,
#                              random = ~ d_reap_v_lneg|id,
#                              correlation = corARMA(
#                                p = .x,
#                                form = ~ trialnumber|id),
#                              data = res,
#                              na.action = na.omit))
# write_rds(autocorr_search3,
#           "/Users/Ben/Dropbox/PhD/Research/Support Self Reg/analyses/autocorr_search3.rds")

autocorr_search3 <- read_rds("/Users/Ben/Dropbox/PhD/Research/Support Self Reg/analyses/autocorr_search3.rds")

autocorr_search3 <- append(list(emofit31), autocorr_search3)

ar_lrt3 <- eval(
  parse(
    text = paste(
      "anova(",
      paste("autocorr_search3[[",
            1:length(autocorr_search1),
            "]]",
            sep = "",
            collapse = ",")
      ,")")
    )
  ) %>% 
  as_tibble() %>% 
  mutate(call = (1:n()) -1) %>% 
  rename(ar = call) %>% 
  mutate(p.adj = p.adjust(`p-value`, method = "fdr"))
print_kable(ar_lrt3)

## ----aim2_ar-------------------------------------------------------------
emo_fit31_ar_tidied <- tibble(ar = 0:(length(autocorr_search3) - 1),
                              autocorr = map(autocorr_search3, tidy)) %>%
  unnest(autocorr) %>% 
  filter(term %in% c(
    "d_reap_v_lneg",
    "condition",
    "d_reap_v_lneg:condition"
    )) %>% 
  select(-effect, -group)

print_kable(emo_fit31_ar_tidied)

acf_plots <- map(c(1:3, 9),
    ~ autocorr_search1[[.x]] %>%              
    ACF(resType = "normalized",
         form = ~ trialnumber | id) %>% 
     plot(alpha = .05, main = paste("AR", as.character(.x - 1)))
)

acf_plots

## ----aim4----------------------------------------------------------------
emofit40 <- lme(rating ~ trialnumber * d_reap_v_lneg * condition +
                   rejfa_friend + # control vars
                   rejfa_partner_fight +
                   ladder +
                   socialdes,
                random = ~ 1|id, # random int only
                data = res,
                na.action = na.omit)
summary(emofit40)

emofit41 <- update(emofit40,
                random = ~ d_reap_v_lneg|id) # random slope
summary(emofit41)

emofit42 <- update(emofit40,
                random = ~ trialnumber|id) # random slope

# random slopes are waranted for condition contrast
# and trialnumber
anova(emofit40, emofit41)
anova(emofit40, emofit42)

emofit43 <- update(emofit40,
                random = ~ trialnumber + d_reap_v_lneg|id) # random slope

emofit44 <- update(emofit40,
                random = ~ trialnumber * d_reap_v_lneg|id)

summary(emofit44)

# even a random slope of the interaction term is suggested
anova(emofit43, emofit44)

## ----aim4 ar search------------------------------------------------------
# autocorr search sometimes returns an error
# so used safely to run the map call
# but this broke something in the anova function model comparison
# so then reverted to normal map call

# autocorr_search4 <- map(1:2,
#                        ~ lme(
#                          rating ~ trialnumber * d_reap_v_lneg * condition +
#                                rejfa_friend + # control vars
#                                rejfa_partner_fight +
#                                ladder +
#                                socialdes,
#                              random = ~ trialnumber + d_reap_v_lneg|id,
#                              correlation = corARMA(
#                                p = .x,
#                                form = ~ trialnumber|id),
#                              data = res,
#                              na.action = na.omit))
# write_rds(autocorr_search4,
#           "/Users/Ben/Dropbox/PhD/Research/Support Self Reg/analyses/autocorr_search4.rds")

autocorr_search4 <- read_rds("/Users/Ben/Dropbox/PhD/Research/Support Self Reg/analyses/autocorr_search4.rds")

autocorr_search4 <- append(list(emofit44),
                           autocorr_search4)

ar_lrt4 <- eval(
  parse(
    text = paste(
      "anova(",
      paste("autocorr_search4[[",
            1:length(autocorr_search4),
            "]]",
            sep = "",
            collapse = ",")
      ,")")
    )
  ) %>% 
  as_tibble() %>% 
  mutate(call = (1:n()) -1) %>% 
  rename(ar = call) %>% 
  mutate(p.adj = p.adjust(`p-value`, method = "fdr"))
print_kable(ar_lrt4)

## ----ar------------------------------------------------------------------
emo_fit44_ar_tidied <- tibble(ar = 0:(length(autocorr_search4) - 1),
                              autocorr = map(autocorr_search4, tidy)) %>%
  unnest(autocorr) %>% 
  filter(term %in% c(
    "trialnumber",
    "d_reap_v_lneg",
    "condition",
    "trialnumber:d_reap_v_lneg",
    "trialnumber:condition",
    "d_reap_v_lneg:condition",
    "trialnumber:d_reap_v_lneg:condition"
    )) %>% 
  select(-effect, -group)

print_kable(emo_fit44_ar_tidied)

# based on AIC ar2 is the best fit
# params of interst don't change

acf_plots <- map(c(1:3),
    ~ autocorr_search1[[.x]] %>%              
    ACF(resType = "normalized",
         form = ~ trialnumber | id) %>% 
     plot(alpha = .05, main = paste("AR", as.character(.x - 1)))
)

acf_plots

## ----cuttings, eval = FALSE----------------------------------------------
## ggplot(emotion %>%
##          filter(id %in% sample(id, 20)),
##        aes(x = trialnumber,
##            y = rating)) +
##   geom_point() +
##   geom_line() +
##   geom_smooth() +
##   facet_wrap(vars(id))
## 
## ggplot(emotion,
##        aes(x = trialnumber,
##            y = rating)) +
##   geom_jitter() +
##   geom_smooth()
## 
## emotion %>%
##   group_by(type) %>%
##   summarise(rating = mean(rating, na.rm = TRUE))
## 
## # comparing the custom dummy coding to a full dummy
## emo_fit12 <- lme(rating ~ d_reap_v_lneg +
##                    rejfa_friend +
##                    rejfa_partner_fight +
##                    ladder +
##                    socialdes,
##                 random = ~ d_reap_v_lneg|id,
##                 data = res,
##                 correlation = corAR1(),
##                 na.action = na.omit)
## summary(emo_fit12)
## 
## emo_fit13 <- lme(rating ~ factor(type) +
##                    rejfa_friend +
##                    rejfa_partner_fight +
##                    ladder +
##                    socialdes,
##                 random = ~ factor(type)|id,
##                 data = res,
##                 correlation = corAR1(),
##                 na.action = na.omit)
## summary(emo_fit13)
## 
## # maybe I should check rate of missingness on emo rating for each participant and screen out high missingness Ps? id 168 seems to have a lot of missingness on emotion ratings.
## 
## # comparing to an aggregate and anova approach
## temp.data <- res %>%
##   group_by(id, type) %>%
##   summarise(rating = mean(rating, na.rm = TRUE),
##             condition = unique(condition)) %>%
##   mutate(d_reap_v_lneg = case_when(
##     type == "reap" ~ 1,
##     type == "lneg" ~ 0,
##     TRUE ~ as.numeric(NA)
##   ),
##   d_lneg_v_lneu = case_when(
##     type == "lneg" ~ 1,
##     type == "lneu" ~ 0
##   )
##   )
## 
## temp.fit <- lm(
##   rating ~ d_lneg_v_lneu*condition,
##   data = temp.data
##   )
## summary(temp.fit)
## 
## temp.fit <- lm(
##   rating ~ d_reap_v_lneg*condition,
##   data = temp.data
##   )
## summary(temp.fit)

