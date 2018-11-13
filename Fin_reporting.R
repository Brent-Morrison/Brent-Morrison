# packages
library("tidyverse")
library("DescTools")
#library("lubridate")

#=========================================================================================
#==    Load mock data                                                                   ==
#==    1. create date & merge ID's for distinct records                                 ==
#==    2. remove notes  (not required for real data)                                    ==
#==    3. expand for all combinations of month and account                              ==
#==    4. order so group by for change works correctly                                  ==
#==    5. replace na's created by step 3                                                ==
#==    6. fill type attribute to newly created records                             ==
#=========================================================================================

bln_ctgy <- as.tibble(read.csv("data_v01.csv", header= T, sep = ",")) %>%           
  mutate(date = as.Date(paste(year, mth, 1, sep = "-"))) %>%                              #1
  unite(cust, cust, unit, sep = "_") %>%                     
  select(-Notes) %>%                                                                    #2
  complete(date = seq.Date(min(date), max(date), by = "month"), cust) %>%               #3
  arrange(cust, date) %>%                                                               #4
  replace_na(list(gca = 0, ledbal = 0, ecl = 0, wo = 0, 
                  pryr = 0, prlt = 0))  %>%                                             #5
  group_by(cust) %>% fill(type) %>% ungroup                                             #6

min.date <- bln_ctgy %>% slice(which.min(date)) %>% select(date)
#df <- bind_rows(replicate(100, bln_ctgy, simplify = FALSE)) copy data frame

#=========================================================================================
#==    Create stage attributes & movement balances                                      ==
#==    1. assign disclosure stage (1 / 2 / 3 / 4=POCI_NCI / 5=POCICI)                   ==
#==    2. fill stage with preceding value (ignores initial na's)                        ==
#==       & default remaining na's to stage 1                                           ==
#==    3. create lagged values of balances and prior period balance                     ==
#==    4. rename closing balance attributes                                             ==
#==    5. add cumulative balances                                                       ==
#==    TO DO - add FINREP change of stage indicator for table 12.1                      ==
#=========================================================================================

bln_ctgy <- bln_ctgy %>%  
  mutate(ctgy = case_when(wo != 0      & poci == "N" ~ 3,                               #1
                          wo != 0      & poci == "Y" ~ 5,
                          wo != 0      & is.na(poci) ~ 3,
                          stage == 1   & poci == "N" ~ 1,
                          stage == 2   & poci == "N" ~ 2,
                          stage == 3   & poci == "N" ~ 3,
                          is.na(stage) & poci == "N" ~ 1,
                          stage == 1   & poci == "Y" ~ 4,
                          stage == 2   & poci == "Y" ~ 4,
                          stage == 3   & poci == "Y" ~ 5,
                          is.na(stage) & poci == "Y" ~ 4,
                          stage == 1   & is.na(poci) ~ 1,
                          stage == 2   & is.na(poci) ~ 2,
                          stage == 3   & is.na(poci) ~ 3)
  ) %>% 
  group_by(cust) %>% fill(ctgy) %>% replace_na(list(ctgy = 1)) %>%                      #2
  mutate(gca.op    = lag(gca),                                                          #3                  
         ledbal.op = lag(ledbal),
         ledbal.pr = ifelse(date == min.date, ledbal, NA),
         ecl.op    = lag(ecl),
         ctgy.op   = lag(ctgy)) %>% fill(ledbal.pr) %>% 
  rename(gca.cl    = gca, ledbal.cl = ledbal, ecl.cl = ecl, 
         ctgy.cl   = ctgy, wo.cl = wo) %>%                                              #4
  mutate(ledbal.y  = ledbal.cl - ledbal.pr + cumsum(wo.cl),                             #5
         y86.1a.y  = if_else(ledbal.y > 0, ledbal.y, 0),
         y84b.r.y  = if_else(ledbal.y < 0, ledbal.y, 0)) %>% 
  ungroup()


#=========================================================================================
#==    Create movement attributes                                                       ==
#=========================================================================================

bln_mvnt <- bln_ctgy %>% group_by(cust) %>% 
  mutate(cover.cl = -ecl.cl / ledbal.cl,
         cover.op = -ecl.op / ledbal.op,
         cover    = Winsorize(round(if_else(is.nan(cover.op),cover.cl, cover.op),2), minval = 0, maxval = 1),
         incr_decr= case_when(ledbal.cl > ledbal.op ~ 'incr',
                              ledbal.cl < ledbal.op ~ 'decr',
                              TRUE ~ 'unch'),
         ctgy.dir   = case_when(ctgy.cl > ctgy.op ~ 'D',
                              ctgy.cl < ctgy.op ~ 'I',
                              TRUE ~ 'U'),
         pre_post = case_when(ctgy.dir == 'I' & incr_decr == 'decr' ~ 'pre',
                              ctgy.dir == 'D' & incr_decr == 'incr' ~ 'pre',
                              TRUE ~ 'post'),
         pre_stage= if_else(pre_post == 'pre', ctgy.op, ctgy.cl),
         g.y86.1a = if_else(type == 'rvlv', y86.1a.y - lag(y86.1a.y), 0),
         g.y86.1b = if_else(type == 'term' & incr_decr == 'incr', ledbal.cl - ledbal.op + wo.cl, 0),
         g.y84a   = if_else(type == 'term' & incr_decr == 'decr' & ledbal.cl == 0, ledbal.cl - ledbal.op + wo.cl, 0),
         g.y84b.t = if_else(type == 'term' & incr_decr == 'decr' & ledbal.cl != 0, ledbal.cl - ledbal.op + wo.cl, 0),
         g.y84b.r = if_else(type == 'rvlv', y84b.r.y - lag(y84b.r.y), 0),
         g.x640.4 = (gca.cl - ledbal.cl) - (gca.op - ledbal.op),
         g.tfr_pre= gca.op + g.y86.1a + g.y86.1b + g.y84a + g.y84b.t + g.y84b.r,
         g.Z04    = -wo.cl,
         g.tfr_out= -case_when(ctgy.dir != 'U' & pre_post == 'pre' ~ g.tfr_pre,
                               ctgy.dir != 'U' & pre_post == 'post' ~ gca.op,
                               TRUE ~ 0),
         g.tfr_in = -g.tfr_out,
         i.y86.1a = -cover * g.y86.1a,
         i.y86.1b = -cover * g.y86.1b,
         i.y84a   = -cover * g.y84a,
         i.y84b.t = -cover * g.y84b.t,
         i.y84b.r = -cover * g.y84b.r,
         i.Z04    = wo.cl,
         i.Y82    = case_when(ctgy.cl == 1 & pryr != 0 ~ ecl.cl + pryr,
                              ctgy.cl != 1 & prlt != 0 ~ ecl.cl + prlt,
                              TRUE ~ 0),
         i.y83.1  = if_else(ctgy.dir != 'U', ecl.cl - ecl.op - i.y86.1a - i.y86.1b - i.y84a - i.y84b.t - i.y84b.r - i.Z04 - i.Y82, 0),
         i.y83.2  = if_else(ctgy.dir == 'U', ecl.cl - ecl.op - i.y86.1a - i.y86.1b - i.y84a - i.y84b.t - i.y84b.r - i.Z04 - i.Y82, 0),
         i.tfr_pre= ecl.op + i.y86.1a + i.y86.1b + i.y84a + i.y84b.t + i.y84b.r,
         i.tfr_out= -case_when(ctgy.dir != 'U' & pre_post == 'pre' ~ i.tfr_pre,
                               ctgy.dir != 'U' & pre_post == 'post' ~ ecl.op,
                               TRUE ~ 0),
         i.tfr_in = -i.tfr_out) %>% ungroup() %>% 
  select(-g.tfr_pre, -i.tfr_pre) %>% 
  mutate_at(vars(starts_with("g.")), funs(replace_na(., 0))) %>% 
  mutate_at(vars(starts_with("i.")), funs(replace_na(., 0))) %>% 
  mutate(g.check  = gca.op + rowSums(select(., contains("g."))) - gca.cl,
         i.check  = ecl.op + rowSums(select(., contains("i."))) - ecl.cl)


#=========================================================================================
#==     Gather to long table                                                            ==
#=========================================================================================

bln_mvnt_long <- bln_mvnt %>% select(date, cust, ctgy.cl, ctgy.op, pre_post, gca.cl, 
                                     ecl.cl, gca.op, ecl.op, g.y86.1a:i.tfr_in) %>% 
  gather(key = "m_ment", value = "tran_ccy1", gca.cl:i.tfr_in) %>% 
  arrange(cust, m_ment, date) %>% filter(tran_ccy1 != 0) %>% 
  mutate(ctgyz = case_when(m_ment == "gca.op" |
                               m_ment == "ecl.op" |
                               m_ment == "g.tfr_out" |
                               m_ment == "i.tfr_out" |
                               m_ment == "g.y86.1a" & pre_post == "pre" |
                               m_ment == "g.y86.1b" & pre_post == "pre" |
                               m_ment == "g.y84a"   & pre_post == "pre" |
                               m_ment == "g.y84b.t" & pre_post == "pre" |
                               m_ment == "g.y84b.r" & pre_post == "pre" ~
                               ctgy.op,
                             TRUE ~ ctgy.cl
  )) 

# TO DO: remove attributes not used (pre_post, ctgy.op/.cl)
# TO DO: add GCA / ECL attribute based on gca/g. and ecl/i.
# TO DO: unsure no duplicates over account/company/month

#=========================================================================================
#==     Write csv                                                                       ==
#=========================================================================================

write.csv(bln_mvnt_long, file = "bln_mvnt_long.csv")










#=========================================================================================
#==     debugging                                                                       ==
#=========================================================================================
cl_dec <- bln_mvnt_long %>% filter(m_ment == "gca.cl" & date == as.Date('2018-02-01'))
op_jan <- bln_mvnt_long %>% filter(m_ment == "gca.op" & date == as.Date('2018-03-01'))
dec_jan<- full_join(cl_dec, op_jan, by = "cust") %>% 
  mutate(check = tran_ccy1.x - tran_ccy1.y) %>% 
  filter(check != 0)
