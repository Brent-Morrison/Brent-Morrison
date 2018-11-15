# packages
library("tidyverse")
library("DescTools")

#=========================================================================================
#==    Load mock data                                                                   ==
#==    1. create date & merge ID's for distinct records                                 ==
#==    2. remove notes  (not required for real data)                                    ==
#==    3. expand for all combinations of month and account                              ==
#==    4. order so group by for change works correctly                                  ==
#==    5. replace na's created by step 3                                                ==
#==    6. fill type attribute to newly created records                                  ==
#=========================================================================================

bln_ctgy <- as.tibble(read.csv("data_v01.csv", header= T, sep = ",")) %>%           
  mutate(date = as.Date(paste(year, mth, 1, sep = "-"))) %>%                            #1
  unite(cust, cust, unit, sep = "_") %>%                     
  select(-Notes) %>%                                                                    #2
  complete(date = seq.Date(min(date), max(date), by = "month"), cust) %>%               #3
  arrange(cust, date) %>%                                                               #4
  replace_na(list(gca = 0, bal = 0, ecl = 0, wof = 0, 
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
#==    5. add cumulative movement                                                       ==
#==    TO DO - add FINREP change of stage indicator for table 12.1                      ==
#=========================================================================================

bln_ctgy <- bln_ctgy %>%  
  mutate(ctgy = case_when(wof != 0     & poci == "N" ~ 3,                               #1
                          wof != 0     & poci == "Y" ~ 5,
                          wof != 0     & is.na(poci) ~ 3,
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
         bal.op    = lag(bal),
         bal.pr    = ifelse(date == min.date, bal, NA),
         ecl.op    = lag(ecl),
         ctgy.op   = lag(ctgy)) %>% fill(bal.pr) %>% 
  rename(gca.cl    = gca, bal.cl = bal, ecl.cl = ecl, 
         ctgy.cl   = ctgy, wof.cl = wof) %>%                                            #4
  mutate(bal.y     = bal.cl - bal.pr + cumsum(wof.cl),                                  #5
         bal.y.dd  = if_else(bal.y > 0, bal.y, 0),
         bal.y.rd  = if_else(bal.y < 0, bal.y, 0)) %>% 
  ungroup()


#=========================================================================================
#==    Create movement attributes                                                       ==
#=========================================================================================

bln_mvnt <- bln_ctgy %>% group_by(cust) %>% 
  mutate(cover.cl     = -ecl.cl / bal.cl,
         cover.op     = -ecl.op / bal.op,
         cover        = Winsorize(round(if_else(is.nan(cover.op),cover.cl, cover.op),2), 
                                  minval = 0, maxval = 1),
         incr.decr    = case_when(bal.cl > bal.op ~ 'incr',
                              bal.cl < bal.op ~ 'decr',
                              TRUE ~ 'unch'),
         ctgy.dir     = case_when(ctgy.cl > ctgy.op ~ 'D',
                              ctgy.cl < ctgy.op ~ 'I',
                              TRUE ~ 'U'),
         pre.post     = case_when(ctgy.dir == 'I' & incr.decr == 'decr' ~ 'pre',
                              ctgy.dir == 'D' & incr.decr == 'incr' ~ 'pre',
                              TRUE ~ 'post'),
         pre.stage    = if_else(pre.post == 'pre', ctgy.op, ctgy.cl),
         gca.m.dd.r   = if_else(type == 'rvlv', bal.y.dd - lag(bal.y.dd), 0),
         gca.m.dd.t   = if_else(type == 'term' & incr.decr == 'incr', 
                                bal.cl - bal.op + wof.cl, 0),
         gca.m.rd.t.f = if_else(type == 'term' & incr.decr == 'decr' & bal.cl == 0, 
                                bal.cl - bal.op + wof.cl, 0),
         gca.m.rd.t   = if_else(type == 'term' & incr.decr == 'decr' & bal.cl != 0, 
                                bal.cl - bal.op + wof.cl, 0),
         gca.m.rd.r   = if_else(type == 'rvlv', bal.y.rd - lag(bal.y.rd), 0),
         gca.m.oth    = (gca.cl - bal.cl) - (gca.op - bal.op),
         g.tfr.pre    = gca.op + gca.m.dd.r + gca.m.dd.t + gca.m.rd.t.f + 
                        gca.m.rd.t + gca.m.rd.r,
         gca.m.wof    = -wof.cl,
         gca.m.tfr.o  = -case_when(ctgy.dir != 'U' & pre.post == 'pre' ~ g.tfr.pre,
                                   ctgy.dir != 'U' & pre.post == 'post' ~ gca.op,
                                   TRUE ~ 0),
         gca.m.tfr.i = -gca.m.tfr.o,
         i.y86.1a = -cover * gca.m.dd.r,
         i.y86.1b = -cover * gca.m.dd.t,
         i.y84a   = -cover * gca.m.rd.t.f,
         i.y84b.t = -cover * gca.m.rd.t,
         i.y84b.r = -cover * gca.m.rd.r,
         i.Z04    = wof.cl,
         i.Y82    = case_when(ctgy.cl == 1 & pryr != 0 ~ ecl.cl + pryr,
                              ctgy.cl != 1 & prlt != 0 ~ ecl.cl + prlt,
                              TRUE ~ 0),
         i.y83.1  = if_else(ctgy.dir != 'U', ecl.cl - ecl.op - i.y86.1a - i.y86.1b - i.y84a - i.y84b.t - i.y84b.r - i.Z04 - i.Y82, 0),
         i.y83.2  = if_else(ctgy.dir == 'U', ecl.cl - ecl.op - i.y86.1a - i.y86.1b - i.y84a - i.y84b.t - i.y84b.r - i.Z04 - i.Y82, 0),
         i.tfr_pre= ecl.op + i.y86.1a + i.y86.1b + i.y84a + i.y84b.t + i.y84b.r,
         i.tfr_out= -case_when(ctgy.dir != 'U' & pre.post == 'pre' ~ i.tfr_pre,
                               ctgy.dir != 'U' & pre.post == 'post' ~ ecl.op,
                               TRUE ~ 0),
         i.tfr_in = -i.tfr_out) %>% ungroup() %>% 
  select(-g.tfr.pre, -i.tfr_pre) %>% 
  mutate_at(vars(starts_with("g.")), funs(replace_na(., 0))) %>% 
  mutate_at(vars(starts_with("i.")), funs(replace_na(., 0))) %>% 
  mutate(g.check  = gca.op + rowSums(select(., contains("g."))) - gca.cl,
         i.check  = ecl.op + rowSums(select(., contains("i."))) - ecl.cl)


#=========================================================================================
#==     Gather to long table                                                            ==
#=========================================================================================

bln_mvnt_long <- bln_mvnt %>% select(date, cust, ctgy.cl, ctgy.op, pre.post, gca.cl, 
                                     ecl.cl, gca.op, ecl.op, gca.m.dd.r:i.tfr_in) %>% 
  gather(key = "m_ment", value = "tran_ccy1", gca.cl:i.tfr_in) %>% 
  arrange(cust, m_ment, date) %>% filter(tran_ccy1 != 0) %>% 
  mutate(ctgy      = case_when(m_ment == "gca.op" |
                               m_ment == "ecl.op" |
                               m_ment == "gca.m.tfr.o" |
                               m_ment == "i.tfr_out" |
                               m_ment == "gca.m.dd.r" & pre.post == "pre" |
                               m_ment == "gca.m.dd.t" & pre.post == "pre" |
                               m_ment == "gca.m.rd.t.f"   & pre.post == "pre" |
                               m_ment == "gca.m.rd.t" & pre.post == "pre" |
                               m_ment == "gca.m.rd.r" & pre.post == "pre" ~
                               ctgy.op,
                             TRUE ~ ctgy.cl
  )) 

# TO DO: remove attributes not used (pre.post, ctgy.op/.cl)
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
