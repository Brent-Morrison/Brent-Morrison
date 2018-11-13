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
#==    6. fill rvlv_term attribute to newly created records                             ==
#=========================================================================================

bln_ctgy <- as.tibble(read.csv("data_v01.csv", header= T, sep = ",")) %>%           
  mutate(date = as.Date(paste(RA_6731, RA_6732, 1, sep = "-"))) %>%                     #1
  unite(RA_716, RA_716, DA_049, sep = "_") %>%                     
  select(-Notes) %>%                                                                   #2
  #select(-DA_010, -DA_011, -DA_012, -DA_187) %>% 
  complete(date = seq.Date(min(date), max(date), by = "month"), RA_716) %>%               #3
  arrange(RA_716,date) %>%                                                              #4
  replace_na(list(DA_017 = 0, DA_059 = 0, DA_103 = 0, RA_523 = 0, 
                  DA_033 = 0, DA_034 = 0))  %>%                                         #5
  group_by(RA_716) %>% fill(rvlv_term) %>% ungroup                                      #6

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
  mutate(RA_689 = case_when(RA_523 != 0   & DA_020 == "N" ~ 3,                          #1
                            RA_523 != 0   & DA_020 == "Y" ~ 5,
                            RA_523 != 0   & is.na(DA_020) ~ 3,
                            DA_018 == 1   & DA_020 == "N" ~ 1,
                            DA_018 == 2   & DA_020 == "N" ~ 2,
                            DA_018 == 3   & DA_020 == "N" ~ 3,
                            is.na(DA_018) & DA_020 == "N" ~ 1,
                            DA_018 == 1   & DA_020 == "Y" ~ 4,
                            DA_018 == 2   & DA_020 == "Y" ~ 4,
                            DA_018 == 3   & DA_020 == "Y" ~ 5,
                            is.na(DA_018) & DA_020 == "Y" ~ 4,
                            DA_018 == 1   & is.na(DA_020) ~ 1,
                            DA_018 == 2   & is.na(DA_020) ~ 2,
                            DA_018 == 3   & is.na(DA_020) ~ 3)
  ) %>% 
  group_by(RA_716) %>% fill(RA_689) %>% replace_na(list(RA_689 = 1)) %>%                #2
  mutate(DA_017.op = lag(DA_017),                                                       #3                  
         DA_059.op = lag(DA_059),
         DA_059.pr = ifelse(date == min.date, DA_059, NA),
         DA_103.op = lag(DA_103),
         RA_689.op = lag(RA_689)) %>% fill(DA_059.pr) %>% 
  rename(DA_017.cl = DA_017, DA_059.cl = DA_059, DA_103.cl = DA_103, 
         RA_689.cl = RA_689, RA_523.cl = RA_523) %>%                                    #4
  mutate(ledbal.y = DA_059.cl - DA_059.pr + cumsum(RA_523.cl),                          #5
         y86.1a.y = if_else(ledbal.y > 0, ledbal.y, 0),
         y84b.r.y = if_else(ledbal.y < 0, ledbal.y, 0)) %>% 
  ungroup()


#=========================================================================================
#==    Create movement attributes                                                       ==
#=========================================================================================

bln_mvnt <- bln_ctgy %>% group_by(RA_716) %>% 
  mutate(cover.cl = -DA_103.cl / DA_059.cl,
         cover.op = -DA_103.op / DA_059.op,
         cover    = Winsorize(round(if_else(is.nan(cover.op),cover.cl, cover.op),2), minval = 0, maxval = 1),
         incr_decr= case_when(DA_059.cl > DA_059.op ~ 'incr',
                              DA_059.cl < DA_059.op ~ 'decr',
                              TRUE ~ 'unch'),
         RD_018   = case_when(RA_689.cl > RA_689.op ~ 'D',
                              RA_689.cl < RA_689.op ~ 'I',
                              TRUE ~ 'U'),
         pre_post = case_when(RD_018 == 'I' & incr_decr == 'decr' ~ 'pre',
                              RD_018 == 'D' & incr_decr == 'incr' ~ 'pre',
                              TRUE ~ 'post'),
         pre_stage= if_else(pre_post == 'pre', RA_689.op, RA_689.cl),
         g.y86.1a = if_else(rvlv_term == 'rvlv', y86.1a.y - lag(y86.1a.y), 0),
         g.y86.1b = if_else(rvlv_term == 'term' & incr_decr == 'incr', DA_059.cl - DA_059.op + RA_523.cl, 0),
         g.y84a   = if_else(rvlv_term == 'term' & incr_decr == 'decr' & DA_059.cl == 0, DA_059.cl - DA_059.op + RA_523.cl, 0),
         g.y84b.t = if_else(rvlv_term == 'term' & incr_decr == 'decr' & DA_059.cl != 0, DA_059.cl - DA_059.op + RA_523.cl, 0),
         g.y84b.r = if_else(rvlv_term == 'rvlv', y84b.r.y - lag(y84b.r.y), 0),
         g.x640.4 = (DA_017.cl - DA_059.cl) - (DA_017.op - DA_059.op),
         g.tfr_pre= DA_017.op + g.y86.1a + g.y86.1b + g.y84a + g.y84b.t + g.y84b.r,
         g.Z04    = -RA_523.cl,
         g.tfr_out= -case_when(RD_018 != 'U' & pre_post == 'pre' ~ g.tfr_pre,
                               RD_018 != 'U' & pre_post == 'post' ~ DA_017.op,
                               TRUE ~ 0),
         g.tfr_in = -g.tfr_out,
         i.y86.1a = -cover * g.y86.1a,
         i.y86.1b = -cover * g.y86.1b,
         i.y84a   = -cover * g.y84a,
         i.y84b.t = -cover * g.y84b.t,
         i.y84b.r = -cover * g.y84b.r,
         i.Z04    = RA_523.cl,
         i.Y82    = case_when(RA_689.cl == 1 & DA_033 != 0 ~ DA_103.cl + DA_033,
                              RA_689.cl != 1 & DA_034 != 0 ~ DA_103.cl + DA_034,
                              TRUE ~ 0),
         i.y83.1  = if_else(RD_018 != 'U', DA_103.cl - DA_103.op - i.y86.1a - i.y86.1b - i.y84a - i.y84b.t - i.y84b.r - i.Z04 - i.Y82, 0),
         i.y83.2  = if_else(RD_018 == 'U', DA_103.cl - DA_103.op - i.y86.1a - i.y86.1b - i.y84a - i.y84b.t - i.y84b.r - i.Z04 - i.Y82, 0),
         i.tfr_pre= DA_103.op + i.y86.1a + i.y86.1b + i.y84a + i.y84b.t + i.y84b.r,
         i.tfr_out= -case_when(RD_018 != 'U' & pre_post == 'pre' ~ i.tfr_pre,
                               RD_018 != 'U' & pre_post == 'post' ~ DA_103.op,
                               TRUE ~ 0),
         i.tfr_in = -i.tfr_out) %>% ungroup() %>% 
  select(-g.tfr_pre, -i.tfr_pre) %>% 
  mutate_at(vars(starts_with("g.")), funs(replace_na(., 0))) %>% 
  mutate_at(vars(starts_with("i.")), funs(replace_na(., 0))) %>% 
  mutate(g.check  = DA_017.op + rowSums(select(., contains("g."))) - DA_017.cl,
         i.check  = DA_103.op + rowSums(select(., contains("i."))) - DA_103.cl)


#=========================================================================================
#==     Gather to long table                                                            ==
#=========================================================================================

bln_mvnt_long <- bln_mvnt %>% select(date, RA_716, RA_689.cl, RA_689.op, pre_post, DA_017.cl, 
                                     DA_103.cl, DA_017.op, DA_103.op, g.y86.1a:i.tfr_in) %>% 
  gather(key = "m_ment", value = "tran_ccy1", DA_017.cl:i.tfr_in) %>% 
  arrange(RA_716, m_ment, date) %>% filter(tran_ccy1 != 0) %>% 
  mutate(RA_689z = case_when(m_ment == "DA_017.op" |
                               m_ment == "DA_103.op" |
                               m_ment == "g.tfr_out" |
                               m_ment == "i.tfr_out" |
                               m_ment == "g.y86.1a" & pre_post == "pre" |
                               m_ment == "g.y86.1b" & pre_post == "pre" |
                               m_ment == "g.y84a"   & pre_post == "pre" |
                               m_ment == "g.y84b.t" & pre_post == "pre" |
                               m_ment == "g.y84b.r" & pre_post == "pre" ~
                               RA_689.op,
                             TRUE ~ RA_689.cl
  )) 

# TO DO: remove attributes not used (pre_post, RA_689.op/.cl)
# TO DO: add GCA / ECL attribute based on DA_017/g. and DA_103/i.
# TO DO: unsure no duplicates over account/company/month

#=========================================================================================
#==     Write csv                                                                       ==
#=========================================================================================

write.csv(bln_mvnt_long, file = "bln_mvnt_long.csv")










#=========================================================================================
#==     debugging                                                                       ==
#=========================================================================================
cl_dec <- bln_mvnt_long %>% filter(m_ment == "DA_017.cl" & date == as.Date('2018-02-01'))
op_jan <- bln_mvnt_long %>% filter(m_ment == "DA_017.op" & date == as.Date('2018-03-01'))
dec_jan<- full_join(cl_dec, op_jan, by = "RA_716") %>% 
  mutate(check = tran_ccy1.x - tran_ccy1.y) %>% 
  filter(check != 0)
