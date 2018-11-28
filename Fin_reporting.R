# required packages
library("tidyverse")
library("DescTools")
library("lubridate")


#=========================================================================================
#==    Script deriving opening to closing reconciliation by month                       ==
#==    Data to be providing in the following format:                                    ==
#==    - "cust"  Customer account identifier                                            ==
#==    - "year"  Year of balance                                                        ==
#==    - "mth"   Month of balance                                                       ==
#==    - "unit"  Reporting unit                                                         ==
#==    - "ccy"   Currency of the loan account                                           ==
#==    - "type"  Type of loan, 'term' or 'rvlv', 'rvlv' are revolving loans             ==
#==    - "stage" The IFRS9 stage (1,2, 3)                                               ==
#==    - "poci"  Purchased or Originated Credit Impaired loan (Y / N)                   ==
#==    - "gca"   Gross carrying amount, the loan balance as it appears in the general   ==
#==              ledger, this may differ to the customer balance due to effective       ==
#==              interest rate accounting of feed                                       ==
#==    - "bal"   Customer balance of loan                                               ==
#==    - "ecl"   Expected credit loss balance                                           ==
#==    - "wof"   Year to date write-off balance                                         ==
#==    - "pryr"  The 12 month ECL using the prior period methodology                    ==
#==    - "prlt"  The lifetime ECL using the prior period methodology                    ==
#=========================================================================================


#=========================================================================================
#==    Load mock data                                                                   ==
#==    1. read raw data                                                                 ==
#==    2. repeat data for performance testing                                           ==
#==    3. create date                                                                   ==
#==    4. expand for all combinations of month and account                              ==
#==    5. fill missing account values (those NA's after step 4)                         ==
#==    6. join account and unit to create distinct records & avoid duplicates           ==
#==    7. replace na's created at step 4 woth zero                                      ==
#==    8. find minimum date                                                             ==
#=========================================================================================

n <- 5 # number of repeats
x <- 6 # number of accounts in data
data <- read.csv("data_v02.csv", header= T, sep = ",")                                  #1
bln_ctgy<- data.frame(data,i = rep(1:n, ea = NROW(data))) %>%                           #2
  mutate(cust.i = ((i - 1) * x) + cust) %>% select(-cust, -i) %>% 
  rename(cust = cust.i) %>% as.tibble() %>% 
  mutate(date = as.Date(paste(year, mth, 1, sep = "-"))) %>%                            #3
  complete(date = seq.Date(min(date), max(date), by = "month"), cust) %>%               #4
  arrange(cust, date) %>% group_by(cust) %>% 
  mutate(type = coalesce(type, na.omit(type)[1]),                                       #5
         ccy  = coalesce(ccy, na.omit(ccy)[1]), 
         unit = coalesce(unit, na.omit(unit)[1])) %>%
  unite(cust, cust, unit, sep = "_") %>%                                                #6                 
  replace_na(list(gca = 0, bal = 0, ecl = 0, wof = 0,                                   #7
                  pryr = 0, prlt = 0))  %>%
  ungroup

min.date <- bln_ctgy %>% slice(which.min(date)) %>% select(date)


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
  rename(gca.cl    = gca, bal.cl = bal, ecl.cl = ecl,                                   #4
         ctgy.cl   = ctgy, wof.cl = wof) %>%                                            
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
         gca.m.tfr.i  = -gca.m.tfr.o,
         ecl.m.dd.r   = -cover * gca.m.dd.r,
         ecl.m.dd.t   = -cover * gca.m.dd.t,
         ecl.m.rd.t.f = -cover * gca.m.rd.t.f,
         ecl.m.rd.t   = -cover * gca.m.rd.t,
         ecl.m.rd.r   = -cover * gca.m.rd.r,
         ecl.m.wof    = wof.cl,
         ecl.m.prm    = case_when(ctgy.cl == 1 & pryr != 0 ~ ecl.cl + pryr,
                                  ctgy.cl != 1 & prlt != 0 ~ ecl.cl + prlt,
                                  TRUE ~ 0),
         ecl.m.rem.mig= if_else(ctgy.dir != 'U', ecl.cl - ecl.op - ecl.m.dd.r - 
                                                 ecl.m.dd.t - ecl.m.rd.t.f - 
                                                 ecl.m.rd.t - ecl.m.rd.r - 
                                                 ecl.m.wof - ecl.m.prm, 0),
         ecl.m.rem    = if_else(ctgy.dir == 'U', ecl.cl - ecl.op - ecl.m.dd.r - 
                                                 ecl.m.dd.t - ecl.m.rd.t.f - 
                                                 ecl.m.rd.t - ecl.m.rd.r - 
                                                 ecl.m.wof - ecl.m.prm, 0),
         ecl.tfr.pre  = ecl.op + ecl.m.dd.r + ecl.m.dd.t + 
                        ecl.m.rd.t.f + ecl.m.rd.t + ecl.m.rd.r,
         ecl.m.tfr.o  = -case_when(ctgy.dir != 'U' & pre.post == 'pre' ~ ecl.tfr.pre,
                                   ctgy.dir != 'U' & pre.post == 'post' ~ ecl.op,
                                   TRUE ~ 0),
         ecl.m.tfr.i = -ecl.m.tfr.o) %>% ungroup() %>% 
  select(-g.tfr.pre, -ecl.tfr.pre) %>% 
  mutate_at(vars(starts_with("g.")), funs(replace_na(., 0))) %>% 
  mutate_at(vars(starts_with("i.")), funs(replace_na(., 0))) %>% 
  mutate(gca.ch  = gca.op + rowSums(select(., contains("gca."))) - gca.cl,
         ecl.ch  = ecl.op + rowSums(select(., contains("ecl."))) - ecl.cl)


#=========================================================================================
#==     Gather to long table  & apply pre / post rules                                  ==
#=========================================================================================

bln_mvnt_long <- bln_mvnt %>% 
  select(date, cust, ctgy.cl, ctgy.op, pre.post, gca.cl, 
         ecl.cl, gca.op, ecl.op, gca.m.dd.r:ecl.m.tfr.i) %>% 
  gather(key = "m.ment", value = "tran_ccy", gca.cl:ecl.m.tfr.i) %>% 
  arrange(cust, m.ment, date) %>% filter(tran_ccy != 0) %>% 
  mutate(bal.type = str_sub(m.ment, 0, 3),
         m.type   = str_sub(m.ment, 5)) %>% 
  mutate(ctgy = case_when(m.type == "op" |
                          m.type == "m.tfr.o" |
                          m.type == "m.dd.r"   & pre.post == "pre" |
                          m.type == "m.dd.t"   & pre.post == "pre" |
                          m.type == "m.rd.t.f" & pre.post == "pre" |
                          m.type == "m.rd.t"   & pre.post == "pre" |
                          m.type == "m.rd.r"   & pre.post == "pre" ~
                          ctgy.op,
                          TRUE ~ ctgy.cl), 
         m.type = if_else(str_detect(m.type, "tfr"), 
                          paste("tfr", ctgy.op, ctgy.cl, sep = "."), 
                          m.type),
         year = year(date),
         month = month(date)) %>% 
  select(date, year, month, cust, ctgy.op, ctgy.cl, ctgy, bal.type, m.type, tran_ccy) %>% 
  arrange(cust, date, bal.type)

# TO DO: unsure no duplicates over account/company/month

#=========================================================================================
#==     Write csv                                                                       ==
#=========================================================================================

write.csv(bln_mvnt_long, file = "bln_mvnt_long.csv")
saveRDS(bln_mvnt_long, file="bln_mvnt_long.Rda")










#=========================================================================================
#==     debugging                                                                       ==
#=========================================================================================
cl_dec <- bln_mvnt_long %>% filter(m.ment == "gca.cl" & date == as.Date('2018-02-01'))
op_jan <- bln_mvnt_long %>% filter(m.ment == "gca.op" & date == as.Date('2018-03-01'))
dec_jan<- full_join(cl_dec, op_jan, by = "cust") %>% 
  mutate(check = tran_ccy1.x - tran_ccy1.y) %>% 
  filter(check != 0)


