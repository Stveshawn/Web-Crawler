library(rvest)
library(tidyverse)
library(stringr)
library(purrr)

# ----------defining helper functions--------
try_to_num = function(v) {
  tryCatch(as.numeric(as.character(v)), warning = function(w) as.character(v))
}

df_str = function(d) {
  colnames(d) = d[1,] %>% str_remove('\\n|\\*')
  d = d %>%
    .[-1,] %>%
    data.frame() %>%
    map_df(try_to_num) %>%
    data.frame()
  # mutate(Maturity = Maturity %>% as.Date('%Y %b %d')) 
  return(d)
}
# ----------------------------------------


# ----1. Treasury Inflation-Protected Securities

url1 = 'http://www.wsj.com/mdc/public/page/2_3020-tips.html?mod=topnav_2_3020'

page1 = read_html(url1)

date1 = page1 %>%
  html_nodes('#showCal~ span') %>%
  html_text() %>%
  as.Date('%a, %b %d, %Y')

df1 = page1 %>% 
  html_nodes('#column0 td') %>%
  html_text() %>%
  matrix(ncol= 7, byrow = TRUE) %>%
  df_str()

R1 = list(wsj_tip_date = date1,
          wsj_tip_df = df1)

# ----2. U.S. Treasury Quotes
Sys.sleep(2) # suspend for 2 seconds

url2 = 'http://www.wsj.com/mdc/public/page/2_3020-treasury.html?mod=topnav_2_3020'

page2 = read_html(url2)

date2 = page2 %>%
  html_nodes('#showCal~ span') %>%
  html_text() %>%
  as.Date('%a, %b %d, %Y')

df2_raw = page2 %>%
  html_nodes('#column0 td') %>%
  html_text()

split_ind = which(df2_raw=='Maturity')[2]

df2_1 = df2_raw[1:(split_ind-1)] %>%
  matrix(. , ncol= 6, byrow = TRUE) %>%
  df_str()

df2_2 = df2_raw[split_ind:length(df2_raw)] %>%
  matrix(. , ncol= 5, byrow = TRUE) %>%
  df_str()

R2 = list(wsj_coupon_date = date2,
         wsj_coupon_df = df2_1,
         wsj_tbill_date = date2,
         wsj_tbill_df = df2_2)


# -----3. U.S. Treasury Strips
Sys.sleep(2) # suspend for 2 seconds

url3 = 'http://www.wsj.com/mdc/public/page/2_3020-tstrips.html?mod=topnav_2_3020'

page3 = read_html(url3)

date3 = page3 %>%
  html_nodes('#showCal~ span') %>%
  html_text() %>%
  as.Date('%a, %b %d, %Y')

df3_raw = page3 %>%
  html_nodes('#column0 td') %>%
  html_text()

split_bp = which(df3_raw=='Treasury Bond, Stripped Principal')
split_np = which(df3_raw=='Treasury Note, Stripped Principal')
split_ci = which(df3_raw=='Stripped Coupon Interest')

df3_1 = c(df3_raw[1:(split_bp-1)],
          df3_raw[(split_bp+1) : (split_np-1)]) %>%
  matrix(. , ncol= 5, byrow = TRUE) %>%
  df_str()

df3_2 = c(df3_raw[1:(split_bp-1)],
          df3_raw[(split_np+1) : (split_ci-1)]) %>%
  matrix(. , ncol= 5, byrow = TRUE) %>%
  df_str()

df3_3 = c(df3_raw[1:(split_bp-1)],
          df3_raw[(split_ci+1) : length(df3_raw)]) %>%
  matrix(. , ncol= 5, byrow = TRUE) %>%
  df_str()

R3 = list(wsj_bp_date = date3,
          wsj_bp_df = df3_1,
          wsj_np_date = date3,
          wsj_np_df = df3_2,
          wsj_ci_date = date3,
          wsj_ci_df = df3_3)

# ----------------combining results

R = c(R1, R2, R3)

# R is a list containing all the expected variables