library(readxl)
library(dplyr)
library(stringr)
library(readr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(purrr)
library(broom)
library(betareg)
library(ggrepel)

##### 這邊是 Function  #####

# variables
variables <- function(winners, losers, margin, data){
  list(
    model.frame(winners, data, na.action = na.pass)[,1],
    model.frame(losers, data, na.action = na.pass)[,1],
    model.frame(margin, data, na.action = na.pass)[,1]
  )
}


# lookup
lookup <- function(name, list, default){
  
  if(name %in% names(list)){
    list[[name]]
  }
  else{
    default
  }
}

# joint_additive
joint_additive <- function(winners, losers, margin, k.margin, k.win, scale.margin, scale.win, data, default = 1500) {		
  
  ratings <- list()
  
  if(missing(data)){
    winners <- as.character(winners)
    losers <- as.character(losers)
  }
  else{
    v <- variables(winners, losers, margin, data)						
    winners <- as.character(v[[1]])
    losers <- as.character(v[[2]])
    margin <- v[[3]]
  }
  
  if(any(is.na(margin))){
    
    warning("Missing values in MOV found and will be excluded.")
    
    exclude <- is.na(margin)
    winners <- winners[!exclude]
    losers <- losers[!exclude]
    margin <- margin[!exclude]
  }
  
  nlength <- length(winners)
  
  results <- data.frame(
    winner = winners,
    loser = losers,
    winner_margin = margin,
    winner_before_elo = numeric(nlength),
    loser_before_elo = numeric(nlength),
    win_prediction = numeric(nlength),
    margin_prediction = numeric(nlength),
    winner_elo = numeric(nlength), 
    loser_elo = numeric(nlength),
    stringsAsFactors = F
  )
  
  
  for (i in 1:nlength) {
    
    cur_winner <- winners[i]
    cur_loser <- losers[i]
    
    winner_elo <- lookup(cur_winner, ratings, default)
    loser_elo <- lookup(cur_loser, ratings, default)
    
    winner_margin <- (winner_elo - loser_elo) / scale.margin
    winner_prob <- 1/(1 + 10^(-1 * (winner_elo - loser_elo)/scale.win))
    
    winner_update <- k.margin * (margin[i] - winner_margin)  + k.win * (1 - winner_prob)
    
    loser_update <-  -1 * k.margin * (margin[i] - winner_margin) + k.win * (0 - (1 - winner_prob))
    
    ratings[[cur_winner]] = winner_elo + winner_update 
    ratings[[cur_loser]] = loser_elo + loser_update 	   
    results$margin_prediction[i] <- winner_margin
    results$win_prediction[i] <- winner_prob
    results$winner_elo[i] <- ratings[[cur_winner]]
    results$loser_elo[i] <- ratings[[cur_loser]]
    results$winner_before_elo[i] <- winner_elo
    results$loser_before_elo[i] <- loser_elo	    
    
  }
  
  results		
}


# safe_log
safe_log <- function(p, eps = 1e-12) log(pmax(pmin(p, 1 - eps), eps))


# ELO 目標函數
objective_winonly <- function(par, dat, default_rating = 1500) {
  k.win     <- par[["k.win"]]
  # scale.win <- 400  # 仍可固定（也可改成參數估）
  scale.win <- par[["scale.win"]]
  
  res <- joint_additive(
    winners = dat$winner,
    losers  = dat$loser,
    margin  = dat$score_margin,   # 仍需傳，但不會用到（因為 k.margin=0）
    k.margin = 0,                 # 關閉 MOV 更新
    k.win    = k.win,
    scale.margin = 1,             # 隨便填，不會用到
    scale.win    = scale.win,
    default = default_rating
  )
  
  Phat <- res$win_prediction
  -mean(safe_log(Phat))           # 只最小化勝負的 log-loss
}


# 最佳化(ELO)
fit_winonly <- function(dat,
                        init  = c(k.win = 30, scale.win = 400),
                        lower = c(k.win = 1, scale.win = 80),
                        upper = c(k.win = 80, scale.win = 1000),
                        default_rating = 1500) {
  obj <- function(p) objective_winonly(p, dat = dat, default_rating = default_rating)
  opt <- optim(par = init, fn = obj, method = "L-BFGS-B",
               lower = lower, upper = upper,
               control = list(maxit = 300))
  list(par = opt$par, value = opt$value, convergence = opt$convergence, message = opt$message)
}


# MOV_ELO 目標函數
objective_joint <- function(par, dat, default_rating = 1500) {
  # 參數
  k.margin     <- par[["k.margin"]]
  k.win        <- par[["k.win"]]
  scale.margin <- par[["scale.margin"]]
  scale.win    <- 400
  
  # 跑一遍 rating 更新，取出對每場的 Mhat、Phat
  res <- joint_additive(
    winners = dat$winner,
    losers  = dat$loser,
    margin  = dat$score_margin,
    k.margin = k.margin,
    k.win    = k.win,
    scale.margin = scale.margin,
    scale.win    = scale.win,
    default = default_rating
  )
  
  # 真實 MOV 與預測
  M    <- dat$score_margin
  Mhat <- res$margin_prediction
  Phat <- res$win_prediction
  
  # 目標函數
  N   <- length(M)
  sdM <- stats::sd(M)
  mov_term <- sum( (Mhat - M)^2 ) / (3 * sdM)
  logloss_term <- -sum( safe_log(Phat) )
  
  (mov_term + logloss_term) / N
}


# 最佳化 (MOV_ELO)
fit_movelo <- function(dat,
                       init  = c(k.margin = 4,  k.win = 30, scale.margin = 20*sd(dat$score_margin)),
                       lower = c(k.margin = 0.1, k.win =  1, scale.margin = 1),
                       upper = c(k.margin = 20,  k.win = 80, scale.margin = 100),
                       default_rating = 1500) {
  
  obj_wrap <- function(p) objective_joint(p, dat = dat, default_rating = default_rating)
  
  opt <- optim(par = init, fn = obj_wrap, method = "L-BFGS-B",
               lower = lower, upper = upper,
               control = list(maxit = 500))
  
  list(par = opt$par,
       value = opt$value,
       convergence = opt$convergence,
       message = opt$message)
}



##### 這邊是 input 資料跟前處理 #####

# 調日期
df <- raw %>%
  filter(狀態 %in% c("勝","敗")) %>%
  mutate(order_id = row_number(),
         date_mmdd = stringr::str_extract(日期, "\\d{1,2}/\\d{1,2}"),
         date_key  = as.Date(paste0("2024/", date_mmdd), format = "%Y/%m/%d"))

# 檢查是否還有 NA 日期
sum(is.na(df$date_key))
df %>% filter(is.na(date_key)) %>% count(日期, sort=TRUE) %>% print(n=30)


# 排序日期
df <- df %>%
  mutate(
    date_mmdd = str_extract(日期, "\\d{2}/\\d{2}"),
    date_key  = as.Date(paste0("2024/", date_mmdd), format = "%Y/%m/%d")
  )

# 去除重複對戰紀錄
df <- df %>%
  mutate(
    t1 = str_trim(隊伍),
    t2 = str_trim(對手),
    a  = pmin(t1, t2),
    b  = pmax(t1, t2),
    game_key = paste(date_key, a, b, sep = "|")
  )

df_keep <- df %>%
  group_by(game_key) %>%
  slice_min(order_by = order_id, n = 1, with_ties = FALSE) %>%
  ungroup()

df_keep$home <- df_keep$`主場/客場`

# 轉 winner / loser、主客場、贏了用大分，輸了用小分
out <- df_keep %>%
  transmute(
    date_raw = 日期,
    date_mmdd,
    date_key,
    order_id,
    winner = if_else(狀態 == "勝", t1, t2),
    loser  = if_else(狀態 == "勝", t2, t1),
    win_score = 大,
    lose_score = 小,
    score_margin = 大 - 小,
    win_is_home = if_else(home == "vs", 1, 0)
  ) %>%
  arrange(date_key, order_id) %>%
  mutate(seq_id = row_number()) %>%
  select(seq_id, date_mmdd, winner, loser, win_is_home, 
         win_score, lose_score, score_margin)


# 輸出 excel
readr::write_excel_csv(out, "colley_input_sorted.csv")


dat <- readr::read_csv("colley_input_sorted.csv", show_col_types = FALSE) %>%
  mutate(
    # 去除隊名空白與奇怪字元
    winner = str_squish(winner),
    loser  = str_squish(loser),
    date_key = as.Date(paste0("2024/", date_mmdd), format = "%Y/%m/%d")
  ) %>%
  arrange(date_key, seq_id)
dat %>% count(date_key) %>% arrange(date_key) %>% print(n=Inf)

# 準備隊伍索引
teams <- sort(unique(c(dat$winner, dat$loser)))
n <- length(teams)
idx <- setNames(seq_len(n), teams)
sd <- sd(dat$score_margin)


##### 這邊是 ELO  #####

# 估計未知參數
dat <- dat %>% arrange(date_key, seq_id)

fit_wl <- fit_winonly(dat)
fit_wl

ratings_wl <- joint_additive(
  ~winner, ~loser, ~score_margin, data = dat,
  k.margin = 0,                 
  k.win    = fit_wl$par[["k.win"]],
  scale.margin = 1,             
  scale.win    = fit_wl$par[["scale.win"]],
  default = 1500
)

# 轉成長格式
elo_long <- ratings_wl %>%
  mutate(date = dat$date_key) %>%
  dplyr::select(date, team = winner, elo = winner_elo) %>%
  bind_rows(
    ratings_wl %>% mutate(date = dat$date_key) %>%
      dplyr::select(date, team = loser, elo = loser_elo)
  ) %>%
  arrange(team, date)


# 若某隊那天沒比賽，用「截至該日最近 Elo」
# target_date <- as.Date("2024-09-29")
# elo_latest <- elo_long %>% filter(date <= target_date) %>%
#   group_by(team) %>% slice_max(order_by = date, n = 1, with_ties = FALSE) %>% ungroup()


# 補齊 Elo (沒比賽用最近 Elo)
all_days  <- seq(min(elo_long$date), max(elo_long$date), by = "day")
all_teams <- sort(unique(elo_long$team))

elo_daily <- tidyr::expand_grid(date = all_days, team = all_teams) %>%
  left_join(elo_long, by = c("date","team")) %>%
  arrange(team, date) %>%
  group_by(team) %>%
  tidyr::fill(elo, .direction = "down") %>%
  mutate(elo = ifelse(is.na(elo), 1500, elo)) %>%
  ungroup()

# ELO 取每週最後一天
elo_weekly <- elo_daily %>%
  mutate(week_start = floor_date(date, unit = "week", week_start = 1)) %>%
  group_by(team, week_start) %>%
  slice_max(order_by = date, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  # 改成新的欄名
  rename(week = week_start)


# 畫圖
# 設定球隊顏色
# --- AL East 
AL_East <- c(
  "洋基" = "#003087",      # New York Yankees navy blue
  "藍鳥" = "#134A8E",      # Toronto Blue Jays royal blue
  "光芒" = "#092C5C",      # Tampa Bay Rays navy
  "紅襪" = "#BD3039",      # Boston Red Sox red
  "金鶯" = "#DF4601"       # Baltimore Orioles orange
)

label_df <- elo_weekly %>%
  group_by(team) %>% slice_max(order_by = week, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  filter(team %in% names(AL_East))  # 只標想強調的隊

ggplot(elo_weekly, aes(x = week, y = elo, group = team)) +
  # 先畫灰色線（所有球隊）
  geom_line(color = "grey85", linewidth = 0.8) +
  # 再畫指定球隊的彩色線（會蓋在上面）
  geom_line(
    data = elo_weekly %>% filter(team %in% names(AL_East)),
    aes(color = team),
    linewidth = 1.6
  ) +
  scale_color_manual(values = AL_East) +
  labs(
    title = "每隊 Elo 分數變化",
    x = "比賽日期", y = "Elo 分數", color = "球隊"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "bottom",
    panel.grid.major = element_line(color = "grey95"),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  geom_text_repel(
    data = label_df,
    aes(label = team, color = team),
    nudge_x = 1,
    direction = "y",
    hjust = 0,
    size = 4,
    fontface = "bold",
    segment.color = NA,      # 不畫連線
    min.segment.length = 0
  ) +
  
  scale_x_date(expand = expansion(mult = c(0.01, 0.05))) +
  coord_cartesian(clip = "off") +
  theme_minimal(base_size = 13) +
  theme(
    plot.margin = margin(t = 10, r = 0, b = 10, l = 10) # 右邊多留邊界
  ) +
  labs(title = "每隊 Elo 分數變化", x = "比賽日期", y = "Elo 分數")

# --- AL Central
AL_Central <- c(
  "老虎" = "#0C2340",      # Detroit Tigers navy
  "守護者" = "#00385D",    # Cleveland Guardians navy
  "雙城" = "#002B5C",      # Minnesota Twins navy
  "皇家" = "#004687",      # Kansas City Royals royal blue
  "白襪" = "#27251F"       # Chicago White Sox black
)

label_df <- elo_weekly %>%
  group_by(team) %>% slice_max(order_by = week, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  filter(team %in% names(AL_Central))  # 只標想強調的隊

ggplot(elo_weekly, aes(x = week, y = elo, group = team)) +
  # 先畫灰色線（所有球隊）
  geom_line(color = "grey85", linewidth = 0.8) +
  # 再畫指定球隊的彩色線（會蓋在上面）
  geom_line(
    data = elo_weekly %>% filter(team %in% names(AL_Central)),
    aes(color = team),
    linewidth = 1.6
  ) +
  scale_color_manual(values = AL_Central) +
  labs(
    title = "每隊 Elo 分數變化",
    x = "比賽日期", y = "Elo 分數", color = "球隊"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "bottom",
    panel.grid.major = element_line(color = "grey95"),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  geom_text_repel(
    data = label_df,
    aes(label = team, color = team),
    nudge_x = 1,
    direction = "y",
    hjust = 0,
    size = 4,
    fontface = "bold",
    segment.color = NA,      # 不畫連線
    min.segment.length = 0
  ) +
  
  scale_x_date(expand = expansion(mult = c(0.01, 0.05))) +
  coord_cartesian(clip = "off") +
  theme_minimal(base_size = 13) +
  theme(
    plot.margin = margin(t = 10, r = 0, b = 10, l = 10) # 右邊多留邊界
  ) +
  labs(title = "每隊 Elo 分數變化", x = "比賽日期", y = "Elo 分數")

# --- AL West
AL_West <- c(
  "太空人" = "#EB6E1F",    # Houston Astros orange
  "水手" = "#005C5C",      # Seattle Mariners teal green
  "天使" = "#BA0021",      # Los Angeles Angels red
  "遊騎兵" = "#003278",    # Texas Rangers blue
  "運動家" = "#003831"     # Oakland Athletics green
)

ggplot(elo_weekly, aes(x = week, y = elo, group = team)) +
  # 先畫灰色線（所有球隊）
  geom_line(color = "grey85", linewidth = 0.8) +
  # 再畫指定球隊的彩色線（會蓋在上面）
  geom_line(
    data = elo_weekly %>% filter(team %in% names(AL_West)),
    aes(color = team),
    linewidth = 1.6
  ) +
  scale_color_manual(values = AL_West) +
  labs(
    title = "每隊 Elo 分數變化",
    x = "比賽日期", y = "Elo 分數", color = "球隊"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "bottom",
    panel.grid.major = element_line(color = "grey95"),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


# --- NL East
NL_East <- c(
  "費城人" = "#E81828",    # Philadelphia Phillies red
  "大都會" = "#002D72",    # New York Mets blue
  "馬林魚" = "#00A3E0",    # Miami Marlins blue
  "勇士" = "#CE1141",      # Atlanta Braves red
  "國民" = "#AB0003"       # Washington Nationals red
)

ggplot(elo_weekly, aes(x = week, y = elo, group = team)) +
  # 先畫灰色線（所有球隊）
  geom_line(color = "grey85", linewidth = 0.8) +
  # 再畫指定球隊的彩色線（會蓋在上面）
  geom_line(
    data = elo_weekly %>% filter(team %in% names(NL_East)),
    aes(color = team),
    linewidth = 1.6
  ) +
  scale_color_manual(values = NL_East) +
  labs(
    title = "每隊 Elo 分數變化",
    x = "比賽日期", y = "Elo 分數", color = "球隊"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "bottom",
    panel.grid.major = element_line(color = "grey95"),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


# --- NL Central
NL_Central <- c(
  "小熊" = "#0E3386",      # Chicago Cubs blue
  "釀酒人" = "#12284B",    # Milwaukee Brewers navy
  "紅雀" = "#C41E3A",      # St. Louis Cardinals red
  "紅人" = "#C6011F",      # Cincinnati Reds red
  "海盜" = "#FDB827"       # Pittsburgh Pirates gold
)

ggplot(elo_weekly, aes(x = week, y = elo, group = team)) +
  # 先畫灰色線（所有球隊）
  geom_line(color = "grey85", linewidth = 0.8) +
  # 再畫指定球隊的彩色線（會蓋在上面）
  geom_line(
    data = elo_weekly %>% filter(team %in% names(NL_Central)),
    aes(color = team),
    linewidth = 1.6
  ) +
  scale_color_manual(values = NL_Central) +
  labs(
    title = "每隊 Elo 分數變化",
    x = "比賽日期", y = "Elo 分數", color = "球隊"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "bottom",
    panel.grid.major = element_line(color = "grey95"),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


# --- NL West
NL_West <- c(
  "道奇" = "#005A9C",      # Los Angeles Dodgers blue
  "教士" = "#2F241D",      # San Diego Padres brown
  "巨人" = "#FD5A1E",      # San Francisco Giants orange
  "響尾蛇" = "#A71930",    # Arizona Diamondbacks red
  "落磯" = "#33006F"       # Colorado Rockies purple
)

label_df <- elo_weekly %>%
  group_by(team) %>% slice_max(order_by = week, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  filter(team %in% names(NL_West))  # 只標想強調的隊

ggplot(elo_weekly, aes(x = week, y = elo, group = team)) +
  # 先畫灰色線（所有球隊）
  geom_line(color = "grey85", linewidth = 0.8) +
  # 再畫指定球隊的彩色線（會蓋在上面）
  geom_line(
    data = elo_weekly %>% filter(team %in% names(NL_West)),
    aes(color = team),
    linewidth = 1.6
  ) +
  scale_color_manual(values = NL_West) +
  labs(
    title = "每隊 Elo 分數變化",
    x = "比賽日期", y = "Elo 分數", color = "球隊"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "bottom",
    panel.grid.major = element_line(color = "grey95"),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  geom_text_repel(
    data = label_df,
    aes(label = team, color = team),
    nudge_x = 1,
    direction = "y",
    hjust = 0,
    size = 4,
    fontface = "bold",
    segment.color = NA,      # 不畫連線
    min.segment.length = 0
  ) +
  
  scale_x_date(expand = expansion(mult = c(0.01, 0.05))) +
  coord_cartesian(clip = "off") +
  theme_minimal(base_size = 13) +
  theme(
    plot.margin = margin(t = 10, r = 0, b = 10, l = 10) # 右邊多留邊界
  ) +
  labs(title = "每隊 Elo 分數變化", x = "比賽日期", y = "Elo 分數")


##### 這邊是 MOV_ELO  #####

# 估計未知參數
fit_mov <- fit_movelo(dat)
fit_mov

ratings_mov <- joint_additive(
  ~winner, ~loser, ~score_margin, data = dat,
  k.margin = fit_mov$par[["k.margin"]],                 
  k.win = fit_mov$par[["k.win"]],
  scale.margin = fit_mov$par[["scale.margin"]],             
  scale.win = 400,
  default = 1500
)

# 轉成長格式
elo_long_mov <- ratings_mov %>%
  mutate(date = dat$date_key) %>%
  dplyr::select(date, team = winner, elo = winner_elo) %>%
  bind_rows(
    ratings_mov %>% mutate(date = dat$date_key) %>%
      dplyr::select(date, team = loser, elo = loser_elo)
  ) %>%
  arrange(team, date)

# 補齊 Elo (沒比賽用最近 Elo)
all_days  <- seq(min(elo_long_mov$date), max(elo_long_mov$date), by = "day")
all_teams <- sort(unique(elo_long_mov$team))

elo_daily_mov <- tidyr::expand_grid(date = all_days, team = all_teams) %>%
  left_join(elo_long_mov, by = c("date","team")) %>%
  arrange(team, date) %>%
  group_by(team) %>%
  tidyr::fill(elo, .direction = "down") %>%
  mutate(elo = ifelse(is.na(elo), 1500, elo)) %>%
  ungroup()

# ELO 取每週最後一天
elo_weekly_mov <- elo_daily_mov %>%
  mutate(week_start = floor_date(date, unit = "week", week_start = 1)) %>%
  group_by(team, week_start) %>%
  slice_max(order_by = date, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  # 改成新的欄名
  rename(week = week_start)


# 畫圖
# 設定球隊顏色
# --- AL East
AL_East <- c(
  "洋基" = "#003087",      # New York Yankees navy blue
  "藍鳥" = "#134A8E",      # Toronto Blue Jays royal blue
  "光芒" = "#092C5C",      # Tampa Bay Rays navy
  "紅襪" = "#BD3039",      # Boston Red Sox red
  "金鶯" = "#DF4601"       # Baltimore Orioles orange
)

ggplot(elo_weekly_mov, aes(x = week, y = elo, group = team)) +
  # 先畫灰色線（所有球隊）
  geom_line(color = "grey85", linewidth = 0.8) +
  # 再畫指定球隊的彩色線（會蓋在上面）
  geom_line(
    data = elo_weekly_mov %>% filter(team %in% names(AL_East)),
    aes(color = team),
    linewidth = 1.6
  ) +
  scale_color_manual(values = AL_East) +
  labs(
    title = "每隊 Elo 分數變化",
    x = "比賽日期", y = "Elo 分數", color = "球隊"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "bottom",
    panel.grid.major = element_line(color = "grey95"),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


# --- AL Central
AL_Central <- c(
  "老虎" = "#0C2340",      # Detroit Tigers navy
  "守護者" = "#00385D",    # Cleveland Guardians navy
  "雙城" = "#002B5C",      # Minnesota Twins navy
  "皇家" = "#004687",      # Kansas City Royals royal blue
  "白襪" = "#27251F"       # Chicago White Sox black
)

ggplot(elo_weekly_mov, aes(x = week, y = elo, group = team)) +
  # 先畫灰色線（所有球隊）
  geom_line(color = "grey85", linewidth = 0.8) +
  # 再畫指定球隊的彩色線（會蓋在上面）
  geom_line(
    data = elo_weekly_mov %>% filter(team %in% names(AL_Central)),
    aes(color = team),
    linewidth = 1.6
  ) +
  scale_color_manual(values = AL_Central) +
  labs(
    title = "每隊 Elo 分數變化",
    x = "比賽日期", y = "Elo 分數", color = "球隊"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "bottom",
    panel.grid.major = element_line(color = "grey95"),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


# --- AL West
AL_West <- c(
  "太空人" = "#EB6E1F",    # Houston Astros orange
  "水手" = "#005C5C",      # Seattle Mariners teal green
  "天使" = "#BA0021",      # Los Angeles Angels red
  "遊騎兵" = "#003278",    # Texas Rangers blue
  "運動家" = "#003831"     # Oakland Athletics green
)

ggplot(elo_weekly_mov, aes(x = week, y = elo, group = team)) +
  # 先畫灰色線（所有球隊）
  geom_line(color = "grey85", linewidth = 0.8) +
  # 再畫指定球隊的彩色線（會蓋在上面）
  geom_line(
    data = elo_weekly_mov %>% filter(team %in% names(AL_West)),
    aes(color = team),
    linewidth = 1.6
  ) +
  scale_color_manual(values = AL_West) +
  labs(
    title = "每隊 Elo 分數變化",
    x = "比賽日期", y = "Elo 分數", color = "球隊"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "bottom",
    panel.grid.major = element_line(color = "grey95"),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


# --- NL East
NL_East <- c(
  "費城人" = "#E81828",    # Philadelphia Phillies red
  "大都會" = "#002D72",    # New York Mets blue
  "馬林魚" = "#00A3E0",    # Miami Marlins blue
  "勇士" = "#CE1141",      # Atlanta Braves red
  "國民" = "#AB0003"       # Washington Nationals red
)

ggplot(elo_weekly_mov, aes(x = week, y = elo, group = team)) +
  # 先畫灰色線（所有球隊）
  geom_line(color = "grey85", linewidth = 0.8) +
  # 再畫指定球隊的彩色線（會蓋在上面）
  geom_line(
    data = elo_weekly_mov %>% filter(team %in% names(NL_East)),
    aes(color = team),
    linewidth = 1.6
  ) +
  scale_color_manual(values = NL_East) +
  labs(
    title = "每隊 Elo 分數變化",
    x = "比賽日期", y = "Elo 分數", color = "球隊"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "bottom",
    panel.grid.major = element_line(color = "grey95"),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


# --- NL Central
NL_Central <- c(
  "小熊" = "#0E3386",      # Chicago Cubs blue
  "釀酒人" = "#12284B",    # Milwaukee Brewers navy
  "紅雀" = "#C41E3A",      # St. Louis Cardinals red
  "紅人" = "#C6011F",      # Cincinnati Reds red
  "海盜" = "#FDB827"       # Pittsburgh Pirates gold
)

ggplot(elo_weekly_mov, aes(x = week, y = elo, group = team)) +
  # 先畫灰色線（所有球隊）
  geom_line(color = "grey85", linewidth = 0.8) +
  # 再畫指定球隊的彩色線（會蓋在上面）
  geom_line(
    data = elo_weekly_mov %>% filter(team %in% names(NL_Central)),
    aes(color = team),
    linewidth = 1.6
  ) +
  scale_color_manual(values = NL_Central) +
  labs(
    title = "每隊 Elo 分數變化",
    x = "比賽日期", y = "Elo 分數", color = "球隊"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "bottom",
    panel.grid.major = element_line(color = "grey95"),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


# --- NL West
NL_West <- c(
  "道奇" = "#005A9C",      # Los Angeles Dodgers blue
  "教士" = "#2F241D",      # San Diego Padres brown
  "巨人" = "#FD5A1E",      # San Francisco Giants orange
  "響尾蛇" = "#A71930",    # Arizona Diamondbacks red
  "落磯" = "#33006F"       # Colorado Rockies purple
)

label_df <- elo_weekly_mov %>%
  group_by(team) %>% slice_max(order_by = week, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  filter(team %in% names(NL_West))  # 只標想強調的隊

ggplot(elo_weekly_mov, aes(x = week, y = elo, group = team)) +
  # 先畫灰色線（所有球隊）
  geom_line(color = "grey85", linewidth = 0.8) +
  # 再畫指定球隊的彩色線（會蓋在上面）
  geom_line(
    data = elo_weekly_mov %>% filter(team %in% names(NL_West)),
    aes(color = team),
    linewidth = 1.6
  ) +
  scale_color_manual(values = NL_West) +
  labs(
    title = "每隊 Elo 分數變化",
    x = "比賽日期", y = "Elo 分數", color = "球隊"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "bottom",
    panel.grid.major = element_line(color = "grey95"),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  geom_text_repel(
    data = label_df,
    aes(label = team, color = team),
    nudge_x = 1,
    direction = "y",
    hjust = 0,
    size = 4,
    fontface = "bold",
    segment.color = NA,      # 不畫連線
    min.segment.length = 0
  ) +
  
  scale_x_date(expand = expansion(mult = c(0.01, 0.05))) +
  coord_cartesian(clip = "off") +
  theme_minimal(base_size = 13) +
  theme(
    plot.margin = margin(t = 10, r = 0, b = 10, l = 10) # 右邊多留邊界
  ) +
  labs(title = "每隊 Elo 分數變化", x = "比賽日期", y = "Elo 分數")


##### 這邊是整理 logistic 所需變數 #####

dat <- dat %>% arrange(date_key, seq_id)
all_teams <- sort(unique(c(dat$winner, dat$loser)))

# 以「有比賽的不同日期」為基準，每 5 天一個斷點
cut_dates <- as.Date(c(
  "2024-04-05", "2024-04-10", "2024-04-15", "2024-04-20", "2024-04-25", "2024-04-30",
  "2024-05-05", "2024-05-10", "2024-05-15", "2024-05-20", "2024-05-25", "2024-05-30",
  "2024-06-05", "2024-06-10", "2024-06-15", "2024-06-20", "2024-06-25", "2024-06-30",
  "2024-07-05", "2024-07-10", "2024-07-15", "2024-07-20", "2024-07-25", "2024-07-30",
  "2024-08-05", "2024-08-10", "2024-08-15", "2024-08-20", "2024-08-25", "2024-08-30", 
  "2024-09-05", "2024-09-10", "2024-09-15", "2024-09-20", "2024-09-25", "2024-09-30"
))

# 從 joint_additive 的輸出中抓「各隊最後 Elo」
final_elo_from_results <- function(res_tbl, train_tbl) {
  elo_long <- res_tbl %>%
    mutate(date = train_tbl$date_key) %>%
    select(date, team = winner, elo = winner_elo) %>%
    bind_rows(
      res_tbl %>% mutate(date = train_tbl$date_key) %>%
        select(date, team = loser,  elo = loser_elo)
    ) %>%
    arrange(team, date)
  
  elo_long %>%
    group_by(team) %>% slice_max(order_by = date, n = 1, with_ties = FALSE) %>%
    ungroup() %>% select(team, elo)
}

# 對某個斷點 cutoff（含當日）估參數、重跑 Elo、取 30 隊 Elo
elo_at_cutoff <- function(cutoff_date) {
  train <- dat %>% filter(date_key <= cutoff_date)
  
  # 只用訓練資料估參數
  fit <- fit_winonly(train)
  kw  <- fit$par[["k.win"]]
  sw  <- fit$par[["scale.win"]]
  
  # 用訓練資料跑一次，拿到截至 cutoff 的 Elo
  res_train <- joint_additive(
    ~winner, ~loser, ~score_margin, data = train,
    k.margin = 0,            # win-only
    k.win    = kw,
    scale.margin = 1,
    scale.win    = sw,
    default = 1500
  )
  
  last_elo <- final_elo_from_results(res_train, train)
  
  # 30 隊都要有：沒出賽的隊伍填 1500
  tibble(team = all_teams) %>%
    left_join(last_elo, by = "team") %>%
    mutate(
      elo  = ifelse(is.na(elo), 1500, elo),
      snap = cutoff_date
    )
}

# 跑所有斷點：得到 Elo 長表
elo_by_cutoff <- map_dfr(cut_dates, elo_at_cutoff)
elo_by_cutoff %>% arrange(snap, desc(elo)) %>% print(n = 60)

# # 轉寬表
# elo_wide <- elo_by_cutoff %>%
#   mutate(snap_chr = format(snap, "%Y-%m-%d")) %>%
#   select(team, snap_chr, elo) %>%
#   pivot_wider(names_from = snap_chr, values_from = elo)

# 從輸出中抓「各隊最後 mov_Elo」
final_movelo_from_results <- function(res_tbl, train_tbl) {
  elo_long_mov <- res_tbl %>%
    mutate(date = train_tbl$date_key) %>%
    select(date, team = winner, elo = winner_elo) %>%
    bind_rows(
      res_tbl %>% mutate(date = train_tbl$date_key) %>%
        select(date, team = loser,  elo = loser_elo)
    ) %>%
    arrange(team, date)
  
  elo_long_mov %>%
    group_by(team) %>% slice_max(order_by = date, n = 1, with_ties = FALSE) %>%
    ungroup() %>% select(team, elo)
}

# 對某個斷點 cutoff（含當日）估參數、重跑 Elo、取 30 隊 Elo
movelo_at_cutoff <- function(cutoff_date) {
  train <- dat %>% filter(date_key <= cutoff_date)
  
  # 只用訓練資料估參數
  fit <- fit_movelo(train)
  kw  <- fit$par[["k.win"]]
  km  <- fit$par[["k.margin"]]
  sm  <- fit$par[["scale.margin"]]
  
  # 用訓練資料跑一次，拿到截至 cutoff 的 Elo
  res_train <- joint_additive(
    ~winner, ~loser, ~score_margin, data = train,
    k.margin = km,            
    k.win = kw,
    scale.margin = sm,
    scale.win = 400,
    default = 1500
  )
  
  last_elo <- final_movelo_from_results(res_train, train)
  
  # 30 隊都要有：沒出賽的隊伍填 1500
  tibble(team = all_teams) %>%
    left_join(last_elo, by = "team") %>%
    mutate(
      elo  = ifelse(is.na(elo), 1500, elo),
      snap = cutoff_date
    )
}

# 跑所有斷點：得到 Elo 長表
movelo_by_cutoff <- map_dfr(cut_dates, movelo_at_cutoff)
movelo_by_cutoff %>% arrange(snap, desc(elo)) %>% print(n = 60)
movelo_by_cutoff <- rename(movelo_by_cutoff, movelo = elo)

# 這邊加 mov_ELO
elo_by_cutoff <- elo_by_cutoff %>%
  left_join(movelo_by_cutoff, by = c("team" = "team", "snap" = "snap"))

# 計算勝率、小分差比率、主客場比率
df$score_margin <- if_else(df$狀態 == "勝", df$大 - df$小, df$小 - df$大)
df$home <- if_else(df$`主場/客場`=="vs", 1, 0)
dfp <- df[,c(7,10,16,17)]

# 函數：對某個截點計算 luck / unluck 比例
luck_summary_at_cutoff <- function(cutoff_date) {
  df_cut <- df %>% filter(date_key <= cutoff_date)
  
  df_cut %>%
    group_by(隊伍) %>%
    summarise(
      # 勝率
      total_games = n(),
      total_wins = sum(score_margin > 0),     
      winp = ifelse(total_games > 0, total_wins / total_games, NA_real_),
      # 小分差比率
      luck_wins   = sum(score_margin %in% c(1, 2)),     # 小分差贏
      unluck_lose = sum(score_margin %in% c(-1, -2)),   # 小分差輸
      luck_winp   = ifelse(total_games > 0, luck_wins / total_games, NA_real_),
      unluck_losep = ifelse(total_games > 0, unluck_lose / total_games, NA_real_),
      # 主客場比率
      home_games = sum(home),
      home_percent = home_games / total_games
    ) %>%
    mutate(cut_date = cutoff_date)
}

# 對每個 cut_date 做迴圈
luck_vars <- map_dfr(cut_dates, luck_summary_at_cutoff)

# 查看結果
luck_vars %>%
  arrange(cut_date, desc(luck_winp)) %>%
  print(n = 30)

# 合併資料
elo_combined <- elo_by_cutoff %>%
  rename(cut_date = snap) %>%
  left_join(luck_vars, by = c("team" = "隊伍", "cut_date" = "cut_date"))


all_teams <- sort(unique(df$隊伍))

# 對單一截斷日計算壓力指數
pressure_at_cutoff <- function(cutoff_date, df, all_teams, target_rank = 12) {
  
  # 截斷日前(含當日)累積戰績
  agg <- df %>%
    filter(date_key <= cutoff_date) %>%
    group_by(隊伍) %>%
    summarise(
      wins  = sum(score_margin > 0, na.rm = TRUE),
      loss  = sum(score_margin < 0, na.rm = TRUE),
      games = n(),
      .groups = "drop"
    )
  
  # 確保所有球隊都有一列（沒出賽補 0）
  standings <- tibble(隊伍 = all_teams) %>%
    left_join(agg, by = "隊伍") %>%
    mutate(across(c(wins, loss, games), ~ tidyr::replace_na(., 0L))) %>%
    # 排名規則：先勝場多，再敗場少，再隊名字母序（可依需要調整）
    arrange(desc(wins), loss, 隊伍) %>%
    mutate(rank = row_number())
  
  # 取第 target_rank 名的「基準勝場」
  baseline_wins <- standings %>%
    arrange(rank) %>%
    slice(target_rank) %>%
    pull(wins)
  
  # 計算距離（距離越小 → 壓力越大）
  maxdiff <- max(abs(standings$wins - baseline_wins))
  out <- standings %>%
    transmute(
      cut_date   = cutoff_date,
      team       = 隊伍,
      wins       = wins,
      baseline_wins_rank12 = baseline_wins,
      pressure_dist_wins   = abs(wins - baseline_wins),                      # 你的定義：距離
      # pressure_norm01      = ifelse(maxdiff > 0, 1 - pressure_dist_wins/maxdiff, 1) # 0–1化（越接近第12名越接近1）
    )
  
  out
}

# 對所有截斷日跑一遍
pressure_by_cut <- map_dfr(cut_dates, ~pressure_at_cutoff(.x, df, all_teams))

elo_final <- pressure_by_cut %>%
  left_join(elo_combined, by = c("team" = "team", "cut_date" = "cut_date"))

path <- "C:/Users/USER/Desktop/2024_colley.csv"
colley_2024 <- read_csv(path)

colley_2024$cut_date <- as.Date(colley_2024$cut_date)
elo_final$cut_date <- as.Date(elo_final$cut_date)

elo_final2024 <- colley_2024 %>%
  left_join(elo_final, by = c("team" = "team", "cut_date" = "cut_date"))

elo_finnaall <- elo_final2024 %>% select("cut_date", "team", "total_games", "total_wins",
                                         "winp", "rating", "elo", "movelo", "luck_wins", "unluck_lose",
                                         "luck_winp", "unluck_losep", "home_games", "home_percent",
                                         everything()) 

# elo_finnaall <- elo_final %>% select("cut_date", "team", "total_games", "total_wins",
#                                          "winp", "elo", "movelo", "luck_wins", "unluck_lose",
#                                          "luck_winp", "unluck_losep", "home_games", "home_percent",
#                                          everything())

final_winp <- data.frame(
  team = c("釀酒人", "費城人", "洋基", "藍鳥", "道奇",
           "小熊", "教士", "水手", "紅襪", "守護者",
           "老虎", "太空人", "紅人", "大都會", "皇家",
           "巨人", "遊騎兵", "響尾蛇", "馬林魚", "紅雀",
           "光芒", "運動家", "勇士", "金鶯", "天使",
           "海盜", "雙城", "國民", "白襪", "落磯" ),
  final_winp = c(0.599, 0.593, 0.580, 0.580, 0.574,
                 0.568, 0.556, 0.556, 0.549, 0.543,
                 0.537, 0.537, 0.512, 0.512, 0.506,
                 0.500, 0.500, 0.494, 0.488, 0.481,
                 0.475, 0.469, 0.469, 0.463, 0.444,
                 0.438, 0.432, 0.407, 0.370, 0.265)
)

elo_lm <- elo_finnaall %>%
  left_join(final_winp, by = c("team" = "team"))

train_dates <- as.Date(c(
  "2024-04-05", "2024-04-10", "2024-04-15", "2024-04-20", "2024-04-25",
  "2024-05-05", "2024-05-10", "2024-05-15", "2024-05-20", "2024-05-25", 
  "2024-06-05", "2024-06-10", "2024-06-15", "2024-06-20", "2024-06-25",
  "2024-07-05", "2024-07-10", "2024-07-15", "2024-07-20", "2024-07-25", 
  "2024-08-05", "2024-08-10", "2024-08-15", "2024-08-20", "2024-08-25", 
  "2024-09-05", "2024-09-10", "2024-09-15", "2024-09-20", "2024-09-25"
))

test_dates <- as.Date(c(
  "2024-04-30", "2024-05-30", "2024-06-30", 
  "2024-07-30", "2024-08-30", "2024-09-30"
))

elo_train <- elo_lm %>%
  filter(cut_date %in% train_dates)

elo_test <- elo_lm %>%
  filter(cut_date %in% test_dates)


each_lm <- function(df_model, strength = c("elo", "movelo", "winp", "rating")) {
  
  # strength 傳進來是字串，例如 "elo"
  strength <- match.arg(strength)
  
  cut_dates <- sort(unique(df_model$cut_date))
  
  results <- map_df(cut_dates, function(cd) {
    
    df_sub <- df_model %>%
      filter(cut_date == cd)
    
    # 動態建立公式：final_winp ~ strength + 其他解釋變數
    rhs_vars <- c(strength, "luck_winp", "unluck_losep","pressure_dist_wins")
    fml <- reformulate(rhs_vars, response = "final_winp")
    
    fit <- betareg(fml, data = df_sub)
    
    broom::tidy(fit) %>%
      mutate(cut_date = cd,
             strength_var = strength)
  })
  
  results
}

lm_results_elo <- each_lm(elo_lm, strength = "elo")
lm_results_movelo <- each_lm(elo_lm, strength = "movelo")
lm_results_winp <- each_lm(elo_lm, strength = "winp")
lm_results_rating <- each_lm(elo_lm, strength = "rating")

eval_lm_by_cut <- function(train_df,
                           test_df = NULL,
                           strength = c("elo", "movelo", "winp", "rating"),
                           topN = 10) {
  
  strength <- match.arg(strength)
  if (is.null(test_df)) test_df <- train_df
  
  # 測試集的所有 cut_date（例如 04-30, 05-30, ...）
  test_dates <- sort(unique(test_df$cut_date))
  
  results <- purrr::map_df(test_dates, function(td) {
    
    # 抓出「年-月」字串，例如 "2024-04"
    ym <- format(td, "%Y-%m")
    
    # 訓練集：同一個年-月的所有 cut_date
    train_sub <- train_df %>%
      dplyr::filter(format(cut_date, "%Y-%m") == ym)
    
    # 測試集：該天（通常是該月 30 號）
    test_sub <- test_df %>%
      dplyr::filter(cut_date == td)
    
    # 如果某個月訓練或測試沒資料，就回傳 NA（避免程式掛掉）
    if (nrow(train_sub) == 0 || nrow(test_sub) == 0) {
      return(tibble::tibble(
        cut_date_test   = td,
        train_start     = as.Date(NA),
        train_end       = as.Date(NA),
        strength_var    = strength,
        rmse            = NA_real_,
        rank_cor        = NA_real_,
        topN_acc_model  = NA_real_,
        winp_rank_cor   = NA_real_,
        winp_topN_acc   = NA_real_
      ))
    }
    
    # ---------- 模型部分 ----------
    rhs_vars <- c(strength, "luck_winp", "unluck_losep","pressure_dist_wins")
    fml <- reformulate(rhs_vars, response = "final_winp")
    
    fit <- betareg(fml, data = train_sub)
    
    # 預測
    test_sub <- test_sub %>%
      dplyr::mutate(pred = predict(fit, newdata = test_sub))
    
    # RMSE
    rmse <- sqrt(mean((test_sub$final_winp - test_sub$pred)^2, na.rm = TRUE))
    
    # 預測 vs 真實 的排名相關
    rank_cor <- suppressWarnings(
      cor(test_sub$pred, test_sub$final_winp,
          method = "spearman", use = "complete.obs")
    )
    
    # 真實前 N 名
    actual_top <- test_sub %>%
      dplyr::arrange(dplyr::desc(final_winp)) %>%
      dplyr::slice_head(n = topN) %>%
      dplyr::pull(team)
    
    # 用「模型預測值」決定前 N 名
    pred_top <- test_sub %>%
      dplyr::arrange(dplyr::desc(pred)) %>%
      dplyr::slice_head(n = topN) %>%
      dplyr::pull(team)
    
    topN_acc_model <- length(intersect(actual_top, pred_top)) / topN
    
    # # ---------- baseline：只看當下勝率 winp ----------
    # winp_rank_cor <- suppressWarnings(
    #   cor(test_sub$winp, test_sub$final_winp,
    #       method = "spearman", use = "complete.obs")
    # )
    # 
    # winp_top <- test_sub %>%
    #   dplyr::arrange(dplyr::desc(winp)) %>%
    #   dplyr::slice_head(n = topN) %>%
    #   dplyr::pull(team)
    # 
    # winp_topN_acc <- length(intersect(actual_top, winp_top)) / topN
    
    tibble::tibble(
      cut_date_test   = td,                          # 該月的測試日 (30 號)
      train_start     = min(train_sub$cut_date),     # 該月訓練起始日
      train_end       = max(train_sub$cut_date),     # 該月訓練結束日
      strength_var    = strength,
      rmse            = rmse,
      rank_cor        = rank_cor,
      topN_acc_model  = topN_acc_model,
      # winp_rank_cor   = winp_rank_cor,
      # winp_topN_acc   = winp_topN_acc
    )
  })
  
  results
}


metrics_elo   <- eval_lm_by_cut(elo_train, elo_test, strength = "elo",   topN = 12)
metrics_winp  <- eval_lm_by_cut(elo_train, elo_test, strength = "winp",  topN = 12)
metrics_movelo<- eval_lm_by_cut(elo_train, elo_test, strength = "movelo",topN = 12)
metrics_rating<- eval_lm_by_cut(elo_train, elo_test, strength = "rating",topN = 12)

winp_rank_table <- elo_lm %>%
  dplyr::group_by(cut_date) %>%
  dplyr::arrange(dplyr::desc(winp), .by_group = TRUE) %>%
  dplyr::mutate(rank_winp = dplyr::row_number()) %>%
  dplyr::ungroup()



### 比較上述 4 種方法在各時間截點下的準確率 (預測的排名 與 最終進季後賽的隊伍中了幾隊)


# 一次把四種實力指標都跑一遍
metrics_all <- bind_rows(
  eval_lm_by_cut(elo_train, elo_test, strength = "elo",    topN = 12),
  eval_lm_by_cut(elo_train, elo_test, strength = "movelo", topN = 12),
  eval_lm_by_cut(elo_train, elo_test, strength = "winp",   topN = 12),
  eval_lm_by_cut(elo_train, elo_test, strength = "rating", topN = 12)
) %>%
  # 把 cut_date_test 改名成 cut_date，方便後續沿用你的舊程式碼
  dplyr::rename(cut_date = cut_date_test)

# 確認一下長相
dplyr::glimpse(metrics_all)

# baseline：只用 winp 當預測指標
baseline_winp <- metrics_all %>%
  group_by(cut_date) %>%
  summarise(
    winp_rank_cor  = first(winp_rank_cor),
    winp_topN_acc  = first(winp_topN_acc),
    .groups = "drop"
  )

# --- Top N 準確率圖 ---
gg_topN <- ggplot() +
  # # baseline：只用 winp 排名抓前 N 隊
  # geom_line(
  #   data = baseline_winp,
  #   aes(x = cut_date, y = winp_topN_acc),
  #   color = "black", linewidth = 1, linetype = "dashed"
  # ) +
  # 模型：不同 strength 的 topN_acc_model
  geom_line(
    data = metrics_all,
    aes(x = cut_date, y = topN_acc_model, color = strength_var),
    linewidth = 1
  ) +
  scale_color_brewer(palette = "Set1") +
  labs(
    title = "各截點模型預測前 N 名準確率（與當下勝率 baseline 比較）",
    x = "cut_date",
    y = "TopN Accuracy",
    color = "Strength 指標"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

gg_topN

# --- 排名相關係數圖 ---
gg_rank <- ggplot() +
  # baseline：winp vs final_winp 的排名相關
  # geom_line(
  #   data = baseline_winp,
  #   aes(x = cut_date, y = winp_rank_cor),
  #   color = "black", linewidth = 1, linetype = "dashed"
  # ) +
  # 模型：pred vs final_winp 的排名相關
  geom_line(
    data = metrics_all,
    aes(x = cut_date, y = rank_cor, color = strength_var),
    linewidth = 1
  ) +
  scale_color_brewer(palette = "Set1") +
  labs(
    title = "各截點模型預測勝率排序相關（與當下勝率 baseline 比較）",
    x = "cut_date",
    y = "Spearman Rank Correlation",
    color = "Strength 指標"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

gg_rank
