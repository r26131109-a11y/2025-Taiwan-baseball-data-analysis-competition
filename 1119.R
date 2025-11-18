## 可以點code旁邊的箭頭縮起來喔，不然會很多

##### 這邊是 載入套件 #####
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
library(car)
library(tibble)

##### 這邊是 Function  #####

### 前處理
data_clean <- function(raw){
  # 調日期
  df <- raw %>%
    filter(狀態 %in% c("勝","敗")) %>%
    mutate(order_id = row_number(),
           date_mmdd = stringr::str_extract(日期, "\\d{1,2}/\\d{1,2}"),
           date_key  = as.Date(paste0("2025/", date_mmdd), format = "%Y/%m/%d"))
  
  # 檢查是否還有 NA 日期
  sum(is.na(df$date_key))
  df %>% filter(is.na(date_key)) %>% count(日期, sort=TRUE) %>% print(n=30)
  
  
  # 排序日期
  df <- df %>%
    mutate(
      date_mmdd = str_extract(日期, "\\d{2}/\\d{2}"),
      date_key  = as.Date(paste0("2025/", date_mmdd), format = "%Y/%m/%d")
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
      date_key = as.Date(paste0("2025/", date_mmdd), format = "%Y/%m/%d")
    ) %>%
    arrange(date_key, seq_id)
  dat %>% count(date_key) %>% arrange(date_key) %>% print(n=Inf)
  
  # 準備隊伍索引
  teams <- sort(unique(c(dat$winner, dat$loser)))
  n <- length(teams)
  idx <- setNames(seq_len(n), teams)
  sd <- sd(dat$score_margin)
  
  list(
    df    = df,
    dat   = dat,
    teams = teams,
    idx   = idx,
    sd    = sd)
  
}

### variables
variables <- function(winners, losers, margin, data){
  list(
    model.frame(winners, data, na.action = na.pass)[,1],
    model.frame(losers, data, na.action = na.pass)[,1],
    model.frame(margin, data, na.action = na.pass)[,1]
  )
}


### lookup
lookup <- function(name, list, default){
  
  if(name %in% names(list)){
    list[[name]]
  }
  else{
    default
  }
}

### joint_additive
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


### safe_log
safe_log <- function(p, eps = 1e-12) log(pmax(pmin(p, 1 - eps), eps))


### ELO 目標函數
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


### 最佳化(ELO)
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


### MOV_ELO 目標函數
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


### 最佳化 (MOV_ELO)
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


run_one_season <- function(dat, df, year = 2025) {
  
  ## 0. 準備基本資料 ----
  dat <- dat %>% arrange(date_key, seq_id)
  all_teams <- sort(unique(c(dat$winner, dat$loser)))
  
  # 依照你原本的邏輯：每月 5、10、15、20、25、30 號當 cut date，
  # 月份抓 5–9 月，再砍掉超過該季最後一場比賽的日期
  make_cut_dates <- function(year, max_date) {
    months <- 5:9
    days   <- c(5, 10, 15, 20, 25, 30)
    
    dates <- as.Date(
      sprintf(
        "%d-%02d-%02d",
        year,
        rep(months, each = length(days)),
        rep(days,   times = length(months))
      )
    )
    dates[dates <= max_date]
  }
  
  cut_dates <- make_cut_dates(year, max(dat$date_key))
  
  ## 1. 從 joint_additive 的輸出抓「各隊最後 Elo」 ----
  final_elo_from_results <- function(res_tbl, train_tbl) {
    elo_long <- res_tbl %>%
      mutate(date = train_tbl$date_key) %>%
      select(date, team = winner, elo = winner_elo) %>%
      bind_rows(
        res_tbl %>%
          mutate(date = train_tbl$date_key) %>%
          select(date, team = loser, elo = loser_elo)
      ) %>%
      arrange(team, date)
    
    elo_long %>%
      group_by(team) %>%
      slice_max(order_by = date, n = 1, with_ties = FALSE) %>%
      ungroup() %>%
      select(team, elo)
  }
  
  # 單一 cut date 的 win-only Elo
  elo_at_cutoff <- function(cutoff_date) {
    train <- dat %>% filter(date_key <= cutoff_date)
    
    # 只用訓練資料估參數
    fit <- fit_winonly(train)
    kw  <- fit$par[["k.win"]]
    sw  <- fit$par[["scale.win"]]
    
    # 用訓練資料跑一次，拿到截至 cutoff 的 Elo
    res_train <- joint_additive(
      ~winner, ~loser, ~score_margin, data = train,
      k.margin     = 0,      # win-only
      k.win        = kw,
      scale.margin = 1,
      scale.win    = sw,
      default      = 1500
    )
    
    last_elo <- final_elo_from_results(res_train, train)
    
    # 所有隊都要有：沒出賽的隊伍填 1500
    tibble(team = all_teams) %>%
      left_join(last_elo, by = "team") %>%
      mutate(
        elo  = ifelse(is.na(elo), 1500, elo),
        snap = cutoff_date
      )
  }
  
  # 跑所有 cut dates：得到 Elo 長表
  elo_by_cutoff <- map_dfr(cut_dates, elo_at_cutoff)
  
  
  ## 2. MOV ELO（movelo） ----
  final_movelo_from_results <- function(res_tbl, train_tbl) {
    elo_long_mov <- res_tbl %>%
      mutate(date = train_tbl$date_key) %>%
      select(date, team = winner, elo = winner_elo) %>%
      bind_rows(
        res_tbl %>%
          mutate(date = train_tbl$date_key) %>%
          select(date, team = loser, elo = loser_elo)
      ) %>%
      arrange(team, date)
    
    elo_long_mov %>%
      group_by(team) %>%
      slice_max(order_by = date, n = 1, with_ties = FALSE) %>%
      ungroup() %>%
      select(team, elo)
  }
  
  movelo_at_cutoff <- function(cutoff_date) {
    train <- dat %>% filter(date_key <= cutoff_date)
    
    # 只用訓練資料估參數
    fit <- fit_movelo(train)
    kw  <- fit$par[["k.win"]]
    km  <- fit$par[["k.margin"]]
    sm  <- fit$par[["scale.margin"]]
    
    res_train <- joint_additive(
      ~winner, ~loser, ~score_margin, data = train,
      k.margin     = km,
      k.win        = kw,
      scale.margin = sm,
      scale.win    = 400,
      default      = 1500
    )
    
    last_elo <- final_movelo_from_results(res_train, train)
    
    tibble(team = all_teams) %>%
      left_join(last_elo, by = "team") %>%
      mutate(
        elo  = ifelse(is.na(elo), 1500, elo),
        snap = cutoff_date
      )
  }
  
  movelo_by_cutoff <- map_dfr(cut_dates, movelo_at_cutoff) %>%
    rename(movelo = elo)
  
  # 合併 Elo + movelo
  elo_by_cutoff <- elo_by_cutoff %>%
    left_join(movelo_by_cutoff, by = c("team", "snap"))
  
  
  ## 3. luck / unluck / 主客場比例 ----
  # 先把 df 補齊需要的欄位
  df <- df %>%
    mutate(
      score_margin = if_else(狀態 == "勝", 大 - 小, 小 - 大),
      home         = if_else(`主場/客場` == "vs", 1L, 0L)
    )
  
  luck_summary_at_cutoff <- function(cutoff_date) {
    df_cut <- df %>% filter(date_key <= cutoff_date)
    
    df_cut %>%
      group_by(隊伍) %>%
      summarise(
        total_games   = n(),
        total_wins    = sum(score_margin > 0),
        winp          = ifelse(total_games > 0, total_wins / total_games, NA_real_),
        luck_wins     = sum(score_margin %in% c( 1,  2)),
        unluck_lose   = sum(score_margin %in% c(-1, -2)),
        luck_winp     = ifelse(total_games > 0, luck_wins   / total_games, NA_real_),
        unluck_losep  = ifelse(total_games > 0, unluck_lose / total_games, NA_real_),
        home_games    = sum(home),
        home_percent  = home_games / total_games,
        .groups = "drop"
      ) %>%
      mutate(cut_date = cutoff_date)
  }
  
  luck_vars <- map_dfr(cut_dates, luck_summary_at_cutoff)
  
  # 跟 Elo 合併
  elo_combined <- elo_by_cutoff %>%
    rename(cut_date = snap) %>%
    left_join(luck_vars, by = c("team" = "隊伍", "cut_date"))
  
  
  ## 4. 壓力指標 ----
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
  all_teams_df <- sort(unique(df$隊伍))
  
  pressure_by_cut <- map_dfr(
    cut_dates,
    ~ pressure_at_cutoff(.x, df, all_teams_df)
  )
  
  elo_final <- pressure_by_cut %>%
    left_join(elo_combined, by = c("team", "cut_date")) %>%
    mutate(season = year)
  
  return(elo_final)
}

##### 這邊是 input 資料跟前處理 #####

path2025 <- "C:/Users/hungh/OneDrive/桌面/mlb_schedule_2025.xlsx"
raw2025 <- read_excel(path2025)
res2025 <- data_clean(raw2025)
df2025 <- res2025$df
dat2025   <- res2025$dat
teams2025 <- res2025$teams
idx2025   <- res2025$idx
sd2025    <- res2025$sd

path2024 <- "C:/Users/hungh/OneDrive/桌面/mlb_schedule_2024.xlsx"
raw2024 <- read_excel(path2024)
res2024 <- data_clean(raw2024)
df2024 <- res2024$df
dat2024   <- res2024$dat
teams2024 <- res2024$teams
idx2024   <- res2024$idx
sd2024    <- res2024$sd

path2023 <- "C:/Users/hungh/OneDrive/桌面/mlb_schedule_2023.xlsx"
raw2023 <- read_excel(path2023)
res2023 <- data_clean(raw2023)
df2023 <- res2023$df
dat2023   <- res2023$dat
teams2023 <- res2023$teams
idx2023   <- res2023$idx
sd2023    <- res2023$sd

path2022 <- "C:/Users/hungh/OneDrive/桌面/mlb_schedule_2022.xlsx"
raw2022 <- read_excel(path2022)
res2022 <- data_clean(raw2022)
df2022 <- res2022$df
dat2022   <- res2022$dat
teams2022 <- res2022$teams
idx2022   <- res2022$idx
sd2022    <- res2022$sd

path2021 <- "C:/Users/hungh/OneDrive/桌面/mlb_schedule_2021.xlsx"
raw2021 <- read_excel(path2021)
res2021 <- data_clean(raw2021)
df2021 <- res2021$df
dat2021   <- res2021$dat
teams2021 <- res2021$teams
idx2021   <- res2021$idx
sd2021    <- res2021$sd


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

label_df <- elo_weekly %>%
  group_by(team) %>% slice_max(order_by = week, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  filter(team %in% names(AL_West))  # 只標想強調的隊

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

# --- NL East
NL_East <- c(
  "費城人" = "#E81828",    # Philadelphia Phillies red
  "大都會" = "#002D72",    # New York Mets blue
  "馬林魚" = "#00A3E0",    # Miami Marlins blue
  "勇士" = "#CE1141",      # Atlanta Braves red
  "國民" = "#AB0003"       # Washington Nationals red
)

label_df <- elo_weekly %>%
  group_by(team) %>% slice_max(order_by = week, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  filter(team %in% names(NL_East))  # 只標想強調的隊

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


# --- NL Central
NL_Central <- c(
  "小熊" = "#0E3386",      # Chicago Cubs blue
  "釀酒人" = "#12284B",    # Milwaukee Brewers navy
  "紅雀" = "#C41E3A",      # St. Louis Cardinals red
  "紅人" = "#C6011F",      # Cincinnati Reds red
  "海盜" = "#FDB827"       # Pittsburgh Pirates gold
)

label_df <- elo_weekly %>%
  group_by(team) %>% slice_max(order_by = week, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  filter(team %in% names(NL_Central))  # 只標想強調的隊

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

label_df <- elo_weekly %>%
  group_by(team) %>% slice_max(order_by = week, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  filter(team %in% names(AL_West))  # 只標想強調的隊

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


# --- NL East
NL_East <- c(
  "費城人" = "#E81828",    # Philadelphia Phillies red
  "大都會" = "#002D72",    # New York Mets blue
  "馬林魚" = "#00A3E0",    # Miami Marlins blue
  "勇士" = "#CE1141",      # Atlanta Braves red
  "國民" = "#AB0003"       # Washington Nationals red
)

label_df <- elo_weekly %>%
  group_by(team) %>% slice_max(order_by = week, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  filter(team %in% names(NL_East))  # 只標想強調的隊

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

# --- NL Central
NL_Central <- c(
  "小熊" = "#0E3386",      # Chicago Cubs blue
  "釀酒人" = "#12284B",    # Milwaukee Brewers navy
  "紅雀" = "#C41E3A",      # St. Louis Cardinals red
  "紅人" = "#C6011F",      # Cincinnati Reds red
  "海盜" = "#FDB827"       # Pittsburgh Pirates gold
)

label_df <- elo_weekly %>%
  group_by(team) %>% slice_max(order_by = week, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  filter(team %in% names(NL_Central))  # 只標想強調的隊

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


##### 這邊是 整理 beta regression 所需變數 #####

# 這邊是需要讓程式跑的
# elo_final2021 <- run_one_season(dat2021, df2021)%>%
#   mutate(season = 2021)
# elo_final2022 <- run_one_season(dat2022, df2022)%>%
#   mutate(season = 2022)
# elo_final2023 <- run_one_season(dat2023, df2023)%>%
#   mutate(season = 2023)
# elo_final2024 <- run_one_season(dat2024, df2024)%>%
#   mutate(season = 2024)
# elo_final2025 <- run_one_season(dat2025, df2025)%>%
#   mutate(season = 2025)

# 這邊也可以直接用我們跑好的資料
elo_final2021 <- read_excel("C:/Users/hungh/OneDrive/桌面/elo_final2021.xlsx")%>%
  mutate(season = 2021)
elo_final2022 <- read_excel("C:/Users/hungh/OneDrive/桌面/elo_final2022.xlsx")%>%
  mutate(season = 2022)
elo_final2023 <- read_excel("C:/Users/hungh/OneDrive/桌面/elo_final2023.xlsx")%>%
  mutate(season = 2023)
elo_final2024 <- read_excel("C:/Users/hungh/OneDrive/桌面/elo_final2024.xlsx")%>%
  mutate(season = 2024)
elo_final2025 <- read_excel("C:/Users/hungh/OneDrive/桌面/elo_final2025.xlsx")%>%
  mutate(season = 2025)


# 真實最終勝率（整季）

compute_final_win <- function(df, season) {
  df %>%
    mutate(
      score_margin = if_else(狀態 == "勝", 大 - 小, 小 - 大)
    ) %>%
    group_by(隊伍) %>%
    summarise(
      wins  = sum(score_margin > 0, na.rm = TRUE),
      loss  = sum(score_margin < 0, na.rm = TRUE),
      games = n(),
      final_winp = ifelse(games > 0, wins / games, NA_real_),
      .groups = "drop"
    ) %>%
    transmute(
      season = season,
      team   = 隊伍,
      final_winp
    )
}

final2021 <- compute_final_win(df2021, 2021)
final2022 <- compute_final_win(df2022, 2022)
final2023 <- compute_final_win(df2023, 2023)
final2024 <- compute_final_win(df2024, 2024)
final2025 <- compute_final_win(df2025, 2025)

final_all <- bind_rows(final2021, final2022, final2023, final2024, final2025)


# 建立總 panel（所有季 × 所有 cut_date）

panel_all <- bind_rows(
  elo_final2021,
  elo_final2022,
  elo_final2023,
  elo_final2024,
  elo_final2025
) %>%
  mutate(team = as.character(team)) %>%
  left_join(final_all, by = c("season", "team"))



##### 這邊是 各月份準確率(切分訓練 驗證 測試集，使用) #####
################################## 11/17 #######################################

# path <- "C:/Users/USER/Desktop/2025_colley1.csv"
# colley_2025 <- read_csv(path)
# 
# colley_2025$cut_date <- as.Date(colley_2025$cut_date)
# elo_final$cut_date <- as.Date(elo_final$cut_date)

elo_final2021 <- colley_2021 %>%
  left_join(elo_final2021, by = c("team" = "team", "cut_date" = "cut_date"))

elo_finnaall2021 <- elo_final2021 %>% select("cut_date", "team", "total_games", "total_wins",
                                             "winp", "rating", "elo", "movelo", "luck_wins", "unluck_lose",
                                             "luck_winp", "unluck_losep", "home_games", "home_percent",
                                             everything()) 

elo_final2022 <- colley_2022 %>%
  left_join(elo_final2022, by = c("team" = "team", "cut_date" = "cut_date"))

elo_finnaall2022 <- elo_final2022 %>% select("cut_date", "team", "total_games", "total_wins",
                                             "winp", "rating", "elo", "movelo", "luck_wins", "unluck_lose",
                                             "luck_winp", "unluck_losep", "home_games", "home_percent",
                                             everything()) 

elo_final2023 <- colley_2023 %>%
  left_join(elo_final2023, by = c("team" = "team", "cut_date" = "cut_date"))

elo_finnaall2023 <- elo_final2023 %>% select("cut_date", "team", "total_games", "total_wins",
                                             "winp", "rating", "elo", "movelo", "luck_wins", "unluck_lose",
                                             "luck_winp", "unluck_losep", "home_games", "home_percent",
                                             everything()) 

elo_final2024 <- colley_2024 %>%
  left_join(elo_final2024, by = c("team" = "team", "cut_date" = "cut_date"))

elo_finnaall2024 <- elo_final2024 %>% select("cut_date", "team", "total_games", "total_wins",
                                             "winp", "rating", "elo", "movelo", "luck_wins", "unluck_lose",
                                             "luck_winp", "unluck_losep", "home_games", "home_percent",
                                             everything()) 

elo_final2025 <- colley_2025 %>%
  left_join(elo_final2025, by = c("team" = "team", "cut_date" = "cut_date"))

elo_finnaall2025 <- elo_final2025 %>% select("cut_date", "team", "total_games", "total_wins",
                                         "winp", "rating", "elo", "movelo", "luck_wins", "unluck_lose",
                                         "luck_winp", "unluck_losep", "home_games", "home_percent",
                                         everything()) 



# 讓模型自動考慮變數
aux_vars <- c("luck_winp", "unluck_losep", "home_percent")


generate_formulas_for_method <- function(core_var, aux_vars) {
  k <- length(aux_vars)
  
  # 所有子集（包含空集合）
  subsets <- map(0:k, ~ combn(aux_vars, .x, simplify = FALSE)) %>%
    unlist(recursive = FALSE)
  
  # 把每個子集變成一個 formula：final_winp ~ core (+ subset)
  formulas <- map(subsets, function(S) {
    if (length(S) == 0) {
      as.formula(paste("final_winp ~", core_var))
    } else {
      rhs <- paste(c(core_var, S), collapse = " + ")
      as.formula(paste("final_winp ~", rhs))
    }
  })
}


find_best_model_LOSO <- function(cutoff_date,
                                 panel,
                                 method,           # "elo", "winp", "movelo", "rating"
                                 aux_vars,
                                 seasons_cv = 2021:2024) {
  
  method <- as.character(method)
  
  if (method == "elo") {
    core_var <- "elo"
  } else if (method == "winp") {
    core_var <- "winp"
  } else if (method == "movelo") {
    core_var <- "movelo"
  } else if (method == "rating") {
    core_var <- "rating"
  } else {
    stop("unknown method: ", method)
  }
  
  model_space <- generate_formulas_for_method(core_var, aux_vars)
  
  results <- purrr::map_dfr(model_space, function(formula) {
    
    cv_each <- purrr::map_dfr(seasons_cv, function(val_season) {
      
      train_seasons <- setdiff(seasons_cv, val_season)
      
      train_dat <- panel %>%
        dplyr::filter(season %in% train_seasons, cut_date == cutoff_date)
      
      test_dat <- panel %>%
        dplyr::filter(season == val_season, cut_date == cutoff_date)
      
      # 取得這個公式用到的變數
      vars_in_model <- all.vars(formula)
      rhs_vars <- setdiff(vars_in_model, "final_winp")
      
      # 把 NA 去掉（只看這個模型有用到的變數）
      train_dat <- train_dat %>%
        dplyr::filter(!if_any(all_of(c("final_winp", rhs_vars)), is.na))
      
      test_dat <- test_dat %>%
        dplyr::filter(!if_any(all_of(c("final_winp", rhs_vars)), is.na))
      
      if (nrow(train_dat) < 10 || nrow(test_dat) < 5) {
        return(tibble::tibble(
          formula      = deparse(formula),
          val_season   = val_season,
          spearman_rho = NA_real_
        ))
      }
      
      fit <- try(betareg::betareg(formula, data = train_dat), silent = TRUE)
      if (inherits(fit, "try-error")) {
        return(tibble::tibble(
          formula      = deparse(formula),
          val_season   = val_season,
          spearman_rho = NA_real_
        ))
      }
      
      test_pred <- test_dat %>%
        dplyr::mutate(pred = predict(fit, newdata = ., type = "response")) %>%
        dplyr::arrange(dplyr::desc(final_winp)) %>%
        dplyr::mutate(true_rank = dplyr::row_number()) %>%
        dplyr::arrange(dplyr::desc(pred)) %>%
        dplyr::mutate(pred_rank = dplyr::row_number())
      
      rho <- suppressWarnings(
        cor(test_pred$pred_rank, test_pred$true_rank, method = "spearman")
      )
      
      tibble::tibble(
        formula      = deparse(formula),
        val_season   = val_season,
        spearman_rho = rho
      )
    })
    
    cv_each
  })
  
  # 對每個 formula 在 4 季上取 Spearman 平均，選最高
  best <- results %>%
    dplyr::group_by(formula) %>%
    dplyr::summarise(
      mean_rho = mean(spearman_rho, na.rm = TRUE),
      .groups  = "drop"
    ) %>%
    dplyr::arrange(dplyr::desc(mean_rho)) %>%
    dplyr::slice(1)
  
  best
}


best_model_table <- tidyr::expand_grid(
  cutoff_date = month_end_cuts,
  method      = c("winp", "elo", "movelo", "rating")
) %>%
  dplyr::mutate(
    best = purrr::map2(
      cutoff_date, method,
      ~ find_best_model_LOSO(
        cutoff_date = .x,
        panel       = panel_all,
        method      = .y,
        aux_vars    = aux_vars
      )
    )
  ) %>%
  tidyr::unnest(best)

best_model_table



predict_2025_with_model <- function(cutoff_date, model_formula, method) {
  
  # 訓練集：2021–2024
  train_dat <- panel_all %>%
    dplyr::filter(season %in% 2021:2024,
                  cut_date == cutoff_date)
  
  # 測試集：2025
  test_dat <- panel_all %>%
    dplyr::filter(season == 2025,
                  cut_date == cutoff_date)
  
  # 這個模型實際用到的變數
  fml <- stats::as.formula(model_formula)
  vars_in_model <- all.vars(fml)
  rhs_vars <- setdiff(vars_in_model, "final_winp")
  
  # 去掉 NA
  train_dat <- train_dat %>%
    dplyr::filter(!if_any(all_of(c("final_winp", rhs_vars)), is.na))
  
  test_dat <- test_dat %>%
    dplyr::filter(!if_any(all_of(c("final_winp", rhs_vars)), is.na))
  
  if (nrow(train_dat) < 10 || nrow(test_dat) < 5) {
    return(tibble::tibble(
      spearman_2025        = NA_real_,
      top12_coverage_2025  = NA_real_
    ))
  }
  
  fit <- try(betareg::betareg(fml, data = train_dat), silent = TRUE)
  if (inherits(fit, "try-error")) {
    return(tibble::tibble(
      spearman_2025        = NA_real_,
      top12_coverage_2025  = NA_real_
    ))
  }
  
  pred <- test_dat %>%
    dplyr::mutate(pred_winp = predict(fit, newdata = ., type = "response")) %>%
    dplyr::arrange(dplyr::desc(final_winp)) %>%
    dplyr::mutate(true_rank = dplyr::row_number()) %>%
    dplyr::arrange(dplyr::desc(pred_winp)) %>%
    dplyr::mutate(pred_rank = dplyr::row_number())
  
  # Spearman（預測排名 vs 真實排名）
  rho <- suppressWarnings(
    cor(pred$pred_rank, pred$true_rank, method = "spearman")
  )
  
  # Top12 覆蓋率
  K <- min(12L, nrow(pred))
  true_topK <- pred %>% dplyr::arrange(true_rank) %>% dplyr::slice_head(n = K) %>% dplyr::pull(team)
  pred_topK <- pred %>% dplyr::arrange(pred_rank) %>% dplyr::slice_head(n = K) %>% dplyr::pull(team)
  cover <- length(intersect(true_topK, pred_topK)) / K
  
  tibble::tibble(
    spearman_2025        = rho,
    top12_coverage_2025  = cover
  )
}



final_2025_results <- best_model_table %>%
  mutate(
    eval2025 = purrr::pmap(
      list(cutoff_date, formula, method),
      ~ predict_2025_with_model(..1, ..2, ..3)
    )
  ) %>%
  tidyr::unnest(eval2025)

final_2025_results



plot_dat <- final_2025_results %>%
  mutate(
    # x 軸標籤
    month_lab = format(cutoff_date, "%m/%d"),
    # 把方法換成比較好看的標籤
    method_f = factor(
      method,
      levels = c("winp", "elo", "movelo", "rating"),
      labels = c("當下勝率", "Elo", "MOV Elo", "Colley")
    )
  )


ggplot(plot_dat,
       aes(x = month_lab, y = spearman_2025,
           group = method_f, color = method_f)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  labs(
    title = "2025 年：不同 cutoff date、不同方法的排名相關（Spearman ρ）",
    x = "cutoff date（2025）",
    y = "Spearman ρ（預測排名 vs 最終排名）",
    color = "方法"
  ) +
  ylim(0, 1) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "bottom"
  )


ggplot(plot_dat,
       aes(x = month_lab, y = top12_coverage_2025,
           group = method_f, color = method_f)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  labs(
    title = "2025 年：不同 cutoff date、不同方法的前 12 名覆蓋率",
    x = "cutoff date（2025）",
    y = "前 12 名覆蓋率（預測前 12 vs 真實前 12）",
    color = "方法"
  ) +
  ylim(0, 1) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "bottom"
  )
