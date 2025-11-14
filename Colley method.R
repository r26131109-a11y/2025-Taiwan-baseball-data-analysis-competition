library(readxl)
library(ggrepel)
library(readr) 
library(dplyr)
library(ggplot2)
library(lubridate)
library(stringr)
library(tidyr)

path <- "D:/User/Downloads/MLB_2024.xlsx"    #根據檔案位置

raw <- read_excel(path, sheet = 1)

#資料處理
df <- raw %>%
    filter(狀態 %in% c("勝","敗")) %>%
    mutate(order_id = row_number(),
           date_mmdd = stringr::str_extract(日期, "\\d{1,2}/\\d{1,2}"),
           date_key  = as.Date(paste0("2024/", date_mmdd), format = "%Y/%m/%d"))

df <- df %>%
    mutate(
      date_mmdd = str_extract(日期, "\\d{2}/\\d{2}"),
      # 用虛擬年份做可比較日期；年份不影響相對順序
        date_key  = as.Date(paste0("2024/", date_mmdd), format = "%Y/%m/%d")
    )
  
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
  
out <- df_keep %>%
    transmute(
      date_raw = 日期,
      date_mmdd,
      date_key,
      order_id,
      winner = if_else(狀態 == "勝", t1, t2),
      loser  = if_else(狀態 == "勝", t2, t1)
    ) %>%
    arrange(date_key, order_id) %>%
    mutate(seq_id = row_number()) %>%
    select(seq_id, date_mmdd, date_raw, winner, loser)
  

# 輸出整理資料CSV
readr::write_excel_csv(out, "2024_colley_input_sorted.csv")
  


dat <- read_csv("2024_colley_input_sorted.csv", show_col_types = FALSE) %>%
  mutate(
    winner   = str_squish(winner),
    loser    = str_squish(loser),
    date_key = as.Date(paste0("2024", date_mmdd), format="%Y/%m/%d")
  ) %>%
  arrange(date_key, seq_id)

teams <- sort(unique(c(dat$winner, dat$loser)))
n <- length(teams)
idx <- setNames(seq_len(n), teams)


# 計算Colley
C <- diag(2, n)    
b <- rep(1, n)    
days <- sort(unique(dat$date_key))

daily_true <- vector("list", length(days))

for (k in seq_along(days)) {
  d <- days[k]
  day_games <- dat %>% filter(date_key == d)
  
  for (i in seq_len(nrow(day_games))) {
    wi <- idx[[ day_games$winner[i] ]]
    li <- idx[[ day_games$loser[i]  ]]
    C[wi, wi] <- C[wi, wi] + 1
    C[li, li] <- C[li, li] + 1
    C[wi, li] <- C[wi, li] - 1
    C[li, wi] <- C[li, wi] - 1
    b[wi] <- b[wi] + 0.5
    b[li] <- b[li] - 0.5
  }
  
  r_true <- as.numeric(solve(C, b))
  daily_true[[k]] <- tibble(date=d, date_mmdd=format(d, "%m/%d"),
                            team=teams, rating=r_true)
}

ratings_daily_true <- bind_rows(daily_true) %>% arrange(date, team)

first_day <- min(ratings_daily_true$date)
chk <- ratings_daily_true %>%
  filter(date == first_day) %>%
  mutate(delta = abs(rating - 0.5)) %>%
  arrange(desc(delta))
print(head(chk, 10))  



# 匯出每日rating CSV
readr::write_excel_csv(ratings_daily_true, "2024_colley_daily_true_long.csv")
readr::write_excel_csv(
  ratings_daily_true %>% select(date, team, rating) %>% 
    pivot_wider(names_from=team, values_from=rating) %>% arrange(date),
  "2024_colley_daily_true_wide.csv"
)

if (!exists("ratings_daily_true")) {
  ratings_daily_true <- readr::read_csv("2024_colley_daily_true_long.csv",
                                        show_col_types = FALSE)
}

cut_dates <- as.Date(c(
  "2024-04-05", "2024-04-10", "2024-04-15", "2024-04-20", "2024-04-25", "2024-04-30",
  "2024-05-05", "2024-05-10", "2024-05-15", "2024-05-20", "2024-05-25", "2024-05-30",
  "2024-06-05", "2024-06-10", "2024-06-15", "2024-06-20", "2024-06-25", "2024-06-30",
  "2024-07-05", "2024-07-10", "2024-07-15", "2024-07-20", "2024-07-25", "2024-07-30",
  "2024-08-05", "2024-08-10", "2024-08-15", "2024-08-20", "2024-08-25", "2024-08-30", 
  "2024-09-05", "2024-09-10", "2024-09-15", "2024-09-20", "2024-09-25", "2024-09-30"
))

# 對每個 cut_date 取「<= 該日的最後一天」
avail_days <- sort(unique(ratings_daily_true$date))
pick_day <- function(cd){
  j <- findInterval(cd, avail_days)
  if (j == 0) NA_Date_ else avail_days[j]
}

lst <- lapply(cut_dates, function(cd){
  sd <- pick_day(cd)
  if (is.na(sd)) return(NULL)
  ratings_daily_true %>%
    filter(date == sd) %>%
    transmute(cut_date = cd, team, rating)
})
ratings_at_cut <- bind_rows(lst) %>% arrange(cut_date, team)

# 匯出 CSV
readr::write_excel_csv(ratings_at_cut, "2024_colley_at_cut_dates_long.csv")

ratings_at_cut_wide <- ratings_at_cut %>%
  pivot_wider(names_from = team, values_from = rating) %>%
  arrange(cut_date)
readr::write_excel_csv(ratings_at_cut_wide, "2024_colley_at_cut_dates_wide.csv")

#   視覺圖

highlight_teams <- c("道奇","教士","巨人","響尾蛇","落磯")

team_colors <- c(
  "道奇"   = "#005A9C",
  "教士"   = "#2F241D",
  "巨人"   = "#FD5A1E",
  "響尾蛇" = "#A71930",
  "落磯"   = "#33006F"
)

# 每隊最後一個節點當標籤位置
lab_df <- ratings_at_cut %>%
  filter(team %in% highlight_teams) %>%
  group_by(team) %>%
  filter(cut_date == max(cut_date)) %>%
  ungroup()

p_hi_cut <- ggplot() +
  geom_line(
    data = ratings_at_cut,
    aes(cut_date, rating, group = team),
    color = "grey70",
    alpha = 0.25
  ) +
  geom_point(
    data = ratings_at_cut,
    aes(cut_date, rating, group = team),
    color = "grey80",
    alpha = 0.25,
    size  = 0.7
  ) +
  geom_line(
    data = filter(ratings_at_cut, team %in% highlight_teams),
    aes(cut_date, rating, color = team, group = team),
    linewidth = 1
  ) +
  geom_point(
    data = filter(ratings_at_cut, team %in% highlight_teams),
    aes(cut_date, rating, color = team, group = team),
    size = 1.5
  ) +
  geom_hline(yintercept = 0.5, linetype = 2) +
  geom_text_repel(
    data    = lab_df,
    aes(x = cut_date, y = rating, label = team, color = team),
    nudge_x = 10,
    direction = "y",
    hjust   = 0,
    segment.alpha = 0.6,
    min.segment.length = 0,
    size    = 3
  ) +
  scale_color_manual(
    values = team_colors,
    breaks = highlight_teams,
    name   = "Team"
  ) +
  labs(
    title = "Colley Rating：國聯西區（每個禮拜結算一次）",
    x     = "Cut date",
    y     = "Rating"
  ) +
  theme_minimal()

print(p_hi_cut)


if (!exists("ratings_daily_true")) {
  ratings_daily_true <- readr::read_csv("2024_colley_daily_true_long.csv",
                                        show_col_types = FALSE)
}

# === 讀逐場清單（算一般勝率用） ===
dat <- readr::read_csv("2024_colley_input_sorted.csv", show_col_types = FALSE) %>%
  mutate(
    winner   = str_squish(winner),
    loser    = str_squish(loser),
    date     = as.Date(paste0("2024/", date_mmdd), "%Y/%m/%d")
  ) %>%
  arrange(date, seq_id)

# === 選擇球隊 ===
one_team <- "守護者"     # 換成你要看的球隊

# 逐日「一般勝率」 ===


# 當日該隊的勝場/敗場
team_daily <- dat %>%
  transmute(date,
            w = as.integer(winner == one_team),
            l = as.integer(loser  == one_team)) %>%
  group_by(date) %>%
  summarise(w = sum(w), l = sum(l), .groups = "drop")

# 算累積勝敗與勝率
all_days <- ratings_daily_true %>%
  distinct(date) %>% arrange(date) %>% pull(date)

wp_daily <- tibble(date = all_days) %>%
  left_join(team_daily, by = "date") %>%
  replace_na(list(w = 0L, l = 0L)) %>%
  mutate(
    W_cum = cumsum(w),
    L_cum = cumsum(l),
    gp    = W_cum + L_cum,
    winpct = ifelse(gp > 0, W_cum / gp, 0.5)  # 開季前設為0.5
  ) %>%
  select(date, winpct)

# 取該隊的每日 Colley rating
colley_daily <- ratings_daily_true %>%
  filter(team == one_team) %>%
  select(date, colley = rating)

# 輸出
compare_df <- colley_daily %>%
  left_join(wp_daily, by = "date") %>%
  arrange(date)

readr::write_excel_csv(compare_df,
                       paste0("compare_winpct_colley_", one_team, ".csv"))

# 視覺化（單一球隊兩條線） 
compare_long <- compare_df %>%
  pivot_longer(c(winpct, colley), names_to = "series", values_to = "value") %>%
  mutate(series = factor(series,
                         levels = c("winpct", "colley"),
                         labels = c("winning probability", "Colley rating")))

# 2) 繪圖
ggplot(compare_long, aes(x = date, y = value,
                         color = series, linetype = series)) +
  geom_line(linewidth = 1) +
  geom_hline(yintercept = 0.5, linetype = 3) +
  scale_color_manual(values = c("winning probability" = "blue",
                                "Colley rating" = "red")) +
  scale_linetype_manual(values = c("winning probability" = "solid",
                                   "Colley rating" = "dashed")) +
  labs(title = paste0(one_team, "：winning probability vs Colley（每日）"),
       x = "Date", y = "Value", color = "指標", linetype = "指標") +
  theme_minimal() +
  theme(
    legend.position        = "inside",
    legend.position.inside = c(1, 1),
    legend.justification   = c("right", "top"),
    legend.background      = element_rect(fill = "white", color = "grey80")
  )

