library(dplyr)
library(tidyr)
library(readr)
library(lubridate)
library(gridExtra)
library(reshape2)
library(tidyverse)
library(ggplot2)
library(grid)


data.path <- './Global_YouTube_Statistics.csv'
youtube_data <- read.csv(data.path)

# Data structure
str(youtube_data)

# Summary of the data
summary(youtube_data)

# View the first few rows
head(youtube_data)

custom_youtube_theme <- function() {
  theme_minimal(base_size = 12) + 
    theme(
      plot.title = element_text(color = "#3366CC", face = "bold", size = 16),
      axis.title = element_text(color = "#666666"),
      axis.text = element_text(color = "#333333"),
      panel.grid.major = element_line(color = "#DDDDDD"),
      panel.grid.minor = element_blank(),
      strip.text.x = element_text(size = 12, colour = "#444444", face = "italic"),
      plot.margin = margin(20, 40, 20, 40)
    )
}

# 1. 转换字符列
youtube_data$Youtuber <- as.character(youtube_data$Youtuber)
youtube_data$Title <- as.character(youtube_data$Title)

# 2. 转换因子列
youtube_data$Country <- as.factor(youtube_data$Country)
youtube_data$channel_type <- as.factor(youtube_data$channel_type)

# 3. 计算subscribers列的平均值，排除非有限值
mean_value <- mean(youtube_data$subscribers[is.finite(youtube_data$subscribers)], na.rm = TRUE)

# 4. 替换subscribers列中的非有限值为平均值
youtube_data$subscribers[!is.finite(youtube_data$subscribers)] <- mean_value

# 5. 将月份名转换为数值
month_lookup <- c(Jan=1, Feb=2, Mar=3, Apr=4, May=5, Jun=6, Jul=7, Aug=8, Sep=9, Oct=10, Nov=11, Dec=12)
youtube_data$created_month_num <- as.integer(month_lookup[youtube_data$created_month])

# 6. 根据'created_year'、'created_month_num'和'created_date'创建'combined_created_date'列
youtube_data$combined_created_date <- with(youtube_data, 
                                           ifelse(!is.na(created_year) & !is.na(created_month_num) & !is.na(created_date),
                                                  as.Date(paste(created_year, created_month_num, created_date, sep="-"), format="%Y-%m-%d"), 
                                                  NA))

# 7. 替换"nan"和其他不适合的值为NA
youtube_data[youtube_data == "nan"] <- NA
youtube_data[youtube_data == "NaN"] <- NA

# 8. 基于提供的数据概要和结构，为确保数据质量，可能需要执行一些额外的清洗步骤

# 替换video.views为0的记录的值为该列的均值
views_mean <- mean(youtube_data$video.views[youtube_data$video.views > 0], na.rm = TRUE)
youtube_data$video.views[youtube_data$video.views == 0] <- views_mean

# 替换uploads为0的记录的值为该列的均值
uploads_mean <- mean(youtube_data$uploads[youtube_data$uploads > 0], na.rm = TRUE)
youtube_data$uploads[youtube_data$uploads == 0] <- uploads_mean

# Step 2: Analyze the number of NA for each variable

na_counts <- apply(is.na(youtube_data), 2, sum)
cat("Total missing data for each column:\n")
print(na_counts)
# 
# Step 3: Segmenting data based on video views

# Determine median views for segmentation
median_views <- median(youtube_data$video_views, na.rm = TRUE)

# Segment the data based on median views
high_views <- youtube_data[youtube_data$video_views > median_views, ]
low_views <- youtube_data[youtube_data$video_views <= median_views & !is.na(youtube_data$video_views), ]

# Analyze missing data in each segment
na_high_views <- apply(is.na(high_views), 2, sum)
na_low_views <- apply(is.na(low_views), 2, sum)

cat("Missing data for channels with high video views:\n")
print(na_high_views)

cat("Missing data for channels with low video views:\n")
print(na_low_views)


# # 使用ggplot来绘制数据分析
# p1 <- ggplot(data = youtube_data, mapping = aes(x = subscribers)) +
#   geom_histogram(aes(y = after_stat(density)), bins = 50, fill = "#99CCFF", color = "#3366CC") +
#   geom_density(color = "#3366CC") +
#   scale_x_continuous(trans = 'log10') +
#   ggtitle("YouTube Subscribers (Log Scale)") + 
#   custom_youtube_theme()
# 
# p2 <- ggplot(data = youtube_data, mapping = aes(x = subscribers)) +
#   geom_histogram(aes(y = after_stat(density)), bins = 100, fill = "#99CCFF", color = "#3366CC") +
#   geom_density(color = "#3366CC") +
#   coord_cartesian(xlim = c(12300000, 50000000)) +
#   ggtitle("YouTube Subscribers (12.3M to 50M)") + 
#   custom_youtube_theme()
# 
# # 绘制使用对数尺度的视频观看数图形
# p3 <- ggplot(data = youtube_data, mapping = aes(x = video.views)) +
#   geom_histogram(aes(y = after_stat(density)), bins = 50, fill = "#99CCFF", color = "#3366CC") +
#   geom_density(color = "#3366CC") +
#   scale_x_continuous(trans = 'log10', labels = scales::comma) +
#   ggtitle("YouTube Video Views (Log Scale)") +
#   custom_youtube_theme()
# 
# # 绘制专注于某一特定范围的视频观看数图形。此处我们可以选取Q1到Q3范围
# p4 <- ggplot(data = youtube_data, mapping = aes(x = video.views)) +
#   geom_histogram(aes(y = after_stat(density)), bins = 100, fill = "#99CCFF", color = "#3366CC") +
#   geom_density(color = "#3366CC") +
#   coord_cartesian(xlim = c(4.288e+09, 1.355e+10)) +
#   ggtitle("YouTube Video Views (4.288B to 13.55B)") +
#   custom_youtube_theme()
# 

# p5 <- ggplot(youtube_data, aes(x=subscribers, y=video.views)) +
#   geom_point(aes(color=channel_type), alpha=0.5) +
#   geom_smooth(method=lm) +
#   labs(title="Relation between Subscribers and Video Views",
#        x="Subscribers", y="Video Views", color="Channel Type") +
#   custom_youtube_theme()
# 
# print(p5)
# cor.test(youtube_data$subscribers, youtube_data$video.views, method="pearson")

# Pearson's product-moment correlation
# 
# data:  youtube_data$subscribers and youtube_data$video.views
# t = 38.018, df = 993, p-value < 2.2e-16
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#  0.7433282 0.7940631
# sample estimates:
#       cor
# 0.7699095

# grid.arrange(p1, p2, p3, p4, ncol = 2)


# 绘制每个国家的YouTuber数量
p6 <- ggplot(data = youtube_data, mapping = aes(x = Country)) +
  geom_bar(fill = "#99CCFF", color = "#3366CC") +
  ggtitle("Number of YouTubers per Country") + 
  custom_youtube_theme()

# print(p6)

# 计算每个国家的平均订阅者数量
Country_avg_subscribers <- youtube_data %>% 
  group_by(Country) %>%
  summarise(avg_subscribers = mean(subscribers))

# 绘制每个国家的平均订阅者数量
p7 <- ggplot(data = Country_avg_subscribers, aes(x = Country, y = avg_subscribers)) +
  geom_bar(stat="identity", fill = "#99CCFF", color = "#3366CC") +
  ggtitle("Average Subscribers per Country") +
  custom_youtube_theme()


print(p7)





