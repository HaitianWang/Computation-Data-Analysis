---
title: "Project1_4"
output: html_document
date: "2023-09-03"
---

``` {r}

library(tidyverse) 
library(lubridate)
library(gridExtra)
library(reshape2)
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

# 获取youtube_data中所有数值列
numeric_columns <- names(youtube_data)[sapply(youtube_data, is.numeric)]

non_numeric_columns <- names(youtube_data)[!sapply(youtube_data, is.numeric)]

# 从Bar plot选择项中删除“Youtuber”和“Title”
non_numeric_columns <- setdiff(non_numeric_columns, c("Youtuber", "Title"))


# 遍历每一个数值列
for (column_name in numeric_columns) {
  
  # 计算列的中位数，排除非有限值
  median_value <- median(youtube_data[[column_name]][is.finite(youtube_data[[column_name]])], na.rm = TRUE)
  
  # 替换该列中的非有限值为中位数
  youtube_data[[column_name]][!is.finite(youtube_data[[column_name]])] <- median_value
}


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


# 9. 清洗 highest_monthly_earnings 列中的零或负值
# 使用最小的正值替换零和负值，这样对数转换就不会产生无限值
min_positive_earning <- min(youtube_data$highest_monthly_earnings[youtube_data$highest_monthly_earnings > 0], na.rm = TRUE)
youtube_data$highest_monthly_earnings[youtube_data$highest_monthly_earnings <= 0] <- min_positive_earning


# i. 替换数值列中的NA值
for (column_name in numeric_columns) {
  
  # 判断是否要使用mean还是median
  # 你可以根据自己的需要更改这部分，例如，可能某些列你想要使用median，某些列使用mean
  mean_value <- mean(youtube_data[[column_name]], na.rm = TRUE)
  median_value <- median(youtube_data[[column_name]], na.rm = TRUE)
  
  # 使用mean替换NA
  youtube_data[[column_name]][is.na(youtube_data[[column_name]])] <- mean_value
  
  # 使用median替换NA (如果你决定用median，那么取消下面这行的注释)
  # youtube_data[[column_name]][is.na(youtube_data[[column_name]])] <- median_value
}




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


# 使用ggplot来绘制数据分析
# 分析subscribers变量的分布情况
p1 <- ggplot(data = youtube_data, mapping = aes(x = subscribers)) +
  geom_histogram(aes(y = after_stat(density)), bins = 50, fill = "#99CCFF", color = "#3366CC") +
  geom_density(color = "#3366CC") +
  scale_x_continuous(trans = 'log10') +
  ggtitle("YouTube Subscribers (Log Scale)") +
  custom_youtube_theme()
# 
p2 <- ggplot(data = youtube_data, mapping = aes(x = subscribers)) +
  geom_histogram(aes(y = after_stat(density)), bins = 100, fill = "#99CCFF", color = "#3366CC") +
  geom_density(color = "#3366CC") +
  coord_cartesian(xlim = c(12300000, 50000000)) +
  ggtitle("YouTube Subscribers (12.3M to 50M)") +
  custom_youtube_theme()
# 
# 绘制使用对数尺度的视频观看数图形
p3 <- ggplot(data = youtube_data, mapping = aes(x = video.views)) +
  geom_histogram(aes(y = after_stat(density)), bins = 50, fill = "#99CCFF", color = "#3366CC") +
  geom_density(color = "#3366CC") +
  scale_x_continuous(trans = 'log10', labels = scales::comma) +
  ggtitle("YouTube Video Views (Log Scale)") +
  custom_youtube_theme()
# #
# 绘制专注于某一特定范围的视频观看数图形。此处我们可以选取Q1到Q3范围
p4 <- ggplot(data = youtube_data, mapping = aes(x = video.views)) +
  geom_histogram(aes(y = after_stat(density)), bins = 100, fill = "#99CCFF", color = "#3366CC") +
  geom_density(color = "#3366CC") +
  coord_cartesian(xlim = c(4.288e+09, 1.355e+10)) +
  ggtitle("YouTube Video Views (4.288B to 13.55B)") +
  custom_youtube_theme()
# 
# 
p5 <- ggplot(youtube_data, aes(x=subscribers, y=video.views)) +
  geom_point(aes(color=channel_type), alpha=0.5) +
  geom_smooth(method=lm) +
  labs(title="Relation between Subscribers and Video Views",
       x="Subscribers", y="Video Views", color="Channel Type") +
  custom_youtube_theme()
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


# 1. 绘制每个国家的YouTuber数量
p6 <- ggplot(data = youtube_data, mapping = aes(x = Country)) +
  geom_bar(fill = "#99CCFF", color = "#3366CC") +
  ggtitle("Number of YouTubers per Country") +
  coord_flip()+
  custom_youtube_theme()
# 
# 2. 选择数量较多的国家
top_countries <- youtube_data %>%
  count(Country) %>%
  arrange(-n) %>%
  head(10)

filtered_data <- youtube_data %>%
  filter(Country %in% top_countries$Country)
# 
# 绘制筛选后国家的YouTuber数量
p7 <- ggplot(data = filtered_data, mapping = aes(x = Country)) +
  geom_bar(fill = "#99CCFF", color = "#3366CC") +
  ggtitle("Number of YouTubers in Top 10 Countries") +
  coord_flip()+
  custom_youtube_theme()
# 
# 3. 对这些国家进行更详细的订阅者数分析
gd <- filtered_data %>%
  group_by(Country) %>%
  summarise(average_subscribers = mean(subscribers, na.rm = T))
# 
p8 <- ggplot(data = filtered_data,
             mapping = aes(y = subscribers, x = Country))+
  geom_violin(fill ='lightgray')+
  geom_point(data = gd, aes(y = average_subscribers, shape='mean'), color='darkred', size=4)+
  ggtitle("Distribution of Subscribers in Top 10 Countries") +
  coord_flip()+
  custom_youtube_theme()
# 
# print(p8)

#分析视频观看次数与最高月收入的关系

# 首先分析最高月收入的分布情况
# 1. 直方图 (对数尺度)
p9 <- ggplot(data = youtube_data, mapping = aes(x = highest_monthly_earnings)) +
  geom_histogram(aes(y = after_stat(density)), bins = 50, fill = "#99CCFF", color = "#3366CC") +
  geom_density(color = "#3366CC") +
  scale_x_continuous(trans = 'log10', labels = scales::comma) +
  ggtitle("Histogram of Highest Monthly Earnings (Log Scale)") +
  custom_youtube_theme()
# 
# 2. 箱线图
p10 <- ggplot(data = youtube_data, mapping = aes(y = highest_monthly_earnings)) +
  geom_boxplot(fill = "#99CCFF", color = "#3366CC") +
  ggtitle("Boxplot of Highest Monthly Earnings") +
  custom_youtube_theme()

grid.arrange(p9, p10, ncol = 2)
# 
# 
# 接着分析视频观看次数与最高月收入的关系
# 散点图
p11 <- ggplot(youtube_data, aes(x=video.views, y=highest_monthly_earnings)) +
  geom_point(aes(color=channel_type), alpha=0.5) +
  geom_smooth(method=lm) + # 添加线性回归线
  labs(title="Relation between Video Views and Highest Monthly Earnings",
       x="Video Views", y="Highest Monthly Earnings", color="Channel Type") +
  custom_youtube_theme()
# 
# print(p11)
# 
# # 分析Pearson相关系数
# cor_result <- cor.test(youtube_data$video.views, youtube_data$highest_monthly_earnings, method="pearson")
# print(cor_result)


# 创建年份的分布情况
p12 <- ggplot(data = youtube_data, mapping = aes(x = created_year)) +
  geom_histogram(aes(y = after_stat(density)), binwidth=1, fill = "#99CCFF", color = "#3366CC") +
  geom_density(color = "#3366CC") +
  ggtitle("Distribution of YouTube Channel Creation Year (Density)") +
  custom_youtube_theme()

# 创建年份2010年至2020年之间的分布情况
p13 <- ggplot(data = youtube_data, mapping = aes(x = created_year)) +
  geom_histogram(aes(y = after_stat(density)), binwidth=0.5, fill = "#99CCFF", color = "#3366CC") +
  geom_density(color = "#3366CC") +
  coord_cartesian(xlim = c(2010, 2020)) +
  ggtitle("YouTube Channel Creation Year (2010 to 2020)") +
  custom_youtube_theme()


# Filter data for years between 2010 and 2020
youtube_data_filtered <- youtube_data[youtube_data$created_year >= 2010 & youtube_data$created_year <= 2020,]

# Plotting the relationship between 'created_year' and 'subscribers' using a bar plot
p14 <- ggplot(data = youtube_data_filtered, aes(x = factor(created_year), y = subscribers)) +
  geom_bar(stat = "summary", fun = mean, fill = "#99CCFF", color = "#3366CC", width = 0.7) +
  geom_smooth(aes(group = 1), method = "lm", color = "red", se = FALSE) +
  labs(title = "Relation between Year of Creation and Average Subscribers (2010-2020)",
       x = "Year of Creation", y = "Average Subscribers") +
  custom_youtube_theme()

print(p14)

cor_result <- cor.test(youtube_data_filtered$created_year, youtube_data_filtered$subscribers, method = "pearson")
print(cor_result)

```