library(shiny)
library(shinyWidgets)


youtube_data <- read.csv("Global_YouTube_Statistics.csv")




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
median_views <- median(youtube_data$video.views, na.rm = TRUE)

# Segment the data based on median views
high_views <- youtube_data[youtube_data$video.views > median_views, ]
low_views <- youtube_data[youtube_data$video.views <= median_views & !is.na(youtube_data$video.views), ]

# Analyze missing data in each segment
na_high_views <- apply(is.na(high_views), 2, sum)
na_low_views <- apply(is.na(low_views), 2, sum)

cat("Missing data for channels with high video views:\n")
print(na_high_views)

cat("Missing data for channels with low video views:\n")
print(na_low_views)


# UI部分
ui <- fluidPage(
  titlePanel("YouTube Data Analysis"),
  sidebarLayout(
    
    
    sidebarPanel(
      selectInput("plot_type", "Select plot type:", choices = c("Pairwise", "Barplot")),
      conditionalPanel(
        condition = "input.plot_type == 'Pairwise'",
        pickerInput(
          inputId = "selected_attributes_numeric",
          label = "Select numeric attributes:",
          choices = numeric_columns,
          multiple = TRUE,
          options = list(`actions-box` = TRUE)
        )
      ),
      conditionalPanel(
        condition = "input.plot_type == 'Barplot'",
        pickerInput(
          inputId = "selected_attributes_non_numeric",
          label = "Select non-numeric attributes:",
          choices = non_numeric_columns,
          multiple = FALSE
        )
      )
    ),
    
    
    mainPanel(
      textOutput("select_prompt"),
      plotOutput("plot", height = "400px", width = "800px")
    )
  )
)

# Server部分
server <- function(input, output) {
  
  output$select_prompt <- renderText({
    if (is.null(input$plot_type) || 
        (input$plot_type == "Pairwise" && length(input$selected_attributes_numeric) < 2) || 
        (input$plot_type == "Barplot" && is.null(input$selected_attributes_non_numeric))) {
      return("Please make appropriate selections before proceeding.")
    } else {
      return(NULL)
    }
  })
  
  output$plot <- renderPlot({
    if(input$plot_type == "Pairwise") {
      # 若未选择足够的属性，则显示提示
      if (length(input$selected_attributes_numeric) < 2) {
        #plot(1, 1, ann=FALSE, axes=FALSE)
        #text(1, 1, "Please select at least two attributes", col="red")
        return()
      }
      
      selected_data <- youtube_data[, input$selected_attributes_numeric, drop=FALSE]
      pairs(selected_data)
    } 
    else if (input$plot_type == "Barplot" && !is.null(input$selected_attributes_non_numeric)) {
      selected_column <- input$selected_attributes_non_numeric
      selected_data <- table(youtube_data[[selected_column]])
      barplot(selected_data, main=paste("Barplot of", selected_column), las=2)
    }
  })
}

shinyApp(ui = ui, server = server)



































