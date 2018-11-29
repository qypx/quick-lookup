library(XML)
library(leafletCN)
# 读取网页的表格
# Sorry for 爬了你家网站
table = readHTMLTable("http://www.pm25.in/rank",  
                      encoding = "UTF-8", stringsAsFactors = F)[[1]]

# 整理数据并命名
dat = table[ , 2:3]
names(dat) = c("city","AQI")
dat$AQI = as.numeric(dat$AQI)

# 调用geojsonMap进行绘制
geojsonMap(dat, "city",
           popup =  paste0(dat$city, ":", dat$AQI),
           palette = "Reds", legendTitle = "AQI")