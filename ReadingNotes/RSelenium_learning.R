##RSelenium

library(RSelenium)


# 1.在jar文件所在文件夹运行命令行：
# java -jar selenium-server-standalone-3.5.1.jar
# or
#  docker 中运行
#  docker run -d -p 4445:4444 selenium/standalone-firefox
#  docker ps 查看虚拟机创建情况
#  docker-machine ip 查看ip


# docker ps -q 查看id
# docker stop $(docker ps -q)停止虚拟机运行
################调用R包#########################################
library(rvest)        # 为了read_html函数
library(RSelenium)    # 为了使用JavaScript进行网页抓取

###############连接Server并打开浏览器############################

#打开网页
remDr <- remoteDriver(remoteServerAddr = "192.168.99.100" 
                      , port = 4445L
                      , browserName = "firefox"
)
remDr$open()
remDr$screenshot(display = TRUE)

remDr$navigate("http://shuju.wdzj.com/")
remDr$screenshot(display=T)
#practice time
remDr$getStatus()
remDr$navigate("https://www.baidu.com/") #跳转网页
remDr$screenshot(display=T)
remDr$goBack()#回退
remDr$screenshot(display=T)

remDr$getCurrentUrl()

#练习目标：
# 目标：在网贷之家的数据平台（http://shuju.wdzj.com/）中
# 爬取近7日各个P2P平台的投资人数、人均投资金额、平均收益率和成交量。
# https://zhuanlan.zhihu.com/p/24772389
remDr$navigate("http://shuju.wdzj.com/")

webElem <- remDr$findElement(using="xpath",value="html/body/div[3]/div[2]/div[1]/div[2]/div[2]/div[1]/ul/li[2]")
webElem$clickElement()
remDr$screenshot(display=T)

webElem <- remDr$findElement(using="xpath",value="html/body/div[3]/div[2]/div[2]/div[1]/a")
webElem$clickElement()

webElem <- remDr$findElement(using="xpath",value="html/body/div[3]/div[2]/div[2]/div[2]/div[2]/ul/li[1]")
webElem$clickElement()

webElem <- remDr$findElement(using="xpath",value="html/body/div[3]/div[2]/div[2]/div[2]/div[2]/ul/li[10]")
webElem$clickElement()

webElem <- remDr$findElement(using="xpath",value="html/body/div[3]/div[2]/div[2]/div[2]/div[1]/div[2]/div/button")
webElem$clickElement()
remDr$screenshot(display=T)


webElem <- remDr$findElement(using="xpath",value="html/body/div[3]/div[2]/div[3]/table[2]")
webElem$describeElement()
data <- webElem$getElementAttribute("outerHTML")[[1]]
htmlpage <- read_html(data)
data <- (htmlpage %>% html_table(header = T))[[1]]
str(data)
