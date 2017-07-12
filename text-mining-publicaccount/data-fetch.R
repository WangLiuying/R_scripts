#test
library(rvest)
#随便取一页加精帖子做一下实验
url <- "http://bbs.tianya.cn/list.jsp?item=funinfo&grade=1&order=1"
page <- read_html(url)
page %>% html_table()

#取出该页的话题，评论数点击数，链接
topics <- page %>% html_table()
links <- page%>%
  html_nodes(xpath = "//body/div/div/div/div/div/table/tbody/tr/td[1]/a")%>%
  html_attr("href")
for (link in links)
{

}

#需用页面跳转取内容，此处随意试验一页
url_link <- paste("http://bbs.tianya.cn",link,sep="")
page_link <- read_html(url_link)
author <- page_link%>%html_node(xpath="//body/div/div/div/div/div/div/span/a")%>%
  html_text()
body <- page_link%>%html_nodes(xpath='//div[@class="bbs-content"]')%>%
  html_text()
# page_link <- GET(url_link,
#                  add_headers(Accept="text/html;q=0.9,*/*;q=0.8",
#                    "Accept-Encoding" = "gzip, deflate",
#                    "Accept-Language"="zh-CN,zh;q=0.8,en-US;q=0.5,en;q=0.3",
#                    "Connection"="keep-alive",
#                    Referer="http://bbs.tianya.cn/list.jsp?item=funinfo&grade=1&order=1"))
