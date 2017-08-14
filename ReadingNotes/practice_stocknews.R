# 分析
# 首先有个live-page，json格式响应，列表global,a-stock,....
# 在global的最后有个cursor,cursor即指向异步加载的内容。


#live-page
url <- "https://api-prod.wallstreetcn.com/apiv1/content/lives/pc?limit=20"
next_url <- parse_url("https://api-prod.wallstreetcn.com/apiv1/content/lives?channel=a-stock-channel&client=pc&cursor=1502704059&limit=20")

HEAD(url)
myhandle <- handle(url)
myheader <- add_headers("Host"="api-prod.wallstreetcn.com",
                        "User-Agent"="Mozilla/5.0 (Windows NT 10.0; WOW64; rv:54.0) Gecko/20100101 Firefox/54.0",
                        "Accept"= "application/json, text/plain, */*",
                        "Referer"="Referer: https://wallstreetcn.com/live/a-stock",
                        "Origin"="https://wallstreetcn.com",
                        "Connection"="keep-alive")

# GET /apiv1/content/lives/pc?limit=20 HTTP/1.1
# 
# Host: api-prod.wallstreetcn.com
# 
# User-Agent: Mozilla/5.0 (Windows NT 10.0; WOW64; rv:54.0) Gecko/20100101 Firefox/54.0
# 
# Accept: application/json, text/plain, */*
#   
#   Accept-Language: zh-CN,zh;q=0.8,en-US;q=0.5,en;q=0.3
# 
# Accept-Encoding: gzip, deflate, br
# 
# X-Ivanka-Platform: wscn-platform
# 
# Referer: https://wallstreetcn.com/live/a-stock
# 
# Origin: https://wallstreetcn.com
# 
# Connection: keep-alive
# 
# If-None-Match: 3AikoIUZgoDFl2f0wV4UXw==
#   
#   Cache-Control: max-age=0

response <- GET(url = url,config=myheader,verbose())
response$status_code
myhandle <- response$handle
response$cookies
data<- content(response)
items <- data$data$a_stock$items

cursor <- data$data$a_stock$next_cursor
next_url$query$channel="a-stock-channel"#可以更改
next_url$query$cursor <- cursor
url2 <- build_url(next_url)

response2 <- GET(url=url2,config=myheader,verbose())
data <- content(response)
#接下来只需要再取出新的cursor
