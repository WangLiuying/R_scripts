#爬取公众号文章
library(rvest)
library(stringr)

account.list <-
  c("realmovie520","shenyebagua818", "gossipmaker", "yansubagua")
df <- data.frame()

#################################################################
#设置:统一从2月中旬开始吧
account <- "yansubagua"
  page <- seq(708,800,by=12)
  #record：严肃八卦180,192,204,216,228,240,252,264,276,288,300,312,324,336,348,360-444;-780
  #realmovie:180,192,204,216,240,252,264,276,288,300,312,324,336,348,360,372,384
  (url <-
    paste("http://chuansong.me/account/",
          account,
          "?start=",
          page,
          sep = ""))
##################################################################  
  for (i in seq_along(url))
  {
    url_page <- read_html(url[i])
    links <- url_page %>% html_nodes(".question_link") %>%
      html_attr("href")
    cat("page",url[i],"\n")
    Sys.sleep(runif(1,5,15))
    for (link in links)
    {
      link <- paste("http://chuansong.me", link, sep = "")
      link_page <-read_html(link)
      body <- link_page %>% html_nodes("#js_content>*") %>% html_text()
      body <- body[nchar(body) > 3]
      body <- Reduce(f = paste, body)
      title <-
        link_page %>% html_node("#activity-name") %>% html_text() %>%
        str_replace_all(pattern = "[:blank:]|\\n", replacement = "")
      time <- link_page %>% html_node("#post-date") %>% html_text()
      hot <-
        link_page %>% html_nodes(".StatsRow>span>strong") %>% html_text()
      read <- hot[1]
      like <- as.numeric(hot[2])
      original <-
        link_page %>% html_node("#copyright_logo") %>% html_text()
      if(is.null(body)) body=NA
      df <- tryCatch(rbind(df, data.frame(title, time, read, like, original,body)),
                     error=function(e) next)
      cat("获取：", title, "\n")
      Sys.sleep(runif(1,5,20))
      Sys.sleep(sample(x = c(0,0,0,1),size = 1)*20)
      
    }
    if(sample(c(0,0,0,1),1)) {cat("sleep for 10 min\n");Sys.sleep(600)}
    save.image()
  }
save.image()
######################################################################  
save(df, file = paste(account, ".RData"))
View(df)
