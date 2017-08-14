#爬虫学习：

#穿越表单
library(rvest)
library(httr)
help(package="rvest")

urlsession <- "http://open.xmu.edu.cn/Login?returnUrl=http%3A%2F%2Fopen.xmu.edu.cn%2Foauth2%2Fauthorize%3Fclient_id%3D1010%26response_type%3Dcode"
session <- html_session(url=urlsession)


form <- html_form(session)
form <- form[[1]]
username="15420161152156"
password="nor5983290"
form_sub <- set_values(form=form,"UserName"=username,
                       "Password"=password)
form_sub
session.out <- submit_form(session=session,form=form_sub,submit=NULL)

session.out$response

next_link <- follow_link(x=session.out,i="Advanced Microeconomics II (2017)")
filelink <- next_link%>% html_nodes(xpath = "//li[@aria-label='Syllabus and Textbooks']//a[@onclick]")
filelink <- filelink %>% html_attr(name="href")

####################################################


library(httr)
help(package="httr")

url <- parse_url("https://www.zhihu.com/search?type=content&q=%E4%BA%BA%E5%B7%A5%E6%99%BA%E8%83%BD")
url <- "https://www.zhihu.com/search?"
response <- GET(url = url,query=list(type="content",q="人工智能"))
response$url

myheader <- add_headers("Accept"="text/html","Accept-Encoding"="UTF-8",
                        "User-Agent"="Mozilla/5.0 (Windows NT 10.0; WOW64; rv:54.0) Gecko/20100101 Firefox/54.0",
                        "Cookie"=cookies1)
myheader
response <- GET(url=url,query=list(type="content",q="人工智能"),
                config=myheader,verbose())

cookies1 <- 'q_c1=992b1214d8d449f78823aee417962321|1501411084000|1495436995000; d_c0="ADBCVaDrcguPTk6SznBWHkjcRVGrmZaUMWA
=|1489470739"; __utma=51854390.321986107.1502695484.1502695484.1502695484.1; __utmz=51854390.1502695484
.1.1.utmcsr=(direct)|utmccn=(direct)|utmcmd=(none); _zap=908df9bd-768f-44d2-bdcd-a6f1402952db; q_c1=992b1214d8d449f78823aee417962321
|1501411085000|1495436995000; r_cap_id="ZWVmNzU1MmY5ZjQ2NGE5NGEyZjJlYTUzOGQ5NjllNWI=|1502695467|7afd9715df6abfeda8cde2e64208be6af4b3d32a"
; aliyungf_tc=AQAAAPlUyUJmogkAitL6tzNVP89elAqp; _xsrf=da03838611433cc299e902b03a91a1f6; __utmb=51854390
.0.10.1502695484; __utmc=51854390; __utmv=51854390.100--|2=registration_date=20160315=1^3=entry_date
=20160315=1; l_cap_id="OWQ5MTliN2I2ZTg4NDg2M2E1OTBiZjczYjEyZWEyZDg=|1502695576|44d321f4c6944950f5eded98e8bd0bed970aea54"
; z_c0=Mi4xcEx2QkFnQUFBQUFBTUVKVm9PdHlDeGNBQUFCaEFsVk5yZDI0V1FEVE94Rkh5VVlyRnc0RkZ3U2dSMkZIM20xS0lB|1502695597
|85c5fa5026e79d560a5e67fe16e7f17c2fdd6164; unlock_ticket="QUZCQWxZTkNuZ2tYQUFBQVlRSlZUYlZYa1ZuQVNlMFZfZ0VLNkNZQVhnaHhTZDVSYzNEMm13PT0
=|1502695597|8b0cd91456f2b41553d67fe16efe585d4d5a5b46"; cap_id="ZGU2ZmUxNjQxZmQ0NDEyM2JiZDY3MzE5NjUzNDRjZWY
=|1502695597|69100709cabbb01d1fbdf61ec3a619085309142f"; _xsrf=da03838611433cc299e902b03a91a1f6'


# cookies2 <- 'q_c1=992b1214d8d449f78823aee417962321|1501411084000|1495436995000; d_c0="ADBCVaDrcguPTk6SznBWHkjcRVGrmZaUMWA
# =|1489470739"; __utma=51854390.321986107.1502695484.1502695484.1502695484.1; __utmz=51854390.1502695484
# .1.1.utmcsr=(direct)|utmccn=(direct)|utmcmd=(none); _zap=908df9bd-768f-44d2-bdcd-a6f1402952db; q_c1=992b1214d8d449f78823aee417962321
# |1501411085000|1495436995000; r_cap_id="ZWVmNzU1MmY5ZjQ2NGE5NGEyZjJlYTUzOGQ5NjllNWI=|1502695467|7afd9715df6abfeda8cde2e64208be6af4b3d32a"
# ; aliyungf_tc=AQAAAPlUyUJmogkAitL6tzNVP89elAqp; _xsrf=da03838611433cc299e902b03a91a1f6; __utmb=51854390
# .0.10.1502695484; __utmc=51854390; __utmv=51854390.100--|2=registration_date=20160315=1^3=entry_date
# =20160315=1; l_cap_id="OWQ5MTliN2I2ZTg4NDg2M2E1OTBiZjczYjEyZWEyZDg=|1502695576|44d321f4c6944950f5eded98e8bd0bed970aea54"
# ; z_c0=Mi4xcEx2QkFnQUFBQUFBTUVKVm9PdHlDeGNBQUFCaEFsVk5yZDI0V1FEVE94Rkh5VVlyRnc0RkZ3U2dSMkZIM20xS0lB|1502695597
# |85c5fa5026e79d560a5e67fe16e7f17c2fdd6164; cap_id="ZGU2ZmUxNjQxZmQ0NDEyM2JiZDY3MzE5NjUzNDRjZWY=|1502695597
# |69100709cabbb01d1fbdf61ec3a619085309142f"; _xsrf=da03838611433cc299e902b03a91a1f6'

page <- read_html(response$url)
######################################################################################
#POST method

url <- "http://httpbin.org/post"
response <- POST(url = url,body=list(a="a",b="b",c="c"),encode="form",verbose())
cat(read_html(response)%>%html_text())


###163music
url <- "http://music.163.com/weapi/pl/count?csrf_token=007d41607fa131483a0b43dad5a275a9"
myheader <- add_headers("Host"="music.163.com","User-Agent"="Mozilla/5.0 (Windows NT 10.0; WOW64; rv:54.0) Gecko/20100101 Firefox/54.0",
                        "Accept"="*/*","Accept-Encoding"="gzip,deflate",
                        "Content-Type"="application/x-www-form-urlencoded",
                        "Referer"="http://music.163.com/playlist?id=803927975",
                        "Cookie"="usertrack=c+5+hVjIuWC32mG2A5LUAg==; _ntes_nnid=cd40accbe5f1561f911d004ad0a214f2,1489549670773; _ntes_nuid=cd40accbe5f1561f911d004ad0a214f2; _ga=GA1.2.1432615566.1489549672; JSESSIONID-WYYY=0sEANsFk55vqFA%2FDsay7fE%2F%2BvKr83bPS5RuvWsnxpdN%5CmooFZ1aY5XlplHo6ggoagUiBEngkSe0kN9WxzwpBpt42%2BO8OWYH5i0mSskE35pGr6DjnU2%5C%5Cwo1N0I2i0Tz5Di42wkOsrBuEgutsaUfK258sxDw%2BMB2zhfS1%2BNPhWaeK3%5C%2BZ%3A1502702430413; _iuqxldmzr_=32; UM_distinctid=15bad717cad32-05889f0a35ed1f8-1263684a-100200-15bad717caf5b; vjuids=-1b9baba00.15bad71dc24.0.d056e0e1f3ccc8; vjlast=1493263572.1502686449.23; vinfo_n_f_l_n3=acb19e3a765ec8fc.1.2.1493263572194.1493266407094.1493271236894; __oc_uuid=a3249a20-646c-11e7-b6a3-c555cbef892f; __utma=187553192.1432615566.1489549672.1499580343.1499580343.1; __utmz=187553192.1499580343.1.1.utmcsr=baidu|utmccn=(organic)|utmcmd=organic; __s_=1; P_INFO=m13656036646@163.com|1502686803|0|mail163|00&99|fuj&1501591396&mail163#fuj&350100#10#0#0|136646&1||13656036646@163.com; Province=0590; City=0793; NTES_hp_textlink1=old; __gads=ID=e7cc09c45a75de3b:T=1502686455:S=ALNI_MYyZULcqbHip8KlsyGtUgRiyw3oWQ; SID=2fe58868-06fe-4900-ab42-c0adb4d15f74; S_INFO=1502686803|0|3&80##|m13656036646; NTES_SESS=z2LfvU52UowF4yZc1yuhl1Zp7t8H3Kht7Q39X2AmaCxMZ2qhZLps8NKRp77lp3EKxlHAec30eXCiDgCY4rsXQVOLnDoPZecF2spPhIQNIavKfDoWjnLxt7KZDhD7D2MroOKL7Q.zdwDw27hoTvxx8n8CfjukC.z3Ic4TeU7Yz9IfgAMX5qr31ynVSVE3s.erA; __utma=94650624.1432615566.1489549672.1502698892.1502698892.1; __utmb=94650624.12.10.1502698892; __utmc=94650624; __utmz=94650624.1502698892.1.1.utmcsr=(direct)|utmccn=(direct)|utmcmd=(none); __remember_me=true; MUSIC_U=8cac640144073da5bd590ca2d30da431405faa58e2ee11fc2ed44c16dcc93f2068e23d20d898aa4968e575afb3d08f2fde39c620ce8469a8; __csrf=e63a7d0649572bfab09fda687c031bc3",
                        "Connection"="keep-alive")

#POST /weapi/pl/count?csrf_token=007d41607fa131483a0b43dad5a275a9 HTTP/1.1



myheader
mybody <- list("params"="Tv7zXYIFJWtViGXdXViDGXoJ65bMSRUa0M5fKEBudH15+eGo5CDgDCvOliCDhUd3PxvnQjNQo3JFAot3Ji+Jx18qVUCo4TuCBuSFtuMNbsCm
+XwEqJafolpF0NU33PtO",
               "encSecKey"="	
1d43c7c79314ce660d3d6ebf42d1684ec2fbdd7bb26ff05518de8fe8bbd49d945ede7cf662ba0406db804d6ff267fdcb3e21
               58221409a2212b9fdde165c9e95ac63a01d6cf1d583c7e03add33d4199ed59682551438185f0c253057f41e45c9f6e55fa9f
               ceb3c9ba7827822e262ced31911b25e9b64170bcb6257aa07f65b011")
mybody
response <- POST(url = url,config=list(myheader),body=mybody,encode="form",verbose())
response



