Mydict1 <- c(readLines("keepwords-yue.txt",encoding = "UTF-8"),
            readLines("keepwords-ni.txt",encoding = "UTF-8"),
            readLines("keepwords-ting.txt",encoding = "UTF-8"))
Mydict1 <- unique(Mydict1)

con <- file("topics.txt",encoding="UTF-8")
writeLines(Mydict1,con=con)
close(con)


library(stringr)


Mydict2 <- readLines("starname2.txt",encoding="UTF-8")
dict2 <- str_c(Mydict2," 5 n")

con <- file("self-dict.txt",encoding="UTF-8")
writeLines(c(dict1,dict2),con=con)
close(con)
