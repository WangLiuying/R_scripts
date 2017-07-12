


####connect
library(RSQLite)
sqlite <- dbDriver("SQLite")
file.exists("pokedex.sqlite")
con <- dbConnect(sqlite, "pokedex.sqlite")

##基础管理操作
# general information
dbGetInfo(con)

# listing tables
dbListTables(con)

# reading tables
pokemon <- dbReadTable(con, "pokemon")
pokemon_species <- dbReadTable(con,"pokemon_species")
pokemon_stats <-dbReadTable(con,"pokemon_stats") 
rm(pokemon_stats)

# writing tables
dbWriteTable(con, "test", pokemon)

# table exists?
dbExistsTable(con, "test")

# remove table
dbRemoveTable(con, "test")

# checking data type
dbDataType(con, pokemon$id)
dbDataType(con, pokemon$base_experience)


# transaction management
dbBeginTransaction(con)
dbRollback(con)

dbBeginTransaction(con)
dbCommit(con)

# closing connection
dbDisconnect(con)


##执行SQl语句

# CREATE TABLE 仿照pokemon_species表来建一个新表
sql <- "CREATE  TABLE  mytest (
id INTEGER  NOT  NULL  ,
identifier VARCHAR(100) NOT  NULL  ,
generation_id INTEGER NOT  NULL  ,
evolution_chain_id INTEGER NOT NULL  ,
PRIMARY  KEY  (id)
) ;"
dbGetQuery(con, sql)
dbReadTable(con,"mytest")

#INSERT INTO
sql <- "INSERT INTO mytest (id, identifier,
              generation_id, evolution_chain_id)
        VALUES  (1, 'bulbasaur', 1, 1),
                (2, 'ivysaur', 1, 1),
                (3, 'venusaur', 1, 1),
                (4, 'charmander', 1, 2),
                (5, 'charmenleon', 1, 2);"
dbGetQuery(con, sql)
dbReadTable(con,"mytest")

# UPDATE
sql <- "ALTER  TABLE  mytest ADD  COLUMN  color_id INT  ; "
dbGetQuery(con, sql)
dbReadTable(con,"mytest")

# DELETE
sql <- "DELETE  FROM  mytest WHERE  identifier = 'bulbasaur' ;"
dbGetQuery(con, sql)
dbReadTable(con, "mytest")

# DROP TABLE
sql <- "DROP TABLE mytest"
dbGetQuery(con, sql)
#也可以用dbRemoveTable()方法

# QUERY
sql <- "SELECT * FROM pokemon
        WHERE weight >=4000 ;"
dbGetQuery(con, sql)

sql <- "SELECT identifier FROM pokemon_species
        WHERE identifier LIKE '%Nido%' ;"
dbGetQuery(con, sql)

sql <- "SELECT pokemon.id, identifier, height, weight FROM pokemon
        INNER JOIN pokemon_species 
          ON pokemon.species_id = pokemon_species.id
        WHERE weight >= 4000 ;"
dbGetQuery(con, sql)

sql <- "SELECT identifier, AVG(height), AVG(weight) FROM pokemon
        INNER JOIN pokemon_species
          ON pokemon.species_id = pokemon_species.id
        GROUP BY pokemon.species_id
          HAVING AVG(height) >= 50 ;"
dbGetQuery(con,sql)

sql <- "SELECT identifier FROM pokemon_species
        WHERE identifier LIKE '%Nido%' ;"
dbGetQuery(con, sql)

# dbSendQuery()
test <- dbSendQuery(con, sql)
fetch(test)

# closing connection
dbDisconnect(con)
