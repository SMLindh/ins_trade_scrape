require(rvest)
require(RSQLite)
require(quantmod)
# Establish in memory Database connection
# Stores perma DB
con = dbConnect(RSQLite::SQLite(), "C:\\Users\\shane\\Desktop\\stonks\\finviz\\InsTrade.db")
# Format volume columns
format_vol = function(x) {as.numeric(gsub(",","", x))}
# URL and reading URL
url = "https://finviz.com/insidertrading.ashx"
cat("Now Getting Data.....")
table = read_html(url)
table = table %>% html_nodes("table") %>%  .[[6]] %>% html_table(header=TRUE,fill = TRUE)
# Formating colnames
noms = names(table)
noms = gsub("#", "Num", noms)
noms = gsub("\\$", "Dollar", noms)
noms = gsub("\\(", "", noms)
noms = gsub("\\)", "", noms)
noms = gsub(" ", "", noms)
colnames(table) = noms
#More Formatting
table$Date = as.Date(as.character(table$Date), format = "%b %d")
table$NumShares = format_vol(table$NumShares)
table$ValueDollar = format_vol(table$ValueDollar)
table$NumSharesTotal = format_vol(table$NumSharesTotal)
table$SECForm4 = as.POSIXct(as.character(table$SECForm4), format = "%b %d %I:%M :p")
#Write table to database
dbWriteTable(con, "InsTrade", table , append = TRUE)
cat("Done..")
#writes DB to DF
test = dbReadTable(con,"InsTrade")
write.csv(test, "Ins_trades.csv")




