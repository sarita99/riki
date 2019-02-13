#####preprocessing/cleaning data
library(readr)
final <- read_csv("C:/Users/KISHOR/Desktop/DS Assign/FINANCE_PROJECT/final.csv")#original data
View(final)
#attach(final)
final = final[, -c(1,2,3)]#occupation and age including row number are removed because they dont matter much in building model

final$gender=ifelse(final$gender=='m',0,1)
final$fortnight = ifelse(final$`SIP(sys invst plan)`=='fortnight',1,0)
final$monthly = ifelse(final$`SIP(sys invst plan)`=='monthly',1,0)
final$quarterly = ifelse(final$`SIP(sys invst plan)`=='quarterly',1,0)
final$yearly = ifelse(final$`SIP(sys invst plan)`=='yearly',1,0)

final$smallcap = ifelse(final$`market capitalization`=='small cap',1,0)
final$midcap = ifelse(final$`market capitalization`=='mid cap',1,0)
final$largecap = ifelse(final$`market capitalization`=='large cap',1,0)

final$balanced = ifelse(final$`type of fund`=='balanced',1,0)
final$hybrid = ifelse(final$`type of fund`=='hybrid',1,0)
final$debt = ifelse(final$`type of fund`=='debt',1,0)
final$moneymarket = ifelse(final$`type of fund`=='money market',1,0)
final$growth = ifelse(final$`type of fund`=='growth',1,0)
final$equity = ifelse(final$`type of fund`=='equity',1,0)

final <- final[,-c(6,9)]#remove SIP and marketcap columns
str(final$`type of fund`)
table(final$`type of fund`)
#removing those rows which have unimportant type of fund that are not among the basic 6 above
which(final$`type of fund` == 'fixed maturity')
final <- final[-c(510,518,564,568), ]
which(final$`type of fund` == 'income')
final <- final[-c(500 ,507 ,508 ,516 ,518 ,519 ,522 ,523 ,524 ,531 ,532 ,541), ]
which(final$`type of fund` == 'index')
final <- final[-555,]
which(final$`type of fund` == 'liquid')
final <- final[-c(503 ,510 ,515 ,519 ,534 ,538),]
which(final$`type of fund` == 'sectoral')
final <- final[-536,]
which(final$`type of fund` == 'pension')
final <- final[-c(502 ,510 ,515 ,527 ,532),]
which(final$`type of fund`=='tax saving')
final <- final[-c(504 ,509 ,510 ,526 ,542),]
table(final$`type of fund`)
which(final$`type of fund`=='#N/A')
final <- final[-c(189 ,197 ,601, 617),]

table(final$`investment goal (in years)`)
table(final$income)
final <- final[,-7]#remove type of fund column
write.csv(final, 'finaldata.csv')
