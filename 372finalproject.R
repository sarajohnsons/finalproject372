#################################################


# Read in the GSS data

library(foreign)
read.dct <- function(dct, labels.included = "yes") {
  temp <- readLines(dct)
  temp <- temp[grepl("_column", temp)]
  switch(labels.included,
         yes = {
           pattern <- "_column\\(([0-9]+)\\)\\s+([a-z0-9]+)\\s+(.*)\\s+%([0-9]+)[a-z]\\s+(.*)"
           classes <- c("numeric", "character", "character", "numeric", "character")
           N <- 5
           NAMES <- c("StartPos", "Str", "ColName", "ColWidth", "ColLabel")
         },
         no = {
           pattern <- "_column\\(([0-9]+)\\)\\s+([a-z0-9]+)\\s+(.*)\\s+%([0-9]+).*"
           classes <- c("numeric", "character", "character", "numeric")
           N <- 4
           NAMES <- c("StartPos", "Str", "ColName", "ColWidth")
         })
  temp_metadata <- setNames(lapply(1:N, function(x) {
    out <- gsub(pattern, paste("\\", x, sep = ""), temp)
    out <- gsub("^\\s+|\\s+$", "", out)
    out <- gsub('\"', "", out, fixed = TRUE)
    class(out) <- classes[x] ; out }), NAMES)
  temp_metadata[["ColName"]] <- make.names(gsub("\\s", "", temp_metadata[["ColName"]]))
  temp_metadata
}

read.dat <- function(dat, metadata_var, labels.included = "yes") {
  read.table(dat, col.names = metadata_var[["ColName"]])
}


GSS_metadata <- read.dct("GSS.dct")
GSS_ascii <- read.dat("GSS.dat", GSS_metadata)
attr(GSS_ascii, "col.label") <- GSS_metadata[["ColLabel"]]
GSS <- GSS_ascii
data <- GSS[GSS$YEAR==2022,]



#####Renaming#####

# Married to clearly interpret binary 1
colnames(data)[colnames(data) == 'MARITAL'] <- 'MARRIED'


# Naming it to show its hours worked
colnames(data)[colnames(data) == 'HRS1'] <- 'AVGHRSWORK'




####### Looking at my variables #########
table(data$`AVG HOURS WORKED A WEEK`, exclude = NULL)
table(data$MARRIED)
table(data$DIVORCE)
table(data$CHILDS)
table(data$AGE)
table(data$YEAR)



# Values that are negative for these variables mean unanswered
# I'm going to get rid of them by assigning as NA as removing
# These are only present in 3 of my variables
data$MARRIED <- ifelse(data$MARRIED == 1, 1, 0)
data$DIVORCE <- ifelse(data$DIVORCE == 1, 1, 0)
data$`AVGHRSWORK` <- ifelse(data$`AVGHRSWORK` < 0, NA, data$`AVGHRSWORK`)
data$DIVORCE <- ifelse(data$DIVORCE < 0, NA, data$DIVORCE)
data$AGE <- ifelse(data$AGE < 0, NA, data$AGE)
data$CHILDS <- ifelse(data$CHILDS < 0, NA, data$CHILDS)

# Getting rid of the NA's that I recoded
data <- na.omit(data)




##### Histograms & descriptions #####

hist(data$`AVGHRSWORK`)
mean(data$`AVGHRSWORK`)
sd(data$`AVGHRSWORK`)
table(data$`AVGHRSWORK`)

hist(data$DIVORCE)
mean(data$DIVORCE)
sd(data$DIVORCE)
table(data$DIVORCE)

hist(data$CHILDS)
mean(data$CHILDS)
sd(data$CHILDS)
table(data$CHILDS)

hist(data$AGE)
mean(data$AGE)
sd(data$AGE)
table(data$AGE)

hist(data$YEAR)
mean(data$YEAR)
sd(data$YEAR)
table(data$YEAR)

hist(data$MARRIED)

###### Scatterplot & Correlation #######

plot(data$`AVGHRSWORK`, data$MARRIED)
cor(data$`AVGHRSWORK`, data$MARRIED)

plot(data$`DIVORCE`, data$MARRIED)
cor(data$`DIVORCE`, data$MARRIED)

plot(data$`CHILDS` , data$MARRIED)
cor(data$`CHILDS`, data$MARRIED)

plot(data$AGE , data$MARRIED)
cor(data$AGE, data$MARRIED)

plot(data$YEAR, data$MARRIED)
cor(data$YEAR, data$MARRIED)

###### plot of association #######
library(dagitty)

nodes <- c("MARRIED", "AVGHOURSWORK", "DIVORCE", "CHILDS", "AGE")

edges <- dagitty( "dag {
  AVGHOURSWORK -> MARRIED
  AVGHOURSWORK -> DIVORCE
  DIVORCE -> MARRIED
  CHILDS -> MARRIED
  AGE -> CHILDS
  AGE -> MARRIED
  CHILDS -> AVGHOURSWORK
  
}")

plot(edges)


model <- lm(data$MARRIED ~ data$AVGHRSWORK + data$CHILDS)

summary(model)

