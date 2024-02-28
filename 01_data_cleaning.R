library(tidyverse)

# Read the data separately
df1 <- read.csv("~/Taiwan Political Affiliation Data Analysis/2012_cleaned.csv")
df2 <- read.csv("~/Taiwan Political Affiliation Data Analysis/2013_cleaned.csv")
df3 <- read.csv("~/Taiwan Political Affiliation Data Analysis/2014_cleaned.csv")

# Set df3's birthday column as integer (mistakenly inputted as character)
df3$birth_year <- as.integer(df3$birth_year)

# Bind the df's together vertically
df <- bind_rows(df1, df2, df3)

# And extract only common columns
common_columns <- Reduce(intersect, list(names(df1), names(df2), names(df3)))
df <- df[, common_columns]

## Clean the necessary columns##

# Specify the values you want to filter rows by
specified_values <- c("台灣人", "都是", "兩者都是", "中國人", "兩者都是:是中國人也是台灣人", "兩者都是:是台灣人也是中國人")

# Eliminate unnecessary values and merge repeated values
df <- df[df$taiwanese_or_chinese %in% specified_values, ]
df[!df$taiwanese_or_chinese %in% c("台灣人", "中國人"), ]$taiwanese_or_chinese <- "兩者"
count(df, taiwanese_or_chinese)

df[df$political_party %in% c("民進黨(跳答h7)", "泛綠"), ]$political_party <- "民進黨"
df[df$political_party %in% c("國民黨(跳答h7)", "泛藍"), ]$political_party <- "國民黨"
df[!df$political_party %in% c("國民黨", "民進黨"), ]$political_party <- "其他"

# See the individual counts for the remaining cleaned items.
count(df, political_party)
count(df, taiwanese_or_chinese)

# Plotting :D
ggplot() +
  geom_bar(data = df, aes(x = political_party, fill = taiwanese_or_chinese), position = "dodge")

ggplot() +
  geom_bar(data = df, aes(x = taiwanese_or_chinese, fill = political_party), position = "dodge")

ggplot() +
  geom_bar(data = df, aes(x = political_party, fill = taiwanese_or_chinese), position = "fill")

# Create a general table for viewing proportions distribution
table_pol_et <- table(df$political_party, df$taiwanese_or_chinese)
table_pol_et <- addmargins(prop.table(table_pol_et, margin = NULL))
print(table_pol_et)

# Eliminate the non-affiliated observations
df_parties <- df[df$political_party %in% c("國民黨", "民進黨"), ]

# See the individual counts for the remaining cleaned items.
count(df_parties, political_party)
count(df_parties, taiwanese_or_chinese)
# And a joint table
table_pol <- table(df_parties$political_party, df_parties$taiwanese_or_chinese)
table_pol


# New plots without the No Party variable
ggplot() +
  geom_bar(data = df_parties, aes(x = taiwanese_or_chinese, fill = political_party), position = "fill")

ggplot() +
  geom_bar(data = df_parties, aes(x = political_party, fill = taiwanese_or_chinese), position = "fill")

# Create dummy variables needed for separation of prediction
df_parties$chinese_or_not <- as.numeric(df_parties$taiwanese_or_chinese == "中國人")
df_parties$taiwanese_or_not <- as.numeric(df_parties$taiwanese_or_chinese == "台灣人")
df_parties$both_or_not <- as.numeric(df_parties$taiwanese_or_chinese == "兩者")

df_parties$political_party <- as.factor(df_parties$political_party)
df_parties$taiwanese_or_chinese <- as.factor(df_parties$taiwanese_or_chinese)
logistic <- glm(political_party ~ taiwanese_or_chinese, data = df_parties, family = "binomial")
summary(logistic)

df_parties <- df_parties[, c("political_party", "chinese_or_not", "taiwanese_or_not", "both_or_not")]


write.csv(df_parties, "~/Taiwan Political Affiliation Data Analysis/data_for_model.csv", row.names = FALSE)
