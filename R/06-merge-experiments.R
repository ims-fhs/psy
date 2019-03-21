df1 <- data.frame(id = 1:5, a = "a", b = NA, fake_id = 1:5)
df2 <- data.frame(id_1 = 6:10, b = "b", a = NA, c = NA, fake_id = 6:10)
df3 <- merge(df1, df2, all = TRUE)
df3
df3 <- merge(df1, df2, by.x = "id", by.y = "id_1", all = TRUE)
df3
