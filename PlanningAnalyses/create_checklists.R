msse_list = list.files("/Users/aaroncaldwell/Dropbox/STORK/Positive Result Rate/Data Collection/Articles/MSSE 2019")
ejss_list = list.files("/Users/aaroncaldwell/Dropbox/STORK/Positive Result Rate/Data Collection/Articles/MSSE 2019")
jsams_list = list.files("/Users/aaroncaldwell/Dropbox/STORK/Positive Result Rate/Data Collection/Articles/JSAMS 2019")

df_msse = data.frame(article = msse_list)
df_ejss = data.frame(article = ejss_list)
df_jsams = data.frame(article = jsams_list)

write.csv(df_msse,"msse_checklist.csv")
write.csv(df_ejss,"ejss_checklist.csv")
write.csv(df_ejss,"jsams_checklist.csv")
