cleaned_data2<-as.data.frame(cleaned_data)
cleaned_data2$urls <- vapply(cleaned_data2$urls, paste, collapse = ", ", character(1L))
write.table(cleaned_data2,"cleaned_data.csv")
