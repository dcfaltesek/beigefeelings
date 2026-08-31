library(tidytext)

#import trump speeches
filenames <- list.files("/Users/faltesed/Desktop/text", full.names = TRUE)
filenames

text_lines <- readLines(filenames[1])

turningpoint_glendale<-readLines(filenames[90])
phxrally<-readLines(filenames[88])
parally<-readLines(filenames[69])
dtwrally<-readLines(filenames[62])
flarally<-readLines(filenames[63])

A<-data.frame(source="turning point at glendale", text=turningpoint_glendale) |> filter(text !="")
B<-data.frame(source="phx at dream city", text=phxrally)|> filter(text !="")
C<-data.frame(source="pennsylvania macks truck", text=parally)|> filter(text !="")
D<-data.frame(source="huntington place detroit",text=dtwrally)|> filter(text !="")
E<-data.frame(source="trump national doral", text=flarally)|> filter(text !="")

real_speech<-bind_rows(A,B,C,D,E)
real_speech<-real_speech |> mutate(id= 1:dim(real_speech)[1] )
View(real_speech)



real_counted<-real_speech |> 
  unnest_tokens(word, text) |> 
  count(id, word, sort=TRUE)

afinn<-get_sentiments("afinn")  

with_scores<-real_counted%>%
  #THIS IS THE INNERJOIN I WAS YELLING ABOUT!
  inner_join(afinn, by="word")

by_line<-with_scores%>%
  group_by(id)%>%
  #notice our per line strategy is SUM
  summarize(line_value=sum(value), line_var=sd(value))


real_sentiments<-inner_join(real_speech, by_line, by="id")
View(real_sentiments)

#now for simulated trump
#import the file with the simulated trump speeches
simrally<-simulated_2024_rally_speeches_fulltext |> mutate(id=1:dim(simulated_2024_rally_speeches_fulltext)[1])

sim_counted<-simrally |> 
  unnest_tokens(word, text) |> 
  count(id, word, sort=TRUE)

sim_scores<-sim_counted%>%
  #THIS IS THE INNERJOIN I WAS YELLING ABOUT!
  inner_join(afinn, by="word")

sim_by_line<-sim_scores%>%
  group_by(id)%>%
  #notice our per line strategy is SUM
  summarize(line_value=sum(value), line_var=sd(value))

sim_sentiments<-inner_join(simrally, sim_by_line, by="id")
View(sim_sentiments)


#combine results for analysis
AA<-sim_sentiments |> mutate(type="simulated")
BB<-real_sentiments |> mutate(type="real")
combined_dataset<-bind_rows(AA,BB)

#graphic
combined_dataset |> ggplot(aes(id, line_value, colour=type, shape=type))+geom_point()+facet_grid()

#wilcoxon test
wilcox.test(line_value ~ type, data=combined_dataset)

#word level analysis 
AAA<-sim_scores |> mutate(type="simulated")
BBB<-with_scores |> mutate(type="real")

usage<-bind_rows(AAA, BBB)
View(usage)

usageB<-usage |> group_by(word, type) |> summarize(use=sum(n), level=mean(value))
usageB |> ggplot(aes(use, level, colour=type, shape=type, size=use))+geom_jitter()+scale_x_log10()

#export the datasets you see here
write.csv(AA, "simulated_beige.csv", row.names = FALSE)
write.csv(BB, "true_beige,csv", row.names = FALSE)

