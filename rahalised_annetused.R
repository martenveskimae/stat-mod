library(tidyverse)
library(ggthemes)
library(htmltab)
library(lubridate)
library(zoo)
Sys.setlocale("LC_ALL", "UTF-8")

calc.age = function(dob, age.day = today(), units = "years", floor = T) {
  age = interval(dob, age.day) / duration(num = 1, units = units)
  if (floor) return(as.integer(floor(age)))
  return(age)
}

urls = expand.grid(period = seq(2013, format(Sys.time(), "%Y"), 1),
                   q = 1:4,
                   code = c(67, 68, 55, 1381, 56, 57))

df = plyr::ldply(1:nrow(urls), function(i){
  print(i)
  url = paste0("http://www.erjk.ee/et/aruanded/tulude-ja-laekumiste-paringud?period=",
               urls$period[i],"&party=",urls$code[i],"&person=all&group=all&quarter=q",urls$q[i])
  
  tmp = tryCatch(htmltab(url,1), error=function(e) NULL)
  if(is.null(tmp)) return(NULL)
  
  tmp = tmp %>%
    head(-2) %>%
    mutate(membership.fees = as.numeric(gsub(" ", "", Liikmemaks, fixed = T)),
           donations = as.numeric(gsub(" ", "", `Rahaline annetus`, fixed = T)),
           party = case_when(urls$code[i] == 55 ~ "ref",
                             urls$code[i] == 67 ~ "kesk",
                             urls$code[i] == 57 ~ "sde",
                             urls$code[i] == 56 ~ "irl",
                             urls$code[i] == 1381 ~ "vaba",
                             urls$code[i] == 68 ~ "ekre"),
           date = as.Date(as.yearqtr(paste(urls$period[i],urls$q[i]),format="%Y %q")),
           name = tools::toTitleCase(tolower(gsub( " *\\(.*?\\) *", "", V1))),
           bday = as.Date(gsub("(?<=\\()[^()]*(?=\\))(*SKIP)(*F)|.", "", V1, perl=T),
                          format="%d.%m.%Y"),
           age = calc.age(bday, date)) %>%
    select(name, bday, age, date, party, membership.fees, donations)
  
    return(tmp)
  })

# save(df, file="df.Rda")
# load("df.Rda")

cycles = data.frame(date = seq.Date(as.Date("2011-03-01"), as.Date("2018-01-01"), by = "month"),
                    np = rep(1:48, 3)[1:83]/48,
                    local = rep(1:48, 3)[18:100]/48)

lm.data = df %>%
  filter(party == "ref") %>%
  group_by(date) %>%
  summarise(donations = sum(donations)) %>%
  left_join(., cycles, by="date")

lm.data %>%
  ggplot() +
  geom_line(aes(date, donations)) +
  geom_point(aes(date, donations), size=.8) +
  theme_bw()

lm.donations = lm(donations ~ np + poly(np,2) + local + poly(local,2), lm.data)

prediction.data = data.frame(date = seq.Date(as.Date("2013-01-01"), as.Date("2018-01-01"), by = "quarter")) %>%
  left_join(., cycles, by="date")

prediction.data %>%
  mutate(yhat = predict(lm.donations, .)) %>%
  left_join(., lm.data) %>%
  ggplot() +
  geom_line(aes(date, donations)) +
  geom_point(aes(date, donations), size=.8) +
  geom_line(aes(date, yhat), color="steelblue") +
  geom_point(aes(date, yhat), size=.8, color="steelblue") +
  theme_bw()
