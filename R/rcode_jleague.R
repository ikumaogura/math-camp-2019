
# Load packages
require(cowplot)
require(dplyr)
require(ggplot2)
require(lfe)
require(RCurl)
require(XML)

# Scrape match data
j1.dat <- rep(NA, 7)
for (i in 1:20){ # Loop over weeks
  # List of matches in the week
  u <- paste0("https://www.jleague.jp/match/section/j1/", i, "/")
  tmp <- htmlParse(getURL(u), encoding = "UTF-8")
  links <- unlist(xpathApply(tmp, "//a[contains(@href, 'trackingdata')]", xmlGetAttr, "href"))
  links.short <- gsub("/trackingdata", "", links)
  for (j in 1:length(links)){ # Loop over matches
    # Teams, scores, and results
    u <- paste0("https://www.jleague.jp", links[j])
    tmp <- htmlParse(getURL(u), encoding = "UTF-8")
    team <- unlist(xpathApply(tmp, "//p[contains(@class, 'leagAccTeam')]/a/span[contains(@class, 'embL')]", xmlValue))
    home <- c(1, 0)
    gf <- c(unlist(xpathApply(tmp, "//div[@class = 'leagLeftScore']", xmlValue)),
            unlist(xpathApply(tmp, "//div[@class = 'leagRightScore']", xmlValue)))
    gf <- as.numeric(gf)
    ga <- c(unlist(xpathApply(tmp, "//div[@class = 'leagRightScore']", xmlValue)),
            unlist(xpathApply(tmp, "//div[@class = 'leagLeftScore']", xmlValue)))
    ga <- as.numeric(ga)
    # Tracking data
    u <- paste0("https://www.jleague.jp", links.short[j], "ajax_trackingdata/")
    tmp <- htmlParse(getURL(u), encoding = "UTF-8")
    tracking <- unlist(xpathApply(tmp, "//td[@class = 'total_km']", xmlValue))
    tracking <- gsub("[^[:digit:]|^[:punct:]]", "", tracking)
    distance <- tracking[1:2]
    sprint <- tracking[3:4]
    # Storing information
    week <- rep(i, 2)
    tmp.dat <- cbind.data.frame(week, team, home, gf, ga, distance, sprint)
    j1.dat <- rbind.data.frame(j1.dat, tmp.dat)
  }
  cat("Finished collecting information on week", i, "matches... \n")
  Sys.sleep(1)
}
j1.dat <- j1.dat[-1,]

# Data preprocessing
## team names in English
teams <- data.frame(team.ja = unique(j1.dat$team),
                    team.en = c("Cerezo Osaka", "Vissel Kobe", "Vegalta Sendai",
                                "Urawa Red Diamonds", "Kawasaki Frontale", "FC Tokyo",
                                "Sanfrecce Hiroshima", "Shimizu S-Pulse", "Sagan Tosu",
                                "Nagoya Grampus", "Kashima Antlers", "Oita Trinita",
                                "Jubilo Iwata", "Matsumoto Yamaga", "Gamba Osaka",
                                "Yokohama F. Marinos", "Shonan Bellmare", "Consadole Sapporo"))
j1.dat <- merge(j1.dat, teams, by.x = "team", by.y = "team.ja")
## results & points
j1.dat$res <- ifelse(j1.dat$gf > j1.dat$ga, "W", ifelse(j1.dat$gf == j1.dat$ga, "D", "L"))
j1.dat$point <- ifelse(j1.dat$res == "W", 3, ifelse(j1.dat$res == "D", 1, 0))
j1.dat <- j1.dat %>% group_by(team) %>% mutate(point.total = sum(point)) %>% ungroup()
### adjusting the points for data missingness
j1.dat$point.total[j1.dat$team.en == "Shimizu S-Pulse"] <- j1.dat$point.total[j1.dat$team.en == "Shimizu S-Pulse"] + 3
## sorting data
j1.dat <- j1.dat[order(j1.dat$point.total, j1.dat$week, decreasing = TRUE),]

# Visualization
j1.dat$distance <- as.numeric(as.character(j1.dat$distance))
j1.dat$sprint <- as.numeric(as.character(j1.dat$sprint))
j1.dat$team.en <- factor(j1.dat$team.en, levels = rev(as.character(unique(j1.dat$team.en))))
g1 <- ggplot(data = j1.dat) + 
  geom_boxplot(aes(x = team.en, y = distance)) + 
  xlab("") + ylab("") + ggtitle("Running Diatance by Team") + theme_bw() + coord_flip()
g2 <- ggplot(data = j1.dat) + 
   geom_boxplot(aes(x = team.en, y = sprint)) + 
   xlab("") + ylab("") + ggtitle("# of Sprints by Team") + theme_bw() + coord_flip()
plot_grid(g1, g2, align = "h")

# Analysis
j1.dat$win <- as.numeric(j1.dat$res == "W")
out <- felm(win ~ log(distance) + log(sprint) + home | team + week | 0 | 0, data = j1.dat)
summary(out)
