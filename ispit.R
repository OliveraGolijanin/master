# ucitavanje
getwd()
atp_matches_2017<-read.csv("atp_matches_2017.csv", stringsAsFactors = FALSE)
tenis_baza<-atp_matches_2017
#upoznavanje
ncol(tenis_baza)
nrow(tenis_baza)
head(tenis_baza,3)
tail(tenis_baza,25)
str(tenis_baza)
#sredjivanje
tenismoja<- tenis_baza[, !(names(tenis_baza) %in% c('tourney_id', 'tourney_date',
                                                    'winner_id','loser_id',
                                                    'best_of','winner_rank_points',
                                                    'loser_rank_points'))]

teniswalkover<-tenismoja[!(tenismoja$score =="W/O" |
                             tenismoja$score =="Walkover") , ]
tenisdavisrr <-teniswalkover[!grepl("Davis Cup", teniswalkover$tourney_name),]
tenisret<-tenisdavisrr[!grepl("RET", tenisdavisrr$score),]
sum(is.na(tenisret)) 
which(is.na(tenisret))
tenismissing<- replace(tenisret, tenisret== "", NA) 
tenis <- na.omit(tenismissing)
#zanimljivosti
which(tenis$minutes==max(tenis$minutes))
which(tenis$l_bpFaced==max(tenis$l_bpFaced))
tenis[1244,'loser_rank']
which(tenis$w_ace==max(tenis$w_ace))
tenis[178,c('surface','minutes','score')]
#provera normalnosti
hist(tenis$w_1stIn_per, main ="Procenat ubacenog prvog servisa (pobednik)" ,xlab = "procenat",
     ylab = "gustina",border ="#3399CC",col = "#66CCFF",xlim = c(0,100),freq = FALSE)
lines(density(tenis$w_1stIn_per))
hist(tenis$l_1stIn_per, main ="Procenat ubacenog prvog servisa (porazeni)" ,xlab = "procenat",
     ylab = "gustina",border ="#3399CC",col = "#66CCFF",xlim = c(0,100),freq = FALSE)
lines(density(tenis$l_1stIn_per))
hist(tenis$w_1stWon_per, main ="Procenat poena osvojenih na prvi servis (pobednik)" ,
     xlab = "procenat", ylab = "gustina",border ="#006600",col = "#33CC33",
     xlim = c(0,100),freq = FALSE)
lines(density(tenis$w_1stWon_per))
hist(tenis$l_1stWon_per, main ="Procenat poena osvojenih na prvi servis (porazeni)" ,
     xlab = "procenat", ylab = "gustina",border ="#006600",col = "#33CC33",
     xlim = c(0,100),freq = FALSE)
lines(density(tenis$l_1stWon_per))
hist(tenis$w_2ndWon_per, main ="Procenat poena osvojenih na drugi servis (pobednik)" ,
     xlab = "procenat", ylab = "gustina",border ="#993366",col = "#CC6699",
     xlim = c(0,100),freq = FALSE)
lines(density(tenis$w_2ndWon_per))
hist(tenis$l_2ndWon_per, main ="Procenat poena osvojenih na drugi servis (porazeni)" ,
     xlab = "procenat", ylab = "gustina",border ="#993366",col = "#CC6699",
     xlim = c(0,100),freq = FALSE)
lines(density(tenis$l_2ndWon_per))
hist(tenis$w_bpSaved_per, main ="Procenat spasenih brejk lopti (pobednik)" ,
     xlab = "procenat", ylab = "gustina",border ="#990000",col = "#CC3333",
     xlim = c(0,100),freq = FALSE)
lines(density(tenis$w_bpSaved_per))
hist(tenis$l_bpSaved_per, main ="Procenat spasenih brejk lopti (porazeni)" ,
     xlab = "procenat", ylab = "gustina",border ="#990000",col = "#CC3333",
     xlim = c(0,100),freq = FALSE)
lines(density(tenis$l_bpSaved_per))

hist(tenis$w_ace, main ="Broj asova (pobednik)" ,
     xlab = "broj asova", ylab = "gustina",border ="#993300",col = "#FF9933",
     xlim = c(0,100),freq = FALSE)
lines(density(tenis$w_ace))
hist(tenis$l_ace, main ="Broj asova (porazeni)" ,
     xlab = "broj asova", ylab = "gustina",border ="#993300",col = "#FF9933",
     xlim = c(0,100),freq = FALSE)
lines(density(tenis$l_ace))
hist(tenis$w_df, main ="Broj duplih gresaka (pobednik)" ,
     xlab = "broj duplih gresaka", ylab = "gustina",border ="#FF9900",col = "#FFCC00",
     xlim = c(0,100),freq = FALSE)
lines(density(tenis$w_df))
hist(tenis$l_df, main ="Broj duplih gresaka (porazeni)" ,
     xlab = "broj duplih gresaka", ylab = "gustina",border ="#FF9900",col = "#FFCC00",
     xlim = c(0,100),freq = FALSE)
lines(density(tenis$l_df))
hist(tenis$w_df, main ="Broj duplih gresaka (pobednik)" ,
     xlab = "procenat", ylab = "gustina",border ="#FF9900",col = "#FFCC00",
     xlim = c(0,100),freq = FALSE)
lines(density(tenis$w_df))
hist(tenis$l_df, main ="Broj duplih gresaka (porazeni)" ,
     xlab = "procenat", ylab = "gustina",border ="#FF9900",col = "#FFCC00",
     xlim = c(0,100),freq = FALSE)
lines(density(tenis$l_df))
hist(tenis$w_df, main ="Broj duplih gresaka (pobednik)" ,
     xlab = "procenat", ylab = "gustina",border ="#FF9900",col = "#FFCC00",
     xlim = c(0,100),freq = FALSE)
lines(density(tenis$w_df))
hist(tenis$l_df, main ="Broj duplih gresaka (porazeni)" ,
     xlab = "procenat", ylab = "gustina",border ="#FF9900",col = "#FFCC00",
     xlim = c(0,100),freq = FALSE)
lines(density(tenis$l_df))
hist(tenis$w_1stIn, main ="Broj ubacenih prvih servisa (pobednik)" ,
     xlab = "broj ubacenih prvih servisa", ylab = "gustina",border ="#333333",col = "#999999",
     xlim = c(0,100),freq = FALSE)
lines(density(tenis$w_1stIn))
hist(tenis$l_1stIn, main ="Broj ubacenih prvih servisa (porazeni)" ,
     xlab = "broj ubacenih prvih servisa", ylab = "gustina",border ="#333333",col = "#999999",
     xlim = c(0,100),freq = FALSE)
lines(density(tenis$l_1stIn))
hist(tenis$w_1stWon, main ="Broj poena dobijenih na prvi servis (pobednik)" ,
     xlab = "broj dobijenih poena na prvi servis", ylab = "gustina",border ="#339966",col = "#66CC99",
     xlim = c(0,100),freq = FALSE)
lines(density(tenis$w_1stWon))
hist(tenis$l_1stWon, main ="Broj poena dobijenih na prvi servis (porazeni)" ,
     xlab = "broj dobijenih poena na prvi servis", ylab = "gustina",border ="#339966",col = "#66CC99",
     xlim = c(0,100),freq = FALSE)
lines(density(tenis$l_1stWon))
hist(tenis$w_2ndWon, main ="Broj poena dobijenih na drugi servis (pobednik)" ,
     xlab = "broj dobijenih poena na drugi servis", ylab = "gustina",border ="#996699",col = "#CC99CC",
     xlim = c(0,100),freq = FALSE)
lines(density(tenis$w_2ndWon))
hist(tenis$l_2ndWon, main ="Broj poena dobijenih na drugi servis (porazeni)" ,
     xlab = "broj dobijenih poena na drugi servis", ylab = "gustina",border ="#996699",col = "#CC99CC",
     xlim = c(0,100),freq = FALSE)
lines(density(tenis$l_2ndWon))
hist(tenis$w_bpSaved, main ="Broj spasenih brejk lopti (pobednik)" ,
     xlab = "broj spasenih brejk lopti", ylab = "gustina",border ="#FF3300",col = "#FF6633",
     xlim = c(0,100),freq = FALSE)
lines(density(tenis$w_bpSaved))
hist(tenis$l_bpSaved, main ="Broj spasenih brejk lopti (porazeni)" ,
     xlab = "broj spasenih brejk lopti", ylab = "gustina",border ="#FF3300",col = "#FF6633",
     xlim = c(0,100),freq = FALSE)
lines(density(tenis$l_bpSaved))
hist(tenis$w_bpFaced, main ="Broj brejk lopti sa kojim se suocio pobednik" ,
     xlab = "broj brejk lopti", ylab = "gustina",border ="#99CCFF",col = "#3399FF",
     xlim = c(0,100),freq = FALSE)
lines(density(tenis$w_bpFaced))
hist(tenis$l_bpFaced, main ="Broj brejk lopti sa kojim se suocio porazeni" ,
     xlab = "broj brejk lopti", ylab = "gustina",border ="#99CCFF",col = "#3399FF",
     xlim = c(0,100),freq = FALSE)
lines(density(tenis$l_bpFaced))
hist(tenis$minutes, main ="Duzina trajanja meca" ,
     xlab = "minuti", ylab = "gustina",border ="#003300",col = "#00FF33",
     xlim = c(0,400),freq = FALSE)
lines(density(tenis$minutes))
hist(tenis$winner_rank, main ="Rang pobednika" ,
     xlab = "pozicija na rang listi", ylab = "gustina",border ="#003300",col = "#00FF33",
     xlim = c(0,400),freq = FALSE)
lines(density(tenis$winner_rank))

#shapiro jaci test
shapiro.test(tenis$w_1stIn_per)
#normalna
shapiro.test(tenis$l_1stIn_per)
#normalna
shapiro.test(tenis$w_1stWon_per)
#nije normalna
shapiro.test(tenis$l_1stWon_per)
#nije normalna nikako
shapiro.test(tenis$w_2ndWon_per)
#nije normalna nikako
shapiro.test(tenis$l_2ndWon_per)
#nije normalna
shapiro.test(tenis$w_bpSaved_per)
#nije normalna
shapiro.test(tenis$l_bpSaved_per)
#nije normalna
shapiro.test(tenis$w_ace)
#nije normalna
shapiro.test(tenis$l_ace)
#nije normalna
shapiro.test(tenis$w_df)
#nije normalna
shapiro.test(tenis$l_df)
#nije normalna
shapiro.test(tenis$w_1stIn)
#nije normalna
shapiro.test(tenis$l_1stIn)
#nije normalna
shapiro.test(tenis$w_1stWon)
#nije normalna
shapiro.test(tenis$l_1stWon)
#nije normalna
shapiro.test(tenis$w_2ndWon)
#nije normalna
shapiro.test(tenis$l_2ndWon)
#nije normalna
shapiro.test(tenis$w_bpSaved)
#nije normalna
shapiro.test(tenis$l_bpSaved)
#nije normalna
shapiro.test(tenis$w_bpFaced)
#nije normalna
shapiro.test(tenis$l_bpFaced)
#nije normalna

shapiro.test(tenis$minutes)
#nije normalna
shapiro.test(tenis$winner_rank)
#nije normalna
# prvo istrazivacko pitanje
cor(tenis[,5],tenis[,c(6,7,9,15,16,18)],method = "spearman")
korelacija_pomocna<-data.frame(tenis$minutes, tenis$w_ace, tenis$w_df, tenis$l_ace,
                               tenis$l_df,tenis$w_1stIn, tenis$l_1stIn)
svaka_sa_svakom_cor = expand.grid(names(korelacija_pomocna), 
                                  names(korelacija_pomocna))
funkcija_za_p = function(col_name1, col_name2, data_frame) {
  cor.test(data_frame[[col_name1]], data_frame[[col_name2]])$p.value}
pvrednosti_kor <- mapply(funkcija_za_p, 
                         col_name1 = svaka_sa_svakom_cor[[1]], 
                         col_name2 = svaka_sa_svakom_cor[[2]], 
                         MoreArgs = list(data_frame = korelacija_pomocna))

matrix (pvrednosti_kor, 7, 7, dimnames = list(names(korelacija_pomocna), 
                                              names(korelacija_pomocna)))

plot(tenis$minutes,tenis$w_ace+tenis$l_ace , main="Duzina meca i asovi",
     xlab="minuti ", ylab="broj asova ", type="p",pch=21,col='#990000',bg='#CC0000')
plot(tenis$minutes,tenis$w_df+tenis$l_df , main="Duzina meca i duple servis greske",
     xlab="minuti ", ylab="broj duplih servis gresaka ", type="p",pch=22,
     col='#CC3300',bg='#FF6600')
plot(tenis$minutes,tenis$w_1stIn+tenis$l_1stIn , main="Duzina meca i ubaceni prvi servis",
     xlab="minuti ", ylab="broj ubacenih prvih servisa ", type="p",pch=24,
     col='#660066',bg='#993366')
pairs(~tenis$minutes+tenis$w_ace+tenis$w_df+tenis$l_ace+tenis$l_df+tenis$w_1stIn+
        tenis$l_1stIn,data=tenis, main="Sta utice na duzinu trajanja meca?", 
        col='#6699CC')
#drugo istrazivacko pitanje
roundfactor <- factor(tenis$round)
roundfactor
levels(roundfactor)
roundfactor_ok <- factor(x=tenis$round, levels = c('R128', 'R64', 'R32','R16','QF',
                                                   'SF','F','RR'))
levels(roundfactor_ok)
round_anova<- aov(tenis$w_1stIn ~ roundfactor_ok, data = tenis)
summary(round_anova)
TukeyHSD(round_anova)
kruskal.test (tenis$w_ace ~ roundfactor_ok, data = tenis) 
pairwise.wilcox.test(tenis$w_ace, roundfactor_ok,
                     p.adjust.method = "BH")
kruskal.test (tenis$w_df ~ roundfactor_ok, data = tenis) 
pairwise.wilcox.test(tenis$w_df, roundfactor_ok,
                     p.adjust.method = "BH")
kruskal.test (tenis$w_1stWon ~ roundfactor_ok, data = tenis) 
pairwise.wilcox.test(tenis$w_1stWon, roundfactor_ok,
                     p.adjust.method = "BH")
kruskal.test (tenis$w_2ndWon ~ roundfactor_ok, data = tenis) 
pairwise.wilcox.test(tenis$w_2ndWon, roundfactor_ok,
                     p.adjust.method = "BH")
kruskal.test (tenis$l_bpSaved ~ roundfactor_ok, data = tenis) 
pairwise.wilcox.test(tenis$l_bpSaved, roundfactor_ok,
                     p.adjust.method = "BH")
kruskal.test (tenis$w_bpFaced ~ roundfactor_ok, data = tenis) 
pairwise.wilcox.test(tenis$w_bpFaced, roundfactor_ok,
                     p.adjust.method = "BH")
#ovde ubaciti bar plotove za drugo pitanje
tenis3<-tenis
moje_boje=c("#99CCFF",  "#FFCCCC", "#66FFFF","#CCCCFF","#FFCC99","#FF9999",
            "#00CC33")

ubaceniprvi<-aggregate(tenis3$w_1stIn, by=list(Category=tenis3$round), FUN=sum)
barplot(height = ubaceniprvi$x,col = moje_boje, 
        beside = TRUE, axis.lty="solid",main="Broj ubacenih prvih servisa po kolima turnira",
        xlab = "kola turnira",ylab = "broj ubacenih prvih servisa",ylim = c(0,45000))
legend("topright", legend = c('F', 'SF', 'QF','R16','R32','R64','R128','RR'), 
       fill = moje_boje, box.lty = 0, cex = 0.8)
ubaceniprvi

asovi<-aggregate(tenis3$w_ace, by=list(Category=tenis3$round), FUN=sum)
barplot(height = asovi$x,col = moje_boje, 
        beside = TRUE, axis.lty="solid",main="Asovi po kolima turnira",
        xlab = "kola turnira",ylab = "broj asova",ylim = c(0,6000))
legend("topright", legend = c('F', 'SF', 'QF','R16','R32','R64','R128','RR'), 
       fill = moje_boje, box.lty = 0, cex = 0.8)

duple<-aggregate(tenis3$w_df, by=list(Category=tenis3$round), FUN=sum)
barplot(height = duple$x,col = moje_boje, 
        beside = TRUE, axis.lty="solid",main="Duple greske po kolima turnira",
        xlab = "kola turnira",ylab = "broj duplih gresaka",ylim = c(0,3000))
legend("topright", legend = c('F', 'SF', 'QF','R16','R32','R64','R128','RR'), 
       fill = moje_boje, box.lty = 0, cex = 0.8)
duple

osvojeniprvi<-aggregate(tenis3$w_1stWon, by=list(Category=tenis3$round), FUN=sum)
barplot(height = osvojeniprvi$x,col = moje_boje, 
        beside = TRUE, axis.lty="solid",main="Poeni osvojeni na prvi servis po kolima turnira",
        xlab = "kola turnira",ylab = "broj poena osvojenih na prvi servis",ylim = c(0,30000))
legend("topright", legend = c('F', 'SF', 'QF','R16','R32','R64','R128','RR'), 
       fill = moje_boje, box.lty = 0, cex = 0.8)
osvojeniprvi

osvojenidrugi<-aggregate(tenis3$w_2ndWon, by=list(Category=tenis3$round), FUN=sum)
barplot(height = osvojenidrugi$x,col = moje_boje, 
        beside = TRUE, axis.lty="solid",main="Poeni osvojeni na drugi servis po kolima turnira",
        xlab = "kola turnira",ylab = "broj poena osvojenih na drugi servis",ylim = c(0,14000))
legend("topright", legend = c('F', 'SF', 'QF','R16','R32','R64','R128','RR'), 
       fill = moje_boje, box.lty = 0, cex = 0.8)
osvojenidrugi

brejkspasene<-aggregate(tenis3$w_bpSaved, by=list(Category=tenis3$round), FUN=sum)
barplot(height = brejkspasene$x,col = moje_boje, 
        beside = TRUE, axis.lty="solid",main="Broj spasenih brejk lopti po kolima turnira",
        xlab = "kola turnira",ylab = "broj spasenih brejk lopti",ylim = c(0,3000))
legend("topright", legend = c('F', 'SF', 'QF','R16','R32','R64','R128','RR'), 
       fill = moje_boje, box.lty = 0, cex = 0.8)
brejkspasene

brejksuocene<-aggregate(tenis3$w_bpFaced, by=list(Category=tenis3$round), FUN=sum)
barplot(height = brejksuocene$x,col = moje_boje, 
        beside = TRUE, axis.lty="solid",main="Broj brejk lopti po kolima turnira",
        xlab = "kola turnira",ylab = "broj brejk lopti",ylim = c(0,5000))
legend("topright", legend = c('F', 'SF', 'QF','R16','R32','R64','R128','RR'), 
       fill = moje_boje, box.lty = 0, cex = 0.8)
brejksuocene

#trece istrazivacko pitanje
regresija_winner_pomocna <- data.frame(tenis$winner_rank, tenis$w_ace, tenis$w_df, 
                                       tenis$w_1stIn, tenis$w_1stWon,
                                       tenis$w_2ndWon,tenis$w_bpSaved)
regresija_winner<- cor(as.matrix(regresija_winner_pomocna), method = "spearman")
regresija_winner
svaka_sa_svakom = expand.grid(names(regresija_winner_pomocna), 
                              names(regresija_winner_pomocna))
funkcija_za_p = function(col_name1, col_name2, data_frame) {
  cor.test(data_frame[[col_name1]], data_frame[[col_name2]])$p.value
}
pvrednosti <- mapply(funkcija_za_p, 
                     col_name1 = svaka_sa_svakom[[1]], 
                     col_name2 = svaka_sa_svakom[[2]], 
                     MoreArgs = list(data_frame = regresija_winner_pomocna))
matrix(pvrednosti, 7, 7, dimnames = list(names(regresija_winner_pomocna), 
                                         names(regresija_winner_pomocna)))
korelacija_dole <- function(x, y){
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- round(cor(x, y), digits=2)
  txt <- paste0("R = ", r)
  cex.cor <- 1.5/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}

korelacija_gore<-function(x, y){
  points(x,y, pch = 19, col = "#FC4E07")
}

pairs(tenis[,c(6,7,9,10,11,13,24)], 
      lower.panel = korelacija_dole,
      upper.panel = korelacija_gore)
corrplot(regresija_winner, method="circle", type="lower")
model_pobednik <- lm(tenis$winner_rank ~ tenis$w_ace + tenis$w_df + tenis$w_1stIn +
                       tenis$w_bpSaved) 
summary(model_pobednik)
model_pobednik_asduplebrejk<-lm(tenis$winner_rank ~ tenis$w_ace + tenis$w_df +
                                  tenis$w_bpSaved) 
summary(model_pobednik_asduplebrejk)
plot(tenis$winner_rank,tenis$w_ace+ tenis$w_df+tenis$w_bpSaved, 
     main="Regresioni model",
     xlab="rang ", ylab="asovi, duple greske i spasene brejk lopte ", type="p",pch=8,
     col='#660066',bg='#993366')
