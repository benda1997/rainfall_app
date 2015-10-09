prec_daily %<>% mutate(year = year(Date))
prec_monsoon <- filter(prec_daily, month(Date)>=5 & month(Date)<=10)

prec_monsoon %<>% mutate(yday = yday(Date))

source("helper_dry_spell.R")

dry_freq <- as.data.frame(group_by(prec_monsoon[,-c(1,643)],yday) %>% summarise_each(
  funs(sum(calcDrySpell(dat=.))/30)))
calcDrySpell(prec_monsoon[,3])
names(dry_freq)[2:642]<-paste0("dist",names(dry_freq)[2:642])
prec_monsoon %<>% mutate (md_date = as.Date(yday,origin="2012-12-31"))
dry_freq %<>% mutate (md_date = as.Date(yday,origin="2012-12-31"))

cpdat <- dplyr::select(dry_freq,md_date,dist339,dist467,dist437,
               dist468,dist538,dist399,dist358)
names(cpdat)[2:8]=c("Bankura","Burhanpur","Dewas","Kutch",
                    "Mahbubnagar","Malkangiri","Palamu")
cpdatmelt <-melt(cpdat,id.vars = "md_date",
                 value.name = "prob_dry",
                 variable.name = "district")

ggplot(cpdatmelt,aes(x=md_date,y=prob_dry))+
  geom_bar(stat="identity",position=position_dodge(width = 0.9), color="black",
           fill="red")+ theme_bw() + ylim(0,1) +
  scale_x_date(breaks = date_breaks("2 weeks"),
               labels = date_format("%d-%b"))
