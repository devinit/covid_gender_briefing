suppressPackageStartupMessages(lapply(c("data.table", "jsonlite","rstudioapi"), require, character.only=T))

#Load FTS utility functions
#setwd("C:/Users/jasminj/Documents/R/covid_gender_briefing-main")

setwd(dirname(getActiveDocumentContext()$path))
setwd("..")

fts <- fread("project_data/fts_flows.csv", encoding = "UTF-8")

keep <- c(
  "id"
  ,
  "description"
  ,
  "year"
  ,
  "flowType"
  ,
  "keywords"
  ,
  "sector"
  ,
  "multisector"
  ,
  "amountUSD_defl"
  ,
  "sourceObjects_Location.name"
  ,
  "sourceObjects_Organization.name"
  ,
  "destinationObjects_Location.name"
  ,
  "destinationObjects_GlobalCluster.name"
  ,
  "destinationObjects_Cluster.name"
  ,
  "destinationObjects_Organization.name"
  ,
  "destination_ngotype"
  ,
  "destinationObjects_Plan.name"
)

fts <- fts[, ..keep]

fts <- fts[as.character(year) >= 2014]

major.keywords <- c(
  "abuse",
  "abuso",
  "maltraitance",
  "arranged marriage",
  "matrimonio concertado",
  "matrimonio arreglado",
  "mariage arrangé",
  "birth control",
  "contraceptive",
  "contraception",
  "anticonceptivo",
  "método anticonceptivo",
  "contraceptif",
  "méthode contraceptive",
  "bisexual",
  "bisexuel",
  "bride price",
  "\\bdote\\b",
  "\\bdote\\b",
  "child marriage",
  "matrimonio infantil",
  "mariage d'enfants",
  "mariage des enfants",
  "coercive sterilization",
  "coercive sterilisation",
  "esterilización forzada",
  "stérilisation forcée",
  "domestic violence",
  "violencia doméstica",
  "violence domestique", 
  "violence familiale", 
  "violence conugale",
  "violence intrafamiliale",
  "condom",
  "condón",
  "préservatif",
  "femicide",
  "feminicidio",
  "féminicide",
  "fémicide",
  "\\bfemale empowerment\\b",
  "empoderamiento femenino",
  "autonomisation des femmes",
  "\\bfemale genital mutilation\\b",
  "\\bfemale genital cutting\\b",
  "\\bfemale circumcision\\b",
  "FGM",
  "mutilación genital femenina",
  "mutilation génitale féminine",
  "mutilation sexuelle féminine", 
  "excision génitale féminine", 
  "circoncision féminine", 
  "MGF",
  "feminism",
  "féminisme",
  "gender",
  "género",
  "genre",
  "gender-based violence",
  "violencia de género",
  "violencia machista",
  "violence basée sur le genre",
  "violence sexiste",
  "gender-blind",
  "gender blindness",
  "indiferente a las cuestiones de género",
  "insensible a las cuestiones de género", 
  "insensible aux questions de genre", 
  "insensible à l'inégalité des sexes",
  "gender discrimination",
  "discriminación de género", 
  "discriminación contra las mujeres",
  "discrimination de genre", 
  "discrimination fondée sur le genre",
  "discrimination sexuelle",
  "discrimination fondée sur le sexe discrimination à l'égard des femmes",
  "GBV",
  "VBG",
  "LGBT",
  "lesbian",
  "lesbienne",
  "gay",
  "homosexual",
  "homosexuel",
  "gynaecology",
  "ginecología",
  "gynécologie",
  "harassment",
  "acoso",
  "harcèlement",
  "heteronormativ",
  "hétéronormativité",
  "married",
  "casada",
  "mariée",
  "masculinity",
  "masculinidad",
  "masculinité",
  "maternal",
  "maternel",
  "menstruation",
  "menstruación",
  "misogyny",
  "misoginia",
  "misogynie",
  "non-binary",
  "no binario",
  "no binaire",
  "obstetric",
  "obstétrique", 
  "obstétrical",
  "patriarchy",
  "patriarcado",
  "patriarcat",
  "period",
  "menstruación",
  "règles",
  "rape",
  "violación sexual",
  "\\bviol\\b",
  "reproductive health",
  "salud reproductiva",
  "santé reproductive", 
  "santé génésique",
  "\\bsex\\",
  "sexo",
  "sexe",
  "sexism",
  "sexual assault",
  "agresión sexual",
  "agression sexuelle",
  "sexual and reproductive health",
  "SRH",
  "ASRH",
  "salud reproductiva",
  "salud sexual",
  "santé sexuelle et reproductive",
  "tampons",
  "tampones",
  "women trafficking",
  "trata de mujeres",
  "traite des femme",
  "trafic des femmes",
  "transsexual",
  "transgender",
  "transgénero",
  "unpaid care work",
  "trabajo doméstico no remunerado",
  "travail de soins non rémunéré", 
  "activités de soins non rémunérées",
  "widow",
  "viuda",
  "vueve",
  "women empowerment",
  "empoderamiento de la mujer",
  "autonomisation des femmes",
  "women's rights",
  "derechos de la mujer",
  "droits des femmes"
)

minor.keywords <- c(
  "female",
  "girl",
  "chica",
  "niña",
  "fille",
  "woman",
  "mujer",
  "mujeres",
  "femme",
  "women",
  "mujeres",
  "femmes"
)

disqualifying.keywords <- c(#
  "\\bmen\\b", 
  "hombres",
  "hommes",
  "\\bman\\b",
  "hombre",
  "homme",
  "\\bboys\\b",
  "chico",
  "niño",
  "garçon",
  "\\bmale\\b."
)
fts$relevance <- "None"

fts[grepl(paste(minor.keywords, collapse = "|"), tolower(paste(fts$description)))]$relevance <- "Minor: Keyword"
fts[grepl(tolower(paste(major.keywords, collapse = "|")), tolower(paste(fts$description)))]$relevance <- "Major: Keyword"
#fts[grepl(paste(disqualifying.keywords, collapse = "|"), tolower(paste(fts$description)))]$relevance <- "disqualifying"

fts[relevance == "Minor"]$check <- "weak keyword"
#fts[relevance != "None"][grepl(paste(disqualifying.keywords, collapse = "|"), tolower(paste(fts[relevance != "None"]$ProjectTitle, fts[relevance != "None"]$description, fts[relevance != "None"]$description)))]$check <- "potential false negative"

fts[, keywordcount := unlist(lapply(tolower(description), function(x) sum(gregexpr(tolower(paste0(major.keywords, collapse = "|")), x)[[1]] > 0, na.rm = T)))]
fts[, disqkeywordcount := unlist(lapply(tolower(description), function(x) sum(gregexpr(tolower(paste0(disqualifying.keywords, collapse = "|")), x)[[1]] > 0, na.rm = T)))]
fts[, minkeywordcount := unlist(lapply(tolower(description), function(x) sum(gregexpr(tolower(paste0(minor.keywords, collapse = "|")), x)[[1]] > 0, na.rm = T)))]

fts[sector == "Protection - Gender-Based Violence"]$relevance <- "Major: GBV"

fts[(relevance == "Minor: Keyword" & minkeywordcount <= disqkeywordcount), relevance := "None"]

#fts <- fts[grepl("Major", relevance) | (relevance == "Minor: Keyword" & minkeywordcount > disqkeywordcount)]

#Global sector assigment
sector.decode <- (fts[!(sector %in% fts$destinationObjects_GlobalCluster.name), .(sector = unique(sector), new_sector = NA_character_)])
sector.decode[grepl("COVID-19", sector), new_sector := "COVID-19"]
sector.decode[grepl("Health", sector), new_sector := "Health"]
sector.decode[grepl("Emergency Livelihoods", sector), new_sector := "Early Recovery"]
sector.decode[grepl("Humanitarian Transportation", sector), new_sector := "Coordination and support"]
sector.decode[grepl("Multisector|All non-COVID|Multi-sector|Refugee|Réfugiés|Multipurpose|Multi Purpose|Multi-Purpose|RMMS", sector, ignore.case = T), new_sector := "Multi-sector"]
sector.decode[grepl("WASH", sector), new_sector := "Water Sanitation Hygiene"]
sector.decode[grepl("Shelter|NFI", sector), new_sector := "Emergency Shelter and NFI"]
sector.decode[grepl("Food Security|Sécurité Alimentaire|^Food$", sector, ignore.case = T), new_sector := "Food Security"]
sector.decode[grepl("Cluster not yet specified", sector, ignore.case = T), new_sector := "Unspecified"]
sector.decode[is.na(new_sector), new_sector := "Other"]

fts[sector %in% sector.decode$sector, sector := merge(fts[sector %in% sector.decode$sector, .(sector)], sector.decode, by = "sector")$new_sector]
fts[sector == "", sector := "Unspecified"]
#fts[grepl("Protection", sector), sector := "Protection"]

write.csv(fts, "fts_output.csv", fileEncoding = "UTF-8", row.names = F)
#