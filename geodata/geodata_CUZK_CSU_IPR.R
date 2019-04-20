library(tidyverse)
library(readxl)
library(foreign)
library(sf)
library(leaflet)
library(plotly)
library(leaflet.extras)

proper_krovak <- "+proj=krovak +lat_0=49.5 +lon_0=24.83333333333333 +alpha=30.28813972222222 +k=0.9999 +x_0=0 +y_0=0 +ellps=bessel +pm=greenwich +units=m +no_defs +towgs84=570.8,85.7,462.8,4.998,1.587,5.261,3.56"
# odsud: http://freegis.fsv.cvut.cz/gwiki/S-JTSK
# protože CSR JTSK je v PROJ4 špatně, chyba je v Praze cca 9 metrů

# http://talk-cz.openstreetmap.narkive.com/WfiEdMIS/volebni-okrsky
# https://lists.openstreetmap.org/pipermail/talk-cz/2016-October/015162.html

# set locale for readr functions
locale("cs", decimal_mark = ",") # no effect here, feed it as value of locale parameter in read_* functions
default_locale() # this is the function for reset

# Pražská open geo data
# http://www.geoportalpraha.cz/cs/opendata#.WP_S11N96Rs

# Volební okrsky - v GML hranice a v sestavách CSV se seznamem a místem budov
# http://vdp.cuzk.cz/vdp/ruian/vymennyformatspecialni/vyhledej?vf.cr=U&_vf.vu=on&vf.vu=VOH&_vf.vu=on&search=Vyhledat
# čísla okrsků jsou stabilní v RUIAN od roku 2014 (volby do EP, krajské, komunální a senátní od 2014 dál)
# http://services.cuzk.cz/sestavy/VO/
# http://www.cuzk.cz/Uvod/Produkty-a-sluzby/RUIAN/Volebni-okrsky-v-RUIAN/Volebni-okrsky-v-RUIAN.aspx
# číselníky všeho ČUZK: http://www.cuzk.cz/Uvod/Produkty-a-sluzby/RUIAN/2-Poskytovani-udaju-RUIAN-ISUI-VDP/Ciselniky-ISUI/Nizsi-uzemni-prvky-a-uzemne-evidencni-jednotky.aspx#UI_ULICE

okrskyP4budovy <- read_excel("~/Documents/Research/Geodata/CUZK_dumps/volebniokrsky/500119-Praha_4/p4.xlsx") %>% # volební okrsky P4 po budovách
  select(sour_x = `Souřadnice X`, sour_y = `Souřadnice Y`, adresa = `Jednořádková adresa`,
         adm = `Kód ADM`, numok = `Číslo volebního okrsku`,
         cdom = `Číslo domovní`, ulice = `Název ulice`) %>% 
  mutate(sour_x = -sour_x, sour_y = -sour_y) %>%
  filter(complete.cases(.)) %>% 
  st_as_sf(coords = c("sour_y","sour_x"),
           crs = 5514)
# plot(okrskyP4budovy, max.plot = 1)
okrskyP4budovy
okrskyP4budovy <- st_transform(okrskyP4budovy, crs = 4326)
okrskyP4budovy

# Vsechny adresy s lokalizací:
# http://nahlizenidokn.cuzk.cz/stahniadresnimistaruian.aspx
adresy_praha <- read_csv2("~/Documents/Research/Geodata/CUZK_dumps/adresy/20170430_OB_554782_ADR.csv.gz",
                          locale = locale(encoding = "WINDOWS-1250", decimal_mark = ".")) %>% 
  filter(`Kód části obce` == 490156)
adresy_vazby_cr <- read_csv2("~/Documents/Research/Geodata/CUZK_dumps/adresy/strukturovane-CSV/adresni-mista-vazby-cr.csv",
                             locale = locale(encoding = "WINDOWS-1250", decimal_mark = ".")) %>% 
  filter(COBCE_KOD == 490156)

# hranice městských částí od IPR
mcP <- st_read("http://opendata.iprpraha.cz/CUR/DTMP/TMMESTSKECASTI_P/S_JTSK/TMMESTSKECASTI_P.json",
               crs = proper_krovak)
mcP <- st_transform(x = mcP, crs = 4326)
plot(mcP[mcP$NAZEV_MC == "Praha 4",], max.plot = 1)

ggp4 <- ggplot(mcP) + geom_sf(aes(fill = KOD_MC)) + coord_sf(datum = 4326, )
ggp4
ggplotly(ggp4, width = 1000, height = 1000)

leaflet() %>%
  setView(lng = 14.4405, lat = 50.08, zoom = 13) %>%
  addTiles(options = tileOptions(detectRetina = T)) %>%
  # addCircleMarkers(data = vop4,
  #            clusterOptions = markerClusterOptions(),
  #            popup = ~as.character(adresa)) %>% 
  # addPolygons(data = mcP, stroke = T, popup = ~NAZEV_1, fill = F, weight = 2) %>% 
  addHeatmap(blur = 20, max = 0.05, radius = 15, data = okrskyP4budovy) %>%
  suspendScroll()

# Vse:
# http://vdp.cuzk.cz/vdp/ruian/vymennyformat/vyhledej

st_layers("~/Documents/Research/Geodata/CUZK_dumps/volebniokrsky/20170403_ST_UVOH.xml") # volební okrsky, všechny
okrCR <- st_read("~/Documents/Research/Geodata/20170403_ST_UVOH.xml", stringsAsFactors = F, crs = proper_krovak)
st_geometry(okrCR) = "OriginalniHranice"
okrCR$geometry  <- okrCR$OriginalniHranice
okrCR <- st_transform(okrCR, crs = 4326)
ggplot(okrCR[okrCR$MomcKod==500119,]) + geom_sf()

# http://vdp.cuzk.cz/vymenny_format/soucasna/20170430_ST_UKSG.xml.gz
st_layers("~/Documents/Research/Geodata/CUZK_dumps/dump/20170430_ST_UKSG.xml.gz") # hranice, vsechny
obce <- st_read("~/Documents/Research/Geodata/CUZK_dumps/dump/20170430_ST_UKSG.xml.gz", layer = "Okresy",
                geometry_column = "OriginalniHranice", stringsAsFactors = F,
                crs = proper_krovak) # hranice, vsechny
obce$geometry <- obce$GeneralizovaneHranice
obce <- st_transform(obce, crs = 4326)
okrgg <- ggplot(obce) + geom_sf()
ggplotly(okrgg)

# Praha 4: kod obce Praha je 554782 (v datech CSU a CUZK ale ne ve volebnich datech)
# kod Momc Praha 4 je 500119 (to je i kod zastupitelstva ve volebnich datech)
# kod casti obce Nusle v ciselniku CSU RSO je 490156
# Část obce-díl "Nusle (Praha 4)" je 400181
# Nas volebni okrsek 4001
# ZUJ 500119
# Kod adresniho mista 21941637 (ADM)
# Kod stavebniho objektu 21775940 (idob)
# Identifikacni cislo parcely 2091820101
# IsknBudovaId: 892288101
# Kod ulice 470945
# jsme v ZSJ "Nuselský obvod" 128198 a statistický obvod číslo 1392 (unikatni cislo 114812)
# ZSJ díl 1281980
# seznam budov v SO : http://apl.czso.cz/irso4/budlist.jsp?b=21&hkodcis=55&kodpol=114812&textokr=Praha&kodcis=55&kod=114812
# Základní sídelní jednotka v Nuslich je geograficky stejná jako "základní sídelní jednotka - díl"
# ale: asi bude užitečnější používat "díl" u ZSJ i u části obce, protože ty kopírují obvod, ne-díly ne.
# Ale: zdá se, že SO se skládají do ZSJ a do částí obce-dílů, ne do ZSJ-dílů a celých částí obce
# (protože za celé části obce a za ZSJ-díly CZSO nedá územní strukturu)

# Obvody a okrsky v Praze 4 komunální volby: http://www.praha4.cz/file/k9p1/Informace-o-poctu-a-sidle-volebnich-okrsku-v-mestske-casti-Praha-4-volby-2014.pdf
# Data lze snad brát odsud: http://apl.czso.cz/irso4/rep1.jsp?kodrep=55 a http://apl.czso.cz/irso4/home.jsp
# a zobrazit na mapě (možná je tam i WMS)
# přes fulltext se dá dostat i na jednotlivé budovy
# Logiku viz na http://apl.czso.cz/irso4/cisel.jsp (jsou tam i volební okrsky!!)

# konverze http://freegis.fsv.cvut.cz/gwiki/RUIAN_/_GDAL 

# http://www.rozhlas.cz/zpravy/data/_zprava/koho-volili-vasi-sousedi-prozkoumejte-nejpodrobnejsi-mapu-s-vysledky-obecnich-voleb--1408350
# http://www.rozhlas.cz/zpravy/data/_zprava/mapa-zajmu-a-nezajmu-o-komunalni-politiku-volebni-ucast-po-okrscich--1407435
# http://mpp.praha.eu/app/map/volebni_mapa/vysledky/index.html

# Ciselnik http://www.cuzk.cz/Uvod/Produkty-a-sluzby/RUIAN/2-Poskytovani-udaju-RUIAN-ISUI-VDP/Ciselniky-ISUI/Atributy-adresniho-mista.aspx#UI_VOLEBNI_OKRSEK
# Odkaz z http://www.cuzk.cz/Uvod/Produkty-a-sluzby/RUIAN/Volebni-okrsky-v-RUIAN/Volebni-okrsky-v-RUIAN.aspx

# Howto on managing multiple geometries: http://freegis.fsv.cvut.cz/gwiki/RUIAN_/_Ruian2gis

# Error described here: http://stackoverflow.com/questions/43656351/st-read-error-null-pointer-returned-by-getgeomfieldref

prahadump <- "~/Documents/Research/Geodata/CUZK_dumps/dump/20170630_OB_554782_UKSH.xml.gz" 

st_layers(prahadump) # megadump CUZK, jen Praha, kompletni
katuzP <- st_read(prahadump, layer = "KatastralniUzemi", geometry_column = 1) # funguje
ulice1 <- st_read(prahadump, layer = "Ulice") # funguje
ulice1 <- st_transform(ulice1, 4326)
obceP <- st_read(prahadump, layer = "Obce", geometry_column = "GeneralizovaneHranice") # funguje
zsjP <- st_read(prahadump, layer = "Zsj") # funguje
momcP <- st_read(prahadump, layer = "Momc") # funguje
mopP <- st_read(prahadump, layer = "Mop") # funguje
spravniobvodyP <- st_read(prahadump, layer = "SpravniObvody") # funguje
castiobciP <- st_read(prahadump, layer = "CastiObci") # funguje
adrmista <- st_read(prahadump, layer = "AdresniMista") # funguje
stavbyP <- st_read(prahadump, layer = "StavebniObjekty") # nefunguje sf 0.5-4
parcelyP <- st_read(prahadump, layer = "Parcely") # nefunguje sf 0.5-4

stavbyP2 <- st_read("~/Downloads/Praha_stavby.shp")
parcelyP2 <- st_read("~/Downloads/Praha_parcely.shp")

adresniMista <- read_csv("~/Documents/Research/Geodata/AdresniMista.csv") %>% # exported by QGIS, works
  filter(!is.na(X) & !is.na(Y)) %>% 
  st_as_sf(coords = c("X","Y"),
           crs = 5514) %>% 
  st_transform(4326)

stavebniObjekty <- read_csv("~/Documents/Research/Geodata/AdresniMista.csv") %>% 
  filter(!is.na(X) & !is.na(Y)) %>% 
  st_as_sf(coords = c("X","Y"),
           crs = 5514) %>% 
  st_transform(4326)

ggplot(adresniMista) + geom_sf()

leaflet() %>%
  setView(lng = 14.4405, lat = 50.08, zoom = 13) %>%
  addTiles(options = tileOptions(detectRetina = T)) %>%
  # addCircleMarkers(data = vop4,
  #            clusterOptions = markerClusterOptions(),
  #            popup = ~as.character(adresa)) %>% 
  # addPolygons(data = okrCR[okrCR$ObecKod==554782,], stroke = T, popup = ~Cislo, fill = F, weight = 2) %>%
  # addPolygons(data = zsjP, stroke = T, fill = F, weight = 2, color = "red") %>% 
  addCircleMarkers(data = adresniMista, clusterOptions = markerClusterOptions()) %>% 
  # addHeatmap(blur = 20, max = 0.05, radius = 15, data = vop4) %>%
  suspendScroll()


zsjP4_csu <- read.dbf("~/Documents/Research/Geodata/CSU/CIS53.dbf") # data o castech obci z CSU, jen P4 - ale malo, jen 100 (asi OK?)
zsjP4_csu <- read.dbf("~/Documents/Research/Geodata/CIS47_latin852.dbf") # data o stat. obvodech z CSU, jen P4
statobvodyP4 <- read.dbf("~/Documents/Research/Geodata/CIS55.dbf") # data o stat. obvodech z CSU, jen P4
sovazbyCR <- read.dbf("~/Documents/Research/Geodata/SO_VAZBY.dbf") # data o stat obvodech z CSU, cela CR
uliceP <- read.dbf("~/Documents/Research/Geodata/CIS66.dbf") # data o stat obvodech z CSU, cela CR
uliceP <- read.dbf("~/Documents/Research/Geodata/CIS66_Praha.dbf") # data o stat obvodech z CSU, cela CR

# v SO_VAZBY jsou souradnice stredu okrsku ("SOJTSK_X a SOJTSK_Y"), ale ne hranice
# toto by teoreticky mohlo jít vykreslit přes budovy/adresní místa (jejich souřadnice jsou v
# megadumpu volebních okrsků z CZSO a adresních míst CUZK a možná i v nějakém jiném, např. tom výše)
# u SO známe střed, výměru, počet domů, bytů a obyvatel
# u domů můžeme znát místo a scrape počet obyvatel

# XML prehled SO http://report.czso.cz/reports/rwservlet?popisso&p_sort=1&p_idso=114812&p_datpohl=01032017&desformat=XML&mimetype=text/plain%0d%0aContent-disposition:attachment;filename=%22popisso_114812.xml%22
# via http://apl.czso.cz/irso4/rep1.jsp 
# obsahuje info o budovách včetně přesného počtu pater, bytů a obyvatel
# což je i v seznamu budov za část obce díl
# http://report.czso.cz/reports/rwservlet?irso_cobcedil_stru&p_datpohl=01032017&p_castobce=400181&p_sort=1&desformat=XML
# popř. za ZSJ
# http://report.czso.cz/reports/rwservlet?irso_zsj_stru&p_datpohl=01032017&p_zsj=128198&desformat=XML
# ale za ZSJ to nechce dát XML!! a v PDF je všechno sečtené podle SO
# (jsou tam jen hlavní adresy ke každé budově, takže non-nested; a je tam IDOB takže napojitelné)
# kdežto na stránce budovy jsou jen rozpětí:

# seznam budov v okrsku: http://apl.czso.cz/irso4/budlist.jsp?kodcis=55&kod=114812
#     (ve skutečnosti seznam adres, ale odkazy vedou na budovu, adresy je potřeba vzít z dalšího odkazu)
# vše o budově: http://apl.czso.cz/irso4/buddet.jsp?budid=685096&idob=1000153843
#     verze pro tisk http://apl.czso.cz/irso4/buddettisk.jsp?budid=685096&idob=1000153843
# seznam adres na budově a detaily: http://apl.czso.cz/irso4/budadr.jsp?budid=685096&idob=1000153843,
#     verze pro tisk: http://apl.czso.cz/irso4/budadrtisk.jsp?budid=685096&idob=1000153843
#     obsahuje i kód adresy v RUIAN

#### Recykly IPR 

recykly <- st_read("http://opendata.iprpraha.cz/CUR/ZPK/ZPK_O_Kont_TOitem_b/WGS_84/ZPK_O_Kont_TOitem_b.json")

paleta <- colorFactor("Set2", as.factor(recykly$TRASHTYPENAME), levels = NULL, ordered = FALSE,
                      na.color = "#808080", alpha = FALSE, reverse = FALSE)

leaflet() %>% 
  setView(lng = 14.4405, lat = 50.08, zoom = 13) %>%
  addTiles(options = tileOptions(detectRetina = T)) %>%
  addProviderTiles(provider = "Stamen.TonerLite", options = tileOptions(detectRetina = T)) %>%
  addCircleMarkers(data = recykly, stroke = F, radius = 6, fillColor = ~paleta(as.factor(TRASHTYPENAME)),
                   fillOpacity = 0.4) %>% 
  addLegend(pal = paleta(~as.factor(TRASHTYPENAME)), values)

# try plotly with frame argument and ggplot + geom_sf to animate map?
