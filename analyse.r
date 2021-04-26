#------------------------------------------------------------------
# Laster inn data ----
#------------------------------------------------------------------

# Laster inn libraries
library(dplyr)
library(tibble)
library(tidyr)
library(readr)
library(ggplot2)
library(vroom)
library(stringr)
library(broom)
library(nnet)
library(labelVector)
library(sjPlot)
library(ggeffects)
library(jtools)
library(car)
library(ggcorrplot)
library(gmodels)
library(rcompanion)

## Path til dataene
path <- "PATH TIL DATA"

## Laster inn data
pfil <- as_tibble(vroom(str_c(path, "PERSONFIL"), col_types = cols()))
rfil <- as_tibble(vroom(str_c(path, "REISEFIL"), col_types = cols()))


#------------------------------------------------------------------
# Trekker utvalg og variabler ----
#------------------------------------------------------------------

# Reduserer til ønskede byområder
region <- as_tibble(vroom(str_c(path, "storbyregioner.csv"), col_types = cols())) %>% 
  filter(knavn %in% c("Trondheim", "Bergen", "Stavanger", "Sandnes", "Oslo"))

# Bor i valgt storby, sammen med ektefelle/samboer eller barn
p_byer <- pfil %>% 
  filter(is.element(B_komnr, region$komnr)) %>% # region
  filter(Spm81_antpers > 1) %>%  # Luker ut enslige
  filter(Spm83_1_person1_slekt == 1 | Spm83_1_person1_slekt == 2 | is.na(Spm83_1_person1_slekt)) %>% 
  filter(Spm83_2_person2_slekt == 1 | Spm83_2_person2_slekt == 2 | is.na(Spm83_2_person2_slekt)) %>%
  filter(Spm83_3_person3_slekt == 1 | Spm83_3_person3_slekt == 2 | is.na(Spm83_3_person3_slekt)) %>%
  filter(Spm83_4_person4_slekt == 1 | Spm83_4_person4_slekt == 2 | is.na(Spm83_4_person4_slekt)) %>%
  filter(Spm83_5_person5_slekt == 1 | Spm83_5_person5_slekt == 2 | is.na(Spm83_5_person5_slekt)) %>% 
  filter(Spm83_6_person6_slekt == 1 | Spm83_6_person6_slekt == 2 | is.na(Spm83_6_person6_slekt)) %>%
  filter(Spm83_7_person7_slekt == 1 | Spm83_7_person7_slekt == 2 | is.na(Spm83_7_person7_slekt)) %>%
  filter(Spm83_8_person8_slekt == 1 | Spm83_8_person8_slekt == 2 | is.na(Spm83_8_person8_slekt)) %>%
  filter(Spm83_9_person9_slekt == 1 | Spm83_9_person9_slekt == 2 | is.na(Spm83_9_person9_slekt)) %>% 
  filter(Spm83_10_person10_slekt == 1 | Spm83_10_person10_slekt == 2 | is.na(Spm83_10_person10_slekt)) %>%
  filter(Spm83_11_person11_slekt == 1 | Spm83_11_person11_slekt == 2 | is.na(Spm83_11_person11_slekt)) %>%
  filter(Spm83_12_person12_slekt == 1 | Spm83_12_person12_slekt == 2 | is.na(Spm83_12_person12_slekt)) %>% 
  filter(Spm83_13_person13_slekt == 1 | Spm83_13_person13_slekt == 2 | is.na(Spm83_13_person13_slekt)) %>%
  filter(Spm83_14_person14_slekt == 1 | Spm83_14_person14_slekt == 2 | is.na(Spm83_14_person14_slekt)) %>%
  filter(Spm83_15_person15_slekt == 1 | Spm83_15_person15_slekt == 2 | is.na(Spm83_15_person15_slekt))

# Luker ut personer som bare bor sammen med barna sine uten ektefelle/samboer
p_byer <- p_byer %>% 
  filter(!(Spm81_antpers >= 2 & Spm83_1_person1_slekt == 2))

# Filtrerer ut respondenter under 18 år
p_byer <- p_byer %>% 
  filter(Alder >= 18)

# Legger til informasjon om kommunenavn
p_byer <- left_join(p_byer, region, by = c("B_komnr" = "komnr"))


# Definerer variabler vi skal ha med oss videre
var_reis <- c("Intnr", "Reisenr", "Trmh", "Registreringsdato", "Ukedag", "Mned",
              "Korr_lengde", "Korr_tid","Spm47_reiseforml", "Start", "Ende")

var_pers <- c("Intnr", "B_komnr", "knavn", "Region", "Kjonn", "Alder",
              "Spm105_utdanning", "Spm81_antpers", 
              "Spm83_1_person1_slekt", "Spm82_1_person1_alder",
              "Spm83_2_person2_slekt", "Spm82_2_person2_alder",
              "Spm83_3_person3_slekt", "Spm82_3_person3_alder",
              "Spm83_4_person4_slekt", "Spm82_4_person4_alder",
              "Spm83_5_person5_slekt", "Spm82_5_person5_alder",
              "Spm83_6_person6_slekt", "Spm82_6_person6_alder",
              "Spm83_7_person7_slekt", "Spm82_7_person7_alder",
              "Spm83_8_person8_slekt", "Spm82_8_person8_alder",
              "Spm83_9_person9_slekt", "Spm82_9_person9_alder",
              "Spm83_10_person10_slekt", "Spm82_10_person10_alder",
              "Spm83_11_person11_slekt", "Spm82_11_person11_alder",
              "Spm83_12_person12_slekt", "Spm82_12_person12_alder",
              "Spm83_13_person13_slekt", "Spm82_13_person13_alder",
              "Spm83_14_person14_slekt", "Spm82_14_person14_alder",
              "Spm83_15_person15_slekt", "Spm82_15_person15_alder",
              "Spm102_inntekt_egen2", "Spm104_inntekt_hushold2", "Spm21_fkort",
              "Spm23_bileierskap", "Spm25_bilantall", "Spm430_bilmulighet", 
              "Spm91_4_tilgang_sykkel",
              "Spm91_3_tilgang_elsykkel", "Spm95_p_bolig", "Spm97_p_ledig",
              "Spm78_p_jobb", "Spm79_p_ledig", "Spm98_bolig_holdeplass",
              "Spm432_kollkort", "Spm433_korttype", "Spm910_kolltilbud_morgen",
              "Spm99_kolltilbud_dagtid", "Spm31_hbeskj", "Spm32_arbeid" ,
              "Spm33_arbeidstimer", "Spm63_arbeidsordning", "Spm86_samboer_jobb",
              "Spm87_samboer_timer", "Spm106_bevegelsesproblem", "Utvalgsvekt", 
              "Spm76_tidtiljobb_bil", "Spm77_tidtiljobb_koll", 
              "Spm710_p_betaling", "Spm34_oppmte")

# Forkorter reisefil til bare ? inneholder individer fra utvalg og variabler
r_var <- rfil %>% 
  filter(is.element(Intnr, p_byer$Intnr)) %>% 
  select(all_of(var_reis))

# Forkorter personfil til ? bare inneholde variabler vi er ute etter
p_sam <- p_byer %>% 
  select(all_of(var_pers))

#-----------------------------------------------------------------------
## Oppretter dummy for reise-fil ----
# ----------------------------------------------------------------------
# Oppretter dummy for virkedag
r_var <- r_var %>% 
  mutate(virkedag = 0) %>% 
  mutate(virkedag = case_when(
    Ukedag == 1 ~ 1,
    Ukedag == 2 ~ 1,
    Ukedag == 3 ~ 1,
    Ukedag == 4 ~ 1,
    Ukedag == 5 ~ 1,
    TRUE ~ as.numeric(virkedag)))


# Reise utført i vintersesong, baserer meg på månedene det er tillatt å bruke
# piggdekk for å identifisere vintermåneder: 1. november til første søndag
# etter andre påskedag. Velger derfor november til og med april som vinter.
r_var <- r_var %>% 
  mutate(maned = as.numeric(vapply(strsplit(Registreringsdato, "/"), 
                                   `[`, 1, FUN.VALUE = character(1)))) %>% 
  mutate(vinter = 0) %>% 
  mutate(vinter = case_when(
    maned %in% c(1, 2, 3, 4, 11, 12) ~ 1,
    TRUE ~ as.numeric(vinter)))

# Til fots
r_var <- r_var %>% 
  mutate(til_fots = 0) %>% 
  mutate(til_fots = case_when(
    Trmh == 1 ~ 1,
    TRUE ~ as.numeric(til_fots)))

# Syklet (inkluderer el-sykkel)
r_var <- r_var %>% 
  mutate(syklet = 0) %>% 
  mutate(syklet = case_when(
    Trmh == 2 ~ 1,
    Trmh == 3 ~ 1,
    TRUE ~ as.numeric(syklet)))

# Bilforer
r_var <- r_var %>% 
  mutate(bil_forer = 0) %>% 
  mutate(bil_forer = case_when(
    Trmh == 6 ~ 1,
    TRUE ~ as.numeric(bil_forer)))

# Bilpassasjer
r_var <- r_var %>% 
  mutate(bil_passasjer = 0) %>% 
  mutate(bil_passasjer = case_when(
    Trmh == 7 ~ 1,
    TRUE ~ as.numeric(bil_passasjer)))

# Buss
r_var <- r_var %>% 
  mutate(buss = 0) %>% 
  mutate(buss = case_when(
    Trmh == 9 ~ 1,
    TRUE ~ as.numeric(buss)))

# Kollektiv - buss, trikk/bybane og T-bane.
# MERK: inkluderer her også tog, nr 13
r_var <- r_var %>% 
  mutate(kollektiv = 0) %>% 
  mutate(kollektiv = case_when(
    Trmh == 9 ~ 1,
    Trmh == 11 ~ 1,
    Trmh == 12 ~ 1,
    Trmh == 13 ~ 1,
    TRUE ~ as.numeric(kollektiv)))

# Oppretter en faktor for alle reisemiddelvalg
r_var <- r_var %>% 
  mutate(reisemiddel = 0) %>% 
  naniar::replace_with_na(replace = list(reisemiddel = 0)) %>%
  mutate(reisemiddel = case_when(
    bil_forer == 1 ~ "bil",
    syklet == 1 ~ "syklet",
    til_fots == 1 ~ "gange",
    kollektiv == 1 ~ "kollektiv",
    TRUE ~ as.character(reisemiddel)))

# Må gjøre korr_lengde til numerisk variabel, med null foran komma
r_var$Korr_lengde <- as.numeric(gsub(",", ".", gsub("\\.", "", r_var$Korr_lengde)))

# Oppretter dummy for reiser kortere enn 5 km
r_var <- r_var %>% 
  mutate(under_5km = 0) %>% 
  mutate(under_5km = if_else(Korr_lengde <= 5, 1, 0))

# Oppretter string for reisemiddelvalg
r_var$Trmh_ord <- r_var$Trmh

r_var <- r_var %>% 
  mutate(Trmh_ord = case_when(.$Trmh == 1 ~ "Til Fots",
                              .$Trmh == 2 ~ "Sykkel/Elsykkel",
                              .$Trmh == 3 ~ "Sykkel/Elsykkel",
                              .$Trmh == 4 ~ "Moped/Motorsykkel",
                              .$Trmh == 5 ~ "Moped/Motorsykkel",
                              .$Trmh == 6 ~ "Bil, forer",
                              .$Trmh == 7 ~ "Bil, passasjer",
                              .$Trmh == 8 ~ "Annet",
                              .$Trmh == 9 ~ "Buss/rutebil/ekspressbuss i rute",
                              .$Trmh == 10 ~ "Annet",
                              .$Trmh == 11 ~ "Trikk/bybane",
                              .$Trmh == 12 ~ "T-bane",
                              .$Trmh == 13 ~ "Tog",
                              .$Trmh == 14 ~ "Annet",
                              .$Trmh == 15 ~ "Annet",
                              .$Trmh == 16 ~ "Annet",
                              .$Trmh == 17 ~ "Annet",
                              .$Trmh == 18 ~ "Annet",
                              .$Trmh == 19 ~ "Annet",
                              .$Trmh == 20 ~ "Annet",
                              .$Trmh == 21 ~ "Annet",
                              .$Trmh == 22 ~ "Annet",
                              .$Trmh == 24 ~ "Vil ikke svare/vet ikke",
                              TRUE ~ as.character(Trmh_ord)))

# Oppretter string for variabel Spm47_reiseforml
r_var$Spm47_reiseforml_ord <- r_var$Spm47_reiseforml

r_var <- r_var %>% 
  mutate(Spm47_reiseforml_ord = case_when(.$Spm47_reiseforml == 1 ~ "Arbeidsreise",
                                          .$Spm47_reiseforml == 2 ~ " Skole",
                                          .$Spm47_reiseforml == 3 ~ "Tjenestereise",
                                          .$Spm47_reiseforml == 4 ~ "Innkjop av dagligvarer",
                                          .$Spm47_reiseforml == 5 ~ "Andre innkjop",
                                          .$Spm47_reiseforml == 6 ~ "Annet",
                                          .$Spm47_reiseforml == 7 ~ "Annet",
                                          .$Spm47_reiseforml == 8 ~ "Folge/hente/bringe barn til/fra barnehage, park, dagmamma eller skole",
                                          .$Spm47_reiseforml == 9 ~ "Folge/hente/bringe barn til/fra sports- og fritidsaktiviteter",
                                          .$Spm47_reiseforml == 10 ~ "Folge/hente/bringe barn eller andre til ulike aktiviteter",
                                          .$Spm47_reiseforml == 11 ~ "Besok (private besok hos familie og venner",
                                          .$Spm47_reiseforml == 12 ~ "Kultureaktiviteter",
                                          .$Spm47_reiseforml == 13 ~ "Kafe, restaurant, pub mv",
                                          .$Spm47_reiseforml == 14 ~ "Fotballkamp, sportsarrangement mv som tilskuer",
                                          .$Spm47_reiseforml == 15 ~ "Organiserte fritidsaktiviteter; musikk,idrett, trening",
                                          .$Spm47_reiseforml == 16 ~ "Gikk/syklet/jogget en tur/skitur/luftet hund",
                                          .$Spm47_reiseforml == 17 ~ "Annet",
                                          .$Spm47_reiseforml == 18 ~ "Annet",
                                          .$Spm47_reiseforml == 19 ~ "Annet",
                                          .$Spm47_reiseforml == 20 ~ "Annet",
                                          .$Spm47_reiseforml == 21 ~ "Annet",
                                          TRUE ~ as.character(Spm47_reiseforml_ord)))

# Oppretter string for variabel Start
r_var$Start_ord <- r_var$Start

r_var <- r_var %>% 
  mutate(Start_ord = case_when(.$Start == 1 ~ "Eget bosted",
                               .$Start == 3 ~ "Egen arbeidsplass",
                               .$Start == 4 ~ "Skole/studiested",
                               .$Start == 5 ~ "Annet sted",
                               .$Start == 6 ~ "Sverige",
                               .$Start == 7 ~ "Danmark",
                               .$Start == 8 ~ "Finland",
                               .$Start == 9 ~ "Utlandet",
                               TRUE ~ as.character(Start_ord)))

# Oppretter string for variabel Ende
r_var$Ende_ord <- r_var$Ende

r_var <- r_var %>% 
  mutate(Ende_ord = case_when(.$Ende == 1 ~ "Eget bosted",
                              .$Ende == 3 ~ "Egen arbeidsplass",
                              .$Ende == 4 ~ "Skole/studiested",
                              .$Ende == 5 ~ "Annet sted",
                              .$Ende == 6 ~ "Sverige",
                              .$Ende == 7 ~ "Danmark",
                              .$Ende == 8 ~ "Finland",
                              .$Ende == 9 ~ "Utlandet",
                              TRUE ~ as.character(Ende_ord)))

#------------------------------------------------------------------
# Oppretter dummyvariabler for alder på barn ----
#------------------------------------------------------------------

# Oppretter dummy for barn i f?rskolealder >= 6 ?r
p_sam <- p_sam %>%
  mutate(forskole_alder = 0) %>% 
  mutate(forskole_alder = case_when(
    Spm82_2_person2_alder <= 6 ~ 1,
    Spm82_3_person3_alder <= 6 ~ 1,
    Spm82_4_person4_alder <= 6 ~ 1,
    Spm82_5_person5_alder <= 6 ~ 1,
    Spm82_6_person6_alder <= 6 ~ 1,
    Spm82_7_person7_alder <= 6 ~ 1,
    TRUE ~ as.numeric(forskole_alder))) 

# Opprett dummy grunnskole (7-15) og videreg?ende (16-18)
grunnskl <- c(7, 8, 9, 10, 11, 12, 13, 14, 15)
barneskole <- c(7, 8, 9, 10, 11, 12)
vidskl <- c(16, 17, 18)
toddlr <- c(0,1,2,3)
toddlr_eld <- c(4,5,6)

p_sam <- p_sam %>% 
  mutate(grunnskole_alder = 0) %>% 
  mutate(grunnskole_alder = case_when(
    Spm82_2_person2_alder %in% grunnskl ~ 1,
    Spm82_3_person3_alder %in% grunnskl ~ 1,
    Spm82_4_person4_alder %in% grunnskl ~ 1,
    Spm82_5_person5_alder %in% grunnskl ~ 1,
    Spm82_6_person6_alder %in% grunnskl ~ 1,
    Spm82_7_person7_alder %in% grunnskl ~ 1,
    TRUE ~ as.numeric(grunnskole_alder))) %>% 
  mutate(barneskole_alder = 0) %>% 
  mutate(barneskole_alder = case_when(
    Spm82_2_person2_alder %in% barneskole ~ 1,
    Spm82_3_person3_alder %in% barneskole ~ 1,
    Spm82_4_person4_alder %in% barneskole ~ 1,
    Spm82_5_person5_alder %in% barneskole ~ 1,
    Spm82_6_person6_alder %in% barneskole ~ 1,
    Spm82_7_person7_alder %in% barneskole ~ 1,
    TRUE ~ as.numeric(barneskole_alder))) %>% 
  mutate(videregaaende_alder = 0) %>% 
  mutate(videregaaende_alder = case_when(
    Spm82_2_person2_alder %in% vidskl ~ 1,
    Spm82_3_person3_alder %in% vidskl ~ 1,
    Spm82_4_person4_alder %in% vidskl ~ 1,
    Spm82_5_person5_alder %in% vidskl ~ 1,
    Spm82_6_person6_alder %in% vidskl ~ 1,
    Spm82_7_person7_alder %in% vidskl ~ 1,
    TRUE ~ as.numeric(videregaaende_alder)))

# Oppretter dummy for å ha barn
p_sam <- p_sam %>% 
  mutate(har_barn = 0) %>% 
  mutate(har_barn = case_when(
    forskole_alder == 1 ~ 1,
    grunnskole_alder == 1 ~ 1,
    videregaaende_alder == 1 ~ 1,
    TRUE ~ as.numeric((har_barn))))

# Oppretter dummy for å ha barn både i forskolealder og grunnskolealder
p_sam <- p_sam %>% 
  mutate(forskole_grunnskole = 0) %>% 
  mutate(forskole_grunnskole = case_when(
    (forskole_alder == 1 & grunnskole_alder == 1) ~ 1,
    TRUE ~ as.numeric(forskole_grunnskole)))

# Oppretter dummy for å ha barn i bare forskolealder eller grunnskolealder
p_sam <- p_sam %>% 
  mutate(bare_forskolealder = 0) %>% 
  mutate(bare_forskolealder = case_when(
    (forskole_alder == 1 & forskole_grunnskole == 0 & videregaaende_alder == 0) ~ 1,
    TRUE ~ as.numeric(bare_forskolealder)))

p_sam <- p_sam %>% 
  mutate(bare_grunnskolealder = 0) %>% 
  mutate(bare_grunnskolealder = case_when(
    (grunnskole_alder == 1 & forskole_grunnskole == 0 & videregaaende_alder == 0) ~ 1,
    TRUE ~as.numeric(bare_grunnskolealder)))

# Oppretter dummy for å ha barn i bare videregaaende alder
p_sam <- p_sam %>% 
  mutate(bare_videregaaendealder = 0) %>% 
  mutate(bare_videregaaendealder = case_when(
    (videregaaende_alder == 1 & forskole_alder == 0 & grunnskole_alder == 0) ~ 1,
    TRUE ~ as.numeric(bare_videregaaendealder)))


# ------------------------------------------------------------------------
# Trekker utvalg ----
# ------------------------------------------------------------------------

# Kombinerer reise- og personfil ----
rp_komb <- left_join(r_var, p_sam, by = "Intnr")


# Trekker utvalg, personer i inntektsgivende arbeid som bor sammen med
# partner/samboer i inntektsgivende arbeid som har utført reise på
# en virkedag, og der alle barn er i alderen mellom 0-15 år
rp_komb <- rp_komb %>% 
  filter(Spm32_arbeid == 1, Spm86_samboer_jobb == 1, virkedag == 1) %>% 
  filter(videregaaende_alder == 0)

# Setter personfil og reisefil til å inneholde de samme individene som utvalg
p_sam <- p_sam %>% 
  filter(is.element(Intnr, rp_komb$Intnr))

r_var <- r_var %>% 
  filter(is.element(Intnr, rp_komb$Intnr))



# ------------------------------------------------------------------------
# Lager dummyvariabler for personfil ----
# ------------------------------------------------------------------------


# Sammenligner NA for Spm33_arbeidstimer og Spm63_arbeidsordning med 
# Spm31_hbeskj og Spm32_arbeid
# Har NA på grunn av at de ikke har inntektsgivende arbeid, 
# omkoder til 0 timer arbeid for Spm33_arbeidstimer
p_sam <- p_sam %>% 
  mutate(Spm33_arbeidstimer = case_when(
    is.na(Spm33_arbeidstimer) ~ 0,
    TRUE ~ as.numeric(Spm33_arbeidstimer)))

# Oppretter dummy for ? v?re kvinne
p_sam <- p_sam %>% 
  mutate(kvinne = 0) %>% 
  mutate(kvinne = case_when(
    Kjonn == 2 ~ 1,
    TRUE ~ as.numeric(kvinne)))

# Oppretter beskrivende kategorier for høyeste fullførte utdannelse
# Slår sammen grunnskole og videregående til en kategori
p_sam <- p_sam %>% 
  mutate(Spm105_utdanning_ord = 0) %>% 
  naniar::replace_with_na(replace = list(Spm105_utdanning_ord = 0)) %>% 
  mutate(Spm105_utdanning_ord = case_when(
    Spm105_utdanning == 1 ~ "Videregaaende",
    Spm105_utdanning == 2 ~ "Videregaaende",
    Spm105_utdanning == 3 ~ "uni_lav",
    Spm105_utdanning == 4 ~ "uni_hoy",
    TRUE ~ as.character(Spm105_utdanning_ord)))

# Oppretter dummy for universitetsutdannelse eller tilsvarende
# Høyskole/universitet, til og med 4 år, eller 5 år og mer
p_sam <- p_sam %>% 
  mutate(hoyere_utdannelse = 0) %>% 
  mutate(hoyere_utdannelse = case_when(
    Spm105_utdanning == 3 ~ 1,
    Spm105_utdanning == 4 ~ 1,
    TRUE ~ as.numeric(hoyere_utdannelse)))

# Transformerer har førerkort til 0 = eier ikke førerkort
p_sam <- p_sam %>% 
  mutate(Spm21_fkort = case_when(
    Spm21_fkort == 2 ~ 0,
    TRUE ~ as.numeric(Spm21_fkort)))

# Transformerer Spm23_bileierskap til 0 = husholdning disponerer ikke bil
p_sam <- p_sam %>% 
  mutate(Spm23_bileierskap = case_when(
    Spm23_bileierskap == 2 ~ 0,
    TRUE ~ as.numeric(Spm23_bileierskap)))

# Spm25_bilantall har 347 NA, sammenlinger med Spm23_bileierskap
# Er NA, fordi de ikke eier bil, omkoder derfor NA til 0
p_sam <- p_sam %>% 
  mutate(Spm25_bilantall = case_when(
    Spm23_bileierskap == 0 ~ 0,
    TRUE ~ as.numeric(Spm25_bilantall)))

# Oppretter kateogrivariabel for å ha ingen, en eller to eller flere biler
p_sam <- p_sam %>% 
  mutate(bil_antall = 0) %>% 
  naniar::replace_with_na(replace = list(bil_antall = 0)) %>% 
  mutate(bil_antall = case_when(
    Spm23_bileierskap == 0 ~ "Ingen bil",
    Spm25_bilantall == 1 ~ "En bil",
    Spm25_bilantall >= 2 ~ "To eller flere",
    TRUE ~ as.character(bil_antall)))

# Omkoder fra NA til 3 for Spm430_bilmulighet. Har NA fordi personen ikke eier bil
#(n = 369) eller har førerkort (n = 90)
p_sam <- p_sam %>% 
  mutate(Spm430_bilmulighet = case_when(
    Spm23_bileierskap == 0 ~ 3,
    Spm21_fkort == 0 ~ 3,
    TRUE ~ as.numeric(Spm430_bilmulighet)))

# Oppretter faktor fra Spm430_bilmulighet
p_sam <- p_sam %>% 
  mutate(Spm430_bilmulighet_ord = 0) %>%
  naniar::replace_with_na(replace = list(Spm430_bilmulighet_ord = 0)) %>% 
  mutate(Spm430_bilmulighet_ord = case_when(
    Spm430_bilmulighet == 1 ~ "Hele dagen",
    Spm430_bilmulighet == 2 ~ "Deler av dagen",
    Spm430_bilmulighet == 3 ~ "Ikke tilgang til bil",
    TRUE ~ as.character(Spm430_bilmulighet_ord)))

# Transformerer dummy for å ha parkeringsplass ved arbeid
# Omkoder 3 ("vet ikke") til nei (93 tilfeller)
p_sam <- p_sam %>% 
  mutate(Spm78_p_jobb = case_when(
    Spm78_p_jobb == 2 ~ 0,
    Spm78_p_jobb == 3 ~ 0,
    Spm23_bileierskap == 0 ~ 0, # Eier ikke bil, ikke mulighet til å parkere
    TRUE ~ as.numeric(Spm78_p_jobb)))

# Spm78_p_jobb har NA på grunn av svarer "Nei" eller "vet ikke" på om
# har fast oppmøtested på jobben, Spm34_oppmte. Endrer til ikke tilgang til
# parkering for disse (n = 289)
p_sam <- p_sam %>% 
  mutate(Spm78_p_jobb = case_when(
    Spm34_oppmte == 2 ~ 0,
    Spm34_oppmte == 3 ~ 0,
    TRUE ~ as.numeric(Spm78_p_jobb)))

# Endrer koding for å betale for parkering ved arbeidsplass. Setter "vet ikke"
# til "nei"
p_sam <- p_sam %>% 
  mutate(Spm710_p_betaling = case_when(
    Spm710_p_betaling == 1 ~ 1,
    Spm710_p_betaling == 2 ~ 0,
    Spm710_p_betaling == 3 ~ 0,
    TRUE ~ as.numeric(Spm710_p_betaling)))

# Oppretter faktor for tilgang til parkering på jobb
p_sam <- p_sam %>% 
  mutate(p_jobb = 0) %>% 
  naniar::replace_with_na(replace = list(p_jobb = 0)) %>% 
  mutate(p_jobb = case_when(
    Spm78_p_jobb == 0 ~ "Ikke tilgang til parkering ved jobb",
    Spm78_p_jobb == 1 & Spm710_p_betaling == 0 ~ "Gratis parkering ved jobb",
    Spm78_p_jobb == 1 & Spm710_p_betaling == 1 ~ "Betaler for parkerings ved jobb",
    TRUE ~ as.character(p_jobb)))

# Oppretter dummy for å ha tilgang til både sykkel og el-sykkel
p_sam <- p_sam %>% 
  mutate(har_sykkel_elsykkel = 0) %>% 
  mutate(har_sykkel_elsykkel = case_when(
    (Spm91_4_tilgang_sykkel == 1 | Spm91_3_tilgang_elsykkel == 1) ~ 1,
    TRUE ~ as.numeric(har_sykkel_elsykkel)))

# Transformerer dummy for å ha inntektsgivende arbeid
p_sam <- p_sam %>% 
  mutate(Spm32_arbeid = case_when(
    Spm32_arbeid == 2 ~ 0,
    TRUE ~ as.numeric(Spm32_arbeid)))

# Oppretter variabel for om respondenten arbeider 100%, mellom 50-100% eller
# mindre enn 50 %
p_sam <- p_sam %>% 
  mutate(stillingsprosent = 0) %>% 
  naniar::replace_with_na(replace = list(stillingsprosent = 0)) %>% 
  mutate(stillingsprosent = case_when(
    Spm33_arbeidstimer >= 37 ~ "Fulltidsstilling",
    (Spm33_arbeidstimer < 37 & Spm33_arbeidstimer >= 19) ~ "100%-50%",
    (Spm33_arbeidstimer < 19 & Spm33_arbeidstimer > 0) ~ "Under 50%",
    TRUE ~ as.character(stillingsprosent)))

# Oppretter variabel for om respondenten arbeider full stilling eller deltid
p_sam <- p_sam %>% 
  mutate(deltid = 0) %>% 
  naniar::replace_with_na(replace = list(deltid = 0)) %>% 
  mutate(deltid = case_when (
    Spm33_arbeidstimer >= 37 ~ 0,
    (Spm33_arbeidstimer < 37 & Spm33_arbeidstimer > 0) ~ 1,
    TRUE ~ as.numeric(deltid)))


# Transformerer dummy for om ektefelle/samboer har arbeid
p_sam <- p_sam %>% 
  mutate(Spm86_samboer_jobb = case_when(
    Spm86_samboer_jobb == 2 ~ 0,
    TRUE ~ as.numeric(Spm86_samboer_jobb)))


# Transformerer dummy for å ha parkeringsplass ved boligen
# Omkoder 3 ("vet ikke") til nei (9 tilfeller)
p_sam <- p_sam %>% 
  mutate(Spm95_p_bolig = case_when(
    Spm95_p_bolig == 2 ~ 0,
    Spm95_p_bolig == 3 ~ 0,
    TRUE ~ as.numeric(Spm95_p_bolig)))

# Transformerer dummy for å ha periodekort for kollektiv-transport
# Koder "vet ikke" (7 personer) til nei
p_sam <- p_sam %>% 
  mutate(Spm432_kollkort = case_when(
    Spm432_kollkort == 2 ~ 0,
    Spm432_kollkort == 3 ~ 0,
    TRUE ~ as.numeric(Spm432_kollkort)))

# Omkoder "vet ikke" (n = 328) for Spm99_kolltilbud_dagtid til gjennomsnittsverdi
# (= 4.147). Runder ned til 4.
p_sam <- p_sam %>% 
  mutate(Spm99_kolltilbud_dagtid_org = Spm99_kolltilbud_dagtid) %>% 
  mutate(Spm99_kolltilbud_dagtid = case_when(
    Spm99_kolltilbud_dagtid == 9 ~ 4,
    TRUE ~ as.numeric(Spm99_kolltilbud_dagtid)))

# Oppretter dummy for kvalitet på kollektivtrafikk
p_sam <- p_sam %>% 
  mutate(kvalitet_kollektiv = 0) %>% 
  naniar::replace_with_na(replace = list(kvalitet_kollektiv = 0)) %>% 
  mutate(kvalitet_kollektiv = case_when(
    Spm98_bolig_holdeplass <= 500 & Spm99_kolltilbud_dagtid %in% c(1,2) ~ 1,
    Spm98_bolig_holdeplass <= 500 & Spm99_kolltilbud_dagtid %in% c(3,4) ~ 2,
    Spm98_bolig_holdeplass <= 500 & Spm99_kolltilbud_dagtid %in% c(5) ~ 3,
    Spm98_bolig_holdeplass <= 500 & Spm99_kolltilbud_dagtid %in% c(6,7,8) ~ 4,
    (Spm98_bolig_holdeplass > 500 & Spm98_bolig_holdeplass <= 1000) & (Spm99_kolltilbud_dagtid %in% c(1,2)) ~ 2,
    (Spm98_bolig_holdeplass > 500 & Spm98_bolig_holdeplass <= 1000) & (Spm99_kolltilbud_dagtid %in% c(3,4)) ~ 3,
    (Spm98_bolig_holdeplass > 500 & Spm98_bolig_holdeplass <= 1000) & (Spm99_kolltilbud_dagtid %in% c(5)) ~ 4,
    (Spm98_bolig_holdeplass > 500 & Spm98_bolig_holdeplass <= 1000) & (Spm99_kolltilbud_dagtid %in% c(6,7,8)) ~ 5,
    Spm98_bolig_holdeplass > 1000 ~ 5,
    TRUE ~ as.numeric(kvalitet_kollektiv)))


# Oppretter beskrivende kategorier for spm31_hbeskj - hovedbeskjeftigelse
p_sam <- p_sam %>% 
  mutate(Spm31_hbeskj_ord = 0) %>%
  naniar::replace_with_na(replace = list(Spm31_hbeskj_ord = 0)) %>% 
  mutate(Spm31_hbeskj_ord = case_when(
    Spm31_hbeskj == 1 ~ "Yrkesaktiv",
    Spm31_hbeskj == 2 ~ "Hjemmevearende",
    Spm31_hbeskj == 3 ~ "Skole eller studerer",
    Spm31_hbeskj == 5 ~ "Fodselspermisjon",
    Spm31_hbeskj == 6 ~ "Pensjonist",
    Spm31_hbeskj == 7 ~ "Ufor",
    Spm31_hbeskj == 8 ~ "Arbeidsledig",
    Spm31_hbeskj == 9 ~ "Annet",
    TRUE ~ as.character(Spm31_hbeskj_ord)))


# Oppretter en faktor for fleksible arbeidstimer
p_sam <- p_sam %>% 
  mutate(Spm63_arbeidsordning_ord = 0) %>%
  naniar::replace_with_na(replace = list(Spm63_arbeidsordning_ord = 0)) %>% 
  mutate(Spm63_arbeidsordning_ord = case_when(
    Spm63_arbeidsordning == 1 ~ "Fast arbeidstid dagtid",
    Spm63_arbeidsordning == 2 ~ "Fleksibel arbeidstid dagtid",
    Spm63_arbeidsordning == 3 ~ "Skift, turnus, nattarbeid mm",
    Spm63_arbeidsordning == 4 ~ "Annen ordning",
    TRUE ~ as.character(Spm63_arbeidsordning_ord)))

# Oppretter dummy hvis arbeidsdagen tillater litt fleksibilitet
p_sam <- p_sam %>% 
  mutate(fleksibel_arbeidsdag = 0) %>% 
  naniar::replace_with_na((replace = list(fleksibel_arbeidsdag = 0))) %>% 
  mutate(fleksibel_arbeidsdag = case_when(
    Spm63_arbeidsordning == 1 ~ 0,
    Spm63_arbeidsordning == 3 ~ 0,
    Spm63_arbeidsordning == 2 ~ 1,
    Spm63_arbeidsordning == 4 ~ 1,
    TRUE ~ as.numeric(fleksibel_arbeidsdag)))

# Legger til den vanligste verdien (mean = 5.18) for 
# Spm104_inntekt_hushold2 for de som svarer "vil ikke oppgi (n = 67),
# "vet ikke" (n = 68) og NA (2 personer)
p_sam <- p_sam %>% 
  mutate(Spm104_inntekt_hushold2 = case_when(
    Spm104_inntekt_hushold2 == 7 ~ 5,
    Spm104_inntekt_hushold2 == 8 ~ 5,
    is.na(Spm104_inntekt_hushold2) ~ 5,
    TRUE ~ as.numeric(Spm104_inntekt_hushold2)))


# Dropper orginalvariabler om slektskapsforhold
var_slekt <- c("Spm83_1_person1_slekt", "Spm82_1_person1_alder",
               "Spm83_2_person2_slekt", "Spm82_2_person2_alder",
               "Spm83_3_person3_slekt", "Spm82_3_person3_alder",
               "Spm83_4_person4_slekt", "Spm82_4_person4_alder",
               "Spm83_5_person5_slekt", "Spm82_5_person5_alder",
               "Spm83_6_person6_slekt", "Spm82_6_person6_alder",
               "Spm83_7_person7_slekt", "Spm82_7_person7_alder",
               "Spm83_8_person8_slekt", "Spm82_8_person8_alder",
               "Spm83_9_person9_slekt", "Spm82_9_person9_alder",
               "Spm83_10_person10_slekt", "Spm82_10_person10_alder",
               "Spm83_11_person11_slekt", "Spm82_11_person11_alder",
               "Spm83_12_person12_slekt", "Spm82_12_person12_alder",
               "Spm83_13_person13_slekt", "Spm82_13_person13_alder",
               "Spm83_14_person14_slekt", "Spm82_14_person14_alder",
               "Spm83_15_person15_slekt", "Spm82_15_person15_alder")

# Dropper variabler vi ikke er interessert i
var_drop <- c("Spm710_p_betaling", "Spm77_tidtiljobb_koll", "Spm76_tidtiljobb_bil",
              "Spm433_korttype", "Spm79_p_ledig", "Spm78_p_jobb", "Spm97_p_ledig",
              "Spm95_p_bolig", "Spm910_kolltilbud_morgen", "Spm102_inntekt_egen2",
              "Spm105_utdanning_ord", "tidtiljobb_koll_max", "tidtiljobb_bil_max",
              "Spm430_bilmulighet", "Spm34_oppmte")

p_sam <- p_sam %>%
  select(!all_of(var_slekt)) %>% 
  select(!all_of(var_drop))

# Kombinerer reise- og personfil ----
rp_komb <- left_join(r_var, p_sam, by = "Intnr")

# Oppretter dummy for individer som har brukt bil for alle reiser
# Regner ikke med personer som også har utført en luftetur i løpet 
# av registeringsdagen
biltur <- rp_komb %>%
  filter(Spm47_reiseforml_ord != "Gikk/syklet/jogget en tur/skitur/luftet hund") %>%
  group_by(Intnr, reisemiddel) %>% 
  count(Intnr, reisemiddel) %>%
  ungroup() %>% 
  group_by(Intnr) %>% 
  filter(n() == 1) %>% 
  filter(reisemiddel == "bil") %>% 
  select(Intnr)

rp_komb <- rp_komb %>% 
  mutate(bil_alle_reiser = if_else(Intnr %in% biltur$Intnr, 1, 0))


# ----------------------------------------------------------------
# Fjerner respondenter som har missing/vil ikke svare på spørsmål ----
# ----------------------------------------------------------------

## Personfil
# Sjekker antall NA
c_names <- names(rp_komb)
k <- 0
cont_NA <- NA
for (i in 1:(dim(rp_komb)[2])) {
  z <- unique(is.na(rp_komb[, i]))
  
  if(length(z) == 2){
    
    if(!is.na(cont_NA)){
      cont_NA <- c(cont_NA, c_names[i])
    }else{
      cont_NA <- c_names[i]
    }
  }
}
rm(i, k, z)

missing <- data.frame("Column" = NA,
                      "Missing_Values" = NA)
for(p in 1:length(cont_NA)){
  s <- sum(is.na(rp_komb[, c_names %in% cont_NA[p]]))
  
  missing[p, 1] <- cont_NA[p]
  missing[p, 2] <- s
}
rm(p, s, cont_NA)
missing




c_names <- names(p_sam)
k <- 0
cont_NA <- NA
for (i in 1:(dim(p_sam)[2])) {
  z <- unique(is.na(p_sam[, i]))
  
  if(length(z) == 2){
    
    if(!is.na(cont_NA)){
      cont_NA <- c(cont_NA, c_names[i])
    }else{
      cont_NA <- c_names[i]
    }
  }
}
rm(i, k, z)

missing <- data.frame("Column" = NA,
                      "Missing_Values" = NA)
for(p in 1:length(cont_NA)){
  s <- sum(is.na(p_sam[, c_names %in% cont_NA[p]]))
  
  missing[p, 1] <- cont_NA[p]
  missing[p, 2] <- s
}
rm(p, s, cont_NA)
missing


# Dropper resterende NA
rp_komb <- rp_komb %>% 
  drop_na()


#S?rger for at alle kolonnenavn starter med liten bokstav
names(p_sam) <- tolower(names(p_sam))
names(rp_komb) <- tolower(names(rp_komb))

# Filtrerer ut personfil til å inneholde de samme personene som reiser_virkedag
p_sam <- p_sam %>% 
  filter(is.element(intnr, rp_komb$intnr))


#------------------------------------------------------------------
# Regresjonsanalyse ---
#------------------------------------------------------------------


# Setter referansekategorier ----
# Setter bil_forer som referanse
rp_komb$reisemiddel <- as.factor(rp_komb$reisemiddel)
rp_komb$reisemiddel <- relevel(rp_komb$reisemiddel, ref = "bil")

# Setter Oslo som referanse som byområde
rp_komb$region <- as.factor(rp_komb$region)
rp_komb$region <- relevel(rp_komb$region, ref = "Oslo")

# Setter "Ingen bil" som referanse for bil_antall
rp_komb$bil_antall <- as.factor(rp_komb$bil_antall)
rp_komb$bil_antall <- relevel(rp_komb$bil_antall, 
                                  ref = "Ingen bil")

# Setter 'hele dagen' som referanse for tilgang til bil i løpet av registeringsdagen
rp_komb$spm430_bilmulighet_ord <- as.factor(rp_komb$spm430_bilmulighet_ord)
rp_komb$spm430_bilmulighet_ord <- relevel(rp_komb$spm430_bilmulighet_ord,
                                              ref = "Hele dagen")

# Setter ikke tilgang til parkering som referanse for p_jobb
rp_komb$p_jobb <- as.factor(rp_komb$p_jobb)
rp_komb$p_jobb <- relevel(rp_komb$p_jobb,
                              ref = "Gratis parkering ved jobb")

# Sette fulltidsstilling som referanse for stillingsprosent
rp_komb$stillingsprosent <- as.factor(rp_komb$stillingsprosent)
rp_komb$stillingsprosent <- relevel(rp_komb$stillingsprosent,
                                        ref = "Fulltidsstilling")


# Transformerer variabler
rp_komb <- rp_komb %>% 
  mutate(aldersgruppe = alder) %>% 
  mutate(aldersgruppe = case_when(
    (alder >= 18 & alder < 28) ~ "18-27",
    (alder >= 28 & alder < 38) ~ "28-37",
    (alder >= 38 & alder < 48) ~ "38-47",
    (alder >= 48 & alder < 58) ~ "48-57",
    (alder >= 58 & alder < 68) ~ "58-67",
    (alder >= 68 & alder <= 78) ~ "68-78",
    TRUE ~ as.character(aldersgruppe)))

# Setter label på data
rp_komb <- 
  set_label(rp_komb,
            har_barn = "Husholdningstype B: Minst ett barn 0-15 ar",
            bare_forskolealder = "Husholdningstype B1:  0-6  ar (minst ett); 7-15  ar (ingen)" ,
            bare_grunnskolealder = "Husholdningstype B2:  0-6  ar (ingen); 7-15  ar (minst ett)", 
            forskole_grunnskole = "Husholdningstype B3:  0-6  ar (minst ett); 7-15  ar (minst ett)",
            under_5km = "Reise under 5 km",
            vinter = "Vinter",
            aldersgruppe = "Aldersgruppe (ref = 18-27 ar)",
            kvinne = "Kvinne",
            hoyere_utdannelse = "Hoyere utdannelse",
            spm104_inntekt_hushold2 = "Inntekt (husholdning samlet)",
            spm21_fkort = "Har forerkort",
            har_sykkel_elsykkel = "Eier sykkel",
            kvalitet_kollektiv = "Kvalitet kollektivtilbud",
            spm432_kollkort = "Har kollektivkort")

reisemiddel_label <- c("Bil", "Gange", "Kollektiv", "Syklet")
names(reisemiddel_label) <- c("bil", "gange", "kollektiv", "syklet")


# Tester hypoteser ----

# Hypotese 1
h1 <- (glm(bil_alle_reiser ~ har_barn + under_5km + region + vinter + aldersgruppe + kvinne +
           hoyere_utdannelse + spm104_inntekt_hushold2 + stillingsprosent + 
           bil_antall + har_sykkel_elsykkel + 
           kvalitet_kollektiv + spm432_kollkort + p_jobb,
         data = rp_komb, family = binomial))

logLik(h1)


# Hypotese 2
h2 <- (glm(bil_alle_reiser ~ bare_forskolealder + bare_grunnskolealder + forskole_grunnskole +
           under_5km + region + vinter + aldersgruppe + kvinne +
           hoyere_utdannelse + spm104_inntekt_hushold2 + stillingsprosent + 
           bil_antall + har_sykkel_elsykkel + 
           kvalitet_kollektiv + spm432_kollkort + p_jobb, 
         data = rp_komb, family = binomial))

# Hypotese 3
h3 <- (multinom(reisemiddel ~ har_barn + under_5km + region + vinter + aldersgruppe + kvinne +
                hoyere_utdannelse + spm104_inntekt_hushold2 + stillingsprosent + 
                spm21_fkort + bil_antall + har_sykkel_elsykkel + 
                kvalitet_kollektiv + spm432_kollkort + p_jobb, 
              data = rp_komb))

# Hypotese 4
h4 <- (multinom(reisemiddel ~ har_barn + under_5km + region + vinter + aldersgruppe + kvinne +
                hoyere_utdannelse + spm104_inntekt_hushold2 + stillingsprosent + 
                spm21_fkort + bil_antall + har_sykkel_elsykkel + 
                kvalitet_kollektiv + spm432_kollkort + p_jobb + 
               under_5km*har_barn, 
              data = rp_komb))
                      
# Hypotese 5
h5 <- multinom(reisemiddel ~ bare_forskolealder + bare_grunnskolealder + 
                forskole_grunnskole + under_5km + region + vinter + 
                aldersgruppe + kvinne + hoyere_utdannelse + spm104_inntekt_hushold2 + 
                stillingsprosent + spm21_fkort + bil_antall + 
                har_sykkel_elsykkel + kvalitet_kollektiv + spm432_kollkort + 
                p_jobb,
              data = rp_komb)


# Hypotese 6
h6 <- multinom(reisemiddel ~ bare_forskolealder + bare_grunnskolealder + 
                forskole_grunnskole + under_5km + region + vinter + 
                aldersgruppe + kvinne + hoyere_utdannelse + spm104_inntekt_hushold2 + 
                stillingsprosent + spm21_fkort + bil_antall + 
                har_sykkel_elsykkel + kvalitet_kollektiv + spm432_kollkort + 
                p_jobb + 
                bare_forskolealder*under_5km + bare_grunnskolealder*under_5km +
                forskole_grunnskole*under_5km,
              data = rp_komb)

# Hypotese 7
h7 <- multinom(reisemiddel ~ har_barn + under_5km + region + vinter + aldersgruppe + kvinne +
                hoyere_utdannelse + spm104_inntekt_hushold2 + stillingsprosent + 
                spm21_fkort + bil_antall + har_sykkel_elsykkel + 
                kvalitet_kollektiv + spm432_kollkort + p_jobb 
                 + har_barn*kvinne,
              data = rp_komb)

# Hypotese 8
h8 <- multinom(reisemiddel ~ bare_forskolealder + bare_grunnskolealder + 
                forskole_grunnskole + under_5km + region + vinter + aldersgruppe + kvinne +
                hoyere_utdannelse + spm104_inntekt_hushold2 + stillingsprosent + 
                spm21_fkort + bil_antall + har_sykkel_elsykkel + 
                kvalitet_kollektiv + spm432_kollkort + p_jobb + 
                bare_forskolealder*kvinne + bare_grunnskolealder * kvinne + 
                forskole_grunnskole*kvinne,
              data = rp_komb)


# Tester om forskjeller i koeffisientestimater er signifikante ----
# Hypotese 2
test_coef_equality(h2, "bare_forskolealder", "bare_grunnskolealder")
linearHypothesis(h2, c("regionBergen = regionStavanger"))
linearHypothesis(h2, c("regionBergen = regionTrondheim"))
linearHypothesis(h2, c("regionStavanger = regionTrondheim"))


# hypotese 3
# Forskjeller i byer
linearHypothesis(h3, c("gange:regionStavanger = gange:regionTrondheim"))
linearHypothesis(h3, c("gange:regionStavanger = gange:regionBergen"))
linearHypothesis(h3, c("gange:regionBergen = gange:regionTrondheim"))

linearHypothesis(h3, c("kollektiv:regionStavanger = kollektiv:regionTrondheim"))
linearHypothesis(h3, c("kollektiv:regionStavanger = kollektiv:regionBergen"))
linearHypothesis(h3, c("kollektiv:regionBergen = kollektiv:regionTrondheim"))

linearHypothesis(h3, c("syklet:regionStavanger = syklet:regionTrondheim"))
linearHypothesis(h3, c("syklet:regionStavanger = syklet:regionBergen"))
linearHypothesis(h3, c("syklet:regionBergen = syklet:regionTrondheim"))


# Hypotese 5
# Forskjeller i effekt etter aldersgruppe
linearHypothesis(h5, c("gange:bare_forskolealder = gange:bare_grunnskolealder"))
linearHypothesis(h5, c("gange:bare_forskolealder = gange:forskole_grunnskole"))
linearHypothesis(h5, c("gange:bare_grunnskolealder = gange:forskole_grunnskole"))


linearHypothesis(h5, c("kollektiv:bare_forskolealder = kollektiv:bare_grunnskolealder"))
linearHypothesis(h5, c("kollektiv:bare_forskolealder = kollektiv:forskole_grunnskole"))
linearHypothesis(h5, c("kollektiv:bare_grunnskolealder = kollektiv:forskole_grunnskole"))

# Hypotese 6
# Forskjell i effekt på avstand etter aldersgruppe
linearHypothesis(h6, c("kollektiv:bare_forskolealder:under_5km = kollektiv:bare_grunnskolealder:under_5km"))
linearHypothesis(h6, c("kollektiv:bare_forskolealder:under_5km = kollektiv:forskole_grunnskole:under_5km"))
linearHypothesis(h6, c("kollektiv:bare_grunnskolealder:under_5km = kollektiv:forskole_grunnskole:under_5km"))

# Hypotese 8
# Forskjell i effekt på avstand etter aldersgruppe
linearHypothesis(h8, c("kollektiv:bare_forskolealder:kvinne = kollektiv:bare_grunnskolealder:kvinne"))
linearHypothesis(h8, c("kollektiv:bare_forskolealder:kvinne = kollektiv:forskole_grunnskole:kvinne"))
linearHypothesis(h8, c("kollektiv:bare_grunnskolealder:kvinne = kollektiv:forskole_grunnskole:kvinne"))

# Skriver ut modeller som tabeller ----
tab_model(h1, h2,
          show.p = TRUE,
          show.aic = TRUE,
          show.loglik = TRUE,
          p.style = c("numeric"),
          digits.p = 3)

tab_model(h3,
          show.p = TRUE,
          show.aic = TRUE,
          show.loglik = TRUE,
          p.style = c("numeric"),
          digits.p = 3,
          p.threshold = c(0.05, 0.01, 0.001))

tab_model(h4,
          show.p = TRUE,
          show.aic = TRUE,
          show.loglik = TRUE,
          p.style = c("numeric"),
          digits.p = 3)

tab_model(h5, 
          show.p = TRUE,
          show.aic = TRUE,
          show.loglik = TRUE,
          p.style = c("numeric"),
          digits.p = 3)

tab_model(h6,
          show.p = TRUE,
          show.aic = TRUE,
          show.loglik = TRUE,
          p.style = c("numeric"),
          digits.p = 3)

tab_model(h7,
          show.p = TRUE,
          show.aic = TRUE,
          show.loglik = TRUE,
          p.style = c("numeric"),
          digits.p = 3)

tab_model(h8,
          show.p = TRUE,
          show.aic = TRUE,
          show.loglik = TRUE,
          p.style = c("numeric"),
          digits.p = 3)


# Tegner grafer ----

# Effekten av unge barn og avstand på reisemiddelvalg
h4_marg <- (ggemmeans(h4, terms = c("har_barn", "under_5km"), weights="proportional"))
as_tibble(h4_marg)
plot(h4_marg, colors = "bw")


ggplot(h4_marg, aes(x, predicted)) +
  geom_ribbon(aes(ymin = conf.low,
                  ymax = conf.high, fill = group),
              show.legend = FALSE) +
  scale_fill_manual(values = c("grey85", "grey85")) +
  geom_line(aes(y = predicted, linetype=group)) +
  geom_point(size = 1) +
  facet_wrap(~response.level, labeller = labeller(response.level = reisemiddel_label)) +
  labs(title = "Gjennomsnittlig predikert sannsynlighet modell 4: Effekten\nav unge barn og korte reiser på reisemiddelvalg",
       x = "Husholdningstype",
       y = "Gjennomsnittlig predikert sannsynlighet",
       caption = "Husholdningstyper:\n Type A: ingen barn\n Type B: minst et barn 0-15 år\nGrå felt anngir 95% konfidensintervall") +
  theme_apa() +
  theme(plot.caption = element_text(hjust = 0)) +
  scale_linetype(name = "", labels = c("Over 5 km", "Under 5 km")) +
  scale_x_continuous(breaks=c(0,1), 
                     labels=c("Type A",
                              "Type B"))


# Effekten av unge barn etter aldersgruppe og avstand på reisemiddelvalg
h6_marg_for <- (ggemmeans(h6, terms = c("bare_forskolealder", "under_5km")))
h6_marg_grun <- (ggemmeans(h6, terms = c("bare_grunnskolealder", "under_5km")))
h6_marg_for_grun <- (ggemmeans(h6, terms = c("forskole_grunnskole", "under_5km")))

h6_marg_grun2 <- h6_marg_grun %>% 
  mutate(x = case_when(
    x == 1 ~ 2,
    TRUE ~ as.numeric(x)))

h6_marg_for_grun3 <- h6_marg_for_grun %>% 
  mutate(x = case_when(
    x == 1 ~ 3,
    TRUE ~ as.numeric(x)))

h6_marg_tot <- rbind(h6_marg_for, h6_marg_grun2, h6_marg_for_grun3)

ggplot(h6_marg_tot, aes(x, predicted)) +
  geom_ribbon(aes(ymin = conf.low,
                  ymax = conf.high, fill = group),
              show.legend = FALSE) +
  scale_fill_manual(values = c("grey85", "grey85")) +
  geom_line(aes(y = predicted, linetype=group)) +
  geom_point(size = 1) +
  facet_wrap(~response.level, labeller = labeller(response.level = reisemiddel_label)) +
  labs(title = "Gjennomsnittlig predikert sannsynlighet modell 5: Effekten\nav unge barn og korte reiser på reisemiddelvalg",
       x = "Husholdningstype",
       y = "Gjennomsnittlig predikert sannsynlighet",
       caption = "Husholdningstyper:\n Type A: ingen barn\n Type B1: 0-6 år (minst ett); 7-15 år (ingen)\n Type B2: 0-6 år (ingen); 7-15 år (minst ett)\n Type B3: 0-6 år (minst ett); 7-15 år (minst ett)\nGrå felt anngir 95% konfidensintervall") +
  theme_apa() +
  theme(plot.caption = element_text(hjust = 0)) +
  scale_linetype(name = "", labels = c("Over 5 km", "Under 5 km")) +
  scale_x_continuous(breaks=c(0,1,2,3), 
                     labels=c("Type A",
                              "Type B1",
                              "Type B2",
                              "Type B3"))

# Effekten av unge barn og kjønn på reisemiddelvalg
h7_marg <- (ggemmeans(h7, terms = c("har_barn", "kvinne")))

ggplot(h7_marg, aes(x, predicted)) +
  geom_ribbon(aes(ymin = conf.low,
                  ymax = conf.high, fill = group),
              show.legend = FALSE) +
  scale_fill_manual(values = c("grey85", "grey85")) +
  geom_line(aes(y = predicted, linetype=group)) +
  geom_point(size = 1) +
  facet_wrap(~response.level, labeller = labeller(response.level = reisemiddel_label)) +
  labs(title = "Gjennomsnittlig predikert sannsynlighet modell 4: Effekten\nav unge barn og kjønn på reisemiddelvalg",
       x = "Husholdningstype",
       y = "Gjennomsnittlig predikert sannsynlighet",
       caption = "Husholdningstyper:\n Type A: ingen barn\n Type B: minst et barn 0-15 år\nGrå felt anngir 95% konfidensintervall") +
  theme_apa() +
  theme(plot.caption = element_text(hjust = 0)) +
  scale_linetype(name = "", labels = c("Mann", "Kvinne")) +
  scale_x_continuous(breaks=c(0,1), 
                     labels=c("Type A",
                              "Type B"))

# Effekten av unge barn etter aldersgruppe og kjønn på reisemiddelvalg
h8_marg_for <- (ggemmeans(h8, terms = c("bare_forskolealder" , "kvinne")))
h8_marg_grun <- (ggemmeans(h8, terms = c("bare_grunnskolealder", "kvinne")))
h8_marg_for_grun <- (ggemmeans(h8, terms = c("forskole_grunnskole", "kvinne")))

h8_marg_grun2 <- h8_marg_grun %>% 
  mutate(x = case_when(
    x == 1 ~ 2,
    TRUE ~ as.numeric(x)))

h8_marg_for_grun3 <- h8_marg_for_grun %>% 
  mutate(x = case_when(
    x == 1 ~ 3,
    TRUE ~ as.numeric(x)))

h8_marg_tot <- rbind(h8_marg_for, h8_marg_grun2, h8_marg_for_grun3)

ggplot(h8_marg_tot, aes(x, predicted)) +
  geom_ribbon(aes(ymin = conf.low,
                  ymax = conf.high, fill = group),
              show.legend = FALSE) +
  scale_fill_manual(values = c("grey85", "grey85")) +
  geom_line(aes(y = predicted, linetype=group)) +
  geom_point(size = 1) +
  facet_wrap(~response.level, labeller = labeller(response.level = reisemiddel_label)) +
  labs(title = "Gjennomsnittlig predikert sannsynlighet modell 8: Effekten\nav unge barn etter aldersgruppe og kjønn på reisemiddelvalg",
       x = "Husholdningstype",
       y = "Gjennomsnittlig predikert sannsynlighet",
       caption = "Husholdningstyper:\n Type A: ingen barn\n Type B1: 0-6 år (minst ett); 7-15 år (ingen)\n Type B2: 0-6 år (ingen); 7-15 år (minst ett)\n Type B3: 0-6 år (minst ett); 7-15 år (minst ett)\nGrå felt anngir 95% konfidensintervall") +
  theme_apa() +
  theme(plot.caption = element_text(hjust = 0)) +
  scale_linetype(name = "", labels = c("Mann", "Kvinne")) +
  scale_x_continuous(breaks=c(0,1,2,3), 
                     labels=c("Type A",
                              "Type B1",
                              "Type B2",
                              "Type B3"))


#---------------------------------------------------------------------------
# Tester hvor godt modellen forklarer varians ----
#---------------------------------------------------------------------------

# Lager trening og test data  ----
set.seed(100)
trenings_rader <- sample(1:nrow(rp_komb), floor(0.8*nrow(rp_komb)))
trening <- rp_komb[trenings_rader,]
test <- rp_komb[-trenings_rader,]
set.seed(NULL)


# Modell h1
mod1 <- glm(bil_alle_reiser ~ har_barn + under_5km + region + vinter + aldersgruppe + kvinne +
               hoyere_utdannelse + spm104_inntekt_hushold2 + stillingsprosent + 
               bil_antall + har_sykkel_elsykkel + 
               kvalitet_kollektiv + spm432_kollkort + p_jobb,
             data = trening, family = binomial)

# Predikerer på test data
predikert_bil_mod1 <- predict.glm(mod1, test, type = "response")
predikert_bil_mod1 <- if_else(predikert_bil_mod1 >= 0.4, 1, 0)

# Beregner accuracy, precision og recall
confusionMatrix(as.factor(predikert_bil_mod1), as.factor(test$bil_alle_reiser))


# Modell h2
mod2 <- (glm(bil_alle_reiser ~ bare_forskolealder + bare_grunnskolealder + forskole_grunnskole +
             under_5km + region + vinter + aldersgruppe + kvinne +
             hoyere_utdannelse + spm104_inntekt_hushold2 + stillingsprosent + 
             bil_antall + har_sykkel_elsykkel + 
             kvalitet_kollektiv + spm432_kollkort + p_jobb, 
           data = trening, family = binomial))

# Predikerer på test data
predikert_bil_mod2 <- predict.glm(mod2, test, type = "response")
predikert_bil_mod2 <- if_else(predikert_bil_mod2 >= 0.5, 1, 0)

# Beregner accuracy, precision og recall
confusionMatrix(as.factor(predikert_bil_mod2), as.factor(test$bil_alle_reiser))

# Modell h3
mod3 <- (multinom(reisemiddel ~ har_barn + under_5km + region + vinter + aldersgruppe + kvinne +
                  hoyere_utdannelse + spm104_inntekt_hushold2 + stillingsprosent + 
                  spm21_fkort + bil_antall + har_sykkel_elsykkel + 
                  kvalitet_kollektiv + spm432_kollkort + p_jobb, 
                data = trening))

confusionMatrix(predict(mod3, test), reference = test$reisemiddel)

# Modell h4
mod4 <- (multinom(reisemiddel ~ har_barn + under_5km + region + vinter + aldersgruppe + kvinne +
                  hoyere_utdannelse + spm104_inntekt_hushold2 + stillingsprosent + 
                  spm21_fkort + bil_antall + har_sykkel_elsykkel + 
                  kvalitet_kollektiv + spm432_kollkort + p_jobb + 
                  under_5km*har_barn, 
                data = trening))

confusionMatrix(predict(mod4, test), reference = test$reisemiddel)

# Hypotese 5
mod5 <- multinom(reisemiddel ~ bare_forskolealder + bare_grunnskolealder + 
                 forskole_grunnskole + under_5km + region + vinter + 
                 aldersgruppe + kvinne + hoyere_utdannelse + spm104_inntekt_hushold2 + 
                 stillingsprosent + spm21_fkort + bil_antall + 
                 har_sykkel_elsykkel + kvalitet_kollektiv + spm432_kollkort + 
                 p_jobb,
               data = trening)

confusionMatrix(predict(mod5, test), reference = test$reisemiddel)

# Hypotese 6
mod6 <- multinom(reisemiddel ~ bare_forskolealder + bare_grunnskolealder + 
                 forskole_grunnskole + under_5km + region + vinter + 
                 aldersgruppe + kvinne + hoyere_utdannelse + spm104_inntekt_hushold2 + 
                 stillingsprosent + spm21_fkort + bil_antall + 
                 har_sykkel_elsykkel + kvalitet_kollektiv + spm432_kollkort + 
                 p_jobb + 
                 bare_forskolealder*under_5km + bare_grunnskolealder*under_5km +
                 forskole_grunnskole*under_5km,
               data = trening)

confusionMatrix(predict(mod6, test), reference = test$reisemiddel)


# Hypotese 7
mod7 <- multinom(reisemiddel ~ har_barn + under_5km + region + vinter + aldersgruppe + kvinne +
                 hoyere_utdannelse + spm104_inntekt_hushold2 + stillingsprosent + 
                 spm21_fkort + bil_antall + har_sykkel_elsykkel + 
                 kvalitet_kollektiv + spm432_kollkort + p_jobb 
               + har_barn*kvinne,
               data = trening)

confusionMatrix(predict(mod7, test), reference = test$reisemiddel)


# Hypotese 8
mod8 <- multinom(reisemiddel ~ bare_forskolealder + bare_grunnskolealder + 
                 forskole_grunnskole + under_5km + region + vinter + aldersgruppe + kvinne +
                 hoyere_utdannelse + spm104_inntekt_hushold2 + stillingsprosent + 
                 spm21_fkort + bil_antall + har_sykkel_elsykkel + 
                 kvalitet_kollektiv + spm432_kollkort + p_jobb + 
                 bare_forskolealder*kvinne + bare_grunnskolealder * kvinne + 
                 forskole_grunnskole*kvinne,
               data = trening)

confusionMatrix(predict(mod8, test), reference = test$reisemiddel)


#---------------------------------------------------------------------------
# Tester forutsetninger for logistisk regresjon ----
#---------------------------------------------------------------------------

# Check for correlation
rp_komb %>% 
  mutate(fulltid = if_else(stillingsprosent == "Fulltidsstilling", 1, 0)) %>% 
  mutate(deltid_over50 = if_else(stillingsprosent == "100%-50%", 1, 0)) %>% 
  mutate(deltid_under50 = if_else(stillingsprosent == "Under 50%", 1, 0)) %>%
  mutate(p_ingen = if_else(p_jobb == "Ikke tilgang til parkering ved jobb", 1, 0)) %>% 
  mutate(p_betaler = if_else(p_jobb == "Betaler for parkerings ved jobb", 1, 0)) %>% 
  mutate(p_gratis = if_else(p_jobb == "Gratis parkering ved jobb", 1, 0)) %>% 
  select(har_barn, under_5km, alder, kvinne,
         hoyere_utdannelse, spm104_inntekt_hushold2, spm21_fkort, har_sykkel_elsykkel,
         kvalitet_kollektiv, spm432_kollkort, vinter,
         spm86_samboer_jobb, fleksibel_arbeidsdag, fulltid, deltid_over50,
         deltid_under50, spm25_bilantall, p_ingen, p_betaler, p_gratis,
         bil_alle_reiser) %>% 
  cor(.) %>% 
  ggcorrplot(., type = "lower", lab = TRUE)


# Diskrimineringsproblemer
# Sjekker krysstabeller
CrossTable(rp_komb$reisemiddel, rp_komb$har_barn, format = "SPSS")
CrossTable(rp_komb$reisemiddel, rp_komb$under_5km, format = "SPSS")
CrossTable(rp_komb$reisemiddel, rp_komb$region, format = "SPSS")
CrossTable(rp_komb$reisemiddel, rp_komb$vinter, format = "SPSS")
CrossTable(rp_komb$reisemiddel, rp_komb$kvinne, format = "SPSS")
CrossTable(rp_komb$reisemiddel, rp_komb$hoyere_utdannelse, format = "SPSS")
CrossTable(rp_komb$reisemiddel, rp_komb$spm104_inntekt_hushold2, format = "SPSS")
CrossTable(rp_komb$reisemiddel, rp_komb$stillingsprosent, format = "SPSS")
CrossTable(rp_komb$reisemiddel, rp_komb$spm21_fkort, format = "SPSS")
CrossTable(rp_komb$reisemiddel, rp_komb$bil_antall, format = "SPSS")
CrossTable(rp_komb$reisemiddel, rp_komb$har_sykkel_elsykkel, format = "SPSS")
CrossTable(rp_komb$reisemiddel, rp_komb$kvalitet_kollektiv, format = "SPSS")
CrossTable(rp_komb$reisemiddel, rp_komb$spm432_kollkort, format = "SPSS")
CrossTable(rp_komb$reisemiddel, rp_komb$p_jobb, format = "SPSS")
CrossTable(rp_komb$reisemiddel, rp_komb$spm430_bilmulighet_ord, format = "SPSS")
CrossTable(rp_komb$reisemiddel, rp_komb$bil_alle_reiser, format = "SPSS")
# Konklusjon: spm21_fkort, bare 4 reiser som bilfører uten førerkort

# Ikke-linearitet i parameterne
u_var <- rp_komb%>% 
  select(bil_forer, kollektiv, til_fots, syklet, reisemiddel,
         har_barn, under_5km, region, vinter, alder,
         alder2, alder_sqrt, kvinne, inntekt_hushold,
         hoyere_utdannelse, spm104_inntekt_hushold2,  stillingsprosent, 
         spm21_fkort, bil_antall, har_sykkel_elsykkel, 
         kvalitet_kollektiv, spm432_kollkort, p_jobb, 
         tidtiljobb_bil_koll, spm430_bilmulighet_ord, bil_alle_reiser)

# Funksjon for å lage graf for å se om det er ikke-linearitet i parameterne
linear_test <- function(df, variables){
  
  # Teller observasjoner for avhengig og uavhengig variabel
  oversikt <- df %>% 
    select(all_of(variables)) %>% 
    rename(avhengig = 1,
           uavhengig = 2) %>% 
    group_by(avhengig, uavhengig) %>% 
    count() %>% 
    ungroup()
  
  # Plukker ut der avhengig er FALSE og TRUE
  avhengig0 <- subset(oversikt, avhengig == 0)
  avhengig1 <- subset(oversikt, avhengig == 1)
  
  # Endrer navn på variabelen i avhengig er TRUE
  avhengig1 <- avhengig1 %>% 
    rename(avhengigx = avhengig,
           uavhengigx = uavhengig,
           nx = n)
  
  # Lager dataframe som kan brukes til utregning
  odds <- cbind(avhengig0, avhengig1)
  
  # Regner ut oddsratio og logaritmen
  odds$oddsratio <- odds$nx/odds$n
  odds$lnodds <- log(odds$oddsratio)
  
  # Lager graf
  p <- ggplot(odds, aes(x = uavhengig, y = lnodds)) +
    geom_point() +
    geom_line() +
    labs(x = variables[2]) +
    theme_minimal()
  
  print(p)
  
} 

## Starter med variablene til multinom regresjon
# spm104_inntekt_hushold
linear_test(u_var, c("bil_forer", "spm104_inntekt_hushold2"))
linear_test(u_var, c("kollektiv", "spm104_inntekt_hushold2"))
linear_test(u_var, c("til_fots", "spm104_inntekt_hushold2"))
linear_test(u_var, c("syklet", "spm104_inntekt_hushold2"))


# alder
alder_var <- u_var
alder_var$alder <- cut(alder_var$alder, seq(min(alder_var$alder), 
                                            max(alder_var$alder), 
                                            10))

linear_test(alder_var, c("bil_forer", "alder"))
linear_test(alder_var, c("kollektiv", "alder"))
linear_test(alder_var, c("til_fots", "alder"))
linear_test(alder_var, c("syklet", "alder"))

# kvalitet_kollektiv
linear_test(u_var, c("bil_forer", "kvalitet_kollektiv"))
linear_test(u_var, c("kollektiv", "kvalitet_kollektiv"))
linear_test(u_var, c("til_fots", "kvalitet_kollektiv"))
linear_test(u_var, c("syklet", "kvalitet_kollektiv"))

# tidtiljobb_bil_koll
tidjobb <- u_var
tidjobb$tidtiljobb_bil_koll <- cut(tidjobb$tidtiljobb_bil_koll, seq(0,
                                                                    12,
                                                                    4)) 

linear_test(tidjobb, c("bil_forer", "tidtiljobb_bil_koll"))
linear_test(tidjobb, c("kollektiv", "tidtiljobb_bil_koll"))
linear_test(tidjobb, c("til_fots", "tidtiljobb_bil_koll"))
linear_test(tidjobb, c("syklet", "tidtiljobb_bil_koll"))


## Tester så variabler for logistisk regresjon
# spm104_inntekt_hushold
linear_test(u_var, c("bil_alle_reiser", "spm104_inntekt_hushold2"))

# alder
linear_test(alder_var, c("bil_alle_reiser", "alder"))

# kvalitet_kollektiv - bør legge til kvadratledd i regresjonen
linear_test(u_var, c("bil_alle_reiser", "kvalitet_kollektiv"))

# tidtiljobb_bil_koll (Kan her sette 1 som step i seq for tidtiljobb_bil_koll)
linear_test(tidjobb, c("bil_alle_reiser", "tidtiljobb_bil_koll"))
