library(readr)
library(dplyr)
library(stringr)
library(gtools)


data_raw <- read_csv("poetry_data_raw.csv")
data_raw_add <- read_csv("poetry_data_additions.csv")


#changing anon_3 to anon_2 since they are the same, and changing anon_4 to anon_3 same with anon_5
data_raw_adjusted <- data_raw %>%
  mutate(
    skald_id = replace(skald_id, skald_id=="anon_3", "anon_2"),
    skald_id = replace(skald_id, skald_id=="anon_4", "anon_3"),
    skald_id = replace(skald_id, skald_id=="anon_5", "anon_4"),
    SkP = NA, #adding a column for citing the SkP books
    ) %>% 
  rename(stanza = source) #renaming the old "source" column to stanza 

data_merged <- bind_rows(data_raw_adjusted, data_raw_add)  #adding the additional data to the table 
data_edit_1 <- data_merged[mixedorder(data_merged$stanza), ] #sorting by stanza alphanumerically 





#### Editing starts here ####

#NOTE TO SELF: add book source column (context_source?), with e.g. \parencite[505]{person2013}


hárfagri_data <- data_edit_1 %>% 
  filter(
    skald_id == "Haraldr hárfagri Hálfdanarson")

#adding a column for citing the SkP books
data_edit_2 <- data_edit_1 %>% 
  mutate(
    SkP = replace(SkP,
                  skald_id == "Haraldr hárfagri Hálfdanarson" & #writing the name may be redundant, but it makes it easier for me to identify the skald
                    stanza == "Hhárf Lv 1",
                  "\\parencite[71]{whaley2012}" #the pagenr for the stanza in SkP
                  )
  ) 

harðráði_data <- data_edit_2 %>% 
  filter(
    skald_id == "Haraldr harðráði Sigurðarson")

#changing the date for the harðráði poem (this is an example sentence for when you actually start)
data_edit_3 <- data_edit_2 %>% 
  mutate(
    SkP = replace(SkP,
                  skald_id == "Haraldr harðráði Sigurðarson" &
                  stanza == "Hharð Gamv 3",
                  "\\parencite[38]{gade2009}" #the pagenr for the stanza in SkP
    ),
    period = replace(period,
                     skald_id == "Haraldr harðráði Sigurðarson" &
                       period == "pre-1047", 
                     "c1046-1047" #changed to 1046 since it seems that it was after he became King
    ), 
    SkP = replace(SkP,
                    skald_id == "Haraldr harðráði Sigurðarson" &
                    stanza == "Hharð Gamv 4",
                    "\\parencite[39]{gade2009}" 
      ),
    SkP = replace(SkP,
                  skald_id == "Haraldr harðráði Sigurðarson" &
                    stanza == "Hharð Lv 3",
                  "\\parencite[45]{gade2009}" 
    ),
    SkP = replace(SkP,
                  skald_id == "Haraldr harðráði Sigurðarson" &
                    stanza == "Hharð Lv 6",
                  "\\parencite[47]{gade2009}" 
    ),
    period = replace(period,
                  skald_id == "Haraldr harðráði Sigurðarson" &
                    period == "pre-1066", 
                  "c1046-1066" #changed to 1046 since it is said expressly that it is King Haraldr
    ),
    SkP = replace(SkP,
                  skald_id == "Haraldr harðráði Sigurðarson" &
                    stanza == "Hharð Lv 9",
                  "\\parencite[50]{gade2009}" 
    ),
    SkP = replace(SkP,
                  skald_id == "Haraldr harðráði Sigurðarson" &
                    stanza == "Hharð Lv 10",
                  "\\parencite[51]{gade2009}" 
    ),
    SkP = replace(SkP,
                  skald_id == "Haraldr harðráði Sigurðarson" &
                    stanza == "Hharð Lv 11",
                  "\\parencite[52]{gade2009}" 
    ),
    SkP = replace(SkP,
                  skald_id == "Haraldr harðráði Sigurðarson" &
                    stanza == "Hharð Lv 13",
                  "\\parencite[54]{gade2009}" 
    ),
    SkP = replace(SkP,
                  skald_id == "Haraldr harðráði Sigurðarson" &
                    stanza == "Hharð Lv 14",
                  "\\parencite[55]{gade2009}" 
    )
  )


håkgod_data <- data_edit_3 %>% 
  filter(
    skald_id == "Hákon inn góði Haraldsson")

data_edit_4 <- data_edit_3 %>% 
  mutate(
    SkP = replace(SkP,
                  skald_id == "Hákon inn góði Haraldsson",
                  "\\parencite[153]{whaley2012}" #the pagenr for the stanza in SkP
    )
  )


hildr_data <- data_edit_4 %>% 
  filter(
    skald_id == "Hildr Hrólfsdóttir nefju")

data_edit_5 <- data_edit_4 %>% 
  mutate(
    SkP = replace(SkP,
                  skald_id == "Hildr Hrólfsdóttir nefju",
                  "\\parencite[139]{whaley2012}" #the pagenr for the stanza in SkP
    ),
    period = replace(period,
                     skald_id == "Hildr Hrólfsdóttir nefju", 
                     "c860-911" #changed based on context information
    )
  )


jorunn_data <- data_edit_5 %>% 
  filter(
    skald_id == "Jórunn skáldmær")

data_edit_6 <- data_edit_5 %>% 
  mutate(
    SkP = replace(SkP,
                  skald_id == "Jórunn skáldmær" &
                    stanza == "Jór Send 2",
                  "\\parencite[146]{whaley2012}" 
    ),
    SkP = replace(SkP,
                  skald_id == "Jórunn skáldmær" &
                    stanza == "Jór Send 5",
                  "\\parencite[149]{whaley2012}" 
    ),
    period = replace(period,
                     skald_id == "Jórunn skáldmær", 
                     "c930-950" #changed based on context information
    )
  )


magber_data <- data_edit_6 %>% 
  filter(
    skald_id == "Magnús berfœttr Óláfsson")

data_edit_7 <- data_edit_6 %>% 
  mutate(
    SkP = replace(SkP,
                  skald_id == "Magnús berfœttr Óláfsson" &
                    stanza == "Mberf Lv 3",
                  "\\parencite[387]{gade2009}" 
    ),
    SkP = replace(SkP,
                  skald_id == "Magnús berfœttr Óláfsson" &
                    stanza == "Mberf Lv 4",
                  "\\parencite[388]{gade2009}" 
    ),
    SkP = replace(SkP,
                  skald_id == "Magnús berfœttr Óláfsson" &
                    stanza == "Mberf Lv 5",
                  "\\parencite[389]{gade2009}" 
    ),
    SkP = replace(SkP,
                  skald_id == "Magnús berfœttr Óláfsson" &
                    stanza == "Mberf Lv 6",
                  "\\parencite[390]{gade2009}" 
    ),
    period = replace(period,
                     skald_id == "Magnús berfœttr Óláfsson", 
                     "c1093-1103" #changed to be more specifically within his reign
    )
  )


olhelgi_data <- data_edit_7 %>% 
  filter(
    skald_id == "Óláfr inn helgi Haraldsson")

data_edit_8 <- data_edit_7 %>% 
  mutate(
    SkP = replace(SkP,
                  skald_id == "Óláfr inn helgi Haraldsson" &
                    stanza == "Ólhelg Lv 4",
                  "\\parencite[522]{whaley2012}" 
    ),
    SkP = replace(SkP,
                  skald_id == "Óláfr inn helgi Haraldsson" &
                    stanza == "Ólhelg Lv 6",
                  "\\parencite[525]{whaley2012}" 
    ),
    SkP = replace(SkP,
                  skald_id == "Óláfr inn helgi Haraldsson" &
                    stanza == "Ólhelg Lv 7",
                  "\\parencite[527]{whaley2012}" 
    ),
    SkP = replace(SkP,
                  skald_id == "Óláfr inn helgi Haraldsson" &
                    stanza == "Ólhelg Lv 9",
                  "\\parencite[529]{whaley2012}" 
    )
  )


thorb_data <- data_edit_8 %>% 
  filter(
    skald_id == "Þorbjǫrn hornklofi")

data_edit_9 <- data_edit_8 %>% 
  mutate(
    SkP = replace(SkP,
                  skald_id == "Þorbjǫrn hornklofi" &
                    stanza == "Þhorn Gldr 1",
                  "\\parencite[75]{whaley2012}" 
    ),
    SkP = replace(SkP,
                  skald_id == "Þorbjǫrn hornklofi" &
                    stanza == "Þhorn Gldr 2",
                  "\\parencite[78]{whaley2012}" 
    ),
    SkP = replace(SkP,
                  skald_id == "Þorbjǫrn hornklofi" &
                    stanza == "Þhorn Gldr 3",
                  "\\parencite[81]{whaley2012}" 
    ),
    SkP = replace(SkP,
                  skald_id == "Þorbjǫrn hornklofi" &
                    stanza == "Þhorn Gldr 5",
                  "\\parencite[84]{whaley2012}" 
    ),
    SkP = replace(SkP,
                  skald_id == "Þorbjǫrn hornklofi" &
                    stanza == "Þhorn Gldr 6",
                  "\\parencite[85]{whaley2012}" 
    ),
    SkP = replace(SkP,
                  skald_id == "Þorbjǫrn hornklofi" &
                    stanza == "Þhorn Gldr 7",
                  "\\parencite[87]{whaley2012}" 
    ),
    SkP = replace(SkP,
                  skald_id == "Þorbjǫrn hornklofi" &
                    stanza == "Þhorn Harkv 1",
                  "\\parencite[94]{whaley2012}" 
    ),
    SkP = replace(SkP,
                  skald_id == "Þorbjǫrn hornklofi" &
                    stanza == "Þhorn Harkv 2",
                  "\\parencite[95]{whaley2012}" 
    ),
    SkP = replace(SkP,
                  skald_id == "Þorbjǫrn hornklofi" &
                    stanza == "Þhorn Harkv 3",
                  "\\parencite[96]{whaley2012}" 
    ),
    SkP = replace(SkP,
                  skald_id == "Þorbjǫrn hornklofi" &
                    stanza == "Þhorn Harkv 4",
                  "\\parencite[97]{whaley2012}" 
    ),
    SkP = replace(SkP,
                  skald_id == "Þorbjǫrn hornklofi" &
                    stanza == "Þhorn Harkv 7",
                  "\\parencite[100]{whaley2012}" 
    ),
    SkP = replace(SkP,
                  skald_id == "Þorbjǫrn hornklofi" &
                    stanza == "Þhorn Harkv 8",
                  "\\parencite[102]{whaley2012}" 
    ),
    SkP = replace(SkP,
                  skald_id == "Þorbjǫrn hornklofi" &
                    stanza == "Þhorn Harkv 9",
                  "\\parencite[103]{whaley2012}" 
    ),
    SkP = replace(SkP,
                  skald_id == "Þorbjǫrn hornklofi" &
                    stanza == "Þhorn Harkv 10",
                  "\\parencite[104]{whaley2012}" 
    ),
    SkP = replace(SkP,
                  skald_id == "Þorbjǫrn hornklofi" &
                    stanza == "Þhorn Harkv 11",
                  "\\parencite[106]{whaley2012}" 
    ),
    SkP = replace(SkP,
                  skald_id == "Þorbjǫrn hornklofi" &
                    stanza == "Þhorn Harkv 13",
                  "\\parencite[107]{whaley2012}" 
    ),
    SkP = replace(SkP,
                  skald_id == "Þorbjǫrn hornklofi" &
                    stanza == "Þhorn Harkv 14",
                  "\\parencite[108]{whaley2012}" 
    ),
    SkP = replace(SkP,
                  skald_id == "Þorbjǫrn hornklofi" &
                    stanza == "Þhorn Harkv 16",
                  "\\parencite[110]{whaley2012}" 
    ),
    SkP = replace(SkP,
                  skald_id == "Þorbjǫrn hornklofi" &
                    stanza == "Þhorn Harkv 17",
                  "\\parencite[111]{whaley2012}" 
    ),
    SkP = replace(SkP,
                  skald_id == "Þorbjǫrn hornklofi" &
                    stanza == "Þhorn Harkv 18",
                  "\\parencite[112]{whaley2012}" 
    ),
    SkP = replace(SkP,
                  skald_id == "Þorbjǫrn hornklofi" &
                    stanza == "Þhorn Harkv 19",
                  "\\parencite[113]{whaley2012}" 
    ),
    SkP = replace(SkP,
                  skald_id == "Þorbjǫrn hornklofi" &
                    stanza == "Þhorn Harkv 22",
                  "\\parencite[115]{whaley2012}" 
    ),
    SkP = replace(SkP,
                  skald_id == "Þorbjǫrn hornklofi" &
                    stanza == "Þhorn Harkv 23",
                  "\\parencite[116]{whaley2012}" 
    ),
    SkP = replace(SkP,
                  skald_id == "Þorbjǫrn hornklofi" &
                    stanza == "Þhorn Lv 1",
                  "\\parencite[117]{whaley2012}" 
    )
  )


eyvindr_data <- data_edit_9 %>% 
  filter(
    skald_id == "Eyvindr skáldaspillir Finnsson")

data_edit_10 <- data_edit_9 %>% 
  mutate(
    SkP = replace(SkP,
                  skald_id == "Eyvindr skáldaspillir Finnsson" &
                    stanza == "Eyv Hák 2",
                  "\\parencite[176]{whaley2012}" 
    ),
    SkP = replace(SkP,
                  skald_id == "Eyvindr skáldaspillir Finnsson" &
                    stanza == "Eyv Hák 3",
                  "\\parencite[177]{whaley2012}" 
    ),
    SkP = replace(SkP,
                  skald_id == "Eyvindr skáldaspillir Finnsson" &
                    stanza == "Eyv Hák 4",
                  "\\parencite[178]{whaley2012}" 
    ),
    SkP = replace(SkP,
                  skald_id == "Eyvindr skáldaspillir Finnsson" &
                    stanza == "Eyv Hák 6",
                  "\\parencite[180]{whaley2012}" 
    ),
    SkP = replace(SkP,
                  skald_id == "Eyvindr skáldaspillir Finnsson" &
                    stanza == "Eyv Hák 9",
                  "\\parencite[184]{whaley2012}" 
    ),
    SkP = replace(SkP,
                  skald_id == "Eyvindr skáldaspillir Finnsson" &
                    stanza == "Eyv Hák 10",
                  "\\parencite[185]{whaley2012}" 
    ),
    SkP = replace(SkP,
                  skald_id == "Eyvindr skáldaspillir Finnsson" &
                    stanza == "Eyv Hák 11",
                  "\\parencite[186]{whaley2012}" 
    ),
    SkP = replace(SkP,
                  skald_id == "Eyvindr skáldaspillir Finnsson" &
                    stanza == "Eyv Hák 14",
                  "\\parencite[188]{whaley2012}" 
    ),
    SkP = replace(SkP,
                  skald_id == "Eyvindr skáldaspillir Finnsson" &
                    stanza == "Eyv Hák 15",
                  "\\parencite[189]{whaley2012}" 
    ),
    SkP = replace(SkP,
                  skald_id == "Eyvindr skáldaspillir Finnsson" &
                    stanza == "Eyv Hák 17",
                  "\\parencite[190]{whaley2012}" 
    ),
    SkP = replace(SkP,
                  skald_id == "Eyvindr skáldaspillir Finnsson" &
                    stanza == "Eyv Hák 18",
                  "\\parencite[191]{whaley2012}" 
    ),
    SkP = replace(SkP,
                  skald_id == "Eyvindr skáldaspillir Finnsson" &
                    stanza == "Eyv Hák 21",
                  "\\parencite[193]{whaley2012}" 
    ),
    SkP = replace(SkP,
                  skald_id == "Eyvindr skáldaspillir Finnsson" &
                    stanza == "Eyv Hál 1",
                  "\\parencite[197]{whaley2012}" 
    ),
    SkP = replace(SkP,
                  skald_id == "Eyvindr skáldaspillir Finnsson" &
                    stanza == "Eyv Hál 6",
                  "\\parencite[204]{whaley2012}" 
    ),
    SkP = replace(SkP,
                  skald_id == "Eyvindr skáldaspillir Finnsson" &
                    stanza == "Eyv Hál 7",
                  "\\parencite[205]{whaley2012}" 
    ),
    SkP = replace(SkP,
                  skald_id == "Eyvindr skáldaspillir Finnsson" &
                    stanza == "Eyv Hál 9",
                  "\\parencite[207]{whaley2012}" 
    ),
    SkP = replace(SkP,
                  skald_id == "Eyvindr skáldaspillir Finnsson" &
                    stanza == "Eyv Lv 1",
                  "\\parencite[215]{whaley2012}" 
    ),
    SkP = replace(SkP,
                  skald_id == "Eyvindr skáldaspillir Finnsson" &
                    stanza == "Eyv Lv 14",
                  "\\parencite[234]{whaley2012}" 
    )
  )

bardr_data <- data_edit_10 %>% 
  filter(
    skald_id == "Bárðr á Upplǫndum")

data_edit_11 <- data_edit_10 %>% 
  mutate(
    SkP = replace(SkP,
                  skald_id == "Bárðr á Upplǫndum",
                  "\\parencite[442]{whaley2012}" 
    )
  )


thjod_data <- data_edit_11 %>% 
  filter(
    skald_id == "Þjóðólfr ór Hvini")

data_edit_12 <- data_edit_11 %>% 
  mutate(
    SkP = replace(SkP,
                  skald_id == "Þjóðólfr ór Hvini" &
                    stanza == "Þjóð Yt 1",
                  "\\parencite[9]{whaley2012}" 
    ),
    SkP = replace(SkP,
                  skald_id == "Þjóðólfr ór Hvini" &
                    stanza == "Þjóð Yt 10",
                  "\\parencite[25]{whaley2012}" 
    ),
    SkP = replace(SkP,
                  skald_id == "Þjóðólfr ór Hvini" &
                    stanza == "Þjóð Yt 12",
                  "\\parencite[28]{whaley2012}" 
    ),
    SkP = replace(SkP,
                  skald_id == "Þjóðólfr ór Hvini" &
                    stanza == "Þjóð Yt 13",
                  "\\parencite[29]{whaley2012}" 
    ),
    SkP = replace(SkP,
                  skald_id == "Þjóðólfr ór Hvini" &
                    stanza == "Þjóð Yt 14",
                  "\\parencite[31]{whaley2012}" 
    ),
    SkP = replace(SkP,
                  skald_id == "Þjóðólfr ór Hvini" &
                    stanza == "Þjóð Yt 15",
                  "\\parencite[34]{whaley2012}" 
    ),
    SkP = replace(SkP,
                  skald_id == "Þjóðólfr ór Hvini" &
                    stanza == "Þjóð Yt 17",
                  "\\parencite[38]{whaley2012}" 
    ),
    SkP = replace(SkP,
                  skald_id == "Þjóðólfr ór Hvini" &
                    stanza == "Þjóð Yt 18",
                  "\\parencite[40]{whaley2012}" 
    ),
    SkP = replace(SkP,
                  skald_id == "Þjóðólfr ór Hvini" &
                    stanza == "Þjóð Yt 19",
                  "\\parencite[42]{whaley2012}" 
    ),
    SkP = replace(SkP,
                  skald_id == "Þjóðólfr ór Hvini" &
                    stanza == "Þjóð Yt 20",
                  "\\parencite[44]{whaley2012}" 
    ),
    SkP = replace(SkP,
                  skald_id == "Þjóðólfr ór Hvini" &
                    stanza == "Þjóð Yt 21",
                  "\\parencite[46]{whaley2012}" 
    ),
    SkP = replace(SkP,
                  skald_id == "Þjóðólfr ór Hvini" &
                    stanza == "Þjóð Yt 22",
                  "\\parencite[48]{whaley2012}" 
    ),
    SkP = replace(SkP,
                  skald_id == "Þjóðólfr ór Hvini" &
                    stanza == "Þjóð Yt 24",
                  "\\parencite[51]{whaley2012}" 
    ),
    SkP = replace(SkP,
                  skald_id == "Þjóðólfr ór Hvini" &
                    stanza == "Þjóð Yt 25",
                  "\\parencite[53]{whaley2012}" 
    ),
    SkP = replace(SkP,
                  skald_id == "Þjóðólfr ór Hvini" &
                    stanza == "Þjóð Yt 26",
                  "\\parencite[55]{whaley2012}" 
    ),
    SkP = replace(SkP,
                  skald_id == "Þjóðólfr ór Hvini" &
                    stanza == "Þjóð Yt 27",
                  "\\parencite[58]{whaley2012}" 
    ),
    SkP = replace(SkP,
                  skald_id == "Þjóðólfr ór Hvini" &
                    stanza == "Þjóð Haustl 1",
                  "\\parencite[432]{gade2017}" 
    ),
    SkP = replace(SkP,
                  skald_id == "Þjóðólfr ór Hvini" &
                    stanza == "Þjóð Haustl 3",
                  "\\parencite[435]{gade2017}" 
    ),
    SkP = replace(SkP,
                  skald_id == "Þjóðólfr ór Hvini" &
                    stanza == "Þjóð Haustl 4",
                  "\\parencite[437]{gade2017}" 
    ),
    SkP = replace(SkP,
                  skald_id == "Þjóðólfr ór Hvini" &
                    stanza == "Þjóð Haustl 6",
                  "\\parencite[440]{gade2017}" 
    ),
    SkP = replace(SkP,
                  skald_id == "Þjóðólfr ór Hvini" &
                    stanza == "Þjóð Haustl 7",
                  "\\parencite[441]{gade2017}" 
    ),
    SkP = replace(SkP,
                  skald_id == "Þjóðólfr ór Hvini" &
                    stanza == "Þjóð Haustl 10",
                  "\\parencite[446]{gade2017}" 
    ),
    SkP = replace(SkP,
                  skald_id == "Þjóðólfr ór Hvini" &
                    stanza == "Þjóð Haustl 11",
                  "\\parencite[447]{gade2017}" 
    ),
    SkP = replace(SkP,
                  skald_id == "Þjóðólfr ór Hvini" &
                    stanza == "Þjóð Haustl 12",
                  "\\parencite[449]{gade2017}" 
    ),
    SkP = replace(SkP,
                  skald_id == "Þjóðólfr ór Hvini" &
                    stanza == "Þjóð Haustl 14",
                  "\\parencite[453]{gade2017}" 
    ),
    SkP = replace(SkP,
                  skald_id == "Þjóðólfr ór Hvini" &
                    stanza == "Þjóð Haustl 15",
                  "\\parencite[455]{gade2017}" 
    ),
    SkP = replace(SkP,
                  skald_id == "Þjóðólfr ór Hvini" &
                    stanza == "Þjóð Haustl 17",
                  "\\parencite[457]{gade2017}" 
    ),
    SkP = replace(SkP,
                  skald_id == "Þjóðólfr ór Hvini" &
                    stanza == "Þjóð Haustl 19",
                  "\\parencite[460]{gade2017}" 
    ),
    SkP = replace(SkP,
                  skald_id == "Þjóðólfr ór Hvini" &
                    stanza == "Þjóð Haustl 20",
                  "\\parencite[461]{gade2017}" 
    ),
    SkP = replace(SkP,
                  skald_id == "Þjóðólfr ór Hvini" &
                    stanza == "Þjóð Har 1",
                  "\\parencite[61]{whaley2012}" 
    ),
    SkP = replace(SkP,
                  skald_id == "Þjóðólfr ór Hvini" &
                    stanza == "Þjóð Har 3",
                  "\\parencite[62]{whaley2012}" 
    ),
    SkP = replace(SkP,
                  skald_id == "Þjóðólfr ór Hvini" &
                    stanza == "Þjóð Har 4",
                  "\\parencite[62]{whaley2012}" 
    ),
    SkP = replace(SkP,
                  skald_id == "Þjóðólfr ór Hvini" &
                    stanza == "Þjóð Har 5",
                  "\\parencite[63]{whaley2012}" 
    ),
    SkP = replace(SkP,
                  skald_id == "Þjóðólfr ór Hvini" &
                    stanza == "Þjóð Lv 1",
                  "\\parencite[64]{whaley2012}" 
    )
  )

eilifr_data <- data_edit_12 %>% 
  filter(
    skald_id == "Eilífr Goðrúnarson")

data_edit_13 <- data_edit_12 %>% 
  mutate(
    period = replace(period,
                  skald_id == "Eilífr Goðrúnarson",
                  "c975-995" 
    ),
    SkP = replace(SkP,
                  skald_id == "Eilífr Goðrúnarson" &
                    stanza == "Eil Þdr 6",
                  "\\parencite[87]{gade2017}" 
    ),
    SkP = replace(SkP,
                  skald_id == "Eilífr Goðrúnarson" &
                    stanza == "Eil Þdr 7",
                  "\\parencite[89]{gade2017}" 
    ),
    SkP = replace(SkP,
                  skald_id == "Eilífr Goðrúnarson" &
                    stanza == "Eil Þdr 8",
                  "\\parencite[91]{gade2017}" 
    ),
    SkP = replace(SkP,
                  skald_id == "Eilífr Goðrúnarson" &
                    stanza == "Eil Þdr 9",
                  "\\parencite[95]{gade2017}" 
    ),
    SkP = replace(SkP,
                  skald_id == "Eilífr Goðrúnarson" &
                    stanza == "Eil Þdr 12",
                  "\\parencite[101]{gade2017}" 
    ),
    SkP = replace(SkP,
                  skald_id == "Eilífr Goðrúnarson" &
                    stanza == "Eil Þdr 14",
                  "\\parencite[105]{gade2017}" 
    ),
    SkP = replace(SkP,
                  skald_id == "Eilífr Goðrúnarson" &
                    stanza == "Eil Þdr 15",
                  "\\parencite[108]{gade2017}" 
    ),
    SkP = replace(SkP,
                  skald_id == "Eilífr Goðrúnarson" &
                    stanza == "Eil Þdr 18",
                  "\\parencite[115]{gade2017}" 
    ),
    SkP = replace(SkP,
                  skald_id == "Eilífr Goðrúnarson" &
                    stanza == "Eil Þdr 19",
                  "\\parencite[117]{gade2017}" 
    ),
    SkP = replace(SkP,
                  skald_id == "Eilífr Goðrúnarson" &
                    stanza == "Eil Þdr 21",
                  "\\parencite[121]{gade2017}" 
    )
  )


bragi_data <- data_edit_13 %>% 
  filter(
    skald_id == "Bragi inn gamli Boddason")

data_edit_14 <- data_edit_13 %>% 
  mutate(
    SkP = replace(SkP,
                  skald_id == "Bragi inn gamli Boddason" &
                    stanza == "Bragi Lv 1a",
                  "\\parencite{nordalForth}" 
    ),
    SkP = replace(SkP,
                  skald_id == "Bragi inn gamli Boddason" &
                    stanza == "Bragi Rdr 1",
                  "\\parencite[28]{gade2017}" 
    ),
    SkP = replace(SkP,
                  skald_id == "Bragi inn gamli Boddason" &
                    stanza == "Bragi Rdr 3",
                  "\\parencite[31]{gade2017}" 
    ),
    SkP = replace(SkP,
                  skald_id == "Bragi inn gamli Boddason" &
                    stanza == "Bragi Rdr 4",
                  "\\parencite[33]{gade2017}" 
    ),
    SkP = replace(SkP,
                  skald_id == "Bragi inn gamli Boddason" &
                    stanza == "Bragi Rdr 5",
                  "\\parencite[35]{gade2017}" 
    ),
    SkP = replace(SkP,
                  skald_id == "Bragi inn gamli Boddason" &
                    stanza == "Bragi Rdr 8",
                  "\\parencite[39]{gade2017}" 
    ),
    SkP = replace(SkP,
                  skald_id == "Bragi inn gamli Boddason" &
                    stanza == "Bragi Rdr 10",
                  "\\parencite[42]{gade2017}" 
    ),
    SkP = replace(SkP,
                  skald_id == "Bragi inn gamli Boddason" &
                    stanza == "Bragi Rdr 11",
                  "\\parencite[43]{gade2017}" 
    ),
    SkP = replace(SkP,
                  skald_id == "Bragi inn gamli Boddason" &
                    stanza == "Bragi Þórr 3",
                  "\\parencite[49]{gade2017}" 
    ),
    SkP = replace(SkP,
                  skald_id == "Bragi inn gamli Boddason" &
                    stanza == "Bragi Þórr 4",
                  "\\parencite[50]{gade2017}" 
    ),
    SkP = replace(SkP,
                  skald_id == "Bragi inn gamli Boddason" &
                    stanza == "Bragi Þórr 5",
                  "\\parencite[51]{gade2017}" 
    ),
    SkP = replace(SkP,
                  skald_id == "Bragi inn gamli Boddason" &
                    stanza == "Bragi Troll 1",
                  "\\parencite[64]{gade2017}" 
    )
  )


magod_data <- data_edit_14 %>% 
  filter(
    skald_id == "Magnús inn góði Óláfsson")

data_edit_15 <- data_edit_14 %>% 
  mutate(
    SkP = replace(SkP,
                  skald_id == "Magnús inn góði Óláfsson",
                  "\\parencite[5]{gade2009}" 
    ),
    period = replace(period,
                  skald_id == "Magnús inn góði Óláfsson",
                  "c1046-1047" 
    )
  )

sigjor_data <- data_edit_15 %>% 
  filter(
    skald_id == "Sigurðr jórsalafari Magnússon")

data_edit_16 <- data_edit_15 %>% 
  mutate(
    SkP = replace(SkP,
                  skald_id == "Sigurðr jórsalafari Magnússon",
                  "\\parencite[468]{gade2009}" 
    ),
    period = replace(period,
                     skald_id == "Sigurðr jórsalafari Magnússon",
                     "c1111-1122" 
    )
  )

oltry_data <- data_edit_15 %>% 
  filter(
    skald_id %in% c("Óláfr Tryggvason","*Óláfr Tryggvason"))

data_edit_17 <- data_edit_16 %>% 
  mutate(
    SkP = replace(SkP,
                  skald_id == "*Óláfr Tryggvason" &
                    stanza == "ÓTr Lv 2",
                  "\\parencite[384]{gade2009}" 
    ),
    period = replace(period,
                     skald_id == "*Óláfr Tryggvason" &
                       stanza == "ÓTr Lv 2",
                     "c995-1000" 
    ),
    skald_id = replace(skald_id,
                  skald_id == "*Óláfr Tryggvason",
                  "Óláfr Tryggvason" 
    ),
    SkP = replace(SkP,
                  skald_id == "Óláfr Tryggvason" &
                    stanza == "ÓTr Lv 1",
                  "\\parencite[879]{ross2022}" 
    )
  )

olof_data <- data_edit_17 %>% 
  filter(
    skald_id == "Ólǫf geisli Þórisdóttir")

data_edit_18 <- data_edit_17 %>% 
  mutate(
    SkP = replace(SkP,
                  skald_id == "Ólǫf geisli Þórisdóttir",
                  "\\parencite[1410]{ross2022}" 
    )
  )

onund_data <- data_edit_18 %>% 
  filter(
    skald_id == "Ǫnundr tréfótr Ófeigsson")

data_edit_19 <- data_edit_18 %>% 
  mutate(
    SkP = replace(SkP,
                  skald_id == "Ǫnundr tréfótr Ófeigsson",
                  "\\parencite[646]{ross2022}" 
    ),
    period = replace(period,
                     skald_id == "Ǫnundr tréfótr Ófeigsson",
                     "c885-892" 
    )
  )

audunn_data <- data_edit_19 %>% 
  filter(
    skald_id == "Auðunn illskælda")

data_edit_20 <- data_edit_19 %>% 
  mutate(
    SkP = replace(SkP,
                  skald_id == "Auðunn illskælda",
                  "\\parencite[122]{whaley2012}" 
    )
  )

guthorm_data <- data_edit_20 %>% 
  filter(
    skald_id == "Guthormr sindri")

data_edit_21 <- data_edit_20 %>% 
  mutate(
    period = replace(period,
                     skald_id == "Guthormr sindri",
                     "c955-961" 
    ),
    SkP = replace(SkP,
                  skald_id == "Guthormr sindri" &
                    stanza == "Gsind Hákdr 1",
                  "\\parencite[157]{whaley2012}" 
    ),
    SkP = replace(SkP,
                  skald_id == "Guthormr sindri" &
                    stanza == "Gsind Hákdr 6",
                  "\\parencite[164]{whaley2012}" 
    ),
    SkP = replace(SkP,
                  skald_id == "Guthormr sindri" &
                    stanza == "Gsind Hákdr 8",
                  "\\parencite[168]{whaley2012}" 
    )
  )

vitgeirr_data <- data_edit_21 %>% 
  filter(
    skald_id == "Vitgeirr seiðmaðr")

data_edit_22 <- data_edit_21 %>% 
  mutate(
    SkP = replace(SkP,
                  skald_id == "Vitgeirr seiðmaðr",
                  "\\parencite[141]{whaley2012}" 
    )
  )


thorgeirr_data <- data_edit_22 %>% 
  filter(
    skald_id == "Þorgeirr flekkr")

data_edit_23 <- data_edit_22 %>% 
  mutate(
    period = replace(period,
                     skald_id == "Þorgeirr flekkr",
                     "c1038" 
    ),
    SkP = replace(SkP,
                  skald_id == "Þorgeirr flekkr",
                  "\\parencite[9]{gade2009}" 
    )
  )


thorgils_data <- data_edit_23 %>% 
  filter(
    skald_id == "Þorgils fiskimaðr")

data_edit_24 <- data_edit_23 %>% 
  mutate(
    period = replace(period,
                     skald_id == "Þorgils fiskimaðr",
                     "c1046-1066" 
    ),
    SkP = replace(SkP,
                  skald_id == "Þorgils fiskimaðr",
                  "\\parencite[333]{gade2009}" 
    )
  )

kveldulf_data <- data_edit_24 %>% 
  filter(
    skald_id == "Kveldúlfr Bjálfason")

data_edit_25 <- data_edit_24 %>% 
  mutate(
    SkP = replace(SkP,
                  skald_id == "Kveldúlfr Bjálfason",
                  "\\parencite[162]{ross2022}" 
    )
  )


skallagrim_data <- data_edit_24 %>% 
  filter(
    skald_id == "Skallagrímr Kveldúlfsson")

data_edit_26 <- data_edit_25 %>% 
  mutate(
    SkP = replace(SkP,
                  skald_id == "Skallagrímr Kveldúlfsson",
                  "\\parencite[162]{ross2022}" 
    )
  )


N171_data <- data_edit_26 %>% 
  filter(
    stanza == "R N 171")

data_edit_27 <- data_edit_26 %>% 
  mutate(
    skald_id = replace(skald_id,
                       stanza == "R N 171",
                       "Hallvarðr Grenski" 
    ),
    period = replace(period,
                     skald_id == "Hallvarðr Grenski",
                     "c1197" 
    ),
    SkP = replace(SkP,
                  skald_id == "Hallvarðr Grenski",
                  "\\parencite{N171_2020}" 
    )
  )

A39_data <- data_edit_26 %>% 
  filter(
    stanza == "R N A39")

data_edit_28 <- data_edit_27 %>% 
  mutate(
    skald_id = replace(skald_id,
                  stanza == "R N A39",
                  "Eilífr Virðivægr" 
    ),
    period = replace(period,
                     skald_id == "Eilífr Virðivægr",
                     "c1250-1325" 
    ),
    SkP = replace(SkP,
                  skald_id == "Eilífr Virðivægr",
                  "\\parencite{NA39_2020}" 
    )
  )

B249_data <- data_edit_26 %>% 
  filter(
    stanza == "R N B249")


B380_data <- data_edit_26 %>% 
  filter(
    stanza == "R N B380")

data_edit_29 <- data_edit_28 %>% 
  mutate(
    SkP = replace(SkP,
                  stanza == "R N B380",
                  "\\parencite{NB380_2020}" 
    )
  )


N319_data <- data_edit_26 %>% 
  filter(
    stanza == "R N 319")

data_edit_30 <- data_edit_29 %>% 
  mutate(
    SkP = replace(SkP,
                  stanza == "R N 319",
                  "\\parencite{N319_2020}" 
    )
  )


GRNoR_data <- data_edit_26 %>% 
  filter(
    stanza == "R GR NoR1998:10")

data_edit_31 <- data_edit_30 %>% 
  mutate(
    stanza = replace(stanza,
                     stanza == "R GR NoR1998:10",
                     "R GR NoR1998;10" 
    ),
    SkP = replace(SkP,
                  stanza == "R GR NoR1998;10",
                  "\\parencite{GRNOR1998;10_2020}" 
    )
  )

B496_data <- data_edit_31 %>% 
  filter(
    stanza == "R N B496")

data_edit_32 <- data_edit_31 %>% 
  mutate(
    SkP = replace(SkP,
                  stanza == "R N B496",
                  "\\parencite{NB496_2020}" 
    ),
    type = replace(type,
                  stanza == "R N B496",
                  "CY" 
    )
  )

B249_data <- data_edit_31 %>% 
  filter(
    stanza == "R N B249")

data_edit_33 <- data_edit_32 %>% 
  mutate(
    SkP = replace(SkP,
                  stanza == "R N B249",
                  "\\parencite{NB249_2020}" 
    ),
    skald_id = replace(skald_id,
                       stanza == "R N B249",
                       "Sigurðr Amundason" 
    )
  )


#### Information about the data ####
stanzas_remaining <- data_edit_33 %>%
  filter(is.na(SkP)) %>%
  distinct(stanza, skald_id)


stanzas__remaining_count <- stanzas_remaining %>%
  count(skald_id, name = "stanza_amount")


skalds <- unique(data_edit_33$skald_id)
skalds
skald_count <- length(skalds)

stanzas <- data_edit_33 %>% distinct(stanza, skald_id)
nrow(stanzas)

stanza_count <- stanzas %>%
  count(skald_id, name = "stanza_amount")


#### Writing the CSV ####
write.csv(data_edit_33, "poetry_data.csv")
