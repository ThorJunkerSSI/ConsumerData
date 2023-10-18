##LOAD helper functions


#############################
#fix_utf8_and_casing_function
#############################
fix_utf8_and_casing_function<-function(x){
  u<-stringr::str_replace_all(tolower(x),"ã¥|å","aa")
  y<-stringr::str_replace_all(tolower(u),"Ã¼|ü","uu")
  z<-stringr::str_replace_all(tolower(y),"ã¸|ø|õ|ô|ö","oe")
  q<-stringr::str_replace_all(tolower(z),"ã¦|æ|ä","ae")
  q}


#---------------------------------------------------------------------------------------------------
##function that import cpd datasets
##--------------------------------------------------------------------------

##data from kemiluppen 2022
cpd_import<-function(cpd_data){
  
  cpd <- readr::read_csv(cpd_data) %>% 
    rename_all(tolower)%>%  mutate_if(is.character, ~fix_utf8_and_casing_function(.))
  cpd
}

#############################
#Import kemiluppen data
#############################
#Clean up dataframe and remove all rows with gtin number shorter than 8 or longer than 14 numbers as 
#they are considered incomplete. Lastly column names are renamed to fit with final dataframe.
import_kemiluppen<-function(data, old.names, target.vars){
  
  kemiluppen_ssi <- readxl::read_excel(data) %>%
    rename_all(tolower)%>% mutate(kilde_kemiluppen=10, ean=as.character(ean))%>%
    mutate_all(~fix_utf8_and_casing_function(.))

  names(kemiluppen_ssi)<-c("gtin","produktnavn" ,   "kategori" ,"underkategori", "ingredientstatement", "klassificering", "kilde")
  
  kemiluppen_ssi <- kemiluppen_ssi %>%
    rowwise() %>% 
    mutate(gtin_length=str_count(gtin,"[:digit:]")) %>%
    filter(!(gtin_length>14|gtin_length<8|gtin=="")) %>%## we remove product that are not at least upc standard
    mutate(gtin=paste0(str_flatten(rep("0",(14-gtin_length)),collapse=""),gtin)) %>%
    rename_with( ~ target.vars[which(!is.na(old.names))], .cols = old.names[!is.na(old.names)]) %>%
    dplyr::select(target.vars[!is.na(old.names)]) %>%
    mutate(datasource = "Kemiluppen")
  
  
  kemiluppen_ssi
}

##############################################
#Import gs1 and gpc. Then combine the two data
##############################################
gs1_gpc_import <- function(gs1_path, gpc_path){
  gs1_ssi <- readr::read_csv(gs1_path)  %>%  rename_all(tolower)
  
  gpc <- read.csv(file.path(gpc_path)) %>% group_by(segmentcode,segmenttitle,familycode,familytitle,classcode,classtitle,brickcode,bricktitle)%>%
    tally() %>% mutate(brickcode1=as.character(brickcode))
  
  gs1_gpc <- gs1_ssi %>% left_join(gpc, by = c("gpccategorycode" = "brickcode"))
  gs1_gpc
}


########################################
#Download and edit open food facts data
########################################
download_open_food_facts<-function(open_food_facts_link){
  
  openfoodfacts <- readr::read_delim(open_food_facts_link,
                                     delim="\t",col_names=TRUE,n_max = 1000, 
                                     col_select=c(code,categories_en,product_name,quantity,ingredients_text,created_datetime,
                                                  proteins_100g, carbohydrates_100g, fat_100g, energy_100g,
                                                  countries_en)) %>% filter(!(is.na(quantity) & is.na(ingredients_text)))
  
  openfoodfacts

}

#################################
#Clean the openfoodfacts dataset
#################################
#Remove incomplete gtin numbers i.e less than 8 numbers or more than 14 and any potentional duplicates.
#Select and rename the desired columns to generate final dataframe
openfoodfacts_clean<-function(openfoodfacts, target.vars, old.names){
  
  openfoodfacts2 <- openfoodfacts %>%
    mutate(gtin_length=str_count(code,"[:digit:]"),
           gtin_zeros=str_count(code,"0")) %>%
    filter(!(gtin_length>14|gtin_length<8|gtin_zeros>7| code=="")) %>%## we remove product that are not at least upc standard
    distinct(code, .keep_all = TRUE)%>%select(-gtin_zeros) %>% ##reveals only 24 duplicates manual inspection shows the are the same products
    rowwise() %>% 
    mutate(code=paste0(str_flatten(rep("0",(14-gtin_length)),collapse=""),code))
  
  
  ##create dataset with only product names and itemnumbers
  openfoodfacts3 <- openfoodfacts2%>%
    mutate(ingredients_text = tolower(ingredients_text),
           weight = gsub("[^[:digit:]]", "", quantity),
           weight_unit = gsub("[[:digit:]]", "", quantity),
           weight_unit = str_replace_all(weight_unit, "[^[:alnum:]]", "")) %>%
    rename_with( ~ target.vars[which(!is.na(old.names))], .cols = old.names[!is.na(old.names)]) %>%
    dplyr::select(target.vars[!is.na(old.names)]) %>%
    mutate(datasource = "openfoodfacts")

  write.csv(openfoodfacts3,file.path(file_path,"OpenFoodFacts.csv"), row.names = FALSE)
  
  openfoodfacts3
}

#############################
#fix_weight_in_cpd_amount
#############################
fix_weight_in_cpd_amount <- function(cpd){
  
  ## make sure that we have purchases, not just scans (that are later removed) and take care of returned goods (that can be presents etc.)
  ## are there any duplicate receipts?
  ##grouping to aggregate on receipt level- demographics not included as they are added above
  
  ## this code fixes the following issues:
  ## Some product are sold by weight, with the weight put in the amount line that is most often the number of items sold
  ##products may be returned in the cashier line, and each purchase may be different when it comes to the discount received, thus returns are corrected for each receipt
  ##
  cpd1<-cpd%>% rename_all(tolower) %>% 
    mutate(
      after_dot=str_detect(amount,("\\.[0-9]|\\.[0-9][1-9]")),
      itemprice=as.numeric(itemprice),
      amount=as.numeric(amount),
      rabat=as.numeric(rabat),
      total_price=amount*itemprice,
      itemname=tolower(itemname),
      weight_amount=case_when(
        after_dot==TRUE~  as.numeric(amount),
        str_detect(itemname,("betjent|vej selv|delikate|koedtorvet|fiskeafdeling|kartofler vaskede|vaegt|fragt"))~as.numeric(amount),
        str_detect(itemname,("kartofler")) & itemprice==0.01~as.numeric(amount)),
      amount=case_when(
        is.na(weight_amount)==FALSE~ 1,
        is.na(weight_amount)==TRUE~amount),
      total_price=case_when( ## adjust total price for prices where the number of items should not be multiplied
        is.na(weight_amount)~total_price,
        !is.na(weight_amount)~total_price, ## sensitive to petrol costs
        !is.na(weight_amount)&itemprice>20~itemprice),
      original_price=case_when(
        is.na(weight_amount)~total_price-rabat,
        !is.na(weight_amount)~total_price-rabat, ## sensitive to petrol costs
        !is.na(weight_amount)&itemprice>20~itemprice-rabat),
    )%>% 
    group_by(participant,receipt, itemname,itemnumber,category,itemprice, rabat,purchdate, weekno,monthno,yearno,merchantname,weight_amount,
             gender, dateofbirth) %>% 
    dplyr::summarise(amount=sum(amount),total_price=sum(total_price), total_original_price=sum(original_price)) %>% 
    arrange(participant,amount,purchdate)%>% filter(!amount==0) ## this section agregates on receiptmlevel and removes pruchases amounting to zero i.e. bought on and returned one ##if there is some where i.e. exactly one kg are sold this should be fixed as well ## aggregate on each person receipt to also summarise multiple scans
  
  ##calculate weight to rice ratio for later use- to help validate units
  cpd1$weight_price_ratio<-NA
  cpd1$weight_price_ratio[!is.na(cpd1$weight_amount)]<-as.numeric(cpd1$total_price[!is.na(cpd1$weight_amount)])/as.numeric(cpd1$weight_amount[!is.na(cpd1$weight_amount)])
  cpd1$weight_amount[str_detect(cpd1$itemname, "vej( |-)selv( |-)slik")& (str_detect(cpd1$merchantname,"Loevbjerg"))]<-cpd1$itemprice[str_detect(cpd1$itemname, "vej( |-)selv( |-)slik")& (str_detect(cpd1$merchantname,"Loevbjerg"))]/100
  
  ## NB! we remove returned goods for now but store them for possible later use, we could look for them in previous receipts
  #cpd_neg<-cpd1%>%filter(itemprice<0) %>%  group_by(itemname, itemnumber)%>% dplyr::summarise(total_amount=sum(amount))
  
  
  
  ##negative amounts are discounts and not necessarily returns off i.e. bottles, "PANT" etc. see example with wasa.
  #cpd_wasa<-cpd1%>%filter(str_detect(itemname,"WASA") |str_detect(itemname,"wasa") )
  
  ## many products sold seem to be sold by g instead of kg, perhaps also some by 100g for now we make rough fix.
  ## we will use this assumption for now but return to revise later.
  
  # weight_by_retailer <- cpd1%>%group_by(merchantname)%>% dplyr::summarise(avg=mean(weight_amount,na.rm=TRUE),
  #                                                                       mini=min(weight_amount,na.rm=TRUE),
  #                                                                       maxi=max(weight_amount,na.rm=TRUE),
  #                                                                       avgweight_price_ratio=mean(weight_price_ratio,na.rm=TRUE),
  #                                                                       miniweight_price_ratio=min(weight_price_ratio,na.rm=TRUE),
  #                                                                       maxiweight_price_ratio=max(weight_price_ratio,na.rm=TRUE))
  
  ## seems to have fixed most of the issues- we return to this in the validation part
  ##cpd1$weight_amount[cpd1$weight_price_ratio<2 & !is.na(cpd1$weight_price_ratio)]<-cpd1$weight_amount[cpd1$weight_price_ratio<2 & !is.na(cpd1$weight_price_ratio)]/1000 ## sold in grams
  
  # cpd_vej <- cpd[str_detect(tolower(cpd$itemname), "vej( |-)selv( |-)slik"),]
  # 
   receipts1<- cpd1%>% group_by(participant,receipt,purchdate)%>% dplyr::summarise(totalorderpricevalue=sum(total_price))
  # 
  # receipts2<-receipts1 %>% group_by(participant,receipt,purchdate,totalorderpricevalue)%>%tally()
  # 
  ##n max 1 means that we have no duplicate receipts
  ## we add total price to cpd dataset
  
  cpd2 <- left_join(cpd1,receipts1) %>%
    mutate(itemnumber=str_replace_all(itemnumber,"-|_",""),
           gtin_length=str_count(itemnumber,"[:digit:]"),
           itemname=tolower(itemname))%>% filter(!(amount<0| gtin_length>14| itemnumber==""| itemnumber=="BLANK"|itemnumber=="VOUCHER"| itemname=="" |itemname==" " | itemprice<0))%>%
    filter(is.na(weight_amount)|weight_amount>0) %>%##exclude returns not on the same receipt, remove 0, and product with no GTIN possibly include gtin_length<2, gtin_length>14
    mutate(itemnumber_fixed=paste0(str_flatten(rep(0,(14-gtin_length)),collapse=""),itemnumber),
           itemnumber=itemnumber_fixed)%>% select(-itemnumber_fixed)
  
  cpd2 
}

#---------------------------------------------------------------------------------------------------
##Enrich consumerdata from product names and create combined product database with variables derived directly from product names
#---------------------------------------------------------------------------------------------------

create_weight_volume_eco_country_vars<-function(cpd2){
  ##create dataset with only product names and itemnumbers for name match
  cpd_match_name <- cpd2%>% 
    group_by(itemname, itemnumber) %>% dplyr::summarize(count_item=1, total_items=sum(amount))%>%arrange(desc(total_items))
  
  names(cpd_match_name)
  
  ##extract and remove dosage information and filter less relevant merchants
  ## create product dataset where each product is unique. product dataset contains aggregated purchase frequencies as well for later use.
  ## code be improved even more with dosage and pack combos for i.e. gajol, and a few other changes beer carlsb. 1883
  cpd_proddata<- cpd_match_name%>%
    mutate(newname= str_squish(itemname),
           newname=str_replace(newname,"(\\&)","og"),
           newname=str_replace(newname,"(a38)","acidophilus tykmaelk"),
           newname=str_replace(newname,"(\\?)","1/2"),
           newname=str_replace(newname,"\\b1883\\b","33cl"),
           newname=str_replace(newname,"REMA 1000|\\?|^ |_|\\?|\\&|vaegt",""),
           newname=str_replace(newname,"(\\#)",""),
           ## detect eco products from the product name
           eco=case_when(
             str_detect(newname,"(konventionel)|(\\bikke oe.o[:alpha:]*\\b)")~ "non eco", 
             str_detect(newname,"\\boe.o[:alpha:]*\\b")~ "eco", 
             TRUE                      ~ "unkwown"),
           ## detect country from the product name
           country=case_when(
             str_detect(newname,"(^|\\s+)d(k|ansk(e|))(\\s+|$)")~ "dk", 
             str_detect(newname,"(^|\\s+)udl(\\.|and(|ske))(\\s+|$)")~ "other",
             str_detect(newname,"(^|\\s+)tysk(|land(e|ske|))(\\s+|$)")~ "other", 
             TRUE                      ~ "unkwown"),
           ##remove eco and country from name
           newname=str_replace(tolower(newname),"(^|\\s+)d(k|ansk(e|))(\\s+|$)|(^|\\s+)udl(\\.|and(|ske))(\\s+|$)|(^|\\s+)tysk(|land(e|ske|))(\\s+|$)|(loese)|(kugle)
                                        (konventionel)|(\\bikke oe.o[:alpha:]*\\b)|(\\boe.o[:alpha:]*\\b)|
                                        (^|\\s+)d(k|ansk(e|))(\\s+|$)|(^|\\s+)tysk(|land(e|ske|))(\\s+|$)",""),
           ##get information about fat % or alcohol % or product type from products
           product_type_raw=str_extract(tolower(newname),"(([[:digit:]]{1,4})(s+|)((\\.|\\,|\\/|)([[:blank:]]|)([[:digit:]]{1,4})|)((%|\\+|t(b|vol|)))(\\.|)|([sml](\\/)[sml(xl)])|(\\b[sml(xl)]\\b)( |$))"),
           product_type_meassure=str_extract(product_type_raw,"([[:digit:]]{1,4}(s+|)((\\.|\\,|\\/|)([[:blank:]]|)([[:digit:]]{1,4})|))"),
           product_type_unit=str_replace(product_type_raw,"([[:digit:]]{1,4}(s+|)((\\.|\\,|\\/)([[:blank:]]|)([[:digit:]]{1,4})|))",""),
           ## get information about packages i.e. 6 pak, number of products sold i.e. 20 aeg etc.
           pieces_raw=str_extract(tolower(newname),"(\\b|^|[[:blank:]]|)([[:digit:]]{1,4})(|[[:blank:]]|)(x|\\*|fl|st(y|)(k|)|pcs|ds.|bk|bakke|(-|)(pk|pa[:alpha:]k|pack[:alpha:]|pak)|\\?s|rl)"),#($|\\.|\\,|\\s| |)
           pieces=case_when(
             str_detect(newname,"aeg")&str_detect(newname,"\\b(([sml](\\/)[sml(xl)])|([sml(xl)]))\\b|$")~str_extract(newname,"(\\b|^|[[:blank:]]|)([[:digit:]]{1,4})"),
             TRUE~str_extract(pieces_raw,"(\\b|^|[[:blank:]]|)([[:digit:]]{1,4})")),
           ##remove the information already extracted above
           newname=str_replace(newname,"([[:digit:]]{1,4}(s+|)((\\.|\\,|x|-|\\*|\\/)([[:blank:]]|)([[:digit:]]{1,4})|)()(%|\\+))|(([sml]\\/[sml(xl)])( |$))([[:digit:]]{1,4})(s+|)(x|\\*|fl|st(y|)(k|)|pcs|hb|bk|pk|pak|pack|pak|\\?s|rl|([sml]\\/[sml(xl)]))($|\\.|\\,|\\.|\\s| |)",""),
           newname=str_replace(newname,"(\\b|^|[[:blank:]]|)([[:digit:]]{1,4})(|[[:blank:]]|)(x|\\*|fl|st(y|)(k|)|pcs|bk|(-|)(pk|pak|pack|pak)|\\?s|rl|([sml]\\/[sml(xl)]))",""),
           ## identify multipliers
           ##multiplier=str_extract(tolower(newname),"[[:digit:]]{1,4}(s+|)(x|\\*|pk|-|\\/)(s+|)[[:digit:]]{1,4}((\\.|\\,|x|-|\\*|\\/)([[:blank:]]|)([[:digit:]]{1,4})|)"),
           number_name=str_extract(tolower(newname),"(\\b|^|[[:blank:]]|)([[:digit:]]{1,4}((|[[:blank:]]|)(\\.|\\,|x|-|\\*|\\/)([[:blank:]]|)[[:digit:]]{1,4}|)((\\.|\\,|x|-|\\*|\\/)([[:blank:]]|)([[:digit:]]{1,4})|))([[:blank:]]|)((((kg\\.|kg|kilo|kil|g(r|ra|ram|\\.|))|l(ite|iter|\\.|[[:blank:]]|\\b)|cl|ml|fl|st(y|)(k|)|(pcs))(\\.|-|)|\\+|%|hb|bk(\\,|\\.|\\s| |)|(-|)(pk(\\.|)|pa(c|)k|\\?s|rl)($|\\.|)|t(b|)|mc|xl|[sml]\\/[sml(xl)])|$)"),
           ##extract not weight units here
           ##number_name=str_extract(tolower(newname),"(\\b|^|[[:blank:]]|)([[:digit:]]{1,4})([[:blank:]]|)(\\.|\\,|x|-|\\*|\\/|)([[:blank:]]|)([[:digit:]]{1,4}|)(\\.|\\,|x|-|\\*|\\/)([[:blank:]]|)([[:digit:]]{1,4}|)([[:blank:]]|)(kg|kilo|kil|g(r|ra|ram|)|(l(ite|iter|))|cl|ml|fl|(st(y|)(k|))|(pcs))([[:blank:]]|\\b|\\.|-|$|)"),
           ##extract not weight units here
           dosage=str_extract(tolower(newname),"((^| )| |^|)(\\b[[:digit:]]+(\\.|\\,|x|-|\\*|\\/)*[[:digit:]]*(\\s+|)(mg)($|\\.|))"),
           unit_raw=str_extract(tolower(number_name),"((((kg\\.|kg|kilo|kil|g(r|ra|ram|\\.|))|l(ite|iter|\\.|)|cl|ml|fl|st(y|)(k|)|(pcs))(\\.|-|)|\\+|%|hb|bk(\\,|\\.|\\s| |)|(-|)(pk(\\.|)|pak|pack|pak|\\?s|rl)($|\\.|)|t(b|)|mc|xl|[sml]\\/[sml(xl)])|$)"),
           measure_raw=str_remove(tolower(number_name),"((((kg\\.|kg|kilo|kil|g(r|ra|ram|\\.|))|l(ite|iter|\\.|)|cl|ml|fl|st(y|)(k|)|(pcs))(\\.|-|)|\\+|%|hb|bk(\\,|\\.|\\s| |)|(-|)(pk(\\.|)|pak|pack|pak|\\?s|rl)($|\\.|)|t(b|)|mc|xl|[sml]\\/[sml(xl)])|$)"),
           measure_raw=str_replace(measure_raw,"x","*"), ##replaces x with * to calculate measure once numeric
           newname=str_replace(tolower(newname),"(\\b|^|[[:blank:]]|)([[:digit:]]{1,4}((|[[:blank:]]|)(\\.|\\,|x|-|\\*|\\/)([[:blank:]]|)[[:digit:]]{1,4}|)((\\.|\\,|x|-|\\*|\\/)([[:blank:]]|)([[:digit:]]{1,4})|))([[:blank:]]|)((((kg\\.|kg|kilo|kil|g(r|ra|ram|\\.|))|l(ite|iter|\\.|[[:blank:]]|\\b)|cl|ml|fl|st(y|)(k|)|(pcs))(\\.|-|)|\\+|%|hb|bk(\\,|\\.|\\s| |)|(-|)(pk(\\.|)|pa(c|)k|\\?s|rl)($|\\.|)|t(b|)|mc|xl|[sml]\\/[sml(xl)])|$)((\\.|\\,|\\/)|)",""),
           newname=str_replace(tolower(newname),"\\.|\\,|-|\\*|\\/|\\+|%",""),
           newname=str_replace(tolower(newname),"(\\b)(m|i|vores|med|salling|gestus|og|til|a|c|s|first|loegismose)\\b",""),
           ##newname=str_replace(tolower(newname),"(r|ene|ende)\\b",""), ##flertals endelser could use NLP Stemming Words functions
           newname= str_squish(newname),
           adjustment=case_when( 
             str_detect(unit_raw,"(cl)")~ "10", 
             str_detect(unit_raw,"(kg\\.|kg|kilo|kil)")~ "1000", 
             str_detect(unit_raw,"(^)(l(ite|iter|\\..|))")~ "1000", 
             str_detect(unit_raw,"(ml)")~ "1", 
             str_detect(unit_raw,"(%|\\+)")~ "1", 
             TRUE                      ~ "1"),
           measure_raw=str_squish(measure_raw),
           measure=case_when(
             str_detect(measure_raw,"\\/")~ as.numeric(str_extract_all(measure_raw,"[[:digit:]]{1,4}(s+|)")[[1]][1])/as.numeric(str_extract_all(measure_raw,"[[:digit:]]{1,4}(s+|)")[[1]][2]), 
             str_detect(measure_raw,"\\*")~ as.numeric(str_extract_all(measure_raw,"[[:digit:]]{1,4}(s+|)")[[1]][1])*as.numeric(str_extract_all(measure_raw,"[[:digit:]]{1,4}(s+|)")[[1]][2]), 
             ## we take the mean of the ranges i.e. 1100-1200g
             str_detect(measure_raw,"-")~ (as.numeric(str_extract_all(measure_raw,"[[:digit:]]{1,4}(s+|)")[[1]][1])+as.numeric(str_extract_all(measure_raw,"[[:digit:]]{1,4}(s+|)")[[1]][2]))/2, 
             TRUE                      ~ as.numeric(measure_raw)),
           final_unit=case_when( ##not converted yet!)
             str_detect(unit_raw,"(cl)")~ "volume_ml", 
             str_detect(unit_raw,"(g(r|ra|ram|\\.|)|(kg\\.|kg|kilo|kil))")~ "weight_g", 
             str_detect(unit_raw,"(ml|cl|l(ite|iter|\\.|))")~ "volume_ml", 
             str_detect(unit_raw,"(bk(\\,|\\.|\\s| |))")~ "bakke_box", 
             TRUE                      ~ "unkwown"),
           adjustment=as.numeric(adjustment))%>%
    rename(name=newname)
  cpd_proddata
  ##still could improve a bit, i.e. by inferring that harboe 6x50 is 6 x 50 cl based on similar patterns- requires knowledge of product type before implementation
} 

cpd_itemname_measure <-function(cpd_data){
  
  ##join product information directly extracted from name and frida
  #cpd_frida <-left_join(cpd_data,frida_datasource, by = c("itemname"))
  ## combines ingredients needed to adjust for more than one ingredient statement or check intra dataset variation
  
  ##table(str_detect(names(cpd_match_all3_1), "nummer"))
  cpd_data1 <- cpd_data %>%  
    #select(itemnumber,itemname,pieces,total_items,measure,netweight_gs1,netweightmeasurementunitcode_gs1,unit_raw,weight,
    #kilde_gs1,kilde_frida,kilde_kemiluppen,foodname_frida, foedevarenavn_frida)%>%
    rename(pieces1=pieces,
           weight = measure )%>%
    mutate(weight = as.character(weight),
           pieces=case_when(
             (str_detect(itemname,"aeg")&is.na(pieces1))~ as.numeric(weight), 
             is.na(pieces1)~ as.numeric(pieces1),
             TRUE~as.numeric(pieces1)),
           ##                                      replace_na(pieces1,"1"))%>%
           total_weight1= as.numeric(weight)*as.numeric(pieces),
           total_weight= as.character(case_when(
             str_detect(unit_raw,"(^|\\s)g(r|ra|ram|m|\\.|)")~ total_weight1, 
             str_detect(unit_raw,"(kg\\.|kg|kilo|kil)")~ total_weight1*1000,
             str_detect(unit_raw,"\\b(l(ite|iter|\\.|))")~ total_weight1*1000,
             str_detect(unit_raw,"cl")~ total_weight1*10, 
             str_detect(unit_raw,"ml")~ total_weight1, 
             TRUE~total_weight1)),
           weight_unit=case_when( 
             str_detect(unit_raw,"(cl)")~ "ml", 
             str_detect(unit_raw,"(g(r|ra|ram|m|\\.|)|(kg\\.|kg|kilo|kil))")~ "grm", 
             str_detect(unit_raw,"(ml|cl|l(ite|iter|\\.|))")~ "ml"),
           datasource = "purchased_itemname") %>% select(itemname, itemnumber, pieces, weight, total_weight, weight_unit, datasource)
  
  
  cpd_data1
}



#---------------------------------------------------------------------------------------------------
##import frida stems and nutricient information from frida 
#---------------------------------------------------------------------------------------------------

import_produkt_and_frida_stems<-function(frida_match_data){
  
  
  #frida_match_data<-file.path(my_directory,"Data","frida_match_data_26_10_22.xlsx")
  ##If not already done above we download the slightly enriched frida dataset with stems and regex to match products
  frida_nutrient <- readxl::read_excel(file.path(my_directory,"Data/Common","frida_nutrient_data.xlsx"), sheet = "frida_nutrient_data")
  names(frida_nutrient)<-unlist(lapply(tolower(names(frida_nutrient)),fix_utf8_and_casing_function))
  
  frida_nutrient <-frida_nutrient%>% 
    rename_all(tolower)   %>% 
    mutate_all(~fix_utf8_and_casing_function(.))%>%
    mutate(across(where(is.character), tolower),
           across(where(is.character), str_squish), 
           across(.cols = everything(),~str_replace( ., "^na$", "empty" ))) %>%
    mutate(foodgroup_eng = foodgroup) %>% dplyr::select(-foodgroup)
  
  
 # frida_match_data <- "G:/ConsumerData/Data/SMIL-frida_match_2403023_newstructure.xlsx"
  frida_match1 <- readxl::read_excel(frida_match_data, sheet = "All_stems_work_in_progress") 
  
  names(frida_match1)<-unlist(lapply(tolower(names(frida_match1)),fix_utf8_and_casing_function))
  
  frida_match<-frida_match1%>% 
    rename_all(tolower)   %>% 
    mutate_all(~fix_utf8_and_casing_function(.))%>%
    mutate(across(where(is.character), tolower),
           across(where(is.character), str_squish), 
           across(.cols = everything(),~str_replace( ., "^na$", "empty" ))) %>%
    left_join(frida_nutrient, by = c("nummer" = "foodid"))
}

#############################
#find_tied_names_function
#############################


# import_frida_generic_product_data<-function(frida_match_data){
#   ##import all the data from frida to further enrich products
#   frida_enrich_data <- readxl::read_excel(frida_match_data, sheet = "Sheet1") %>% 
#     mutate(nummer=as.character(nummer))
#   names(frida_enrich_data)<-sapply(tolower(paste(names(frida_enrich_data),"_frida",sep="")),fix_utf8_and_casing_function)
#   frida_enrich_data 
# }

evaluate_overlap_in_frida_product_stems<-function(frida_match){
  
  ##If not already done above we download the slightly enriched frida dataset with stems and regex to match products
  
  ##Create output dataset for match function to iterate to iterate over
  ##Create output dataset for match function to iterate to iterate over
  cpd_tied<-as.data.frame(rbind(c(rep(NA,ncol(frida_match)+12))))
  names(cpd_tied)<-c(paste(names(frida_match)),"match1","match2","match3","match4","not_match","match_score","match_navn","match_stem1","match_stem2","match_stem3","match_gruppe","n_items")
  
  ##tjek frida data for any stems present in other frida stems, that may be affecting the matching creating unwanted ties that may produce
  ## erronous results
  ## function that looks for overlapping stems
  find_tied_names_function<-function(x){
    match_db<-frida_match %>% 
      mutate(           match_stem1=frida_match$stem1[x],
                        match_stem2=frida_match$stem2[x],
                        match_stem3=frida_match$stem2[x],
                        match1=case_when(
                          (frida_match$stem1[x]=="empty")~0,
                          (str_detect(stem1,match_stem1)==TRUE)~1,
                          TRUE ~0),
                        match2=case_when(
                          (frida_match$stem2[x]=="empty")~0,
                          (str_detect(stem2,match_stem2)==TRUE)~1,
                          TRUE ~0),
                        match3=case_when(
                          (frida_match$stem3[x]=="empty")~0,
                          (str_detect(stem3,match_stem3)==TRUE)~1,
                          TRUE ~0),
                        match4=case_when(
                          (frida_match$stem4[x]=="empty")~0,
                          (str_detect(stem4,frida_match$stem4[x]))==TRUE~1,
                          TRUE ~0),
                        not_match=str_count(not_include1,frida_match$stem1[x]),
                        match_score=match1*2+match2*1+match3*0.99+match4*0.98,
                        match_navn=frida_match$navn[x],
                        match_gruppe=frida_match$gruppe[x])##%>% arrange(-match_score)
    cpd_tied<<-bind_rows(cpd_tied, match_db)%>% filter(match_score>1.99&(!(match_navn==navn|match_gruppe==gruppe)))%>%relocate(match_navn,match_stem1,match_stem2,match_stem3,match_gruppe)
    #assign("cpd_tied", envir = .targets, value = match_db1)
    rm(list = ls(all.names = TRUE))
  }
  
  system.time(sapply(1:nrow(frida_match),find_tied_names_function)) 
  
  ##qc_tjek<- nrow
  
  cpd_tied_pruned<-cpd_tied%>%distinct()%>% add_count(match_navn,name="n_items")
  
  write.table(cpd_tied_pruned, file = file.path(file_path, "non_specific_stems.csv"),sep = ";", row.names = FALSE, dec=".")
  
  cpd_tied_pruned 
  
}

#---------------------------------------------------------------------------------------------------
##enrich_with_frida_data
#---------------------------------------------------------------------------------------------------

#############################
#match_names_function
#############################


enrich_cpd_with_frida_data_new <- function(frida_match, cpd_proddata){
  
  frida_match <- tar_read(frida_match)
  cpd_proddata <- tar_read(cpd_proddata)
  ##Create output dataset for match function to iterate to iterate over
  frida_stems <- c(5,6,6,3,2,2,0.1,0.1,0.1,2,0.1)
  names(frida_stems) <- c("brand_group_stem","brand_specific_stem","generic_specific_stem","generic_group_stem",
                          "generic_type_stem","generic_taste_stem","specific_taste_stem","processing_stem","eco_stem", "light_stem","unit")
  
  
  ##The function takes a preproccessed product name, the original full itemname and matches to a number of predifined word stems
  match_names_function <- function(i, cpd_proddata, frida_match, frida_stems){
    #match_db <- sapply(frida_stems, function(i) str_count(cpd_proddata$name[x], i))
    match_db <- apply(frida_match[,names(frida_stems)], 2, function(x){
      str_count(cpd_proddata$name[i], x)
      #str_count("bananer,loese", x)
    })
    
    match_frida<-frida_match%>% select(priority, lower_percent_match,upper_percent_match)
    match_frida2<-bind_cols(match_frida,(cpd_proddata[i,]%>%select(itemname,product_type_unit,product_type_meassure)))%>%
      mutate_all(~str_replace((.),"empty","0"))%>%
      mutate(
        priority=as.numeric(priority),
        percent_match_l=case_when(
          lower_percent_match==0~0,
          product_type_meassure>lower_percent_match~0.1,
          TRUE ~0),
        percent_match_u=case_when(
          upper_percent_match==0~0,
          product_type_meassure<upper_percent_match~0.1,
          TRUE ~0),
        percent_match=percent_match_l*percent_match_u)
    
    str(match_frida2)
    #bind to matrix
    new_match_db<-cbind(match_db, match_frida2$priority,match_frida2$percent_match)
    
    ## update frida stems with the two new stems
    frida_stems <- c(5,6,6,3,2,2,0.1,0.1,0.1,2,0.1,0.1,0.1)
    
    weighted_matches <- as.matrix(new_match_db) %*% frida_stems
    
    new_match_db<-data.frame(weighted_matches) 
    
    frida_match2 <- frida_match[which.max(weighted_matches),]
    
    
    frida_match2 <- frida_match2 %>% 
      mutate(match_score = max(weighted_matches, na.rm = TRUE),
             itemname = cpd_proddata$itemname[i],
             itemnumber = as.character(cpd_proddata$itemnumber[i]),
             name = cpd_proddata$name[i],
             total_items = cpd_proddata$total_items[i],
             frida_name = navn) %>%
      dplyr::select(-navn, itemname, itemnumber, name, match_score, total_items,frida_name , everything() )
    
    frida_match2
  }
  
  #
  ## run function, NB runtime is low on low powered CPU
  #Took 3440 seconds 
  #system.time(sapply(1:500,match_names_function)) # run top 1000 products
  
  #cl <- makeCluster(detectCores())
  cl <-  parallel::makeCluster(6)
  clusterEvalQ(cl, {library(dplyr); library(stringr)})
  
  
  cpd_frida <- rbindlist(parLapply(cl, 1:nrow(cpd_proddata), match_names_function, cpd_proddata = cpd_proddata, frida_match = frida_match,
                                   frida_stems = frida_stems))
  #cpd_frida <- rbindlist(parLapply(cl, 1:500, match_names_function, cpd_proddata = cpd_proddata))
  
  cpd_frida
}



enrich_cpd_with_frida_data <- function(frida_match, cpd_proddata){
  ##Create output dataset for match function to iterate to iterate over
  frida_stems <- c(5,6,6,3,2,2,0.1,0.1,0.1,2,0.1)
  names(frida_stems) <- c("brand_group_stem","brand_specific_stem","generic_specific_stem","generic_group_stem",
                          "generic_type_stem","generic_taste_stem","specific_taste_stem","processing_stem","eco_stem", "light_stem","unit")
  
  
  ##The function takes a preproccessed product name, the original full itemname and matches to a number of predifined word stems
  match_names_function<-function(i, cpd_proddata){
    #match_db <- sapply(frida_stems, function(i) str_count(cpd_proddata$name[x], i))
    match_db <- apply(frida_match[,names(frida_stems)], 2, function(x){
      str_count(cpd_proddata$name[i], x)
      #str_count("bananer,loese", x)
    })
    
    
    weighted_matches <- match_db %*% frida_stems
    
    frida_match2 <- frida_match[which.max(weighted_matches),]
    
    
    frida_match2 <- frida_match2 %>% 
      mutate(match_score = max(weighted_matches),
             itemname = cpd_proddata$itemname[i],
             itemnumber = as.character(cpd_proddata$itemnumber[i]),
             name = cpd_proddata$name[i],
             total_items = cpd_proddata$total_items[i],
             frida_name = navn) %>%
      dplyr::select(-navn, itemname, itemnumber, name, match_score, total_items,frida_name , everything() )
    
    frida_match2
  }
  
  #
  ## run function, NB runtime is low on low powered CPU
  #Took 3440 seconds 
  #system.time(sapply(1:500,match_names_function)) # run top 1000 products
  
  #cl <- makeCluster(detectCores())
  cl <- makeCluster(3)
  clusterEvalQ(cl, {library(dplyr); library(stringr)})
  
  
  cpd_frida <- rbindlist(parLapply(cl, 1:nrow(cpd_proddata), match_names_function, cpd_proddata = cpd_proddata))
  #cpd_frida <- rbindlist(parLapply(cl, 1:500, match_names_function, cpd_proddata = cpd_proddata))
  
  cpd_frida
}

#############################
#match_results
#############################

match_results<-function(cpd_frida1, matchscore_cutoff,fg_path, old.names, target.vars){
  

  
  low_matched_prod<-cpd_frida1%>% filter(match_score<matchscore_cutoff)
  
  ## 
  write.table(low_matched_prod, file = file.path(file_path, "Validating/low_matched_prod.csv"),sep = ";", row.names = FALSE, dec=".")
  
  ## find most frequent problems (greedy)
  low_matched_names<-cpd_frida1%>% mutate(count_var=1)%>%filter(match_score<matchscore_cutoff) %>% group_by(name)  %>%
    summarise(total_products_matched=sum(count_var),total_items_matched=sum(total_items))%>% arrange(-total_items_matched)
  
  ## save contacts pairs
  write.table(low_matched_names, file = file.path(file_path,"Validating/low_matched_name_for_stems.csv"), sep = ";", row.names = FALSE, dec=".")
  
  ##find most frequent unmatched words
  unique_low_matched<-as.data.frame(table(unlist(str_split(low_matched_prod$name," ")))) %>% arrange(-Freq)
  
  ## save contacts pairs
  write.table(unique_low_matched, file = file.path(file_path,"Validating/low_matched_words.csv"), sep = ";", row.names = FALSE, dec=".") 
  
  
  
  
  cpd_frida2 <- cpd_frida1 %>%
    filter(match_score >= matchscore_cutoff) %>%
    rename_with( ~ target.vars[which(!is.na(old.names_frida))], .cols = old.names_frida[!is.na(old.names_frida)]) %>%
    dplyr::select(target.vars[!is.na(old.names_frida)]) %>%
    mutate(datasource = "frida")
  
  cpd_frida2
  
  
}

#---------------------------------------------------------------------------------------------------
##add_gpc_data_to_gs1
#---------------------------------------------------------------------------------------------------

join_gpc_data_to_gs1<-function( gs1_gpc, old.names, target.vars){
  
  
  gs1_ssi1 <- gs1_gpc %>%rename_all(tolower)%>%mutate(count_var=1)%>%
    mutate_all(~fix_utf8_and_casing_function(tolower(.))) %>% filter(tradeitemunitdescriptorcode=="base_unit_or_each") %>%
    group_by(gtin,functionalname,ingredientstatement, netweight,netweightmeasurementunitcode,gpccategorycode,
             descriptionshort, lastchangedatetime,istradeitemavariableunit,effectivedatetime, nutrienttypecode,
             quantitycontainedmeasurementunitcode, quantitycontained, servingsize, classtitle)%>%
    tally()%>%mutate(kilde=1, date_time=as.POSIXct(paste(StrExtractBetween(lastchangedatetime,"^","t"),StrExtractBetween(lastchangedatetime,"t","\\+"),sep=" ")))%>%
    mutate(gtin_length=nchar(gtin)
    )%>%
    arrange(desc(date_time), gtin) %>% slice(1)
  
  
  #split gs1 depending on whether a product has nutricient information or not
  gs1_ssi1_list <- split(gs1_ssi1, is.na(gs1_ssi1$nutrienttypecode))
  
  gs1_ssi1.2 <- gs1_ssi1%>%
    rowwise() %>%
    mutate(quantitycontained = paste(unlist(strsplit(quantitycontained,";"))[
      -which(unlist(strsplit(quantitycontainedmeasurementunitcode,";")) == "e14")],collapse = ";"),
      quantitycontainedmeasurementunitcode = paste(unlist(strsplit(quantitycontainedmeasurementunitcode,";"))[
        -which(unlist(strsplit(quantitycontainedmeasurementunitcode,";")) == "e14")],collapse = ";"))
  
  
  gs1_ssi2 <- gs1_ssi1.2 %>% filter((length(unlist(strsplit(nutrienttypecode, ";"))) ==
                                       length(unlist(strsplit(quantitycontained, ";")))) & length(unlist(strsplit(servingsize,";"))) == 1) %>%
    separate_rows( nutrienttypecode, quantitycontainedmeasurementunitcode, quantitycontained, sep = ";",
                   convert = TRUE)   %>%  ungroup()
  
  #return(gs1_ssi2)
  gs1_ssi3 <- gs1_ssi2 %>% dplyr::select(-quantitycontainedmeasurementunitcode) %>%
    group_by(across(c(-"quantitycontained"))) %>% slice(1) %>% ungroup() %>%
    filter(nutrienttypecode %in% c("ener-","fat", "pro-","choavl")) %>%
    pivot_wider(names_from = nutrienttypecode,
                values_from = quantitycontained)
  
  
  ## we select the most recently updated version of product for now- need to take changing products iver time with the same GTIN into acccount in later versions
  gs1_ssi4 <- gs1_ssi1 %>% 
    mutate(full_name=paste(functionalname,descriptionshort, sep=" "),
           gpccategorycode = as.character(gpccategorycode)) %>% 
    group_by(gtin) %>% 
    mutate(csum=cumsum(kilde)) %>% filter(csum==1) %>% ungroup()
  
  #names(gs1_ssi2)<-paste(names(gs1_ssi2),"_gs1",sep="")
  ##to use later when we account for changing products over time without the filter%>% select(-functionalname,-descriptionshort, -n_items)
  
  #gs1_ssi5<- left_join(gs1_ssi4,gpc2, by=c("gpccategorycode"="brickcode1")) ## %>%
  ## mutate(gtin_gs1=as.character(as.numeric(gtin_gs1)))##fixes issues with zeros removed from storebox itemnumbers
  
  gs1_datasource<- gs1_ssi4 %>% rename_with( ~ target.vars[which(!is.na(old.names))], .cols = old.names[!is.na(old.names)]) %>%
    dplyr::select(target.vars[!is.na(old.names)]) %>%
    mutate(datasource = "gs1")
  
  
  gs1_datasource
}

#---------------------------------------------------------------------------------------------------
## prepare measure dataset to enable exposure quantification
#---------------------------------------------------------------------------------------------------
combine_measures <-function(data){
  ## prepare measure dataset to enable exposure quantification
  cpd_measures1<-data %>%  
    #select(itemnumber,itemname,pieces,total_items,measure,netweight_gs1,netweightmeasurementunitcode_gs1,unit_raw,bruto_vaegt,
    #kilde_gs1,kilde_frida,kilde_kemiluppen,foodname_frida, foedevarenavn_frida)%>%
    rename(pieces1=pieces)%>%
    mutate(pieces=case_when(
      (str_detect(itemname,"aeg")&is.na(pieces1))~ as.numeric(measure), 
      is.na(pieces1)~ as.numeric(pieces1),
      TRUE~as.numeric(pieces1)),
      ##                                      replace_na(pieces1,"1"))%>%
      measure_adjusted1=as.numeric(measure)*as.numeric(pieces),
      bruto_adjusted=(as.numeric(bruto_vaegt))*as.numeric(pieces),
      netweight_gs1=str_replace(netweight_gs1,"\\.",""),
      measure_adjusted=case_when(
        str_detect(unit_raw,"(^|\\s)g(r|ra|ram|m|\\.|)")~ measure_adjusted1, 
        str_detect(unit_raw,"(kg\\.|kg|kilo|kil)")~ measure_adjusted1*1000,
        str_detect(unit_raw,"\\b(l(ite|iter|\\.|))")~ measure_adjusted1*1000,
        str_detect(unit_raw,"cl")~ measure_adjusted1*10, 
        str_detect(unit_raw,"ml")~ measure_adjusted1, 
        TRUE~measure_adjusted1),
      weight1=case_when(
        !is.na(netweight_gs1)~ as.numeric(netweight_gs1),
        !is.na(measure)&is.na(netweight_gs1)~ measure_adjusted1,
        is.na(measure)&is.na(netweight_gs1)&!(bruto_vaegt=="empty")~ bruto_adjusted),
      weight_unit2=case_when(
        !is.na(netweightmeasurementunitcode_gs1)~ netweightmeasurementunitcode_gs1,
        !is.na(unit_raw)& is.na(netweightmeasurementunitcode_gs1)~ unit_raw,
        !(bruto_vaegt=="empty")& is.na(unit_raw)& is.na(netweightmeasurementunitcode_gs1)~ "grm"),
      items_total=total_items*as.numeric(pieces),
      weight_unit1=tolower(weight_unit2),
      weight_unit=case_when( 
        str_detect(weight_unit1,"(cl)")~ "ml", 
        str_detect(weight_unit1,"(g(r|ra|ram|m|\\.|)|(kg\\.|kg|kilo|kil))")~ "grm", 
        str_detect(weight_unit1,"(ml|cl|l(ite|iter|\\.|))")~ "ml"))%>% select(-measure_adjusted1,-weight_unit2,-weight_unit1)
  
  table(cpd_measures1$unit_raw)
  
  cpd_measures1}

## join to full dataset to get get one price, number of items, and weight volume for each purchase

combine_datasources <- function(..., new_frida_group_names){
  
  combined_datasources <- do.call("bind_rows", lapply(list(...), function(x) mutate(x, across(everything(),as.character))))
  combined_datasources <- type_convert(combined_datasources)
  combined_datasources
  
  #Map frida names to nfg (New frida names) which are more broad defined
  map_frida_to_nfg <- new_frida_group_names$nfg
  names(map_frida_to_nfg) <- new_frida_group_names$gruppe
  
  combined_datasources <- combined_datasources %>% 
    mutate(group_broad = ifelse(productname %in% names(map_frida_to_nfg),map_frida_to_nfg[productname],productname))
  
}

##Create overall frida group##
newfoodgroups <- function(data, fg_path){
  
  fg <- read.xlsx(fg_path) %>% mutate_all(~fix_utf8_and_casing_function(.))
  
  setDT(fg)
  setDT(data)
  
  fg[nfg_V5==1,nfg:="alcoholic drinks"]
  fg[nfg_V5==2,nfg:="butter, oil and dressings"] #skinkesalat osv. 
  fg[nfg_V5==3,nfg:="bread"]
  fg[nfg_V5==4,nfg:="cereals"]
  fg[nfg_V5==5,nfg:="cheese"]
  fg[nfg_V5==6,nfg:="confecture, chocolate, ice cream, and sugar"]
  fg[nfg_V5==7,nfg:="cookies, crackers, biscuits and cakes"]
  fg[nfg_V5==8,nfg:="eggs"]
  fg[nfg_V5==9,nfg:="fish and other aquatic animals"]
  fg[nfg_V5==10,nfg:="fruit, raw"]
  fg[nfg_V5==11,nfg:="fruit products"] #(dried and canned) + high fat + juice + smoothie
  fg[nfg_V5==12,nfg:="grains"]
  fg[nfg_V5==122,nfg:="legumes"]
  fg[nfg_V5==13,nfg:="milk products"]
  fg[nfg_V5==14,nfg:="non-sugary drinks"] #te og kaffe 
  fg[nfg_V5==15,nfg:="non food"]
  fg[nfg_V5==16,nfg:="nuts and seeds"] #rugkerner, byggryn 
  fg[nfg_V5==17,nfg:="other"] #breast milk sub + krydder
  fg[nfg_V5==18,nfg:="poultry"]
  fg[nfg_V5==19,nfg:="processed red meat"]
  fg[nfg_V5==20,nfg:="ready meals"] #t?rtedej og pizzadej
  fg[nfg_V5==21,nfg:="red meat"]
  fg[nfg_V5==22,nfg:="rice and pasta"]
  fg[nfg_V5==23,nfg:="salty snacks"] #peanuts 
  fg[nfg_V5==24,nfg:="sugary drinks"] # saft og limonade + cacao m?lk og cacao + milkshake 
  fg[nfg_V5==25,nfg:="sweet spreads"] #nutella, syltet?j, figenp?l?g, peanutbutter
  fg[nfg_V5==26,nfg:="tobacco and nicotine gum"]
  fg[nfg_V5==27,nfg:="unknown"] 
  fg[nfg_V5==28,nfg:="vegetables, raw"]
  fg[nfg_V5==29,nfg:="vegetable products"] #humus, taco sauce, pesto, lidt usundt, gr?ntsagsjuice
  
  fg <- unique(fg[,.(frida.name,nfg)])
  
  cat("Func creates 26 new foodgroups, var names nfg; unknown, non food, rabat, tobacco and nicotine gum & other are categories by themself")
  
  data <- fg[data,on=.(frida.name=navn)]
 
  new_frida_group_names <- data.frame("nfg" = fg$nfg, "gruppe" = fg$frida.name)
  #new_frida_group_names <- data %>% group_by(nfg, gruppe) %>% summarise(n = n()) %>% 
  #  filter(!is.na(nfg)) %>% 
  # arrange(-n) %>% relocate(n) %>%
  #  group_by(gruppe) %>% slice(1)
  
  new_frida_group_names
  }



### Combine datasources and standardize their group names. Then for each product select GS1 information if available, else Open Food Facts and 
#lastly frida. 
cpd_enriched_clean <- function(cpd_enriched,cpd_original, gs1_gpc, new_frida_group_names){
  
  gs1_gpc2 <- gs1_gpc  %>%
    mutate(itemnumber = gtin) %>%
    dplyr::select(itemnumber, -gtin, segmenttitle, familytitle, classtitle, bricktitle) %>%
    group_by(itemnumber, segmenttitle, familytitle, classtitle, bricktitle) %>% slice(1)
  
  #Map frida names to nfg (New frida names) which are more broad defined
  map_frida_to_nfg <- new_frida_group_names$nfg
  names(map_frida_to_nfg) <- new_frida_group_names$gruppe
  
  cpd_enriched <- cpd_enriched %>% 
    mutate(group_broad = ifelse(productname %in% names(map_frida_to_nfg),map_frida_to_nfg[productname],productname))
  
  #Find all products that are matched with both frida and gs1. Find the most common mapping between
  #between gs1 and frida for each unique gs1 group and use that for all gs1 names. 
  
  # frida_to_gs1 <- cpd_enriched %>%
  #   group_by(itemname, itemnumber)%>%
  #   filter(all(c("frida", "gs1") %in% datasource)) %>% ungroup() %>%
  #   dplyr::select(itemname, itemnumber, datasource, group) %>% ungroup() %>%
  #   filter(!is.na(group)) %>% spread(.,key = "datasource", value = "group")%>%
  #   left_join(gs1_gpc2, by = "itemnumber")%>%
  #   group_by( frida, gs1, segmenttitle, familytitle, classtitle, bricktitle) %>%
  #   summarise(n = n(), all_item_names = paste(itemname, collapse = " / ")) %>%
  #   arrange(-n) %>% relocate(n) 
  # 
  # write.xlsx(frida_to_gs1, "Validating/Frida_Gs1_matching.xlsx")
  # 
  # frida_broad_to_gs1 <- cpd_enriched %>%
  #   group_by(itemname, itemnumber)%>%
  #   filter(all(c("frida", "gs1") %in% datasource)) %>% ungroup() %>%
  #   dplyr::select(itemname, itemnumber, datasource, group_broad) %>% ungroup() %>%
  #   filter(!is.na(group_broad)) %>% spread(.,key = "datasource", value = "group_broad")%>%
  #   left_join(gs1_gpc2, by = "itemnumber")%>%
  #   group_by( frida, gs1, segmenttitle, familytitle, classtitle, bricktitle) %>%
  #   summarise(n = n(), all_item_names = paste(itemname, collapse = " / ")) %>%
  #   arrange(-n) %>% relocate(n) %>%
  #   group_by(gs1)  %>% slice(1)
  # 
  # 
  # #Define mapping
  # map_frida <- frida_broad_to_gs1$frida
  # names(map_frida) <- frida_broad_to_gs1$gs1  
  # 
  # #Identical to the previous section but this time between frida and openfoodfacts
  # openfoodfacts_to_frida <- cpd_enriched %>%
  #   group_by(itemname, itemnumber)%>%
  #   filter(all(c("frida", "openfoodfacts") %in% datasource)) %>%
  #   dplyr::select(itemname, itemnumber, datasource, group) %>%  ungroup() %>%
  #   filter(!is.na(group)) %>% #Remove all na from group names
  #   spread(. ,key = "datasource", value = "group") %>%
  #   group_by( frida, openfoodfacts) %>%
  #   summarise(n = n(), all_item_names = paste(itemname, collapse = " / ")) %>%
  #   arrange(-n) %>% relocate(n) %>%
  #   group_by(openfoodfacts) %>% slice(1)
  # 
  # #Define mapping
  # map_openfoodfacts <- openfoodfacts_to_frida$frida
  # names(map_openfoodfacts) <- openfoodfacts_to_frida$openfoodfacts  
  # 
  #Perform the mapping
  
  cpd1 <- cpd_enriched %>%  
    #mutate(group_broad = ifelse(group_broad %in% names(map_openfoodfacts),map_openfoodfacts[group_broad],group_broad),
    #                            group_broad = ifelse(group_broad %in% names(map_frida), map_frida[group_broad],group_broad)) %>%
    filter(!is.na(group_broad)) %>%
    group_by(itemname, itemnumber) %>% 
    arrange(factor(datasource, levels = c("gs1", "openfoodfacts", "frida")), desc(datasource)) %>% 
    slice(1)
  
  cpd_original2 <- cpd_original %>% left_join(cpd1 %>% mutate(itemnumber = as.character(itemnumber)), by = c("itemname", "itemnumber"))
  
  write.csv(cpd_original2, "ConsumerReceiptData_Enriched.csv")
  
  return(cpd_original2)
  
}


cpd_join_datasources <- function(cpd_data, datasources){
  
  #Split datasources based on whether it should match on itemnumber or itemname
  datasources_list <- split(datasources, datasources$datasource %in% c("frida","purchased_itemname"))
  
  cpd_enriched_itemname <- datasources_list[["TRUE"]] %>% group_by(itemname) %>% slice(1) %>% ungroup() %>%
    dplyr::select(-itemnumber) %>%
    right_join(cpd_data, by = c("itemname"))
  
  cpd_enriched_itemnumber <- datasources_list[["FALSE"]] %>%group_by(itemnumber) %>% slice(1) %>% ungroup() %>%
    dplyr::select(-itemname) %>%
    right_join(cpd_data, by = c("itemnumber"))
  
  cpd_enriched <- rbind(cpd_enriched_itemname,cpd_enriched_itemnumber)
  cpd_enriched
}



create_flowcharts <-function(cpd_participants,kemiluppen_datasource,gs1_datasource,frida_match, open_food_facts_datasource){
  
  ##read data from file not included in enrichment pipeline
  kemiluppen_ssi <- kemiluppen_datasource
  gs1_ssi3 <-gs1_datasource
  demografi2 <- readr::read_csv("Demografi.csv")
  demografi_disease <- readr::read_csv("SelfReportedDisease.csv")
  
  
  flowchart_data1 <- cpd_participants %>%
    group_by(participant) %>% #summarise(min_purchdate=min(purchdate),max_purchdate=max(purchdate))%>%
    mutate(ObsTime = round((interval(min(purchdate), max(purchdate))%/% days(1)/30.25),2),
           Exclude = case_when(ObsTime == 0 | is.na(ObsTime) ~ 1,
                               TRUE ~ 0))
  names(flowchart_data1)
  
  flowchart_data2 <- flowchart_data1[flowchart_data1$Exclude == 0, ]
  
  flowchart_data3 <- flowchart_data2[nchar(flowchart_data2 $itemnumber) > 2 & 
                                       nchar(flowchart_data2 $itemnumber) <= 14,]
  
  flowchart_data4 <- flowchart_data3 %>% group_by(itemnumber, itemname, receipt)
  #slice(1)
  
  ## variables to help visualize data collection
  no_unique_id_cpd<- format(length(unique(demografi2$participant)), big.mark=",") ##1
  no_unique_id_cpd_consent <- format(length(unique(flowchart_data2$participant)), big.mark = ",") ##2
  no_unique_receipts<- format(length(unique(flowchart_data1$receipt)), big.mark = ",") ##3
  no_unique_receipts2<-format(length(unique(flowchart_data3$receipt)), big.mark = ",") ##3
  
  no_unique_products<-format(flowchart_data1 %>% ungroup() %>% dplyr::select(itemname, itemnumber) %>% n_distinct(), big.mark = ",")
  no_unique_products2<-format(flowchart_data3 %>% ungroup() %>% dplyr::select(itemname, itemnumber) %>% n_distinct(), big.mark = ",") ##4
  no_unique_products_no_format <- flowchart_data3 %>% ungroup() %>% dplyr::select(itemname, itemnumber) %>% n_distinct()
  
  not_product_names_excluded<- "to be quantified from cpd to cpd2"##length(unique(cpd$itemname[cpd$itemnumber==""]))##5
  no_enriched_GS1 <- format(sum(flowchart_data4$datasource=="gs1",na.rm=T), big.mark = ",") ##6
  no_enriched_kemiluppen <- format(sum(flowchart_data4$datasource=="Kemiluppen",na.rm=T), big.mark = ",") #/10##7 
  no_enriched_kemiluppen_gs1 <- format(flowchart_data4 %>% filter(datasource %in% c("gs1", "Kemiluppen")) %>% nrow(), big.mark = ",")  
  
  
  products_kemiluppen_gs1_frida <- unlist(as.vector(flowchart_data3 %>% ungroup() %>% filter(datasource %in% c("gs1", "Kemiluppen", "frida")) %>% 
                                                      dplyr::select(itemname, itemnumber) %>%
                                                      summarise(n = format(n_distinct(.), big.mark = ","), n_perc = round(n_distinct(.)/no_unique_products_no_format*100,1))))
  
  
  products_kemiluppen_gs1_frida_off <- unlist(as.vector(flowchart_data3 %>% ungroup() %>% filter(datasource %in% c("gs1", "Kemiluppen", "frida","openfoodfacts")) %>% 
                                                          dplyr::select(itemname, itemnumber) %>%
                                                          summarise(n = format(n_distinct(.), big.mark = ","), n_perc = round(n_distinct(.)/no_unique_products_no_format*100,1))))
  
  
  products_kemiluppen <- format(flowchart_data3 %>% ungroup() %>% filter(datasource %in% c("Kemiluppen")) %>%
                                  dplyr::select(itemname, itemnumber) %>% n_distinct(), big.mark = ",") ##4
  
  products_gs1 <- unlist(as.vector(flowchart_data3 %>% ungroup() %>% filter(datasource %in% c("gs1")) %>% dplyr::select(itemname, itemnumber) %>%
                                     summarise(n = format(n_distinct(.), big.mark = ","), n_perc = round(n_distinct(.)/no_unique_products_no_format*100,1)))) ##4
  
  
  products_frida <- format(flowchart_data3 %>% ungroup() %>% filter(datasource %in% c("frida")) %>%
                             dplyr::select(itemname, itemnumber) %>% n_distinct(), big.mark = ",") ##4
  
  products_off <- format(flowchart_data3 %>% ungroup() %>% filter(datasource %in% c("openfoodfacts")) %>%
                           dplyr::select(itemname, itemnumber) %>% n_distinct(), big.mark = ",")
  
  products_gs1_kemiluppen<- unlist(as.vector(flowchart_data3 %>% ungroup() %>% filter(datasource %in% c("gs1", "Kemiluppen")) %>% 
                                               dplyr::select(itemname, itemnumber) %>%
                                               summarise(n = format(n_distinct(.), big.mark = ","), n_perc = round(n_distinct(.)/no_unique_products_no_format*100,1))))
  
  
  no_of_unique_products_gs1 <- format(length(unique(gs1_ssi3$itemnumber)), big.mark = ",") ##9
  no_of_unique_products_kemiluppen <- format(length(unique(kemiluppen_ssi$itemnumber)), big.mark = ",") ##10
  no_of_unique_product_types_frida <- format(nrow(frida_match), big.mark = ",") ##11
  no_of_unique_products_openfoodfacts <- format(length(unique(open_food_facts_datasource$itemnumber)), big.mark = ",") ##11
  
  
  no_unique_id_cpd_with_outcome <- format(nrow(demografi2 %>% filter(!is.na(disease)) %>% count(participant)), big.mark = ",") ##12outcome of interest
  not_given_consent_to_current_study <-format(sum(demografi2$consentenvironment != 1), big.mark = ",") ##13
  no_participant_excluded <- format(as.numeric(no_unique_id_cpd) -
                                      length(unique(flowchart_data3[flowchart_data3$Exclude == 0,]$participant))
                                    , big.mark = ",")
  
  no_unique_products_excluded <- format(flowchart_data2 %>% ungroup() %>% dplyr::select(itemname, itemnumber) %>% n_distinct() - 
                                          flowchart_data3 %>% ungroup() %>% dplyr::select(itemname, itemnumber) %>% n_distinct(), big.mark = ",")
  
  
  ### Get number of total products enriched from each datasource
  cpd_participants2 <- cpd_participants %>% group_by(itemname, itemnumber, receipt)
  
  
  n_total_items <- as.numeric(flowchart_data4%>% slice(1) %>% ungroup() %>%
                                summarise(sum(amount)))
  
  total_items_enriched <- flowchart_data4 %>% group_by(datasource) %>%
    summarise(n_total = sum(amount))
  
  total_items_enriched_frac <- round(total_items_enriched$n_total/n_total_items*100,1)
  total_items_enriched$n_total <- format(total_items_enriched$n_total, big.mark = ",")
  
  total_items_gs1 <- unlist(as.vector(flowchart_data4 %>% filter(datasource %in% c("gs1")) %>% slice(1) %>% ungroup() %>%
                                        summarise( n = format(sum(amount), big.mark = ","), n_perc = round(sum(amount)/n_total_items*100,1))))
  
  total_items_gs1_kemiluppen <- unlist(as.vector(flowchart_data4 %>% filter(datasource %in% c("gs1", "Kemiluppen")) %>% slice(1) %>% ungroup() %>%
                                                   summarise(n = format(sum(amount), big.mark = ","), n_perc = round(sum(amount)/n_total_items*100,1))))
  
  total_items_gs1_kemiluppen_frida <- unlist(as.vector(flowchart_data4 %>% filter(datasource %in% c("gs1", "Kemiluppen", "frida")) %>% slice(1) %>% ungroup() %>%
                                                         summarise(n = format(sum(amount), big.mark = ","), n_perc = round(sum(amount)/n_total_items*100,1))))
  
  total_items_gs1_kemiluppen_frida_off <- unlist(as.vector(flowchart_data4 %>% filter(datasource %in% c("gs1", "Kemiluppen", "frida", "openfoodfacts")) %>%
                                                             slice(1) %>% ungroup() %>%
                                                             summarise(n = format(sum(amount), big.mark = ","), n_perc = round(sum(amount)/n_total_items*100,1))))
  
  n_total_items <- format(n_total_items, big.mark = ",")
  ##this code helps you create a nice strobe recommended figure 1 to provide clarity on inclusion and exclusion
  ## it can be automated to avoid troubles with manual data due to extended timeframes, changes in exclusion criteria etc.
  
  ##this code helps you create a nice strobe recommended like figure 1 to provide clarity on inclusion and exclusion
  ## it can be automated to avoid troubles with manual data due to extended timeframes, changes in exclusion criteria etc.
  g<-DiagrammeR::grViz("
digraph {
    graph [rankdir = LR splines=line] ##you can also use orthogonal
##define seccond collumn   
    {
        rank=same
        node [style='rounded' fillcolor=White shape = rectangle fontsize = 10 fontname = Helvetica]
rec0 [label='Recruited at mineindkob.dk  \n N=@@1']
rec1 [label = 'Enrolled for study: N=@@2  \nTotal receipts: N=@@3  \n Unique products: N=@@5']
rec15[label = 'Product enrichment' style='rounded,filled' fillcolor = grey shape = rectangle fontsize = 12] ##label = 'force arrow '
rec2 [label = 'Available for analysis: N=@@2  \nTotal receipts: N=@@4 \n \n Enriched products \n Unique: N=@@10 (@@11%) \n Total: N=@@12 (@@13%)'] 
rec3 [label = 'One or more self-reported disease \n N=@@7']
    }

##define third collumn   

    {
        rank=same
        node [style='rounded,filled' fillcolor=White shape = rectangle fontsize = 11 fontname = Helvetica]
        data0 [label ='Excluded due to lack of receipts generated* \n N=@@9'] 
    }

## define relationsships that are not connected rowwise
    rec0-> data0[style=invisible, arrowhead=none]

## define connections that are not connected collumnwise  
    ##rec0->rec1[style=invisible, arrowhead=none]
 

## define connections rowwise  
    rec0->data0
    
## define connections collumnwise 
    rec0->rec1
    rec1->rec15
    rec15->rec2
    rec2->rec3}

[1]: no_unique_id_cpd[1]
[2]: no_unique_id_cpd_consent[1]
[3]: no_unique_receipts[1]
[4]: no_unique_receipts2[1]
[5]: no_unique_products[1] 
[6]: no_unique_products2[1]
[7]: no_unique_id_cpd_with_outcome[1]
[8]: not_given_consent_to_current_study[1] 
[9]: no_participant_excluded[1]
[10]: products_kemiluppen_gs1_frida_off[1]
[11]: products_kemiluppen_gs1_frida_off[2]
[12]: total_items_gs1_kemiluppen_frida_off[1]
[13]: total_items_gs1_kemiluppen_frida_off[2]
", height = 1000)
  
  
  g %>% DiagrammeRsvg::export_svg() %>% charToRaw %>% rsvg_pdf("Figures/Figure s1.pdf")
  g %>% DiagrammeRsvg::export_svg() %>% charToRaw %>% rsvg_png("Figures/Figure s1.png", height = 1000 )
  
  
  
  g<-DiagrammeR::grViz("
digraph {
    graph [rankdir = LR splines=line] ##you can also use orthogonal

##define seccond collumn   
    {
        rank=same
        node [style='rounded,filled' fillcolor=Lightblue shape = rectangle fontname = Helvetica]
        reca [fontsize = 13, label='  Product enrichment']

        node [style='rounded,filled' fillcolor=white shape = rectangle fontsize=10 fontname = Helvetica]
        data0 [label ='Unique products: N=@@18 \n Products enriched: N = 0  \n
        Total products bought N=@@19']
        data1 [label ='Enriched products from GS1: N=@@6  \n Unique products enriched: N=@@6 (@@28%)  \n
         Total products enriched: N=@@20 (@@21%)'] 
        data2 [label ='Enriched products from Kemiluppen: N=@@7 \n Unique products enriched: N=@@10 (@@29%) \n
          Total products enriched: N=@@22 (@@23%)']
        data3 [label ='Enriched products from Frida: N=@@8 \n Unique products enriched: N=@@11 (@@30%)  \n 
        Total products enriched: N=@@24 (@@25%)']  ## style=invisible
        data4 [label = 'Enriched products from Open Food Facts: N=@@9 \n Unique products enriched: N=@@12 (@@31%)\n
        Total products enriched: N=@@26 (@@27%)']
        data5 [label = 'force arrow ' style=invisible]
    }
    
##define third collumn  

    {
        rank=same
        node [style='rounded,filled' fillcolor=White shape = rectangle fontsize = 11 fontname = Helvetica]
        dataa0 [label = 'force arrow ' style=invisible]
        dataa [label = 'Products with incomplete \n item numbers are removed*\n N=@@17']
        datab [label = 'GS1 product data \n N=@@13 products']
        datac [label = 'Kemiluppen product data \n N=@@14 products'] ## style=invisible
        datad [label = 'Frida Food Database \n N=@@15 product types']
        datae [label = 'Open Food Facts database \n N=@@16 products']
        dataf [label = 'force arrow ' style=invisible]
    }

## define relaptionsships that are not connected columnwise
reca -> data0[style=invisible, arrowhead=none]

## define relationsships that are not connected rowwise
    data0->dataa[style=invisible, arrowhead=none]
    data1->datab[style=invisible, arrowhead=none]
    data2->datac[style=invisible, arrowhead=none]
    data3->datad[style=invisible, arrowhead=none]
    data4->datae[style=invisible, arrowhead=none]

## define connections rowwise  
    data0->dataa
    datab->data1[side='right']
    datac->data2[side='right']
    datad->data3[side='right']
    datae->data4[side='right']

## define connections collumnwise 
    data0-> data1
    data1->data2
    data2->data3
    data3->data4}
    
    
[1]: no_unique_id_cpd[1]
[2]: no_unique_id_cpd_consent[1]
[3]: no_unique_receipts[1]
[4]: no_unique_products[1] 
[5]: no_unique_products2[1] 
[6]: products_gs1[1]
[7]: products_kemiluppen[1]
[8]: products_frida[1]
[9]: products_off[1]
[10]: products_gs1_kemiluppen[1]
[11]: products_kemiluppen_gs1_frida[1]
[12]: products_kemiluppen_gs1_frida_off[1]
[13]: no_of_unique_products_gs1[1]
[14]: no_of_unique_products_kemiluppen[1]
[15]: no_of_unique_product_types_frida[1]
[16]: no_of_unique_products_openfoodfacts[1]
[17]: no_unique_products_excluded[1]
[18]: no_unique_products[1] 
[19]: n_total_items[1]
[20]: total_items_gs1[1]
[21]: total_items_gs1[2]
[22]: total_items_gs1_kemiluppen[1]
[23]: total_items_gs1_kemiluppen[2]
[24]: total_items_gs1_kemiluppen_frida[1]
[25]: total_items_gs1_kemiluppen_frida[2]
[26]: total_items_gs1_kemiluppen_frida_off[1]
[27]: total_items_gs1_kemiluppen_frida_off[2]
[28]: products_gs1[2]
[29]: products_gs1_kemiluppen[2]
[30]: products_kemiluppen_gs1_frida[2]
[31]: products_kemiluppen_gs1_frida_off[2]")
  
  #g
  
  g %>% DiagrammeRsvg::export_svg() %>% charToRaw %>% rsvg_pdf("Figures/Figure 2.pdf")
  g %>% DiagrammeRsvg::export_svg() %>% charToRaw %>% rsvg_png("Figures/Figure 2.png", height = 1000)
  
  flowchart_data1
  
  
}


nutricient_fun <- function(cpd_data, cpd_original,frida_match,off_df,gs1_df){
  
  
  rel_diff <- function(x){
    return(abs(diff(x))/(sum(x)/2))
  }
  
  max_mean_diff <- function(x){
    MMD <- abs(diff(abs(x)))/max(abs(x))
    
    if(is.infinite(MMD)){
      return(NA)
    }
    return(MMD)
  }
  
  
# max_mean_diff <- function(x){
#   MMD <- abs(diff(x))/(sum(x)/2)
#   
#   if(is.infinite(MMD)){
#     return(NA)
#   }
#   return(MMD)
# }

  #frida_match <- tar_read(frida_match)
  cpd_data <- tar_read(cpd_enriched)
  cpd_original <- tar_read(cpd_enriched_final)
  
  
  # cpd_data1 <- cpd_data %>% left_join(frida_match %>% dplyr::select(gruppe, foodgroup_eng) %>% group_by(gruppe) %>%
  #                                       slice(1), by = c("group" = "gruppe")) %>% 
  #   group_by(itemname, itemnumber) %>%
  #   filter(all(c("frida", "gs1") %in% datasource)) %>%
  #   # summarise(frida_group = group[datasource == "frida"], frida_group_eng = foodgroup_eng[datasource == "frida"], KJ = max_mean_diff(KJ), protein = max_mean_diff(protein),
  #   #           fat = max_mean_diff(fat), carbohydrates = max_mean_diff(carbohydrates))
  #   summarise(frida_name = productname[datasource == "frida"], frida_group =  foodgroup_eng[datasource == "frida"], 
  #             frida_group_eng = foodgroup_eng[datasource == "frida"], KJ = max_mean_diff(KJ), protein = max_mean_diff(protein),
  #             fat = max_mean_diff(fat), carbohydrates = max_mean_diff(carbohydrates)) %>%
  #   mutate_all(~ifelse(is.nan(.), NA, .))
  # 
  cpd_data1 <- cpd_data  %>% 
    group_by(itemname, itemnumber) %>%
    filter(all(c("frida", "gs1") %in% datasource)) %>%
    # summarise(frida_group = group[datasource == "frida"], frida_group_eng = foodgroup_eng[datasource == "frida"], KJ = max_mean_diff(KJ), protein = max_mean_diff(protein),
    #           fat = max_mean_diff(fat), carbohydrates = max_mean_diff(carbohydrates))
    summarise(group_broad = group_broad[datasource == "frida"], 
              KJ = max_mean_diff(KJ),
              protein = max_mean_diff(protein),
              fat = max_mean_diff(fat),
              carbohydrates = max_mean_diff(carbohydrates)) %>%
    mutate_all(~ifelse(is.nan(.), NA, .))
  
  
  cpd_data2 <- cpd_original %>% group_by(itemname, itemnumber) %>%
    summarise(n_total = sum(amount)) %>% right_join(cpd_data1 %>% ungroup(), by = c("itemname", "itemnumber")) #%>%
    #filter(!is.na(frida_group))
  
  #Identify the total amount of product purchased fron each class
  N_purchases <- cpd_data %>% dplyr::select(itemname, itemnumber, group_broad, datasource) %>% 
    filter(datasource == "frida") %>%
    filter(!is.na(group_broad)) %>%
    left_join(cpd_original %>% dplyr::select(itemname, itemnumber, amount), by =c("itemname", "itemnumber")) %>%
    mutate(group_broad = capitalize(gsub("_", " ", group_broad)),) %>%
    filter(!(is.na(group_broad)|group_broad %in% c("Non food", "Pant retur", "Other","Empty", "Baerepose","Plantebaserede produkter"))) %>% 
    group_by(group_broad) %>% 
    summarise(N = sum(amount)) %>% ungroup() 
  
  #cpd_original %>% group_by(group_broad) %>% summarise(N = sum(amount)) %>% arrange(-N)
  
  KJ_cumu_unique <- ecdf(cpd_data1$KJ)
  KJ_cumu_total <- ewcdf(cpd_data2$KJ, weights = cpd_data2$n_total)
  save(KJ_cumu_unique, file = "KJ_cumulation_unique")
  save(KJ_cumu_total, file = "KJ_cumulation_total")
  
  
  png("Figures/FridaGs1_Compare_plot.png", height = 6, width = 8, units = "in", res = 200)
  par(mfrow = c(2,2))
  par(mar = c(1.5,4,5,0))
  plot(ewcdf(cpd_data2$KJ, weights = cpd_data2$n_total), col = "black", xlab = "", ylab = "",
       main = "", 
       xlim = c(0,1), lwd = 2, cex.axis = 1.2, cex.lab = 1.4, xaxt='n', yaxt = "n")
  lines(ecdf(cpd_data1$KJ), col = "grey", lwd = 2)
  axis(2, at = c(0,0.2,0.4,0.6,0.8,1), labels =TRUE)
  title("KJ", line = 0.5)
  title(ylab = "Cumulative Proportion", line = 2, cex.lab = 1.3)
  
  
  par(mar = c(1.5,3,5,1))
  plot(ewcdf(cpd_data2$protein, weights = cpd_data2$n_total), col = "black", xlab = "", ylab = "",
       main = "", 
       xlim = c(0,1), lwd = 2, cex.axis = 1.2, cex.lab = 1.4, xaxt='n', yaxt = "n")
  lines(ecdf(cpd_data1$protein), col = "grey", lwd = 2)
  title("Protein", line = 0.5)
  
  
  par(mar = c(5,4,1.5,0))
  plot(ewcdf(cpd_data2$fat, weights = cpd_data2$n_total), col = "black", xlab = "", ylab = "",
       main = "", 
       xlim = c(0,1), lwd = 2, cex.axis = 1.2, cex.lab = 1.4, xaxt='n', yaxt = "n")
  lines(ecdf(cpd_data1$fat), col = "grey", lwd = 2)
  axis(1, at = c(0,0.2,0.4,0.6,0.8,1), labels =TRUE,   padj= -0.7)
  axis(2, at = c(0,0.2,0.4,0.6,0.8,1), labels =TRUE)
  title("Fat", line = 0.5)
  title(ylab = "Cumulative Proportion", line = 2, cex.lab = 1.3)
  title(xlab = "Relative difference", line = 1.5, cex.lab = 1.3)
  
  par(mar = c(5,3,1.5,1))
  plot(ewcdf(cpd_data2$carbohydrates, weights = cpd_data2$n_total), col = "black", xlab = "", ylab = "", main = "",
       xlim = c(0,1), lwd = 2, cex.axis = 1.2, cex.lab = 1.4, xaxt='n', yaxt = "n")
  lines(ecdf(cpd_data1$carbohydrates), col = "grey", lwd = 2)
  axis(1, at = c(0,0.2,0.4,0.6,0.8,1), labels =TRUE,   padj= -0.7)
  title(xlab = "Relative difference", line = 1.5, cex.lab = 1.3)
  title("Carbohydrates", line = 0.5)
  
  par(mar = c(0,0,0,0))
  par(mfrow = c(1,1))
  legend(x = "bottom", inset = -0.38,
         legend = c("Unique products", "Total products"), col =c("grey", "black"), lwd = 2, horiz = TRUE,
         bty = "n")
  dev.off()
  
  
  
  Nutri_table <- function(X, Nutri_name, N_purchases){
 
    X_table <- X %>%
      mutate(group_broad = capitalize(gsub("_", " ", group_broad)),) %>% 
      group_by(group_broad) %>%
      filter(!( is.na(!!sym(Nutri_name)) |is.nan(!!sym(Nutri_name)))) %>%
      summarise("min" = min(!!sym(Nutri_name)), 
                "quan_25" = quantile(!!sym(Nutri_name),probs = 0.25, na.rm = TRUE),
                "quan_50" = quantile(!!sym(Nutri_name),probs = 0.50, na.rm = TRUE),
                "quan_75" = quantile(!!sym(Nutri_name),probs = 0.75, na.rm = TRUE),
                "max" = max(!!sym(Nutri_name)), n = n()) %>%
      ungroup() %>%
      right_join(N_purchases, by = "group_broad") %>%
      arrange(-N) %>% #slice(1:20)  %>% 
      mutate(n = case_when(is.na(n) ~  0,
                           TRUE ~ as.numeric(n))) %>%
      # rbind(., X %>%
      #         mutate(group_broad = capitalize(gsub("_", " ", group_broad)),) %>% 
      #         group_by(group_broad) %>%
      #         filter(!( is.na(!!sym(Nutri_name)) |is.nan(!!sym(Nutri_name)))) %>%
      #         summarise("min" = min(!!sym(Nutri_name)), 
      #                   "quan_25" = quantile(!!sym(Nutri_name),probs = 0.25, na.rm = TRUE),
      #                   "quan_50" = quantile(!!sym(Nutri_name),probs = 0.50, na.rm = TRUE),
      #                   "quan_75" = quantile(!!sym(Nutri_name),probs = 0.75, na.rm = TRUE),
      #                   "max" = max(!!sym(Nutri_name)), n = n()) %>% 
      #         ungroup() )
      #slice(1:25) %>% 
      #Calculate the mean of the corresponding 30 groups
      add_row("group_broad" = "Mean", "min" = sum(.$min*.$n, na.rm = TRUE)/sum(.$n, na.rm = TRUE), "quan_25" = sum(.$quan_25*.$n, na.rm = TRUE)/sum(.$n, na.rm = TRUE), 
              "quan_50" = sum(.$quan_50*.$n, na.rm = TRUE)/sum(.$n, na.rm = TRUE),"quan_75" =sum(.$quan_75*.$n, na.rm = TRUE)/sum(.$n, na.rm = TRUE),
              "max" = sum(.$max*.$n, na.rm = TRUE)/sum(.$n, na.rm = TRUE), "n" = sum(.$n, na.rm = TRUE), "N" = sum(.$N, na.rm = TRUE) ) %>%
      slice_max(N, n = 21) %>%
      gt()
    
    X_table2 <- X_table %>% cols_label(
      group_broad = md("Product group"),
      min = md("Min"), 
      quan_25 = md("25%"),
      quan_50 = md("50%"),
      quan_75 = md("75%"),
      max = md("Max"),
      n = md("Products in GS1"),
      N = md("N")
    ) %>%
      fmt_number(
        columns = c("min","quan_25", "quan_50","quan_75","max"),
        suffixing = F,
        decimals = 2
      ) %>%
      fmt_number(
        columns = c("n", "N"),
        use_seps = T,
        decimals = 0
      ) %>%
      tab_header(title  = paste("Summary table of the relative error difference in ",Nutri_name, "between Frida and GS1")) %>%
      tab_spanner(label = "Quantile", columns = c("min","quan_25", "quan_50", "quan_75","max")) %>%
      tab_style(
        style = list(
          cell_text(weight = "bold"),
          cell_borders(sides = "top", weight = "bold")
        ),
        locations = cells_body(
          #      columns = group,
          rows = group_broad %in% c("Mean"))
      )
    
    gtsave(X_table2, paste0("Tables/Table s3_", Nutri_name,".docx"))
    
  }
  
  Nutri_table2 <- function(X, Nutri_name){
    
    
    X_table <- X %>% 
      mutate(frida_name = capitalize(gsub("_", " ", frida_name))) %>% 
      group_by(frida_name) %>%
      filter(!( is.na(!!sym(Nutri_name)) |is.nan(!!sym(Nutri_name)))) %>%
      summarise("min" = min(!!sym(Nutri_name)), 
                "quan_25" = quantile(!!sym(Nutri_name),probs = 0.25, na.rm = TRUE),
                "quan_50" = quantile(!!sym(Nutri_name),probs = 0.50, na.rm = TRUE),
                "quan_75" = quantile(!!sym(Nutri_name),probs = 0.75, na.rm = TRUE),
                "max" = max(!!sym(Nutri_name)), n = n()) %>%
      #filter(n >= 500) %>% 
      arrange(-n) %>% ungroup() %>%
      #Calculate the mean of the corresponding 30 groups
      add_row("frida_name" = "Mean", "min" = sum(.$min*.$n)/sum(.$n), "quan_25" = sum(.$quan_25*.$n)/sum(.$n), 
              "quan_50" = sum(.$quan_50*.$n)/sum(.$n),"quan_75" =sum(.$quan_75*.$n)/sum(.$n),
              "max" = sum(.$max*.$n)/sum(.$n), "n" = sum(.$n) ) 
    
    write.xlsx(X_table, paste0("Validating/",Nutri_name, "_frida_names.xlsx"))
    
  }
  
  
  Nutri_table(cpd_data2, "KJ", N_purchases = N_purchases)
  Nutri_table(cpd_data2, "protein", N_purchases = N_purchases)
  Nutri_table(cpd_data2, "fat", N_purchases = N_purchases)
  Nutri_table(cpd_data2, "carbohydrates", N_purchases = N_purchases)
  
  #For validating 
  # Nutri_table2(cpd_data2, "KJ")
  # Nutri_table2(cpd_data2, "protein")
  # Nutri_table2(cpd_data2, "fat")
  # Nutri_table2(cpd_data2, "carbohydrates")
  # 
  ###########################
  # Compare GS1 and Frida
  ###########################
  # 
  # matched_items <- off_df[(off_df$itemnumber %in% gs1_df$itemnumber),]$itemnumber
  # 
  # cpd_data1 <- rbind(gs1_df[gs1_df$itemnumber %in% matched_items,],
  #                    off_df[off_df$itemnumber %in% matched_items,]) %>% group_by(itemnumber) %>%
  #   filter(!(any(is.na(KJ)) | any(is.na(fat)) | any(is.na(protein)) | any(is.na(carbohydrates)))) %>%
  #   group_by(itemnumber,datasource) %>% slice(1) %>% group_by(itemnumber) %>%
  #   summarise(KJ = max_mean_diff(KJ), protein = max_mean_diff(protein),
  #             fat = max_mean_diff(fat), carbohydrates = max_mean_diff(carbohydrates))
  # 
  # colorpal <- brewer.pal(4,"Pastel1")
  # 
  # par(mar = c(5.1, 4.1, 4.1, 2.1))
  # png("GS1OpenFoodFacts_Compare_plot.png", height = 500, width = 700)
  # Nutri_plot2 <- plot(ecdf(cpd_data1$KJ), col = colorpal[1], xlab = "Max mean difference", ylab = "Cumulative Proportion",
  #                     main = "Accuracy in nutritional information between GS1 and OpenFoodFacts", 
  #                     xlim = c(0,1), lwd = 2, cex.axis = 1.2, cex.lab = 1.4, pch = 20)
  # lines(ecdf(cpd_data1$protein), col =  colorpal[2], lwd = 2, pch = 20)
  # lines(ecdf(cpd_data1$fat), col = colorpal[3] ,lwd = 2, pch = 20)
  # lines(ecdf(cpd_data1$carbohydrates), col = colorpal[4], lwd = 2, pch = 20)
  # legend("bottomright", legend = c("KJ", "Protein", "Fat", "Carbohydrates"), col = colorpal, lwd = 2, pch = 20)
  # Nutri_plot2
  # dev.off()
  
  matched_items <- off_df[(off_df$itemnumber %in% gs1_df$itemnumber),]$itemnumber
  
  cpd_data1 <- rbind(gs1_df[gs1_df$itemnumber %in% matched_items,],
                     off_df[off_df$itemnumber %in% matched_items,]) %>% group_by(itemnumber) %>%
    filter(!(any(is.na(KJ)) | any(is.na(fat)) | any(is.na(protein)) | any(is.na(carbohydrates)))) %>%
    group_by(itemnumber,datasource) %>% slice(1) %>% group_by(itemnumber) %>%
    summarise(KJ = max_mean_diff(KJ), protein = max_mean_diff(protein),
              fat = max_mean_diff(fat), carbohydrates = max_mean_diff(carbohydrates))
  
  #colorpal <- brewer.pal(4,"Pastel1")
  #HEAP colorpalette
  colorpal <- rgb(red = c(30,115,236,243), green = c(105,181,102,149), blue = c(160,169,105,104), maxColorValue = 255)
  
  par(mar = c(5.1, 4.1, 4.1, 2.1))
  par(family = "arial")
  pdf("Figures/GS1OpenFoodFacts_Compare_plot.pdf", height = 6, width = 8)
  Nutri_plot2 <- plot(ecdf(cpd_data1$KJ), col = colorpal[1], xlab = "Max mean difference", ylab = "Cumulative Proportion",
                      main = "Accuracy in nutritional information between GS1 and OpenFoodFacts", 
                      xlim = c(0,1), lwd = 2, cex.axis = 1.2, cex.lab = 1.4, pch = 20)
  lines(ecdf(cpd_data1$protein), col =  colorpal[2], lwd = 2, pch = 20)
  lines(ecdf(cpd_data1$fat), col = colorpal[3] ,lwd = 2, pch = 20)
  lines(ecdf(cpd_data1$carbohydrates), col = colorpal[4], lwd = 2, pch = 20)
  legend("bottomright", legend = c("KJ", "Protein", "Fat", "Carbohydrates"), col = colorpal, lwd = 2, pch = 20)
  Nutri_plot2
  dev.off()
  
  
}



Items_cumu_plot <- function(cpd_original){
  
  cpd_n_items <- cpd_original %>% group_by(itemname) %>%
    filter(!(itemname %in% c("pant","flaskepant", "daasepant", "pose", "baerepose"))) %>%
    summarise(n_items = sum(amount)) %>% arrange(-n_items)
  
  Percentage_of_itemnames <- cumsum(cpd_n_items$n_items)/sum(cpd_n_items$n_items)*100
  Percentage_of_1000_itemnames <- round(cumsum(cpd_n_items[1:1000,]$n_items)/sum(cpd_n_items$n_items)*100,1)
  
  par(family = "arial")
  pdf("Figures/Figure s2.pdf", height = 6, width = 8)
  plot(c(1:10000), Percentage_of_itemnames[1:10000], xlab = "Products", ylab = "Cumulative percentage (%)",
       lty = 1,type = "l",  lwd =2, frame.plot = FALSE, ylim = c(0,100), xaxt = "n")
  axis(1, at  =seq(0,10000, by  =1000), las = 1)
  abline(v = seq(0, 10000, by = 1000), lty = 2, col = "grey")
  abline(h = seq(0, 100, by = 20), lty = 2, col = "grey")
  dev.off()
}

Table2_figures <- function(cpd2){
  
  cpd_purch1 <- cpd2  %>% group_by(receipt) %>%
    mutate(StoreType = case_when(merchantname %in% tolower(c("Bilka", "Foetex", "MENY", "Min Koebmand", "Netto", "REMA1000",
                                                             "SPAR", "Loevbjerg", "ABC Lavpris")) ~ "Groceries",
                                 merchantname %in% tolower(c("Carls Jr.", "Lagkagehuset")) ~ "Cafe/Restaurant",
                                 merchantname %in% tolower(c("Matas", "Normal")) ~ "Cosmetics",
                                 TRUE ~ "Other"),
           TotalMoney = sum(total_price),
           n_items = sum(amount),
           n_items_unique = length(unique(itemname))) %>%
    ungroup()  %>% 
    mutate(new_receipt = !duplicated(receipt),
           new_itemname = !duplicated(itemname),
           Grocery_Store = case_when(merchantname == "bilka" ~ "Store 1",
                                     merchantname == "foetex" ~ "Store 2",
                                     merchantname == "netto" ~ "Store 3",
                                     merchantname == "rema1000" ~ "Store 4",
                                     (StoreType == "Groceries") & !(merchantname %in% c("bilka", "foetex", "netto","rema1000")) ~ "Other grocery stores",
                                     TRUE ~ NA_character_)) %>%
    dplyr::select(receipt,itemname, amount, StoreType, Grocery_Store, yearno)
  
  #cpd_purch1$yearno <- as.factor(cpd_purch1$yearno)
  #cpd_purch1$yearno <- factor(cpd_purch1$yearno, levels = c(2014:2023), labels = c("2014-2015","2014-2015", "2016-2017","2016-2017",
  #                                                    "2018-2019","2018-2019","2020-2021","2020-2021",
  #                                                             "2022-2023","2022-2023"))
  
  #cpd_data2 <- cpd_data1[1:10000,]
  facet_names <- list(
    "receipt" = "Number of receipts",
    "itemname" = "Number of unique product names",
    "Total number of products bought"
  )
  facet_labeller <- function(variable, value){
    return(facet_names[value])
  }
  
  p1 <- cpd_purch1 %>% dplyr::select(-StoreType, -Grocery_Store) %>%
    group_by(yearno) %>%
    summarise(receipt = length(unique(receipt)),
              itemname = length(unique(itemname)),
              items_bought = sum(amount)) %>% ungroup() %>%
    gather(Variable, n, c("receipt","itemname", "items_bought"), factor_key = TRUE) %>%
    mutate(yearno = as.factor(yearno)) %>%
    ggplot(., aes(x = yearno, y = n)) +
    geom_point() + facet_wrap(~Variable, ncol = 1, labeller=facet_labeller, scales = "free") + theme_bw() + 
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank()) + 
    ylab("")
  
  png("Figures/Purchases_Time.png", height = 6, width = 8, units = "in", res = 500)
  p1
  dev.off()
  
  
  p2 <- cpd_purch1 %>% dplyr::select(-StoreType, -receipt, -itemname, amount) %>%
    group_by(yearno, Grocery_Store) %>%
    summarise(n = sum(amount)) %>% filter(!is.na(Grocery_Store)) %>% ungroup() %>%
    mutate(yearno = as.factor(yearno)) %>% complete(.,yearno, Grocery_Store, fill = list(n = 0)) %>%
    ggplot(., aes(x = yearno, y = n, fill = Grocery_Store)) +
    geom_bar(stat = "identity",width = 0.9, position = position_dodge()) +
    theme_bw() + theme(legend.position = "bottom",
                       plot.title = element_text(hjust = 0.5)) +
    xlab("Year") + ylab("") + ggtitle("Products bought in different stores") +
    scale_fill_viridis(discrete = TRUE, name = "Grocery Store:")
  
  png("Figures/Store_dist_frac_Time.png", height = 6, width = 8, units = "in", res = 500)
  p2
  dev.off()
  
  #grid.arrange(p1,p2, ncol = 1)
  # 
  # 
  # g1 <- ggplotGrob(p1)
  # g2 <- ggplotGrob(p2)
  # g <- rbind(g1,g2)
  # g$widths <- unit.pmax(g1$widths, g2$widths)
  # grid.newpage()
  # grid.draw(g)
  
}


StoreInfluence <- function(cpd){
  n_large_groups <- function(data,n,n_group, group, ...){
    X <- aggregate(data[,n_group], data[,group], FUN = sum)
    
    return(X[order(X[,n_group],decreasing = TRUE)[1:n],group])
  }
  
  #cpd <- read.csv(file.path(my_directory,"Data",format(DateofData, "%Y%b%d"),"ConsumerReceiptData_Enriched.csv"))
  cpd <- tar_read(cpd_participants)
  
  cpd <- cpd %>% filter(datasource == "frida")%>%
    mutate(group_broad = case_when(group %in% c("saebe og hygiejne","saebe_og_hygiejne") ~ "saebe og hygiejne",
                                   group == "rengoering" ~ "rengoering",
                                   group == "tandsundhed" ~ "tandsundhed",
                                   TRUE ~ group_broad)) %>%
    ungroup() %>% filter(!(group %in% c("empty", "pant_retur", "other", "plantebaserede produkter"))) %>%
    filter(!(group_broad %in% c("other", "plantebaserede produkter", "non food"))) %>%
    filter(!is.na(group)) 
  
  
  df_total <- cpd %>% #filter(merchantname %in% c("bilka", "foetex", "netto", "rema1000")) %>%
    group_by(participant) %>% 
    mutate(total_purchases = sum(amount)) %>% 
    group_by(participant,  group_broad) %>%
    summarise(group_total_frac = sum(amount)/first(total_purchases))
  
  #Grouped for each Merchant
  df_merchant <- cpd %>% filter(merchantname %in% c("bilka", "foetex", "netto", "rema1000")) %>%
    group_by(participant,  merchantname) %>% 
    mutate(total_purchases = sum(amount)) %>% 
    group_by(participant,  group_broad, merchantname) %>%
    summarise(n_items = sum(amount)) %>% 
    group_by(participant) %>% 
    complete(group_broad, merchantname, fill = list(n_items = 0))#, group_merchant_frac = sum(amount)/first(total_purchases))
  
  
  filter_df_merchant <- function(df_merchant, df_total, filter_arg){
    df_merchant1 <- df_merchant %>% filter(merchantname %in% filter_arg) %>%
      group_by(participant) %>% mutate(total_items = sum(n_items)) %>% 
      group_by(participant, group_broad) %>% 
      summarise(n_items = sum(n_items), group_merchant_frac = sum(n_items)/first(total_items)) %>%
      mutate(new_merchantname = paste0(filter_arg, collapse = ", "),
             no_merchant = length(filter_arg)) %>% ungroup()
    
    df_final <- df_total %>% right_join(df_merchant1, by = c("participant", "group_broad")) %>%
      mutate(group_broad = capitalize(gsub("_", " ", group_broad)),
             group_diff = (group_merchant_frac - group_total_frac)/group_total_frac)
             #group_diff = group_merchant_frac - group_total_frac)
    return(df_final)
  }
  
  
  merchantname_list <- c("bilka", "foetex", "netto", "rema1000")
  df_1_store <- do.call("rbind", apply(combn(merchantname_list,1),2, filter_df_merchant,
                                       df_merchant = df_merchant, df_total= df_total))
  df_2_store <- do.call("rbind", apply(combn(merchantname_list,2),2, filter_df_merchant,
                                       df_merchant = df_merchant, df_total= df_total))
  df_3_store <- do.call("rbind", apply(combn(merchantname_list,3),2, filter_df_merchant,
                                       df_merchant = df_merchant, df_total= df_total))
  df_4_store <- do.call("rbind", apply(combn(merchantname_list,4),2, filter_df_merchant,
                                       df_merchant = df_merchant, df_total= df_total))
  
  df_StoreInfluence <- rbind(df_1_store,df_2_store,
                             df_3_store, df_4_store) %>% ungroup() %>%
    dplyr::select(participant, group_broad, no_merchant, group_diff) %>%
    mutate(no_merchant = case_when(no_merchant == 1 ~ "Single_Stores",
                                   no_merchant == 2 ~ "Two_Stores",
                                   no_merchant == 3 ~ "Three_Stores",
                                   no_merchant == 4 ~ "Four_Stores"))
  
  # df2 <- df_total %>% left_join(df_merchant, by = c("participant", "group")) %>%
  #   mutate(group = capitalize(gsub("_", " ", group))) %>%
  #   group_by(participant, group, merchantname) %>% slice(1) %>% ungroup() %>% 
  #   mutate(group_diff = group_total_frac - group_merchant_frac) %>%
  #   mutate(group_diff = case_when(is.na(group_diff) ~ 0,
  #                    TRUE ~ group_diff)) %>% ungroup() %>%
  #   mutate(merchantname = as.factor(factor(merchantname, levels = unique(merchantname), labels = c("Store1", "Store2", "Store3", "Store4")))) %>%
  #   group_by(participant) %>%
  #   complete(group, merchantname, fill = list(group_diff = NA)) %>% group_by(participant, group) %>% 
  #   mutate(group_diff = case_when(is.na(group_diff) ~ -group_total_frac[!is.na(group_total_frac)][1],
  #                                 TRUE ~ group_diff))
  
  
  ##############
  library(Hmisc)
  library(gt)
  
  
  twenty_largest_groups <- n_large_groups(cpd %>% group_by(group_broad) %>%
                                            summarise(n_group = sum(amount)), n = 20, n_group = "n_group", group = "group_broad")
  
  
  X_table <-  df_StoreInfluence  %>%
    group_by(group_broad, no_merchant) %>%
    #spread(merchantname, group_diff) %>% 
    summarise(median_diff = paste0(round(quantile(group_diff*100, probs = 0.5),1)," (",
                                   round(quantile(group_diff*100, probs = 0.1),1), "; ",
                                   round(quantile(group_diff*100, probs = 0.9),1), ")")) %>%
    group_by(group_broad) %>%
    filter(group_broad %in% capitalize(gsub("_", " ", twenty_largest_groups))) %>% 
    ungroup() %>%  spread(no_merchant, median_diff) %>%
    left_join( cpd %>% filter(group_broad %in% twenty_largest_groups) %>% 
                 mutate(group_broad = capitalize(gsub("_", " ", group_broad)))  %>%
                 group_by(group_broad) %>% summarise(n_items_group = sum(amount, na.rm = TRUE)), by = "group_broad") %>%
    arrange(-n_items_group) %>%  
    #Add total of the 20 biggest food groups
    rbind(., df_StoreInfluence %>% filter(group_broad %in% capitalize(gsub("_", " ", twenty_largest_groups))) %>%
            #spread(merchantname, group_diff) %>% 
            group_by(no_merchant) %>% 
            summarise(median_diff = paste0(round(quantile(group_diff*100, probs = 0.5),1)," (",
                                           round(quantile(group_diff*100, probs = 0.10),1), "; ",
                                           round(quantile(group_diff*100, probs = 0.90),1), ")"),
                      "group_broad" = "Total of most frequent groups") %>% 
            ungroup()  %>% spread(no_merchant, median_diff) %>% 
            mutate(n_items_group = sum(cpd[cpd$group_broad %in% twenty_largest_groups,]$amount, na.rm = TRUE))) %>%
    rbind(., df_StoreInfluence %>% filter(group_broad %in% c("Rengoering", "Tandsundhed", "Tobacco and nicotine gum",
                                                             "Saebe_og_hygiejne", "Saebe og hygiejne")) %>%
            mutate(group_broad = case_when(group_broad %in% c("Saebe_og_hygiejne", "Saebe og hygiejne") ~ "Soap and Hygiene",
                                           group_broad == "Rengoering" ~ "Cleaning products",
                                           group_broad == "Tandsundhed" ~ "Dental products",
                                           group_broad == "Tobacco and nicotine gum" ~ "Tobacco")) %>%
            group_by(group_broad, no_merchant) %>% 
            summarise(median_diff = paste0(round(quantile(group_diff*100, probs = 0.5),1)," (",
                                           round(quantile(group_diff*100, probs = 0.10),1), "; ",
                                           round(quantile(group_diff*100, probs = 0.90),1), ")")) %>% 
            ungroup() %>%   spread(no_merchant, median_diff) %>% 
            left_join( cpd %>% filter(group_broad %in% c("rengoering", "tandsundhed", "tobacco and nicotine gum",
                                                         "saebe_og_hygiejne", "saebe og hygiejne")) %>%
                         mutate(group_broad = capitalize(gsub("_", " ", group_broad))) %>%
                         mutate(group_broad = case_when(group_broad %in% c("Saebe_og_hygiejne", "Saebe og hygiejne") ~ "Soap and Hygiene",
                                                        group_broad == "Rengoering" ~ "Cleaning products",
                                                        group_broad == "Tandsundhed" ~ "Dental products",
                                                        group_broad == "Tobacco and nicotine gum" ~ "Tobacco")) %>%
                         group_by(group_broad) %>% 
                         summarise(n_items_group = sum(amount, na.rm = TRUE)), by = "group_broad") %>%
            arrange(-n_items_group)) %>%  
    # #Add total of all product groups
    rbind(., df_StoreInfluence %>%
            group_by(no_merchant) %>% 
            summarise(median_diff = paste0(round(quantile(group_diff*100, probs = 0.5),1)," (",
                                           round(quantile(group_diff*100, probs = 0.10),1), "; ",
                                           round(quantile(group_diff*100, probs = 0.90),1), ")"),
                      group_broad = "Total") %>% 
            ungroup()  %>% 
            spread(no_merchant, median_diff) %>% 
            mutate(n_items_group = sum(cpd$amount, na.rm = TRUE))) %>%
    mutate(n_items_group = format(n_items_group, big.mark = ",")) %>%
    dplyr::select(-Three_Stores)
  
  
  
  X_table2 <- X_table %>% gt() %>%   cols_label(
    group_broad = md("Products Group"), 
    Single_Stores = md("One"),
    Two_Stores = md("Two"),
    #  Three_Stores = md("Three"),
    Four_Stores = md("Four"),
    n_items_group = md("N products")) %>%
    tab_header(title = "Median (10th; 90th percentiles) change in the estimated proportion of purchases by product group when using only information from 1,2 or 4 retailers compared to using all information",
               subtitle = paste0("")) %>%
    tab_spanner(label = "Number of retailers",
                columns = c(Single_Stores,Two_Stores,
                            Four_Stores)) %>%
    tab_style(
      style = list(
        cell_text(weight = "bold"),
        cell_borders(sides = "top", weight = "bold")
      ),
      locations = cells_body(
        #      columns = group,
        rows = group_broad %in% c("Total","Total of most frequent groups")
      )) %>% 
    cols_move_to_end(Single_Stores) %>% 
    cols_move_to_end(Two_Stores) %>% 
    #  cols_move_to_end(Three_Stores) %>% 
    cols_move_to_end(Four_Stores) %>% 
    cols_move_to_end(n_items_group)%>%
    tab_source_note(
      source_note = "Table 2 shows the median, 10th, 90th percentiles, of product group specific difference in the fraction of 
    purchases in sets containing a different number of stores compared to all stores used, across all participants, and the four largest stores."
    )
  
  gtsave(X_table2, "Tables/Table 2.docx" )
}

