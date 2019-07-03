library(XBRL)
library(Rcpp)
library(dplyr)

principal<-"C:/Users/ivecle01/Documents/Principal/Estados_financieros_Principal/Principal.xbrl"
consorcio<-"C:/Users/ivecle01/Documents/Consorcio/Consorcio/Consorcio.xbrl"
metlife<-"C:/Users/ivecle01/Documents/Metlife/Metlife/Metlife.xbrl"
consolidada<-"C:/Users/ivecle01/Documents/Chilena Consolidada/Consolidada/Consolidada/Consolidada.xbrl"
bice<-"C:/Users/ivecle01/Documents/Bice/BICE/Bice.xbrl"
mutualseguros<-"C:/Users/ivecle01/Documents/Mutual/Mutual/Mutual.xbrl"


options(stringsAsFactors = FALSE)

doc_principal <- xbrlParse(principal)
doc_consorcio <- xbrlParse(consorcio)
doc_metlife <- xbrlParse(metlife)
doc_consolidada <- xbrlParse(consolidada)
doc_bice <- xbrlParse(consolidada)
doc_seguros <- xbrlParse(mutualseguros)
## Get a data frame with facts:
fct_principal <- xbrlProcessFacts(doc_principal)
fct_consorcio <- xbrlProcessFacts(doc_consorcio)
fct_metlife <- xbrlProcessFacts(doc_metlife)
fct_consolidada <- xbrlProcessFacts(doc_consolidada)
fct_bice <- xbrlProcessFacts(doc_bice)
fct_seguros <- xbrlProcessFacts(doc_seguros)

## Get a data frame with contexts:
#cts <- xbrlProcessContexts(doc)
## Get a data frame with units:
#unt <- xbrlProcessUnits(doc)
## Free the external memory used:
xbrlFree(doc)
## End(Not run)
xbrl.vars_principal <- xbrlDoAll(principal,cache.dir = "XBRLcache_prin", prefix.out = NULL)
xbrl.vars_consorcio <- xbrlDoAll(consorcio,cache.dir = "XBRLcache_consorcio", prefix.out = NULL)
xbrl.vars_metlife <- xbrlDoAll(metlife,cache.dir = "XBRLcache_metlife", prefix.out = NULL)
xbrl.vars_consolidada <- xbrlDoAll(consolidada,cache.dir = "XBRLcache_consolidada", prefix.out = NULL)
xbrl.vars_bice  <- xbrlDoAll(bice,cache.dir = "XBRLcache_consolidada", prefix.out = NULL)
xbrl.vars_seguros <- xbrlDoAll(mutualseguros,cache.dir = "XBRLcache_seguros", prefix.out = NULL)
#xbrl.sec <- xbrlSECdev01(xbrl.vars)
#xbrl.sec$showStatements()

library(devtools)
#install_github("bergant/finstr")



targetRF<- c("cl-cs_ActivosFinancierosCostoAmortizado",
             "cl-cs_ActivosFinancierosCostoAmortizadoNacional",
             "cl-cs_DerivadosCostoAmortizado",
             "cl-cs_DeterioroInversionesCostoAmortizado",
             "cl-cs_InstrumentoDeudaOCreditoCostoAmortizado",
             "cl-cs_InstrumentosDelEstadoCostoAmortizado",
             "cl-cs_InstrumentosEmitidosPorSistemaFinancieroCostoAmortizado",
             "cl-cs_InversionesRentaFijaNacionalCostoAmortizado",
             "cl-cs_OtrosRentaFijaNacionalResultadoInversiones",
             "cl-cs_RentaFijaExtranjeraResultadoInversiones",
             "cl-cs_InstrumentosEmpresasNacionalesTransadosEnExtranjeroCostoAmortizado",
             "cl-cs_MutuosHipotecariosCostoAmortizado",
             "cl-cs_OtrasInversionesExtranjerasRentaFijaCostoAmortizado",
             "cl-cs_OtrasInversionesRentaFijaNacionalCostoAmortizado",
             "cl-cs_OtrosActivosFinancierosCostoAmortizado",
             "cl-cs_InversionesRentaFijaExtranjeraCostoAmortizado",
             "cl-cs_TitulosEmitidosPorBancosYFinancierasExtranjerasCostoAmortizado",
             "cl-cs_TitulosEmitidosPorEmpresasExtranjerasCostoAmortizado",
             "cl-cs_TitulosEmitidosPorEstadosYBancosCentralesExtranjerosCostoAmortizado")

Renta_Fija_principal<- xbrl.vars_principal$fact %>%
  filter(elementId %in% targetRF) %>%
  #filter(elementId == "cl-cs_ActivosFinancierosCostoAmortizado" ) %>%
  filter(contextId == "CierreTrimestreActual") %>%
  left_join(xbrl.vars_principal$context, by = "contextId") %>%
  filter(is.na(dimension1)) %>%
  select(elementId, fact) #%>% 
  #(knitr::kable)(format = "markdown")

Renta_Fija_consorcio<- xbrl.vars_consorcio$fact %>%
  filter(elementId %in% targetRF) %>%
  #filter(elementId == "cl-cs_ActivosFinancierosCostoAmortizado" ) %>%
  filter(contextId == "CierreTrimestreActual") %>%
  left_join(xbrl.vars_consorcio$context, by = "contextId") %>%
  filter(is.na(dimension1)) %>%
  select(elementId, fact) #%>% 
#(knitr::kable)(format = "markdown")

Renta_Fija_metlife<- xbrl.vars_metlife$fact %>%
  filter(elementId %in% targetRF) %>%
  #filter(elementId == "cl-cs_ActivosFinancierosCostoAmortizado" ) %>%
  filter(contextId == "p1_Instant") %>%
  left_join(xbrl.vars_metlife$context, by = "contextId") %>%
  #filter(is.na(dimension1)) %>%
  select(elementId, fact) #%>% 
#(knitr::kable)(format = "markdown")

Renta_Fija_consolidada<- xbrl.vars_consolidada$fact %>%
  filter(elementId %in% targetRF) %>%
  #filter(elementId == "cl-cs_ActivosFinancierosCostoAmortizado" ) %>%
  filter(contextId == "p1_Instant") %>%
  left_join(xbrl.vars_consolidada$context, by = "contextId") %>%
  filter(is.na(dimension1)) %>%
  select(elementId, fact) #%>% 
#(knitr::kable)(format = "markdown")

Renta_Fija_bice<- xbrl.vars_bice$fact %>%
  filter(elementId %in% targetRF) %>%
  #filter(elementId == "cl-cs_ActivosFinancierosCostoAmortizado" ) %>%
  filter(contextId == "CierreTrimestreActual") %>%
  left_join(xbrl.vars_bice$context, by = "contextId") %>%
  filter(is.na(dimension1)) %>%
  select(elementId, fact) #%>% 
#(knitr::kable)(format = "markdown")

Renta_Fija_seguros<- xbrl.vars_seguros$fact %>%
  filter(elementId %in% targetRF) %>%
  #filter(elementId == "cl-cs_ActivosFinancierosCostoAmortizado" ) %>%
  filter(contextId == "CierreTrimestreActual") %>%
  left_join(xbrl.vars_seguros$context, by = "contextId") %>%
  filter(is.na(dimension1)) %>%
  select(elementId, fact) #%>% 
#(knitr::kable)(format = "markdown")

write.xlsx(Renta_Fija_principal, "C:/Users/ivecle01/Documents/Principal/Estados_financieros_Principal/principal_RF.xlsx")
write.xlsx(Renta_Fija_consorcio, "C:/Users/ivecle01/Documents/Consorcio/Consorcio/consorcio_RF.xlsx")
write.xlsx(Renta_Fija_metlife, "C:/Users/ivecle01/Documents/Metlife/Metlife/metlife_RF.xlsx")
write.xlsx(Renta_Fija_consolidada, "C:/Users/ivecle01/Documents/Chilena Consolidada/Consolidada/consolidada_RF.xlsx")
write.xlsx(Renta_Fija_bice, "C:/Users/ivecle01/Documents/Bice/BICE/bice_RF.xlsx")
write.xlsx(Renta_Fija_seguros, "C:/Users/ivecle01/Documents/Mutual/Mutual/mutual_RF.xlsx")


targetRV<- c("cl-cs_AccionesSociedadesAnonimasAbiertasValorRazonable",
             "cl-cs_AccionesSociedadesAnonimasCerradasValorRazonable",
             "cl-cs_AccionesSociedadesExtranjerasValorRazonable",
             "cl-cs_ActivosFinancierosValorRazonable",
             "cl-cs_ActivosFinancierosValorRazonableNacional",
             "cl-cs_CuotasFondosInversionConstituidosPaisCuyosActivosEstanInvertidosValoresExtranjerosValorRazonable",
             "cl-cs_CuotasFondosInversionExtranjerosValorRazonable",
             "cl-cs_CuotasFondosMutuosExtranjerosValorRazonable",
             "cl-cs_EmitidosPorEstadosYBancosCentralesExtranjerosValorRazonable",
             "cl-cs_FondosInversionValorRazonable",
             "cl-cs_FondosMutuosValorRazonable",
             "cl-cs_InstrumentoDeudaOCreditoValorRazonable",
             "cl-cs_InstrumentosEmitidosSistemaFinancieroValorRazonable",
             "cl-cs_InstrumentosEmpresasNacionalesTransadosEnExtranjeroValorRazonable",
             "cl-cs_InstrumentosEstadoValorRazonable",
             "cl-cs_InversionesExtranjeroValorRazonable",
             "cl-cs_InversionesNacionalesValorRazonable",
             "cl-cs_MutuosHipotecariosValorRazonable",
             "cl-cs_OtraRentaFijaNacionalValorRazonable",
             "cl-cs_OtraRentaVariableNacionalValorRazonable",
             "cl-cs_RentaFijaExtranjeraValorRazonable",
             "cl-cs_RentaFijaNacionalValorRazonable",
             "cl-cs_RentaVariableExtranjeraValorRazonable",
             "cl-cs_RentaVariableNacionalValorRazonable",
             "cl-cs_TitulosEmitidosPorBancosYFinancierasExtranjerasValorRazonable",
             "cl-cs_TitulosEmitidosPorEmpresasExtranjerasValorRazonable",
             "cl-cs_ValorRazonablePropiedadesInversion",
             "cl-cs_ValorRazonablePropiedadesUsoPropio")



Renta_Variable_principal<- xbrl.vars_principal$fact %>%
  filter(elementId %in% targetRV) %>%
  #filter(elementId == "cl-cs_ActivosFinancierosCostoAmortizado" ) %>%
  filter(contextId == "CierreTrimestreActual") %>%
  left_join(xbrl.vars_principal$context, by = "contextId") %>%
  filter(is.na(dimension1)) %>%
  select(elementId, fact) #%>% 
  #(knitr::kable)(format = "markdown")

Renta_Variable_consorcio<- xbrl.vars_consorcio$fact %>%
  filter(elementId %in% targetRV) %>%
  #filter(elementId == "cl-cs_ActivosFinancierosCostoAmortizado" ) %>%
  filter(contextId == "CierreTrimestreActual") %>%
  left_join(xbrl.vars_consorcio$context, by = "contextId") %>%
  filter(is.na(dimension1)) %>%
  select(elementId, fact) #%>% 
#(knitr::kable)(format = "markdown")

Renta_Variable_metlife<- xbrl.vars_metlife$fact %>%
  filter(elementId %in% targetRV) %>%
  #filter(elementId == "cl-cs_ActivosFinancierosCostoAmortizado" ) %>%
  filter(contextId == "p1_Instant") %>%
  left_join(xbrl.vars_metlife$context, by = "contextId") %>%
  filter(is.na(dimension1)) %>%
  select(elementId, fact) #%>% 
#(knitr::kable)(format = "markdown")

Renta_Variable_consolidada<- xbrl.vars_consolidada$fact %>%
  filter(elementId %in% targetRV) %>%
  #filter(elementId == "cl-cs_ActivosFinancierosCostoAmortizado" ) %>%
  filter(contextId == "p1_Instant") %>%
  left_join(xbrl.vars_consolidada$context, by = "contextId") %>%
  filter(is.na(dimension1)) %>%
  select(elementId, fact) #%>% 
#(knitr::kable)(format = "markdown")

Renta_Variable_bice<- xbrl.vars_bice$fact %>%
  filter(elementId %in% targetRV) %>%
  #filter(elementId == "cl-cs_ActivosFinancierosCostoAmortizado" ) %>%
  filter(contextId == "CierreTrimestreActual") %>%
  left_join(xbrl.vars_bice$context, by = "contextId") %>%
  filter(is.na(dimension1)) %>%
  select(elementId, fact) #%>% 
#(knitr::kable)(format = "markdown")

Renta_Variable_seguros<- xbrl.vars_seguros$fact %>%
  filter(elementId %in% targetRV) %>%
  #filter(elementId == "cl-cs_ActivosFinancierosCostoAmortizado" ) %>%
  filter(contextId == "CierreTrimestreActual") %>%
  left_join(xbrl.vars_seguros$context, by = "contextId") %>%
  filter(is.na(dimension1)) %>%
  select(elementId, fact) #%>% 
#(knitr::kable)(format = "markdown")

library(xlsx)
library(timeSeries)

#write.xlsx(xbrl.vars$fact, "C:/Users/ivecle01/Documents/Principal/principal.xlsx")
write.xlsx(Renta_Variable_principal, "C:/Users/ivecle01/Documents/Principal/Estados_financieros_Principal/principal_RV.xlsx")
write.xlsx(Renta_Variable_consorcio, "C:/Users/ivecle01/Documents/Consorcio/Consorcio/consorcio_RV.xlsx")
write.xlsx(Renta_Variable_metlife, "C:/Users/ivecle01/Documents/Metlife/Metlife/metlife_RV.xlsx")
write.xlsx(Renta_Variable_consolidada, "C:/Users/ivecle01/Documents/Chilena Consolidada/Consolidada/consolidada_RV.xlsx")
write.xlsx(Renta_Variable_bice, "C:/Users/ivecle01/Documents/Bice/BICE/bice_RV.xlsx")
write.xlsx(Renta_Variable_seguros, "C:/Users/ivecle01/Documents/Mutual/Mutual/mutual_RV.xlsx")

########################################################################################################


str(xbrl.vars_principal, max.level = 1)

library(knitr)
library(dplyr)

xbrl.vars$fact %>%
  filter(elementId == "cl-cs_TipoCambioMercadoSwap" ) %>%
  left_join(xbrl.vars$context, by = "contextId") %>%
  filter(is.na(dimension1)) %>%
  select(startDate, endDate, fact, unitId, elementId) %>% 
  (knitr::kable)(format = "markdown")


table(xbrl.vars_principal$type)


htmlTable::htmlTable(data.frame(Statements=
                                  with(
                                    xbrl.vars$role[xbrl.vars_principal$role$type=="Statement", ],
                                    paste(roleId, "\n<br/>", definition, "\n<p/>")
                                  )),
                     align = "l",
                     rnames = FALSE
)

conn.data <- xbrl.vars_principal$showStatements()

sch <- xbrlGetSchemaName(doc)

## Use on an instance file
xbrlGetSchemaName(doc)
## Use on a taxonomy file
xbrlGetLinkbaseNames(doc)
xbrlGetImportNames(doc)



library(tidyr)
library(dplyr)
library(finstr)

# let's get the balace sheet
role_id <- principal2
# prepare presentation linkbase : 
# filter by role_id an convert order to numeric

#names(xbrl.vars$presentation)
pres <- 
  xbrl.vars$presentation %>%
  #filter(roleId %in% role_id) %>%
  mutate(order = as.numeric(order))

xbrl.vars$role
names(xbrl.vars)
# start with top element of the presentation tree
pres_df <- 
  pres %>%
  anti_join(pres, by = c("fromElementId" = "toElementId")) %>%
  select(elementId = fromElementId)

# breadth-first search
while({
  df1 <- pres_df %>%
    na.omit() %>%
    left_join( pres, by = c("elementId" = "fromElementId")) %>%
    arrange(elementId, order) %>%
    select(elementId, child = toElementId);
  nrow(df1) > 0
}) 
{
  # add each new level to data frame
  pres_df <- pres_df %>% left_join(df1, by = "elementId")
  names(pres_df) <-  c(sprintf("level%d", 1:(ncol(pres_df)-1)), "elementId")
}
# add last level as special column (the hierarchy may not be uniformly deep)
pres_df["elementId"] <- 
  apply( t(pres_df), 2, function(x){tail( x[!is.na(x)], 1)})
pres_df["elOrder"] <- 1:nrow(pres_df) 

# the final data frame structure is
str(pres_df, vec.len = 1 )






# join concepts with context, facts
pres_df_num <-
  pres_df %>%
  left_join(xbrl.vars$fact, by = "elementId") %>%
  left_join(xbrl.vars$context, by = "contextId") %>%
  #filter(is.na(dimension1)) %>%
  #filter(!is.na(endDate)) %>%
  select(elOrder, contains("level"), elementId, fact, decimals, endDate) %>%
  mutate( fact = as.numeric(fact)*10^(as.numeric(decimals))) %>%
  spread(endDate, fact ) %>%
  arrange(elOrder)

library(pander)
pres_df_num %>% 
  select(elementId, contains("2013"), contains("2012")) %>%
  pandoc.table(
    style = "rmarkdown",
    split.table = 200,
    justify = c("left", "right", "right")
  )


# labels for our financial statement (role_id) in "en-US" language:
x_labels <-
  xbrl.vars$presentation %>%
  filter(roleId == role_id) %>%
  select(elementId = toElementId, labelRole = preferredLabel) %>%
  semi_join(pres_df_num, by = "elementId") %>%
  left_join(xbrl.vars$label, by = c("elementId", "labelRole")) %>%
  filter(lang == "en-US") %>%
  select(elementId, labelString)

# calculated elements in this statement component
x_calc <- xbrl.vars$calculation %>%
  filter(roleId == role_id) %>%
  select(elementId = fromElementId, calcRoleId = arcrole) %>%
  unique()

# join concepts and numbers with labels
balance_sheet_pretty <- pres_df_num %>%
  left_join(x_labels, by = "elementId") %>%
  left_join(x_calc, by = "elementId") %>%
  select(labelString, contains("2013"), contains("2012"), calcRoleId)


names(balance_sheet_pretty)[1] <- 
  "CONDENSED CONSOLIDATED BALANCE SHEETS (mio USD $)"

names(balance_sheet_pretty)[2:3] <-
  format(as.Date(names(balance_sheet_pretty)[2:3]), "%Y")
# rendering balance sheet

pandoc.table(
  balance_sheet_pretty[,1:3],
  style = "rmarkdown",
  justify = c("left", "right", "right"),
  split.table = 300,
  big.mark = ",",
  emphasize.strong.rows = which(!is.na(balance_sheet_pretty$calcRoleId))
)
