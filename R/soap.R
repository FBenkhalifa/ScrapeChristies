library(RCurl)
library(XML)
library(xml2)
library(tidyverse)

headerFields =
  c(Accept = "text/xml",
    Accept = "multipart/*",
    'Content-Type' = "text/xml; charset=utf-8",
    SOAPAction = "http://www.christies.com/LotFinderAPI/Searchresults.asmx")

body = '<?xml version="1.0" encoding="utf-8"?>
<soap:Envelope xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/">
  <soap:Body>
    <GetSaleDetails xmlns="http://www.christies.com/LotFinderAPI/Searchresults">
      <SaleID>"25313"</SaleID>
      <PageNo>""</PageNo>
      <ClientGuid>""</ClientGuid>
      <AccountNumber>""</AccountNumber>
      <ApiKey>""</ApiKey>
    </GetSaleDetails>
  </soap:Body>
</soap:Envelope>'

h = basicTextGatherer()
curlPerform(url = "http://www.christies.com/LotFinderAPI/Searchresults.asmx",
            httpheader = headerFields,
            postfields = body
)
h$value()
xmlParse(body)
c <- read_xml(body)
c <- read_xml(body)
c %>% xml_name
c %>% xml_children

GET("http://www.christies.com/interfaces/LotFinderAPI/Searchresults.asmx/GetSaleDetails?SaleID=25313")

        "https://www.christies.com/lotfinder/lot/a-pair-of-chinese-red-overlay-yellow-glass-5913075-details.aspx?from=salesummery&intobjectid=5913075&sid=2e93d45d-09f4-4570-95f9-fa9fd3a13fc3"
<a href="https://www.christies.com/lotfinder/lot/a-pair-of-chinese-red-overlay-yellow-glass-5913075-details.aspx?from=salesummery&amp;intobjectid=5913075&amp;sid=32881f09-dada-4e0f-aad8-e03bc5c6a780" onclick="addHash('lot_5913075')">

  <div class="image--container_square">
  <picture>
  <source media="(max-width: 767px)" srcset="https://www.christies.com/img/LotImages/2015/NYR/2015_NYR_03755_0001_000(a_pair_of_chinese_red-overlay_yellow_glass_bottle_vases_19th_century).jpg?mode=max&amp;width=767">
  <source media="(max-width: 1024px)" srcset="https://www.christies.com/img/LotImages/2015/NYR/2015_NYR_03755_0001_000(a_pair_of_chinese_red-overlay_yellow_glass_bottle_vases_19th_century).jpg?mode=max&amp;width=1024">
  <source media="(min-width: 1025px)" srcset="https://www.christies.com/img/LotImages/2015/NYR/2015_NYR_03755_0001_000(a_pair_of_chinese_red-overlay_yellow_glass_bottle_vases_19th_century).jpg?mode=max&amp;width=1025">
  <img class="image" alt="A PAIR OF CHINESE RED-OVERLAY YELLOW GLASS BOTTLE VASES" src="https://www.christies.com/img/LotImages/2015/NYR/2015_NYR_03755_0001_000(a_pair_of_chinese_red-overlay_yellow_glass_bottle_vases_19th_century).jpg?mode=max&amp;width=1025">
  </picture>

  </div>
  </a>











if(url.exists("http://www.omegahat.net/RCurl")) {
  h = basicTextGatherer()
  curlPerform(url = "http://www.omegahat.net/RCurl", writefunction = h$update)
  # Now read the text that was cumulated during the query response.
  h$value()
}


if(url.exists("http://services.soaplite.com/hibye.cgi")) {
  # SOAP request
  body = '<?xml version="1.0" encoding="UTF-8"?>\
<SOAP-ENV:Envelope SOAP-ENV:encodingStyle="http://schemas.xmlsoap.org/soap/encoding/" \
                   xmlns:SOAP-ENV="http://schemas.xmlsoap.org/soap/envelope/" \
                   xmlns:xsd="http://www.w3.org/1999/XMLSchema" \
                   xmlns:SOAP-ENC="http://schemas.xmlsoap.org/soap/encoding/" \
                   xmlns:xsi="http://www.w3.org/1999/XMLSchema-instance">\
  <SOAP-ENV:Body>\
       <namesp1:hi xmlns:namesp1="http://www.soaplite.com/Demo"/>\
  </SOAP-ENV:Body>\
</SOAP-ENV:Envelope>\n'


  h$reset()
  curlPerform(url = "http://services.soaplite.com/hibye.cgi",
              httpheader=c(Accept="text/xml", Accept="multipart/*",
                           SOAPAction='"http://www.soaplite.com/Demo#hi"',
                           'Content-Type' = "text/xml; charset=utf-8"),
              postfields=body,
              writefunction = h$update,
              verbose = TRUE
  )

  body = h$value()

}


# Using a C routine as the reader of the body of the response.
if(url.exists("http://www.omegahat.net/RCurl/index.html")) {
  routine = getNativeSymbolInfo("R_internalWriteTest", PACKAGE = "RCurl")$address
  curlPerform(URL = "http://www.omegahat.net/RCurl/index.html",
              writefunction = routine)
}
