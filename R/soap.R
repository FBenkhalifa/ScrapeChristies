library(RCurl)
library(XML)

headerFields =
  c(Accept = "text/xml",
    Accept = "multipart/*",
    'Content-Type' = "text/xml; charset=utf-8",
    SOAPAction = "http://www.christies.com/LotFinderAPI/Searchresults/GetSearchResults")

body = '<?xml version="1.0" encoding="utf-8"?>
<soap:Envelope xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/">
  <soap:Body>
    <GetSearchResults xmlns="http://www.christies.com/LotFinderAPI/Searchresults">
      <StrUrl>string</StrUrl>
      <PageSize>1</PageSize>
      <ClientGuid>""</ClientGuid>
      <GeoCountryCode>""</GeoCountryCode>
      <LanguageID>string</LanguageID>
      <IsLoadAll>int</IsLoadAll>
    </GetSearchResults>
  </soap:Body>
</soap:Envelope>'

h = basicTextGatherer()
curlPerform(url = "http://www.christies.com/LotFinderAPI/Searchresults/GetSearchResults.asmx",
            httpheader = headerFields,
            postfields = body
)
h$value()

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
c <- xmlParse(body)
c %>% str
# Using a C routine as the reader of the body of the response.
if(url.exists("http://www.omegahat.net/RCurl/index.html")) {
  routine = getNativeSymbolInfo("R_internalWriteTest", PACKAGE = "RCurl")$address
  curlPerform(URL = "http://www.omegahat.net/RCurl/index.html",
              writefunction = routine)
}
