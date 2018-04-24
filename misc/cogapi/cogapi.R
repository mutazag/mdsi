## example using cog api text analytics to get topics and sentiment, ref:
## https://www.r-bloggers.com/r-quick-tip-microsoft-cognitive-services-text-analytics-api/

# install.packages("dplyr")
# install.packages("httr")
# install.packages("jsonlite")
library(httr)
library(jsonlite)

library(dplyr)

# Setup
cogapikey<-"fe43a4c63feb4f38aa00395557569102"
cogapi <- "https://australiaeast.api.cognitive.microsoft.com/text/analytics/v2.0"
# cogapi<-"https://westus.api.cognitive.microsoft.com/text/analytics/v2.0/languages"

text=c("is this english?"
       ,"tak er der mere kage"
       ,"merci beaucoup"
       ,"guten morgen"
       ,"bonjour"
       ,"merde"
       ,"Hello world. This is some input text that I love."
       ,"R is awesome"
       ,"صباح الخير"
       ,"قال رئيس الوزراء الأسترالي مالكوم تيرنبول أن سياسة حماية الحدود المتشددة هي المفتاح للحفاظ على ثقة الاستراليين بالحكومة وقدرتها على تنظيم عملية الهجرة."
       , "بشرى سارة لعشاق القهوة: اشرب 3 فناجين يومياً واحمِ قلبك!") 

# Prep data
df<-data_frame(id=1:length(text),text)
mydata<-list(documents= df)


# Topic detection 






#We have some different languages and we need to first do
# language detection before we can analyse the sentiment of our phrases

# Construct a request

response<-POST(paste0(cogapi,"/languages"), 
               add_headers(`Ocp-Apim-Subscription-Key`=cogapikey),
               body=toJSON(mydata))

# Now we need to consume our response such that we can add the language code to
# our existing data.frame. The structure of the response JSON doesn’t play well
# with others so I use data.table’s nifty rbindlist. It is a **very good*
# candidate for purrr but I’m not up to speed on that yet.

# Process response
respcontent<-content(response, as="text")
jsonResp <- fromJSON(respcontent)$documents$detectedLanguages
iso6391Name <- do.call("rbind",jsonResp)$iso6391Name
respdf<-data_frame(
  id=as.numeric(fromJSON(respcontent)$documents$id), 
  iso6391Name=iso6391Name
)

# Now that we have a table, we can join the two together

# Combine
dft <- df %>%
  inner_join(respdf) %>%
  select(id, language=iso6391Name, text) 



# key phrases
response<-POST(paste0(cogapi,"/keyPhrases"), 
               add_headers(`Ocp-Apim-Subscription-Key`=cogapikey),
               body=toJSON(mydata))
respcontent<-content(response, as="text")
jsonResp <- fromJSON(respcontent)$documents$keyPhrases

jsonResp <- lapply(jsonResp, function(x) 
{
  if(is.na(x[1])){
  return("")
}
  else {
   return(paste(x, collapse = ","))
  }
  })


keyPhrases <- unlist(jsonResp)
respdf<-data_frame(
  id=as.numeric(fromJSON(respcontent)$documents$id), 
  keyPhrases=keyPhrases
)


dft2 <- dft %>%
  inner_join(respdf)  



# Sentiment analysis With an ID, text, and a language code, we can now request
# the sentiment of our text be analysed.

# New info
mydata<-list(documents= dft)

# Construct a request
response<-POST(paste0(cogapi,"/sentiment"), 
               add_headers(`Ocp-Apim-Subscription-Key`=cogapikey),
               body=toJSON(mydata))
# Processing this response is simpler than processing the language response

# Process reponse
respcontent<-content(response, as="text")

sentimentResponse <- fromJSON(respcontent)$documents %>%
  mutate(id=as.numeric(id))
  

# Combine
finalDft <- dft2 %>%
  left_join(sentimentResponse) 


prettify(toJSON(subset( finalDft, language == "ar")))



##
##
## Translate Text 

cogTranslateApi <- "https://api.cognitive.microsoft.com/sts/v1.0"
cogTranslateApiKey <- "37b98bfef810450fa65ee9715f99656d"


response <- GET(cogTranslateApi,
                add_headers("Ocp-Apim-Subscription-Key"=cogTranslateApiKey))

translateRequest <- list("appid")


jsonRequest <- "<TranslateArrayRequest>
  <AppId />
<From>language-code</From>
<Options>
<Category xmlns=\"http://schemas.datacontract.org/2004/07/Microsoft.MT.Web.Service.V2\" >string-value</Category>
<ContentType xmlns=\"http://schemas.datacontract.org/2004/07/Microsoft.MT.Web.Service.V2\">text/plain</ContentType>
<ReservedFlags xmlns=\"http://schemas.datacontract.org/2004/07/Microsoft.MT.Web.Service.V2\" />
<State xmlns=\"http://schemas.datacontract.org/2004/07/Microsoft.MT.Web.Service.V2\" >int-value</State>
<Uri xmlns=\"http://schemas.datacontract.org/2004/07/Microsoft.MT.Web.Service.V2\" >string-value</Uri>
<User xmlns=\"http://schemas.datacontract.org/2004/07/Microsoft.MT.Web.Service.V2\" >string-value</User>
</Options>
<Texts>
<string xmlns=\"http://schemas.microsoft.com/2003/10/Serialization/Arrays\">string-value</string>
<string xmlns=\"http://schemas.microsoft.com/2003/10/Serialization/Arrays\">string-value</string>
</Texts>
<To>language-code</To>
</TranslateArrayRequest>"

textts <- c("text1", "text2")
textts_df <- data_frame(string = textts  )

rr <- list(TranslateArrayRequest = list(From="en", 
                                Options = list(
                                  Category = "scat", 
                                  ContentType = "cc",
                                  ReservedFlags = "", 
                                  State = 1, 
                                  Uri = "as", 
                                  User = "user"), 
                                Texts = textts_df, 
                                To = "en"
                                )
) 
rr
