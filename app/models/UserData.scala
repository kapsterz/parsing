package models

/**
  * Created by wegod on 20.02.2017.
  */
case class UserData(requestValue: String = "Bing",
                    requestType: String = "proxy",
                    requestAnother: String = "",
                    numberOfPages: Int = 1,
                    pasreFromURL: String = "2",
                    urlToParse: String = "")
