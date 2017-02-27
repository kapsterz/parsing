package models.forms

/**
  * Created by wegod on 20.02.2017.
  */
case class UserData(requestValue: String = "Bing",
                    requestType: String = "proxy",
                    requestAnother: String = "proxy+list",
                    numberOfPages: Int = 100,
                    pasreFromURL: String = "2",
                    urlToParse: String = "")
