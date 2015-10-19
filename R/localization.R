with_sensible_localization <- function(code)
{
  if(.Platform$OS.type == "windows")
  {
    lang <- Sys.getenv("LANG", "en")
    lc_ctype <- Sys.getlocale("LC_CTYPE")
    new_lc_ctype <- switch(
      lang,
      de = "German_Germany",
      fr = "French_France",
      hu = "Hungarian_Hungary",
      kr = "Korean_Korea",
      nl = "Dutch_Netherlands",
      ru = "Russian_Russia",
      sv = "Swedish_Sweden",
      tr = "Turkish_Turkey",
      uk = "Ukranian_Ukraine",
      "English_United States"
    )
    devtools::with_locale(c(LC_CTYPE = new_lc_ctype), code)
  } else
  {
    lc_messages <- Sys.getlocale("LC_MESSAGES")
    if(!grepl("utf8", lc_messages))
    {
      new_lc_messages <- paste0(lc_messages, ".utf8")
    }
    devtools::with_locale(c(LC_MESSAGES = new_lc_messages), code)
  }
}