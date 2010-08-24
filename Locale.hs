module Yanagi.Locale where


import System.Locale

jaTimeLocale :: TimeLocale
jaTimeLocale = TimeLocale { wDays = [("Sunday",    "日")
                                    ,("Monday",    "月")
                                    ,("Tuesday",   "火")
                                    ,("Wednesday", "水")
                                    ,("Thursday",  "木")
                                    ,("Friday",    "金")
                                    ,("Saturday",  "土")
                                    ]
                          ,months = [("January",  "1")
                                    ,("February", "2")
                                    ,("March",    "3")
                                    ,("April",    "4")
                                    ,("May",      "5")
                                    ,("June",     "6")
                                    ,("July",     "7")
                                    ,("August",   "8")
                                    ,("September","9")
                                    ,("October", "10")
                                    ,("November","11")
                                    ,("December","12")
                                    ]
                          , intervals = [("year","年")
                                        ,("month","月")
                                        ,("day","日")
                                        ,("hour","時")
                                        ,("min","分")
                                        ,("sec","秒")
                                        ,("usec","usecs")
                                        ]
                          , amPm = ("午前","午後")
                          , dateTimeFmt = "%a %b %e %H:%M:%S %Z %Y"
                          , dateFmt = "%m/%d/%y"
                          , timeFmt = "%H:%M:%S"
                          , time12Fmt = "%I:%M:%S %p"
                          }
