# Crashes 2018

This is the data, analysis, and source code for the interactive
[Out most fatal morning](https://insights.nzherald.co.nz/article/crashes-2018/)

This code has been made public pretty much as is once the article has been published.

We (the _New Zealand Herald_ data journalism team) are trying out the apporach of just
releasing data, analysis, and source code once an article is published - rather than
planning to tidy up the code and then releasing it. Hopefully this is helpful and makes
our reporting more transparent. If you would like something to be better documented
please contact me (Chris) on twitter (@vizowl) or by email chris.knox@nzherald.co.nz

# License

The data, analysis, and visualisations are released under a Creative Commons attribution license
[CC BY 4](https://creativecommons.org/licenses/by/4.0/). You can use it, but please
attribute the _New Zealand Herald_ and we would prefer it if you got in touch and let us know
how you are using it.

The data was released to the _New Zealand Herald_ by _NZTA_ under the Official Information
Act, and the original data file is `data/crashes by severity and hour.csv` - data released
under OIA generally has no explicit license so is treated as falling under the general
[NZGOAL Framework](https://www.ict.govt.nz/guidance-and-resources/open-government/new-zealand-government-open-access-and-licensing-nzgoal-framework/)


# General approach

The source code is broken into 4 directories, data, analysis, preparation, and interactive.

Haskell's [shake](http://hackage.haskell.org/package/shake) build system is used to marshal
all the data into a database. The analysis directory is where open ended analysis is carried out. These analysis
scripts are often not complete - but show some of the directions looked at. The build process
converts data into (usually JSON) and drops it into the interactive directory.

All the data build products are checked into git so that it is not necessary to have to run the
haskell, R, and PostgreSQL portions.

The actual interactive is an [Elm](http://elm-lang.org/) app. 

There are a lot of reasons for working in Haskell and Elm - which I won't document here -
but the primary motivation is to be confident in the struture of all the data we have marshaled - both now
and in a year when we revisit this article.


# Building the code

This article uses a lot of tools so getting it running may be frustrating - and it may
not be possible on a Windows computer.

## Prerequisites

__The build will drop a local PostgreSQL database called 'crashes-18' so DO NOT run it if this will cause you problems__

- [Haskell stack](https://docs.haskellstack.org/en/stable/README/)
- [R](https://www.r-project.org/)
- [Node](https://nodejs.org/en/)
- [PostgreSQL](https://www.postgresql.org/)

R will need to have the following libraries installed.

```r
library(tidyverse)
library(ggthemes)
library(RPostgreSQL)
library(zoo)
library(lubridate)
library(here)
library(knitr)
library(stats)
```

You will need to run the build as a user that can access a PostreSQL server without
a password and create new databases.

Then just run `stack build --exec build` and this should create the database _crashes-18_ and
prepare all the data and produce an interactive in `interactive/dist`