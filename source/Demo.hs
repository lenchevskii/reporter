{-# LANGUAGE OverloadedStrings #-}

module Demo where

import           Project (Project (Project, ProjectGroup))

project :: Project
project = ProjectGroup "Sweden" [stockholm, gothenburg, malmo]
  where
    stockholm = Project 1 "Stockholm"
    gothenburg = Project 2 "Gothenburg"
    malmo = ProjectGroup "Malmo" [city, limhamn]
    city = Project 3 "Malmo City"
    limhamn = Project 4 "Limhamn"
