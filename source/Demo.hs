{-# LANGUAGE OverloadedStrings #-}

module Demo where

import           Project (Project, project, projectGroup)

_project :: Project
_project = projectGroup "Sweden" [stockholm, gothenburg, malmo]
  where
    stockholm = project 1 "Stockholm"
    gothenburg = project 2 "Gothenburg"
    malmo = projectGroup "Malmo" [city, limhamn]
    city = project 3 "Malmo City"
    limhamn = project 4 "Limhamn"
