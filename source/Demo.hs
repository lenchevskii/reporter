{-# LANGUAGE OverloadedStrings #-}

module Demo where

import           Project (Project (Project, ProjectGroup), ProjectId)

project :: Project ProjectId
project = ProjectGroup "Sweden" [stockholm, gothenburg, malmo]
  where
    stockholm = Project "Stockholm" 1
    gothenburg = Project "Gothenburg" 2
    malmo = ProjectGroup "Malmo" [city, limhamn]
    city = Project "Malmo City" 3
    limhamn = Project "Limhamn" 4
