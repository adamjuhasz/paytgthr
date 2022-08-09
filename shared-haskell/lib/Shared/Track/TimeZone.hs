module Shared.Track.TimeZone where

import           Data.Text                      ( Text )

stateToTZ :: Text -> Text
stateToTZ "AL" = "US/Central"
stateToTZ "AK" = "US/Alaska"
stateToTZ "AZ" = "US/Arizona"
stateToTZ "AR" = "US/Central"
stateToTZ "CA" = "US/Pacific"
stateToTZ "CO" = "US/Mountain"
stateToTZ "CT" = "US/Eastern"
stateToTZ "DE" = "US/Eastern"
stateToTZ "DC" = "US/Eastern"
stateToTZ "FL" = "US/Eastern"
stateToTZ "GA" = "US/Eastern"
stateToTZ "HI" = "US/Hawaii"
stateToTZ "ID" = "US/Mountain"
stateToTZ "IN" = "US/Eastern"
stateToTZ "IA" = "US/Central"
stateToTZ "KS" = "US/Central"
stateToTZ "KY" = "US/Central"
stateToTZ "LA" = "US/Central"
stateToTZ "ME" = "US/Eastern"
stateToTZ "MD" = "US/Eastern"
stateToTZ "MA" = "US/Eastern"
stateToTZ "MI" = "US/Michigan"
stateToTZ "MN" = "US/Central"
stateToTZ "MS" = "US/Central"
stateToTZ "MO" = "US/Central"
stateToTZ "MT" = "US/Mountain"
stateToTZ "NE" = "US/Central"
stateToTZ "NV" = "US/Pacific"
stateToTZ "NH" = "US/Eastern"
stateToTZ "NJ" = "US/Eastern"
stateToTZ "NM" = "US/Mountain"
stateToTZ "NY" = "US/Eastern"
stateToTZ "NC" = "US/Eastern"
stateToTZ "ND" = "US/Central"
stateToTZ "OH" = "US/Eastern"
stateToTZ "OK" = "US/Central"
stateToTZ "OR" = "US/Pacific"
stateToTZ "PA" = "US/Eastern"
stateToTZ "RI" = "US/Eastern"
stateToTZ "SC" = "US/Eastern"
stateToTZ "SD" = "US/Mountain"
stateToTZ "TN" = "US/Central"
stateToTZ "TX" = "US/Central"
stateToTZ "UT" = "US/Mountain"
stateToTZ "VT" = "US/Eastern"
stateToTZ "VA" = "US/Eastern"
stateToTZ "WA" = "US/Pacific"
stateToTZ "WV" = "US/Eastern"
stateToTZ "WI" = "US/Central"
stateToTZ "WY" = "US/Mountain"
stateToTZ _    = "US/Central"
