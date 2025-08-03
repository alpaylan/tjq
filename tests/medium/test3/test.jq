def production_rate_per_hour:
  . | if ((.>4) and (.<9)) then 0.9*. elif (.==9) then 0.8*. elif(.==10) then 0.77*. end | .*221
;

def working_items_per_minute:
  . | production_rate_per_hour | . / 60 | trunc
;

.speed | (production_rate_per_hour, working_items_per_minute)
