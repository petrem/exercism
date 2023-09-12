def parse_date_toiso8601:
  capture("(?<date>\\d{4}-\\d{2}-\\d{2})(?:T(?<time>\\d{2}:\\d{2}:\\d{2})Z?)?")
  | .date + "T" + (.time // "00:00:00") + "Z";


.moment
  | parse_date_toiso8601
  | fromdateiso8601
  | . + 1000000000
  | todateiso8601
  | rtrimstr("Z")
