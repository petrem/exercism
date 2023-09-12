# (ab)using variables

[ 40
, (.actual_minutes_in_oven // 0)
, (.number_of_layers // 1)
] as [$TOTAL_TIME, $actual_time, $layers]
  | (2 * $layers) as $prep_time
  | {
    "expected_minutes_in_oven": $TOTAL_TIME,
    "remaining_minutes_in_oven": ($TOTAL_TIME - $actual_time),
    "preparation_time": $prep_time,
    "total_time": ($prep_time + $actual_time)
  }
