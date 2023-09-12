def new_remote_control_car:
  {
    battery_percentage: 100,
    distance_driven_in_meters: 0,
    nickname: null
  };

def new_remote_control_car(nickname):
  new_remote_control_car | .nickname = nickname;

def display_distance:
  "\(.distance_driven_in_meters) meters";

def _has_juice: .battery_percentage > 0;

def display_battery:
  "Battery " + if _has_juice
               then "at \(.battery_percentage)%"
               else "empty"
               end;

def drive:
  if _has_juice
  then .distance_driven_in_meters += 20 | .battery_percentage -= 1
  else .
  end;
