debug | 40 as $expected_minutes_in_oven  
| 40 - (if(.actual_minutes_in_oven? ==null) then 0 else .actual_minutes_in_oven end )as  $remaining_minutes_in_oven 
| if(.number_of_layers? == null) then 2 else .number_of_layers?*2 end as $preparation_time 
| $preparation_time + (40 - $remaining_minutes_in_oven | abs) as $total_time |
{
    $expected_minutes_in_oven,
    $remaining_minutes_in_oven,
    $preparation_time,
    $total_time
}
