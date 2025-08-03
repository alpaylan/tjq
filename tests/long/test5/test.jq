def check_monotone:
  (sort == .) or ((sort | reverse) == .);

def check_inc:
  . as $arr
  | [ range($arr|length - 1)
      | . as $i
      | ($arr[$i+1] - $arr[$i]
         | abs
         | (.<=3 and .!=0)
        )
    ];

def is_safe:
  check_monotone and (check_inc | all);

def dampener:
  if is_safe then
    true
  else
    . as $arr
    | [ range($arr|length)
        | . as $i
        | ($arr | del(.[$i]))
        | is_safe
      ]
    | any
  end;

. as $rows
| [ $rows[] | select(dampener) ]
| length
| [.]