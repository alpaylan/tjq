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

. as $rows

| [ $rows[]
    | select(check_monotone)
    | check_inc
    | select(all)
  ]
| length | [.]
