def calc:
  . as $rows
  | {
      list1: ($rows | map(.[0]) | sort),
      list2: ($rows | map(.[1]) | sort)
    } as $lists
  | {
      res:
        [ $lists.list1, $lists.list2 ]
        | transpose
        | map(.[0] - .[1])
        | map(abs)
        | add
    }
  | .res | [.];

calc