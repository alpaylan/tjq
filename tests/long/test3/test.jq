

. as $rows
# extract the two columns
| {
    list1: ($rows | map(.[0])),
    list2: ($rows | map(.[1]))
  } as $lists

# build a frequency map of the second column
| {
    l1: $lists.list1,
    t1: $lists.list2
         | sort
         | group_by(.)
         | map({ v: .[0], c: length })
  } as $int

# turn that freq list into a lookup object {"val":count, …}
| ($int.t1
   | map({ "\(.v)": .c })
   | add
) as $freq

# multiply each element of list1 by its matching frequency
| {
    res: ($int.l1
           | map(. * ($freq["\(.)"] // 0))
           | add)
  }
| .res | [.]