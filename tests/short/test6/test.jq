[
(.ingredients | length),

(.ingredients[] | select(.item == "sugar") | .amount.quantity),

(.ingredients + ."optional ingredients" | map(select(has("substitute")) | {(.item): .substitute}) | add) ]
