[ .name,

(.ingredients | length),


(.ingredients | .[] | select(.item == "sugar") | .amount.quantity)]
