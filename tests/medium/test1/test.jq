(.ingredients + ."optional ingredients"
| map(select(.substitute? != null))         
| reduce .[] as $i ({}; .[$i.item] += [$i.substitute])
| map_values(join(", "))      )