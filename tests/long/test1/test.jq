def get_index(arr; elem):
  arr | to_entries | map(select(.value == elem)) | .[0].key;

def rebuild_tree(preorder; inorder):
  if (preorder | length) == 0 then
    null
  else
    preorder[0] as $root |
    get_index(inorder; $root) as $root_idx |
    {
      "value": $root,
      "left": rebuild_tree(
        preorder[1:$root_idx+1];  #left of root  from the preorder
        inorder[0:$root_idx] #similarly LHS of root
      ),
      "right": rebuild_tree( #RHS of root this time
        preorder[$root_idx+1:]; 
        inorder[$root_idx+1:]
      )
    }
  end;


. | rebuild_tree(.preorder; .inorder)