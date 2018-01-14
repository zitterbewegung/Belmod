(def 'only-node-owner (node)
  (when (!= (caller) (get-owner node))
    (panic)))
