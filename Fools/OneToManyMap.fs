namespace Fools

module OneToManyMap =
  let findSet k m =
    match Map.tryFind k m with
    | None -> Set.empty
    | Some s -> s

  let add k v m =
    let set = findSet k m
    Map.add k (Set.add v set) m

  let remove k v m =
    let set = findSet k m
    let set' = Set.remove v set
    if Set.isEmpty set'
    then Map.remove k m
    else Map.add k set' m
