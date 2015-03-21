namespace Fools

module OneToManyMap =
  let findSet k m =
    match Map.tryFind k m with
    | None -> Set.empty
    | Some s -> s

  let add k v m =
    let set = findSet k m
    if Set.contains v set
    then failwithf "already added %A %A" k v
    Map.add k (Set.add v set) m

  let remove k v m =
    let set = findSet k m
    if not <| Set.contains v set
    then failwith "was not added"
    let set' = Set.remove v set
    if Set.isEmpty set'
    then Map.remove k m
    else Map.add k set' m
