[<AutoOpen>]
module MapExtensions

module Map =
    /// Create a singleton map with one key-value pair.
    let singleton key value =
        Map.empty |> Map.add key value
