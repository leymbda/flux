[<AutoOpen>]
module ListExtensions

module List =
    /// Fold back over a list with the index of each element.
    let foldBacki folder list state =
        let rec loop i acc list =
            match list with
            | [] -> acc
            | x :: xs ->
                let acc' = folder i x acc
                loop (i + 1) acc' xs

        loop 0 state list
