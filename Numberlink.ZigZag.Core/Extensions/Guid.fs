[<AutoOpen>]
module GuidExtensions

open System

module Guid =
    let generate (random: Random) =
        let bytes = Array.zeroCreate<byte> 16
        random.NextBytes(bytes)
        Guid(bytes)
