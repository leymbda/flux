namespace Numberlink.ZigZag.Core

open System

type Level = {
    Id: Guid
    Template: Template
    Links: Guid list list
}

module Level =
    let generate (id: Guid) (template: Template) =
        let links = [] // TODO: Implement WFC and calculate links from generated graph

        { Id = id; Template = template; Links = links }
