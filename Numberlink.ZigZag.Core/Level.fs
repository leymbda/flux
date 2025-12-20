namespace Numberlink.ZigZag.Core

open System

type Level = {
    Template: Template
    Links: Guid list list
}
