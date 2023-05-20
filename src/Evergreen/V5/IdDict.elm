module Evergreen.V5.IdDict exposing (..)


type NColor
    = Red
    | Black


type IdDict k v
    = RBNode_elm_builtin NColor String v (IdDict k v) (IdDict k v)
    | RBEmpty_elm_builtin
