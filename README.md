# `xray`

Macros for debugging and understanding clojure code flow.

## Bugs and Enhacements

Please open issues against the [official xray repo on Github]
https://github.com/jpmonettas/xray/issues

## Usage


Ahhhhh!!! crazy gmail client!

(dlet [a 5
        ?b (+ a 2)]   ;; Notice the ? mark
       a)

is expanding to:

(let [a 5
       b (do 

