## functional

@nth = (generator, n) ->
    i = n
    while i > 1
        generator()
        i--
    generator()

@cycle = (array) ->
    i = -1
    () ->
        if i < (array.length - 1) then i++ else 0
        array[i]

## fizzbuzz

count = (() ->
    x = 0
    () ->
        x++
        x
)

@fizzbuzz = 
    init: () ->
        nextint = count()
        fizz = cycle ["", "", "fizz"]
        () ->
            fizz() or nextint().toString()
