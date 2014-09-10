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
        i = if i < (array.length - 1) then i+1 else 0
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
        buzz = cycle ["", "", "", "", "buzz"]
        () ->
            n = nextint().toString()
            fizz() + buzz() or n
