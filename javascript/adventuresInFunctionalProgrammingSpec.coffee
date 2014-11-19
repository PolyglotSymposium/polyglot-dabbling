addOne = (a) -> a + 1
double = (b) -> 2 * b
add = (a, b) -> a + b

describe 'compose', ->
  describe 'given 2 functions', ->
    it 'returns a function that, when applied, performs the application of both', ->
      expect(compose(double, addOne)(20)).toBe 42

describe 'curry', ->
  describe 'given a function that takes two arguments', ->
    it 'returns a partial add function', ->
      expect(curry(add)(35)(7)).toBe 42
