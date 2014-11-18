addOne = (a) -> a + 1
double = (b) -> 2 * b

describe 'compose', ->
  describe 'given 2 functions', ->
    it 'returns a function that, when applied, performs the application of both', ->
      expect(compose(double, addOne)(20)).toBe 42
