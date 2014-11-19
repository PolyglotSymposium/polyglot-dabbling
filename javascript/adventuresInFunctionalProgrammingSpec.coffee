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

describe 'uncurry', ->
  it 'undoes curry', ->
      expect(uncurry(curry(add))(35, 7)).toBe 42

describe 'map', ->
  it 'applies a function across each param', ->
    expect([1, 2, 3].map(double)).toEqual [2, 4, 6]

describe 'array', ->
  it 'makes [] into []', ->
    expect(array([])).toEqual []
  it 'makes [42] into [42]', ->
    expect(array([42])).toEqual [42]
  it 'makes null into []', ->
    expect(array(null)).toEqual []
  it 'makes undefined into []', ->
    expect(array(undefined)).toEqual []
  it 'makes false into [false]', ->
    expect(array(false)).toEqual [false]
  it 'makes 42 into [42]', ->
    expect(array(42)).toEqual [42]
