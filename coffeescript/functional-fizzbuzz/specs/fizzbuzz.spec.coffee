describe 'Fizzbuzz sequence', ->
    next = null
    beforeEach ->
        next = window.fizzbuzz.init()
    it 'should have 1, 2 as its first two elements', ->
        expect(next()).to.equal '1'
        expect(next()).to.equal '2'
    it 'should have fizz as its first third element', ->
        expect(nth(next, 3)).to.equal 'fizz'
