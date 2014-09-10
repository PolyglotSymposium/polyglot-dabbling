describe 'Fizzbuzz sequence', ->
    next = null
    beforeEach ->
        next = window.fizzbuzz.init()
    it 'should have 1, 2 as its first two elements', ->
        expect(next()).to.equal '1'
        expect(next()).to.equal '2'
    it 'should have fizz as its first third element', ->
        expect(nth next, 3).to.equal 'fizz'
    it 'should have 4 as its fourth element', ->
        expect(nth next, 4).to.equal '4'
    it 'should have buzz as its first fifth element', ->
        expect(nth next, 5).to.equal 'buzz'
