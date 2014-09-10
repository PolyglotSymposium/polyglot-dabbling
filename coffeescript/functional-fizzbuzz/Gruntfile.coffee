module.exports = (grunt) ->
    grunt.initConfig
        watch:
            src:
                files: 'src/*.coffee',
                tasks: ['coffee:compile', 'specs']
            specs:
                files: 'specs/*.coffee',
                tasks: ['coffee:compile', 'specs']
        coffee:
            compile:
                expand: true,
                flatten: true,
                files:
                    'build/fizzbuzz.spec.js': ['specs/*.coffee'],
                    'build/fizzbuzz.js': ['src/*.coffee']
        mocha:
            options:
                run: true,
                reporter: 'Spec'
            test:
                src: ['specs/specs.html']

    grunt.loadNpmTasks 'grunt-contrib-coffee'
    grunt.loadNpmTasks 'grunt-contrib-watch'
    grunt.loadNpmTasks 'grunt-mocha'

    grunt.registerTask 'specs', ['coffee', 'mocha'] 
    grunt.registerTask 'default', ['specs'] 
    grunt.registerTask 'dev', ['default', 'watch'] 
