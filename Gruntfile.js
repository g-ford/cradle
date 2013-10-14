module.exports = function(grunt) {
  grunt.initConfig({
    pkg: grunt.file.readJSON('package.json'),
    concat: {
      dist: {
        src: [
          'bower_components/jquery/jquery.min.js',
          'bower_components/bootstrap/js/transition.js',
          'bower_components/bootstrap/js/collapse.js',
          '_assets/up.js'
        ],
        dest: 'js/up.js'
      }
    },
    uglify: {
      build: {
        src: 'js/up.js',
        dest: 'js/up.min.js'
      }
    },
    less: {
      development: {
        options: {
          paths: [
            '_assets/',
            'bower_components/bootstrap/less/',
            'bower_components/font-awesome/less/'
          ]
        },
        files: {
          'css/up.css': '_assets/up.less'
        }
      },
      production: {
        options: {
          paths: [
            '_assets/',
            'bower_components/bootstrap/less/',
            'bower_components/font-awesome/less/'
          ],
          yuicompress: true
        },
        files: {
          'css/up.css': '_assets/up.less'
        }
      }
    },
    watch: {
      scripts: {
        files: ['_assets/*.js'],
        tasks: ['concat', 'uglify'],
        options: {
          spawn: false,
        },
      },
      less: {
        // We watch and compile sass files as normal but don't live reload here
        files: ['_assets/*.less'],
        tasks: ['less'],
      },
    },
    copy: {
      main: {
        files: [
          {
            expand: true,
            flatten: true,
            cwd: 'bower_components/font-awesome/font/',
            src: '**',
            dest: 'css/font/',
            filter: 'isFile'
          }
        ]
      }
    }
  });

  // Load the plugins
  grunt.loadNpmTasks('grunt-contrib-concat');
  grunt.loadNpmTasks('grunt-contrib-uglify');
  grunt.loadNpmTasks('grunt-contrib-less');
  grunt.loadNpmTasks('grunt-contrib-watch');
  grunt.loadNpmTasks('grunt-contrib-copy');

  // Default task(s).
  grunt.registerTask('default', ['concat', 'uglify', 'less', 'copy']);
};
