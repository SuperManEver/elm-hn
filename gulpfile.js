var gulp        = require('gulp');
var sass        = require('gulp-sass');

var bs          = require('browser-sync').create();
var elm         = require('gulp-elm');

var paths = {
  dest: 'build',
  elm: 'src/*.elm',  
};

// Initialize gulp-elm
gulp.task('elm-init', elm.init);

gulp.task('make', ['elm-init'], () => {
  return gulp.src('src/App.elm')
    .pipe(elm())
    .pipe(gulp.dest(paths.dest))
});

gulp.task('browser-sync', () => {
  bs.init({
    server : {
      baseDir : "./"
    }
  });
});

gulp.task('sass', () => {
  return gulp.src('scss/main.scss')
   .pipe(sass())
   .pipe(gulp.dest('build/'))
   .pipe(bs.reload({ stream : true }));
});

gulp.task('watch', ['browser-sync'], () => {
  gulp.watch("scss/**/*.scss", ['sass']);
  gulp.watch('build/*.*').on('change', bs.reload);
  gulp.watch('index.html').on('change', bs.reload);
});
