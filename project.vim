" project specific stuff
set autowrite
set makeprg=sbt\ test-compile
set efm=%E\ %#[error]\ %f:%l:\ %m,%C\ %#[error]\ %p^,%C\ %#[error]\ \ %m,%-C%.%#,%Z,
       \%W\ %#[warn]\ %f:%l:\ %m,%C\ %#[warn]\ %p^,%-C%.%#,%Z,
       \%-G%.%#
set ofu=syntaxcomplete#Complete
cd %:p:h
set path=%:p:h/**
au BufWritePost *.scala,*.java silent! !ctags -R -f %:p:h/tags --tag-relative=no %:p:h/src 
set tags=%:p:h/tags
e %:p:h
