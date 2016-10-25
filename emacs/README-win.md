On Windows, use:

# git clone prelude into C:\dev\prelude
cd c:\dev\prelude
rm -r personal
mklink /d personal C:\dev\config\emacs\personal
cd C:\Users\asm\AppData\Roaming
mklink /d .emacs.d c:\dev\prelude

things _should_ work

good luck, future Alex
