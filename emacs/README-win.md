# Windows Emacs config

On Windows, use:

``` bash
cd c:\dev # or c:\proj
git clone https://github.com/bbatsov/prelude.git
cd prelude
rm -rf personal
mklink /d personal C:\dev\config\emacs\personal
cd C:\Users\asm\AppData\Roaming
mklink /d .emacs.d c:\dev\prelude
```

things _should_ work

good luck, future Alex
