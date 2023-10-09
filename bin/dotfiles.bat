@echo off

set home=%USERPROFILE%\AppData\Roaming

echo %home%

del /F /Q %home%\.emacs.d\init.el
del /F /Q %home%\.emacs.d\plugins

mklink "%home%\.emacs.d\init.el" "%CD%\emacs.d\init.el"
mklink /D "%home%\.emacs.d\plugins" "%CD%\emacs.d\plugin"


del /F /Q %USERPROFILE%\.gitconfig
mklink "%USERPROFILE%\.gitconfig" "%CD%\gitconfig"
