


echo %3\ > eco.txt
del %2\%1.boh
del %2\%1.post.res
del %2\%1.post.dat
del %2\%1.err


call Rscript.exe '%3'\desp3d.R '%2\%1'

