### Old name RedisInsert
### New name haskell-redis-index-snippet

* index files
    * *AronModule.hs*
    * *AronLib.h*
    * *Aron.java*
    * *Print.java*
    * *snippet.hs*

### Update

* Tuesday, 28 December 2021 16:01 PST
* Add AronLib.h
* Refactor the code
    * Add a few functions

### UPDATE: Thu 30 Dec 16:30:06 2021 
* Refactor the code to use Emacs TAGS file under *haskell-redis-index-snippet*
```
    # haskell-redis-index-snippet/TAGS
    etags -e -f $PWD/TAGS $cpplib/AronLib.h $b/clib/AronCLibNew.h
```
