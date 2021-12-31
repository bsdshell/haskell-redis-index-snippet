### Old name RedisInsert
### New name haskell-redis-index-snippet

* index files
    * *AronModule.hs*
    * *AronLib.h*
    * *$b/clib/AronCLibNew.h
    * *Aron.java*
    * *Print.java*
    * *snippet.hs*

### Update

* Tuesday, 28 December 2021 16:01 PST
* Add AronLib.h
* Refactor the code
    * Add a few functions

### UPDATE: Thu 30 Dec 16:30:06 2021 
# See *$scr/cpp_etags_emacs.sh*
* Refactor the code to use Emacs TAGS file under *haskell-redis-index-snippet*
```
    # Tags file →  haskell-redis-index-snippet/TAGS
    etags -e -f $PWD/TAGS $cpplib/AronLib.h $b/clib/AronCLibNew.h
           ↑ 
           + -> -e must be before -f
```

* TAGS file format
    int max(int a, int b, int c){max132,2815
    int max(int a, int b, int c, int d){max136,2878
    string reverseStr(string s){reverseStr140,2956
```
