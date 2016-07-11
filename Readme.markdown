# Credit #

This is my personal site.
It's based pretty heavily upon the following sites:
* [Jaspervdj's](https://github.com/jaspervdj/jaspervdj) (creater of haykll)
* [blaenk's](https://github.com/blaenk/blaenk.github.io)
* [Stephen Diehl's](http://www.stephendiehl.com/)

## ToDo ##
- Customise styling to make it more 'me'.
- add support for 'updated dates'. <https://david.sferruzza.fr/posts/2014-06-18-new-blog-with-hakyll.html>
- automate Resume / CV building

### Deployment workflow (Rough thoughts)

- Commit to master
- CI builds
- CI tests
- CI runs below
- webhook with new source and restart docker container
- new site is served, yay!

~~~{.bash}
git clone -b html_source  git@github.com:KyleOndy/kyleondy.com.git html_source
rm -r html_source/*
cp -a _site/* html_source
cd html_source
git add .
git commit --no-gpg-sign -m "Commit message"
git push
~~~

Resources are cheap. One container per site. East to remove / add on the fly
sad
