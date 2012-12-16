# Up

Up is a clean and beautiful [Bootstrap](http://getbootstrap.com) based layout
for [Jekyll](https://github.com/mojombo/jekyll).

This is designed to be an easy layout to modify for your own blog. It was
based on [zachholman's](http://zachholman.com/) blog themes: the "old" one, now
opensourced as [left](http://github.com/holman/left), and also in his actual
theme, that's not opensource (I believe), but I steal some ideas anyway. I also
took something from [jekyll-bootstrap](https://github.com/plusjade/jekyll-bootstrap),
and, of course, I'm using [bootstrap](https://github.com/twitter/bootstrap) as
a base for all the thing.

![Up](http://i.imgur.com/4bKG5.png)

## Installation

- Install Jekyll: `gem install jekyll`
- Fork this repository
- Rename it to `YOUR-USER.github.com`
- Clone it: `git clone https://github.com/YOUR-USER/YOUR-USER.github.com`
- Run the jekyll server in the blog folder: `rake preview`.

You should have a server up and running locally at <http://localhost:4000>.

## Customization

Next you'll want to change a few things. The list of files you may want to
change is the following:

- [_config.yml](https://github.com/caarlos0/up/blob/gh-pages/_config.yml): Put
your config there, almost everything will be up and running.
- [about.html](https://github.com/caarlos0/up/blob/gh-pages/about.html): Well, that's
about you, I'll gonna change it if I am you... OH WAIT!
- [CNAME](https://github.com/caarlos0/up/blob/gh-pages/CNAME): If you're using
this on GitHub Pages with a custom domain name, you'll want to change this
to be the domain you're going to use. All that should be in here is a
domain name on the first line and nothing else (like: `example.com`).
- [favicon.ico](https://github.com/caarlos0/up/blob/gh-pages/favicon.ico): This
is a smaller version of my gravatar for use as the icon in your browser's
address bar. You should change it to whatever you'd like.
- [apple-touch-icon.jpg](https://github.com/caarlos0/up/blob/gh-pages/apple-touch-icon.jpg):
Again, this is my gravatar, and it shows up in iOS and various other apps
that use this file as an "icon" for your site.


## Deployment

You should deploy with [GitHub Pages](http://pages.github.com)- it's just
easier.

All you should have to do is rename your repository on GitHub to be
`username.github.com`. Since everything is on the `gh-pages` branch, you
should be able to see your new site at <http://username.github.com>.

## Licensing

This is [MIT](https://github.com/caarlos0/up/blob/master/LICENSE) with no
added caveats, so feel free to use this on your site without linking back to
me or using a disclaimer or anything silly like that.

If you'd like give [me](http://github.com/caarlos0),
[holman](http://github.com/holman)
(from [left](http://github.com/holman/left) layout),
[plusjade](https://github.com/plusjade)
(from [jekyll-bootstrap](https://github.com/plusjade/jekyll-bootstrap)),
[fat](https://github.com/fat) and [mdo](https://github.com/mdo) (from
[bootstrap](https://github.com/twitter/bootstrap)) credit somewhere on your
all-new blog or tweet a shout out to us, well hey, sure we'll take it.
