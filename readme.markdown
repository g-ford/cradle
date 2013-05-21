> This is the new WIP version. You can take a look at old 1.4.0 release
[here](https://github.com/caarlos0/up/tree/v1.4.0).

Up
--------------

Up is a clean and beautiful [Bootstrap](http://getbootstrap.com) based layout
for [Jekyll](https://github.com/mojombo/jekyll).

This is designed to be an easy layout to modify for your own blog. It is
based on [zachholman's](http://zachholman.com/) blog themes: the "old" one, now
opensourced as [left](http://github.com/holman/left), and also in his actual
theme, that's not opensource (I believe), but I stole some ideas anyway. I also
took something from [jekyll-bootstrap](https://github.com/plusjade/jekyll-bootstrap),
and, of course, I'm using [bootstrap](https://github.com/twitter/bootstrap) as
a base for whole thing.

![Up 2](http://f.cl.ly/items/3S2m1X2I1V0s0E2P1s38/Captura%20de%20Tela%202013-04-08%20%C3%A0s%2000.37.30.png)

## Installation

- [Fork this repository](https://github.com/caarlos0/up/fork)
- Rename it to `YOUR-USER.github.com`
- Clone it: `git clone https://github.com/YOUR-USER/YOUR-USER.github.com`
- Run the bundler in the blog folder to get the dependencies: `bundle`
- Run the jekyll server: `rake preview`.

You should have a server up and running locally at <http://localhost:4000>.

## Customization

Next you'll want to change a few things. The list of files you may want to
change is the following:

- [_config.yml](https://github.com/caarlos0/up/blob/gh-pages/_config.yml): Put
your config there, almost everything will be up and running.
- [about.html](https://github.com/caarlos0/up/blob/gh-pages/about.html): Well, that's
about you, I would change it if I were you... OH WAIT!
- [CNAME](https://github.com/caarlos0/up/blob/gh-pages/CNAME): If you're using
this on GitHub Pages with a custom domain name, you might want to change this to be
the domain you're going to use. All that should be in here is a
domain name on the first line and nothing else (like: `example.com`).
- [favicon.ico](https://github.com/caarlos0/up/blob/gh-pages/favicon.ico): This
is a smaller version of my gravatar for use as the icon in your browser's
address bar. You may change it to whatever you like.
- [apple-touch-icon.jpg](https://github.com/caarlos0/up/blob/gh-pages/apple-touch-icon.jpg):
Again, this is my gravatar, and it shows up in iOS and various other apps
that use this file as an "icon" for your site.

### Custom CSS/JS

If you need custom CSS or JS, you will need `node.js'` `npm` executable in
your `PATH`, as well `recess` and `uglify-js`. To do this, after installed
`npm`, in your blog folder, run: `npm install`.

It will install `recess` and `uglify-js` executables for you. Now, do your
changes in `less` and/or `js` files, and run `make` to compile the files.

Note: I'm not using any Jekyll asset pipeline because it's not supported
by [GitHub Pages](http://pages.github.com), so, I prefer to do it by myself.


### Update `favicon` and `apple-precomposed` icons based on gravatar

First, be sure you have the author email configured in `_config.yml`,
then, just run:

```sh
rake icons
```

The script will generate your email hash and get your gravatar, then, using
RMagick, it will create all needed icons.


## Deployment

You should deploy with [GitHub Pages](http://pages.github.com)- it's just
easier.

All you should have to do is to rename your repository on GitHub to be
`username.github.com`. Since everything is on the `gh-pages` branch, you
should be able to see your new site at <http://username.github.com>.


## Licensing

This is [MIT](https://github.com/caarlos0/up/blob/master/LICENSE) with no
added caveats, therefore feel free to use this on your site without
linking back to me or using a disclaimer or anything silly like that.

If you'd like give [me](http://github.com/caarlos0),
[holman](http://github.com/holman)
(from [left](http://github.com/holman/left) layout),
[plusjade](https://github.com/plusjade)
(from [jekyll-bootstrap](https://github.com/plusjade/jekyll-bootstrap)),
[fat](https://github.com/fat) and [mdo](https://github.com/mdo) (from
[bootstrap](https://github.com/twitter/bootstrap)) credit somewhere on your
all-new blog or tweet a shout out to us, well hey, sure we'll take it.

## Donate

You can also thank me doing a donation =)

[![Donate](https://www.paypalobjects.com/en_US/i/btn/btn_donate_LG.gif)](https://www.paypal.com/cgi-bin/webscr?cmd=_s-xclick&hosted_button_id=DXEJBUD2KYT7L)

Thanks.
