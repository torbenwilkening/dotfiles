# Dotfiles #

## Emacs ##
### First Steps ###
* Install Emacs Prelude (https://github.com/bbatsov/prelude)
* Append init.el :
`curl -XGET https://raw.githubusercontent.com/torbenwilkening/dotfiles/master/emacs.d/init.el >> ~/.emacs.d/init.el`
### For Javascript Development ###
* Install neccessary packages via npm for ES6 and TypeScript development :
`npm install -g eslint babel-eslint eslint-plugin-react tern typescript typescript-tools`
### For GO Development ###
* Make sure that your $GOPATH is set and install go code (https://github.com/nsf/gocode) for auto completion
`go install github.com/nsf/gocode`
### For Python Development ###
* Make sure that virtualenv is installed, if not use easy_install or pip to install it :
  * easy_install : `easy_install virtualenv`
  * pip : `pip install virtualenv`
