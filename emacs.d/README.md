# My Emacs Configuration

My Emacs configuration written as literate program using amazing org mode.

I'm using unusual way of dependency management for a configuration.
I use Cask and pallet to manage all dependecies as I don't really like to
handle the loading of packages within the configuration itself.

## Install Cask

This is how to install Cask on any unix like system. If this method doesn't work please see [documentation](https://github.com/cask/cask)

```shell
$ curl -fsSL https://raw.githubusercontent.com/cask/cask/master/go | python
```

## Configuration Details

All the important information is captured in configuration itself.
Please see [turbo_mack.org](turbo_mack.org) for details.
