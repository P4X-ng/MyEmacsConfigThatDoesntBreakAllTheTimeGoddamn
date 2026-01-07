#!/usr/bin/env bash

rsync -IvzHlr dot.emacs.d/ ~/.emacs.d/
rsync -IvzHlr dot.emacs.d/ /home/punk/.emacs.d/
chown -R punk:punk /home/punk/.emacs.d/
