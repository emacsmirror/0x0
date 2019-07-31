`0x0` for Emacs
===============

`0x0` for Emacs defines three interactive functions:

- `0x0-upload` will upload the active region or if not existent the
  entire buffer to a `0x0.st` compatible server.
- `0x0-upload-file` will upload a selected file to a `0x0.st` compatible
  server.
- `0x0-upload-string` will upload a string to a `0x0.st` compatible
  server, that has been read in from the minibuffer.
  
By default, all these functions default to `0x0.st` as their server of
choice, but by customising the variable `0x0-url`, this can be changed.

Bug reports and patches should be sent to my [public inbox].

Copying
-------

`0x0.el` is distributed under the [CC0 1.0 Universal (CC0 1.0) Public
Domain Dedication][cc0] license.

[public inbox]: https://lists.sr.ht/~zge/public-inbox
[cc0]: https://creativecommons.org/publicdomain/zero/1.0/deed
