`0x0` for Emacs
===============

`0x0` for Emacs defines three interactive functions:

- `0x0-upload` will upload the active region or if not existent the
  entire buffer to a `0x0.st` compatible server.
- `0x0-upload-file` will upload a selected file to a `0x0.st` compatible
  server.
- `0x0-upload-string` will upload a string to a `0x0.st` compatible
  server, that has been read in from the minibuffer.
  
By default, all these functions default to https://0x0.st as their
server of choice, but by customising the variable `0x0-default-host`,
this can be changed, or by prefixing the above mentioned commands with
a prefix argument. A list of servers is to be found in `0x0-services`.

See https://github.com/lachs0r/0x0 for more details, and consider
donating to https://liberapay.com/lachs0r/donate if you like the
service.

How to use
----------

Using [MELPA] and `use-package`, a minimal setup might look something like
this:

	(use-package 0x0
	  :bind ("F8" . 0x0-upload))

Bug reports and patches should be sent to my [public inbox].

Copying
-------

`0x0.el` is distributed under the [CC0 1.0 Universal (CC0 1.0) Public
Domain Dedication][cc0] license.

[MELPA]: https://melpa.org/#/0x0
[public inbox]: https://lists.sr.ht/~zge/public-inbox
[cc0]: https://creativecommons.org/publicdomain/zero/1.0/deed
