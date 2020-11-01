`0x0` for Emacs
===============

`0x0` for Emacs defines a few commands:

- `0x0-upload` will upload the active region or if not existent the
  entire buffer
- `0x0-upload-file` will upload a selected file
- `0x0-upload-string` will upload a string
- `0x0-upload-kill-ring` uploads the content of the kill ring.
- `0x0-popup` creates and displays a buffer and lets you uploads it
  contents later.

By default, all these functions default to https://0x0.st as their
server of choice, but by customising the variable `0x0-default-host`,
this can be changed, or by prefixing the above mentioned commands with
a prefix argument. A list of servers is to be found in `0x0-services`.

[Curl][curl] will be used to accelerate defaults, if installed. The
specific behaviour can be configured using `0x0-use-curl`.

See https://github.com/lachs0r/0x0 for more details, and consider
donating to https://liberapay.com/lachs0r/donate if you like the
service.

Bug reports and patches should be sent to my [public inbox].

---

`0x0.el` is distributed under the [CC0 1.0 Universal (CC0 1.0) Public
Domain Dedication][cc0] license.

[curl]: https://curl.haxx.se/
[MELPA]: https://melpa.org/#/0x0
[public inbox]: https://lists.sr.ht/~zge/public-inbox
[cc0]: https://creativecommons.org/publicdomain/zero/1.0/deed
