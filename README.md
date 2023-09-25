# rfcinfo.el

## An emacs package for RFCs

This is an emacs package for:

 - displaying and browsing RFC's status information
 - downloading and jumping to RFC locations

If you don't know what RFCs are, then you certainly want to stop
reading here and read something more useful for you.

### Getting status information

Status information is extracted from IETF-provided xml file
(downloaded through ange-ftp).

Information displayed includes:

 - title, author, date of publication
 - official status
 - links to dependency-related RFCs (with visual information about their own status)
   - RFCs obsolated or updated by this one
   - RFCs that obsolate or update this one
 - and more

As an example, here's what's displayed for rfc2865:

![information for RFC2865](https://github.com/cdeleuze/rfcinfo.el/wiki/2865.png)

If the RFC has an errata page (as is the case here), you can open it
in your favorite browser with a single keystroke.

### Reading RFC text

#### Downloading and viewing

You can download an RFC from a keystroke.  Downloaded RFCs are kept in
a local cache for later faster or off-line reading.

If `rfcview-mode` or `irfc-mode` is used to view RFCs, a few keys are
also added to their key map so that rfcinfo functions can easily be
called from the buffer displaying the RFC.  However, rfcinfo does not
require these modes to be loaded or available.

#### Jumping to locations

The package also allows to define references to specific locations in
an RFC.  Consider the following sentence:

> The AA bit indicates the data matching the question name is
> authoritative (rfc1034-6.2.7)

If you set point in the above RFC reference, you can either get status
information on rfc1034 or open rfc1034 (downloading it if necessary)
and setting point at header of section 6.2.7.

### More details

1. Read the small tutorial in the wiki
2. Have a look at the (commented) source!
