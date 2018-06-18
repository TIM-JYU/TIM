DocParagraph
============

A :class:`~timApp.documentmodel.docparagraph.DocParagraph` has the following basic properties:

* markdown content (md)
* identifier (id)
* attributes (attrs)
* hash (t)

Markdown content
----------------
Markdown content is currently Pandoc flavour with some customizations. For Pandoc documentation, see
http://pandoc.org/MANUAL.html for more information.

Customizations
..............
TODO

Identifier
----------
The identifier is a random alphanumeric string of length 12. The last character is a checksum of the first 11 ones
to prevent accidental modification of the identifier.

Attributes
----------
A paragraph can have any user-defined attributes. Certain attributes have a reserved meaning.

An attribute can be either a key-value pair (`x=y`) or a class name (`.name`).

The attribute `taskId=something` has a shorthand syntax `#something`. It can be regarded as a user-defined
identifier, so the name `taskId` does not perfectly describe all of its use cases.

The special attributes are the following:

* `rd`: The paragraph is a reference to another paragraph or section, usually in a different document. The HTML
  content for the paragraph is retrieved from the referenced paragraph (unless this is a non-empty translated
  paragraph; see the `tr` attribute). When specified, this attribute must be accompanied with `ra` or `rp`
  attribute, but not both. Value = id of the referenced document.
* `ra`: The paragraph is a reference to a named section. Value = the name of the area in the referenced document.
* `rp`: The paragraph is a reference to a paragraph. Value = the id of the paragraph.
  in the referenced document.
* `rt`: The hash of the referenced paragraph. This is valid only when rd and rp are defined.
* `rl`: If defined, either `force` or `no`. If `force`, a link to the source paragraph is rendered in the
  document view.
* `r`: If defined, either `c` or `tr`. The value `tr` denotes this is a translated paragraph, and the `rp` and `rd`
  attributes identify the source paragraph.
* `settings` : The paragraph contains the settings of a document. Currently this must be the first paragraph of the
  document. The value of this attribute is not used.
* `plugin` : The paragraph contains a plugin. Value = the type of the plugin.
* `.nonumber` : Any headings contained in the paragraph should not be autonumbered.
* `question` : The paragraph is a question. The markdown content is a YAML block containing the question data.
  Value = ???

Hash
----
The hash of the paragraph is based on the markdown content and the attribute values.
