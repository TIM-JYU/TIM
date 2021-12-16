``` {settings=""}
description: "Force a paragraph onto a page when printing documents"
```

```scss
@media print {
  .parContent {
    page-break-inside: avoid !important;
  }
}
```