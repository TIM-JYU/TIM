``` {settings=""}
macros:
  subject:
  sendername:
  senderemail:
  recipients:
css: |!!
.messageheader p {
    font-size: small;
}
.messageheader {
    border-bottom-style: solid;
    border-bottom-width: thin;
    margin-bottom: 2em;
}
span.messagetitle {
    font-size: large;
    font-weight: bold;
}
!!
```

``` {plugin="timMenu" .hidden-print}
menu: |!!
- Read receipts
  rights: manage
  - [Read users](/timMessage/readReceipts?message_doc=%%docid%%&include_read=true)
  - [Unread users](/timMessage/readReceipts?message_doc=%%docid%%&include_unread=true)
  - [All users](/timMessage/readReceipts?message_doc=%%docid%%&include_unread=true&include_read=true)
!!
```

# - {.messageheader}

[%%subject%%]{.messagetitle} \
Sender: %%sendername%%, %%senderemail%% \
Recipients: %%recipients%%
