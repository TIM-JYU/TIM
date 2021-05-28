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

#- {.messageheader}
[%%subject%%]{.messagetitle} \
Sender: %%sendername%%, %%senderemail%% \
Recipients: %%recipients%%
