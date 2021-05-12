/**
 * Specifies how the iteration of document {@link Paragraph}s should proceed when
 * encountering a {@link ReferenceParagraph}.
 */
export enum DerefOption {
    /**
     * When iterating the document, yield the target paragraph(s) of the {@link ReferenceParagraph}.
     */
    Deref,

    /**
     * When iterating the document, yield only the {@link ReferenceParagraph} itself.
     */
    NoDeref,
}
