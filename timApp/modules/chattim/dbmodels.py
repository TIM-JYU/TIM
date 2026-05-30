from typing import Optional
from sqlalchemy import Integer, String, ForeignKey, JSON
from sqlalchemy.ext.mutable import MutableList
from sqlalchemy.orm import Mapped, mapped_column, relationship
from sqlalchemy.dialects.postgresql import ARRAY
from timApp.timdb.sqa import db


class LLMRule(db.Model):
    """Associates an LLM plugin instance with an owner and defines rules for the instance."""

    __tablename__ = "llm_rule"

    id: Mapped[int] = mapped_column(primary_key=True)
    document_id: Mapped[int] = mapped_column(Integer, default=0)
    owner: Mapped[int] = mapped_column(ForeignKey("useraccount.id"))

    # API key fields
    public_key: Mapped[str] = mapped_column(String, default="")
    """The public key associated with the API key or the chosen key for the plugin."""
    provider: Mapped[str] = mapped_column(String, default="")
    """Provider of the API key. Only used when saving the key."""
    api_key: Mapped[str] = mapped_column(String, default="")
    """The API key. Only used when saving the key."""
    groups: Mapped[list[int]] = mapped_column(ARRAY(Integer), default=[])
    """User group IDs that have access to this API-key using `public_key`."""
    paths: Mapped[list[str]] = mapped_column(ARRAY(String), default=[])
    """Document or folder paths where the API key can be used on."""

    # Model options
    use_streaming: Mapped[bool] = mapped_column(default=False)
    """If `True`, the plugin uses streaming model response."""
    temperature: Mapped[Optional[float]] = mapped_column(nullable=True)
    """
    Optional temperature setting to use when creating the model response.
    If null, the temperature parameter is not used.
    """
    include_citations: Mapped[bool] = mapped_column(default=False)
    """Whether to include citations to the agent messages."""
    similarity_threshold: Mapped[Optional[float]] = mapped_column(nullable=True)
    """Threshold for a block to be included in the agent context. Should be between -1 and 1."""
    top_k_chunks: Mapped[int] = mapped_column(Integer, default=3)
    """Number of most relevant chunks to retrieve for context."""

    current_mode: Mapped[str] = mapped_column(
        String, default=""
    )  # summarizing or creative
    total_tokens_spent: Mapped[int] = mapped_column(Integer, default=0)
    indexed_document_ids: Mapped[list[int]] = mapped_column(ARRAY(Integer), default=[])
    system_prompt_path: Mapped[str] = mapped_column(String, default="")
    # TODO: this should probably be something like:
    # system_prompt_doc: Mapped[Optional[str]] = mapped_column(
    #     String, ForeignKey("docentry.name"), nullable=True
    # )

    agent: Mapped[str] = mapped_column(String, default="")
    conv_time_window: Mapped[int] = mapped_column(Integer, default=0)
    policy: Mapped[list["Policy"]] = relationship(
        "Policy", back_populates="llm_rule", cascade="all, delete-orphan"
    )
    usage: Mapped[list["Usage"]] = relationship(
        "Usage", back_populates="llm_rule", cascade="all, delete-orphan"
    )


class Policy(db.Model):
    """Defines usage limits for the instance."""

    __tablename__ = "policy"

    id: Mapped[int] = mapped_column(primary_key=True)
    for_user: Mapped[Optional[int]] = mapped_column(
        ForeignKey("useraccount.id"), nullable=True
    )
    llm_rule_id: Mapped[int] = mapped_column(
        ForeignKey("llm_rule.id", ondelete="CASCADE")
    )
    llm_rule: Mapped["LLMRule"] = relationship("LLMRule", back_populates="policy")
    token_time_window_type: Mapped[Optional[str]] = mapped_column(
        String, nullable=True
    )  # d,h,min,sec
    token_time_window_num: Mapped[Optional[int]] = mapped_column(
        Integer, nullable=True
    )  # 5 of type
    time_window_tokens: Mapped[Optional[int]] = mapped_column(
        Integer, nullable=True
    )  # token limit for the window
    max_tokens_per_user: Mapped[Optional[int]] = mapped_column(Integer, nullable=True)
    token_pool: Mapped[Optional[int]] = mapped_column(Integer, nullable=True)


class Usage(db.Model):
    """Associates a user and a conversation with an LLMRule context and tracks token usage."""

    __tablename__ = "usage"

    id: Mapped[int] = mapped_column(primary_key=True)
    user: Mapped[int] = mapped_column(ForeignKey("useraccount.id"))
    llm_rule_id: Mapped[int] = mapped_column(
        ForeignKey("llm_rule.id", ondelete="CASCADE")
    )
    llm_rule: Mapped[LLMRule] = relationship("LLMRule", back_populates="usage")
    used_tokens: Mapped[int] = mapped_column(Integer)
    token_usage_history: Mapped[list[dict[str, int]]] = mapped_column(
        MutableList.as_mutable(JSON),
        default=list,
    )  # [{"timestamp": 123, "tokens": 456}]
